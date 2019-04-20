use crate::environment::Environment;
use crate::paramexp::ParamExpr;
use crate::parse_assignment_word;
use crate::string::ShellString;
use failure::{bail, format_err, Error, Fallible};
use lazy_static::lazy_static;
use regex::{Captures, Regex};
use std::convert::{TryFrom, TryInto};
use std::ffi::OsString;
use std::fmt::Write;

lazy_static! {
    static ref TILDE_RE: Regex =
        Regex::new(r"(~$|~/|~([a-zA-Z_][a-zA-Z0-9_]+))").expect("failed to compile TILDE_RE");
}

#[derive(Debug, PartialEq, Eq)]
pub enum WordComponent {
    Literal(String),
    LiteralOs(OsString),
    TildeExpand(Option<String>),
    AssignmentName(String),
    ParameterExpansion(ParamExpr),
    FieldSplittable(String),

    /// An internal state while producing an ExpandableWord;
    /// the string is a candidate for ParameterExpansion and
    /// later expansion stages
    DollarExpandable(String),
    CheckForQuotes(String),
    TildeExpandable(String),
}

impl TryFrom<WordComponent> for String {
    type Error = Error;
    fn try_from(component: WordComponent) -> Fallible<String> {
        match component {
            WordComponent::Literal(s)
            | WordComponent::AssignmentName(s)
            | WordComponent::DollarExpandable(s)
            | WordComponent::TildeExpandable(s)
            | WordComponent::CheckForQuotes(s) => Ok(s),
            _ => bail!(
                "WordComponent {:?} is not representable as a String",
                component
            ),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ExpandableWord {
    components: Vec<WordComponent>,
}

impl ExpandableWord {
    pub fn new(word: &ShellString) -> Fallible<Self> {
        let word = match word {
            ShellString::Os(s) => {
                return Ok(Self {
                    components: vec![WordComponent::LiteralOs(s.to_owned())],
                })
            }
            ShellString::String(s) => s,
        };

        let mut components = vec![];
        Self::check_for_quotes(word, &mut components)?;
        Self::tilde_expansion(&mut components);
        Self::dollar_expansion(&mut components);

        Ok(Self { components })
    }

    fn find_and_remove<F: Fn(&mut WordComponent) -> bool>(
        components: &mut Vec<WordComponent>,
        pred: F,
    ) -> Option<(usize, String)> {
        for (idx, word) in components.iter_mut().enumerate() {
            if pred(word) {
                let word = components.remove(idx);
                return Some((idx, word.try_into().unwrap()));
            }
        }
        None
    }

    fn find_and_remove_dollar_expandable(
        components: &mut Vec<WordComponent>,
    ) -> Option<(usize, String)> {
        Self::find_and_remove(components, |comp| match comp {
            WordComponent::DollarExpandable(_) => true,
            _ => false,
        })
    }

    fn find_and_remove_tilde_expandable(
        components: &mut Vec<WordComponent>,
    ) -> Option<(usize, String)> {
        Self::find_and_remove(components, |comp| match comp {
            WordComponent::TildeExpandable(_) => true,
            _ => false,
        })
    }

    fn find_unquoted(s: &str, what: char) -> Option<usize> {
        let mut prev = None;
        for (idx, c) in s.char_indices() {
            match prev.replace(c) {
                Some('\\') => continue,
                _ => {
                    if c == what {
                        return Some(idx);
                    }
                }
            }
        }
        None
    }

    fn find_char_position(s: &str, what: char) -> Option<usize> {
        for (idx, c) in s.char_indices() {
            if c == what {
                return Some(idx);
            }
        }
        None
    }

    fn find_unquoted_single_or_double_quote(s: &str) -> Option<(usize, char)> {
        let mut prev = None;
        for (idx, c) in s.char_indices() {
            match prev.replace(c) {
                Some('\\') => continue,
                _ => {
                    if c == '"' || c == '\'' {
                        return Some((idx, c));
                    }
                }
            }
        }
        None
    }

    fn check_for_quotes(word: &str, components: &mut Vec<WordComponent>) -> Fallible<()> {
        let mut remainder = word;
        loop {
            if let Some((quote_pos, quote)) = Self::find_unquoted_single_or_double_quote(remainder)
            {
                if quote_pos > 0 {
                    components.push(WordComponent::TildeExpandable(
                        remainder[..quote_pos].to_owned(),
                    ));
                    remainder = &remainder[quote_pos..];
                }
                // remainder always starts with the quote pos, so make sure we don't use it below
                drop(quote_pos);

                if quote == '\'' {
                    let end_quote =
                        Self::find_char_position(&remainder[1..], '\'').ok_or_else(|| {
                            format_err!(
                                "missing closing single quote in {}, components {:?}",
                                remainder,
                                components
                            )
                        })?;

                    let literal = &remainder[1..=end_quote];
                    components.push(WordComponent::Literal(literal.to_owned()));
                    remainder = &remainder[end_quote + 2..];
                } else if quote == '"' {
                    let end_quote = Self::find_unquoted(&remainder[1..], '"').ok_or_else(|| {
                        format_err!(
                            "missing closing double quote in {}, components {:?}",
                            remainder,
                            components
                        )
                    })?;

                    let literal = &remainder[1..=end_quote];
                    // FIXME: we should tell that expansion that we are double quoted,
                    // as it impacts word splitting and filename generation.
                    components.push(WordComponent::DollarExpandable(literal.to_owned()));
                    remainder = &remainder[end_quote + 2..];
                }
            } else {
                // There are no more quoted portions, to the remainder is to be expanded as-is
                components.push(WordComponent::TildeExpandable(remainder.to_owned()));
                return Ok(());
            }
        }
    }

    fn dollar_expansion(components: &mut Vec<WordComponent>) {
        while let Some((mut idx, word)) = Self::find_and_remove_dollar_expandable(components) {
            let mut remainder = word.as_str();
            loop {
                if let Some(dollar_pos) = Self::find_unquoted(remainder, '$') {
                    if dollar_pos > 0 {
                        components.insert(
                            idx,
                            WordComponent::FieldSplittable(remainder[..dollar_pos].to_owned()),
                        );
                        idx += 1;
                    }
                    remainder = &remainder[dollar_pos..];

                    if remainder.starts_with("$((") {
                        // arithmetic expansion - not yet handled
                        components.insert(idx, WordComponent::Literal("$((".to_owned()));
                        idx += 1;
                        remainder = &remainder[3..];
                    } else if remainder.starts_with("$(") {
                        // command substitution - not yet handled
                        components.insert(idx, WordComponent::Literal("$(".to_owned()));
                        idx += 1;
                        remainder = &remainder[2..];
                    } else {
                        // parameter expansion
                        if let Some((expr, extra)) = ParamExpr::try_parse(remainder) {
                            components.insert(idx, WordComponent::ParameterExpansion(expr));
                            idx += 1;
                            remainder = extra;
                        } else {
                            components.insert(idx, WordComponent::Literal("$".to_owned()));
                            idx += 1;
                            remainder = &remainder[1..];
                        }
                    }
                } else {
                    if !remainder.is_empty() {
                        components
                            .insert(idx, WordComponent::FieldSplittable(remainder.to_owned()));
                    }
                    break;
                }
            }
        }
    }

    fn tilde_expansion(components: &mut Vec<WordComponent>) {
        while let Some((mut idx, word)) = Self::find_and_remove_tilde_expandable(components) {
            let mut tilde_components = vec![];
            if let Some((key, value)) = parse_assignment_word(&word) {
                tilde_components.push(WordComponent::AssignmentName(key.to_owned()));
                tilde_components.push(WordComponent::Literal("=".to_owned()));
                Self::tilde_expand_assign(value, &mut tilde_components);
            } else {
                Self::tilde_expand_single(&word, &mut tilde_components);
            }
            for comp in tilde_components {
                components.insert(idx, comp);
                idx += 1;
            }
        }
    }

    /// Single word tilde expansion can only occur at the start of the word
    fn tilde_expand_single(s: &str, components: &mut Vec<WordComponent>) {
        if let Some(caps) = TILDE_RE.captures(s) {
            let all = caps.get(0).unwrap();
            if all.start() == 0 {
                let name = match caps.get(2) {
                    None => None,
                    Some(cap) => Some(cap.as_str().to_owned()),
                };
                components.push(WordComponent::TildeExpand(name.clone()));
                if name.is_none() && caps.get(1).unwrap().as_str() == "~/" {
                    components.push(WordComponent::Literal("/".to_owned()));
                }
                let remainder = &s[all.end()..];
                if !remainder.is_empty() {
                    components.push(WordComponent::DollarExpandable(remainder.to_owned()));
                }
                return;
            }
        }
        components.push(WordComponent::DollarExpandable(s.to_owned()));
    }

    /// Assignment-word tilde expansion can occur either at the start of the value (after
    /// the `=` sign) or after an *unquoted* colon character
    fn tilde_expand_assign(s: &str, components: &mut Vec<WordComponent>) {
        let mut prev = None;
        for (idx, element) in s
            .split(|c| match prev.replace(c) {
                Some('\\') => false,
                _ => c == ':',
            })
            .enumerate()
        {
            if idx > 0 {
                components.push(WordComponent::Literal(":".to_owned()));
            }
            Self::tilde_expand_single(element, components);
        }
    }
}

pub trait Expander {
    /// Look up the home directory for the specified user.
    /// If user is not specified, look it up for the current user.
    fn lookup_homedir(
        &self,
        user: Option<&str>,
        environment: &mut Environment,
    ) -> Fallible<ShellString>;

    fn expand_word(
        &self,
        word: &ShellString,
        environment: &mut Environment,
    ) -> Fallible<Vec<ShellString>> {
        match word {
            ShellString::Os(_) => Ok(vec![word.clone()]),
            ShellString::String(word) => {
                let mut expanded = String::new();
                self.expand_word_into(&mut expanded, word, environment)?;

                let mut fields = vec![];
                self.split_fields(&expanded, environment, &mut fields)?;

                // pathname expansion
                let mut expanded = vec![];
                for field in fields {
                    self.maybe_glob_expand(&field, &mut expanded)?;
                }

                // Quote removal
                for field in &mut expanded {
                    self.remove_quotes(field);
                }

                Ok(expanded)
            }
        }
    }

    fn remove_quotes(&self, s: &mut ShellString) {
        match s {
            ShellString::String(s) => {
                let mut removed = String::with_capacity(s.len());
                let mut backslash = false;
                for c in s.chars() {
                    if backslash {
                        backslash = false;
                        removed.push(c);
                        continue;
                    }
                    if c == '\\' {
                        backslash = true;
                        continue;
                    }
                    if c == '"' || c == '\'' {
                        continue;
                    }
                    removed.push(c)
                }
                *s = removed;
            }
            _ => {}
        }
    }

    fn maybe_glob_expand(&self, text: &str, target: &mut Vec<ShellString>) -> Fallible<()> {
        let mut backslash = false;
        let mut do_glob = false;
        for c in text.chars() {
            if backslash {
                backslash = false;
                continue;
            }
            if c == '\\' {
                backslash = true;
                continue;
            }
            if c == '*' {
                do_glob = true;
                break;
            }
        }

        if !do_glob {
            target.push(text.into());
            return Ok(());
        }

        for path in glob::glob(text)? {
            match path {
                Ok(path) => target.push(path.as_os_str().into()),
                Err(e) => {
                    eprintln!("wzsh: while globbing {}: {}", text, e);
                }
            }
        }

        Ok(())
    }

    fn split_fields(
        &self,
        text: &str,
        env: &mut Environment,
        target: &mut Vec<String>,
    ) -> Fallible<()> {
        let ifs = env.get_str("IFS")?.unwrap_or(" \t\n");
        if ifs.is_empty() {
            // No field splitting is needed
            target.push(text.to_owned());
            return Ok(());
        }

        let ifs_set: std::collections::HashSet<char> = ifs.chars().collect();
        let mut in_quotes = false;
        let mut start = None;

        for (idx, c) in text.char_indices() {
            if in_quotes {
                if c == '"' {
                    in_quotes = false;
                }
                continue;
            }

            if c == '"' {
                in_quotes = true;
            }

            if ifs_set.contains(&c) {
                // Delimit a field
                let first = start.take().unwrap_or(idx);
                if idx > first {
                    target.push(text[first..idx].to_owned());
                }
            } else if start.is_none() {
                start = Some(idx);
            }
        }

        if let Some(start) = start {
            target.push(text[start..].to_owned());
        }

        Ok(())
    }

    /// Performs tilde, parameteter, command and arithmetic expansion.
    /// Does NOT perform field splitting or filename generation
    fn expand_word_into(
        &self,
        target: &mut String,
        word: &str,
        environment: &mut Environment,
    ) -> Fallible<()> {
        let word = self.tilde_expand(&word.to_owned().into(), environment)?;
        let word = self.parameter_expansion(&word, environment)?;

        // TODO: command expansion
        // TODO: arithmetic expansion

        match word {
            ShellString::String(s) => target.push_str(&s),
            ShellString::Os(_) => bail!("unable to convert OsString to str during word expansion"),
        }

        Ok(())
    }

    // Section 2.6.1
    fn tilde_expand(
        &self,
        word: &ShellString,
        environment: &mut Environment,
    ) -> Fallible<ShellString> {
        match word {
            ShellString::Os(_) => Ok(word.clone()),
            ShellString::String(s) => {
                if let Some((key, value)) = parse_assignment_word(s) {
                    Ok(format!("{}={}", key, self.tilde_expand_assign(value, environment)?).into())
                } else {
                    Ok(self.tilde_expand_single(s, environment)?.into())
                }
            }
        }
    }

    // Section 2.6.2
    fn parameter_expansion(
        &self,
        word: &ShellString,
        environment: &mut Environment,
    ) -> Fallible<ShellString> {
        match word {
            ShellString::Os(_) => Ok(word.clone()),
            ShellString::String(s) => {
                let mut result = String::new();
                self.parameter_expand_into(&mut result, s.as_str(), environment)?;
                Ok(result.into())
            }
        }
    }

    fn parameter_expand_into(
        &self,
        target: &mut String,
        word: &str,
        environment: &mut Environment,
    ) -> Fallible<()> {
        let mut remainder = word;
        loop {
            if let Some(dollar_pos) = remainder.find('$') {
                if dollar_pos > 0 {
                    target.push_str(&remainder[..dollar_pos]);
                }
                remainder = &remainder[dollar_pos..];
                if let Some((expr, extra)) = ParamExpr::try_parse(remainder) {
                    match expr {
                        ParamExpr::Get { name } => {
                            if let Some(value) = environment.get_str(&name)? {
                                target.push_str(value);
                            }
                        }
                        ParamExpr::StringLength { name } => {
                            if let Some(value) = environment.get_str(&name)? {
                                write!(target, "{}", value.len())?;
                            } else {
                                target.push('0');
                            }
                        }
                        ParamExpr::GetDefault {
                            name,
                            allow_null,
                            word,
                        } => {
                            if let Some(value) = environment.get_str(&name)? {
                                if value.is_empty() && !allow_null {
                                    self.expand_word_into(target, &word, environment)?;
                                } else {
                                    target.push_str(value);
                                }
                            } else {
                                self.expand_word_into(target, &word, environment)?;
                            }
                        }
                        ParamExpr::AssignDefault {
                            name,
                            allow_null,
                            word,
                        } => {
                            if let Some(value) = environment.get_str(&name)? {
                                if value.is_empty() && !allow_null {
                                    let mut expansion = String::new();
                                    self.expand_word_into(&mut expansion, &word, environment)?;
                                    target.push_str(&expansion);
                                    environment.set(&name, expansion);
                                } else {
                                    target.push_str(value);
                                }
                            } else {
                                let mut expansion = String::new();
                                self.expand_word_into(&mut expansion, &word, environment)?;
                                target.push_str(&expansion);
                                environment.set(&name, expansion);
                            }
                        }
                        ParamExpr::CheckSet {
                            name,
                            allow_null,
                            word,
                        } => {
                            if let Some(value) = environment.get_str(&name)? {
                                if value.is_empty() && !allow_null {
                                    if word.is_empty() {
                                        bail!("wzsh: {}: parameter not set", name);
                                    } else {
                                        bail!("wzsh: {}: {}", name, word);
                                    }
                                } else {
                                    target.push_str(value);
                                }
                            } else {
                                if word.is_empty() {
                                    bail!("wzsh: {}: parameter not set", name);
                                } else {
                                    bail!("wzsh: {}: {}", name, word);
                                }
                            }
                        }
                        ParamExpr::AlternativeValue {
                            name,
                            allow_null,
                            word,
                        } => {
                            if let Some(value) = environment.get_str(&name)? {
                                if value.is_empty() && !allow_null {
                                    target.push_str(value);
                                } else {
                                    self.expand_word_into(target, &word, environment)?;
                                }
                            }
                        }
                    }
                    remainder = extra;
                } else {
                    target.push('$');
                    remainder = &remainder[1..];
                }
            } else {
                target.push_str(remainder);
                return Ok(());
            }
        }
    }

    fn tilde_expand_helper(
        &self,
        caps: &Captures,
        environment: &mut Environment,
        target: &mut String,
    ) -> Fallible<()> {
        let name = match caps.get(2) {
            None => None,
            Some(cap) => Some(cap.as_str()),
        };

        match self.lookup_homedir(name, environment) {
            Ok(ShellString::String(home)) => {
                target.push_str(&home);
            }
            Err(_) | Ok(ShellString::Os(_)) => {
                // If we failed to expand for whatever reason, don't
                // expand.
                // Note that we treat a successful expansion that results
                // in an OsStr result as a failure, because we don't have
                // a good way to retain its Os-ness from here out.
                target.push('~');
                if let Some(name) = name {
                    target.push_str(name);
                }
            }
        }

        if name.is_none() && caps.get(1).unwrap().as_str() == "~/" {
            target.push('/');
        }

        Ok(())
    }

    /// Single word tilde expansion can only occur at the start of the word
    fn tilde_expand_single(&self, s: &str, environment: &mut Environment) -> Fallible<String> {
        match TILDE_RE.captures(s) {
            Some(caps) => {
                let all = caps.get(0).unwrap();
                if all.start() == 0 {
                    let mut expanded = String::new();
                    self.tilde_expand_helper(&caps, environment, &mut expanded)?;
                    expanded.push_str(&s[all.end()..]);
                    Ok(expanded)
                } else {
                    Ok(s.into())
                }
            }
            None => Ok(s.into()),
        }
    }

    /// Assignment-word tilde expansion can occur either at the start of the value (after
    /// the `=` sign) or after an *unquoted* colon character
    fn tilde_expand_assign(&self, s: &str, environment: &mut Environment) -> Fallible<String> {
        let mut expanded = String::new();
        let mut prev = None;
        for (idx, element) in s
            .split(|c| match prev.replace(c) {
                Some('\\') => false,
                _ => c == ':',
            })
            .enumerate()
        {
            if idx > 0 {
                expanded.push(':');
            }
            expanded.push_str(&self.tilde_expand_single(element, environment)?);
        }
        Ok(expanded)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use failure::bail;
    use pretty_assertions::assert_eq;

    struct MockExpander {}
    impl Expander for MockExpander {
        fn lookup_homedir(
            &self,
            user: Option<&str>,
            _environment: &mut Environment,
        ) -> Fallible<ShellString> {
            match user {
                Some("someone") => Ok("/home/someone".into()),
                Some("wez") => Ok("/home/wez".into()),
                None => Ok("/home/wez".into()),
                _ => bail!("no such user"),
            }
        }
    }

    #[test]
    fn tilde_expand() {
        let expander = MockExpander {};
        let mut env = Environment::new();
        assert_eq!(
            expander.tilde_expand_assign("~", &mut env).unwrap(),
            "/home/wez".to_owned()
        );
        assert_eq!(
            expander.tilde_expand_assign("~/", &mut env).unwrap(),
            "/home/wez/".to_owned()
        );
        assert_eq!(
            expander.tilde_expand_assign("~:", &mut env).unwrap(),
            "/home/wez:".to_owned()
        );
        assert_eq!(
            expander.tilde_expand_single("~", &mut env).unwrap(),
            "/home/wez".to_owned()
        );
        assert_eq!(
            expander
                .tilde_expand_assign("~:~someone:~wez:~noone", &mut env)
                .unwrap(),
            "/home/wez:/home/someone:/home/wez:~noone".to_owned()
        );
        assert_eq!(
            expander
                .tilde_expand_assign("~:~someone\\:~wez:~noone", &mut env)
                .unwrap(),
            "/home/wez:/home/someone\\:~wez:~noone".to_owned()
        );
        assert_eq!(
            expander
                .tilde_expand_single("~:~someone:~wez:~noone", &mut env)
                .unwrap(),
            "~:~someone:~wez:~noone".to_owned()
        );
        assert_eq!(
            expander
                .tilde_expand_assign("hello~there", &mut env)
                .unwrap(),
            "hello~there".to_owned()
        );
        assert_eq!(
            expander
                .tilde_expand_assign("hello~/there", &mut env)
                .unwrap(),
            "hello~/there".to_owned()
        );
        assert_eq!(
            expander
                .tilde_expand_assign("hello~wez/there", &mut env)
                .unwrap(),
            "hello~wez/there".to_owned()
        );
        assert_eq!(
            expander
                .tilde_expand_assign("hello~someone/there", &mut env)
                .unwrap(),
            "hello~someone/there".to_owned()
        );

        assert_eq!(
            expander.tilde_expand(&"~".into(), &mut env).unwrap(),
            "/home/wez".into()
        );
        assert_eq!(
            expander.tilde_expand(&"THING=~".into(), &mut env).unwrap(),
            "THING=/home/wez".into()
        );
        assert_eq!(
            expander
                .tilde_expand(&"PATH=~/:~someone".into(), &mut env)
                .unwrap(),
            "PATH=/home/wez/:/home/someone".into()
        );
    }

    fn mock_env() -> Environment {
        let mut env = Environment::new();
        env.set("TEST", "this");
        env.unset("NOTSET");
        env.set("EMPTY", "");
        env.set("SOMETHING", "something");
        env
    }

    #[test]
    fn word_expand() -> Fallible<()> {
        let expander = MockExpander {};
        let mut env = mock_env();

        env.set("MULTI", "multiple words");

        assert_eq!(
            expander.expand_word(&"hello there".into(), &mut env)?,
            vec!["hello".to_owned().into(), "there".to_owned().into()]
        );

        assert_eq!(
            expander.expand_word(&"hel\"lo\" there".into(), &mut env)?,
            vec!["hello".to_owned().into(), "there".to_owned().into()]
        );
        assert_eq!(
            expander.expand_word(&"hel'lo' there".into(), &mut env)?,
            vec!["hello".to_owned().into(), "there".to_owned().into()]
        );
        assert_eq!(
            expander.expand_word(&"hel\\lo there".into(), &mut env)?,
            vec!["hello".to_owned().into(), "there".to_owned().into()]
        );
        assert_eq!(
            expander.expand_word(&"hel\\\\lo there".into(), &mut env)?,
            vec!["hel\\lo".to_owned().into(), "there".to_owned().into()]
        );

        assert_eq!(
            expander.expand_word(&"hello$MULTI".into(), &mut env)?,
            vec!["hellomultiple".to_owned().into(), "words".to_owned().into(),]
        );

        assert_eq!(
            expander.expand_word(&"*.toml".into(), &mut env)?,
            vec!["Cargo.toml".to_owned().into()]
        );
        Ok(())
    }

    #[test]
    fn parameter_get() -> Fallible<()> {
        let expander = MockExpander {};
        let mut env = mock_env();

        assert_eq!(
            expander.parameter_expansion(&"hello".into(), &mut env)?,
            "hello".into()
        );
        assert_eq!(
            expander.parameter_expansion(&"$TEST".into(), &mut env)?,
            "this".into()
        );
        assert_eq!(
            expander.parameter_expansion(&"${TEST}".into(), &mut env)?,
            "this".into()
        );
        assert_eq!(
            expander.parameter_expansion(&"a$TEST".into(), &mut env)?,
            "athis".into()
        );
        assert_eq!(
            expander.parameter_expansion(&"a${TEST}z".into(), &mut env)?,
            "athisz".into()
        );
        Ok(())
    }

    #[test]
    fn parameter_string_len() -> Fallible<()> {
        let expander = MockExpander {};
        let mut env = mock_env();

        assert_eq!(
            expander.parameter_expansion(&"${#TEST}".into(), &mut env)?,
            "4".into()
        );
        Ok(())
    }

    #[test]
    fn parameter_get_default() -> Fallible<()> {
        let expander = MockExpander {};
        let mut env = mock_env();

        assert_eq!(
            expander.parameter_expansion(&"${TEST:-notest}".into(), &mut env)?,
            "this".into()
        );
        assert_eq!(
            expander.parameter_expansion(&"${NOTSET:-notest}".into(), &mut env)?,
            "notest".into()
        );
        assert_eq!(
            expander.parameter_expansion(&"${EMPTY:-notest}".into(), &mut env)?,
            "notest".into()
        );
        assert_eq!(
            expander.parameter_expansion(&"${EMPTY-notest}".into(), &mut env)?,
            "".into()
        );
        Ok(())
    }

    #[test]
    fn parameter_get_default_recursive() -> Fallible<()> {
        let expander = MockExpander {};
        let mut env = mock_env();

        assert_eq!(
            expander.parameter_expansion(&"${NOTSET:-hello$SOMETHING}".into(), &mut env)?,
            "hellosomething".into()
        );
        Ok(())
    }

    #[test]
    fn parameter_assign_default() -> Fallible<()> {
        let expander = MockExpander {};
        let mut env = mock_env();

        assert_eq!(
            expander.parameter_expansion(&"${TEST:=notest}".into(), &mut env)?,
            "this".into()
        );

        assert_eq!(
            expander.parameter_expansion(&"${NOTSET=woot}".into(), &mut env)?,
            "woot".into()
        );
        assert_eq!(env.get_str("NOTSET")?.unwrap(), "woot");
        env.unset("NOTSET");
        assert_eq!(env.get_str("NOTSET")?, None);

        assert_eq!(
            expander.parameter_expansion(&"${NOTSET:=woot}".into(), &mut env)?,
            "woot".into()
        );
        assert_eq!(env.get_str("NOTSET")?.unwrap(), "woot");
        env.unset("NOTSET");

        assert_eq!(
            expander.parameter_expansion(&"${EMPTY=woot}".into(), &mut env)?,
            "".into()
        );
        assert_eq!(env.get_str("EMPTY")?.unwrap(), "");
        Ok(())
    }

    #[test]
    fn parameter_check_set() -> Fallible<()> {
        let expander = MockExpander {};
        let mut env = mock_env();
        assert_eq!(
            format!(
                "{}",
                expander
                    .parameter_expansion(&"${NOTSET:?}".into(), &mut env)
                    .unwrap_err()
            ),
            "wzsh: NOTSET: parameter not set"
        );

        assert_eq!(
            format!(
                "{}",
                expander
                    .parameter_expansion(&"${EMPTY:?}".into(), &mut env)
                    .unwrap_err()
            ),
            "wzsh: EMPTY: parameter not set"
        );

        assert_eq!(
            format!(
                "{}",
                expander
                    .parameter_expansion(&"${NOTSET:?it's not set}".into(), &mut env)
                    .unwrap_err()
            ),
            "wzsh: NOTSET: it's not set"
        );

        assert_eq!(
            expander.parameter_expansion(&"${EMPTY?}".into(), &mut env)?,
            "".into()
        );
        Ok(())
    }

    #[test]
    fn parameter_alternative_value() -> Fallible<()> {
        let expander = MockExpander {};
        let mut env = mock_env();

        assert_eq!(
            expander.parameter_expansion(&"${TEST:+notest}".into(), &mut env)?,
            "notest".into()
        );
        assert_eq!(
            expander.parameter_expansion(&"${NOTSET:+notest}".into(), &mut env)?,
            "".into()
        );
        assert_eq!(
            expander.parameter_expansion(&"${EMPTY:+notest}".into(), &mut env)?,
            "".into()
        );
        assert_eq!(
            expander.parameter_expansion(&"${EMPTY+notest}".into(), &mut env)?,
            "notest".into()
        );
        Ok(())
    }

    fn ifs(input: &str, ifs: Option<&str>) -> Fallible<Vec<String>> {
        let mut env = Environment::new();
        match ifs {
            Some(ifs) => env.set("IFS", ifs),
            None => env.unset("IFS"),
        };
        let mut target = Vec::new();
        let expander = MockExpander {};
        expander.split_fields(input, &mut env, &mut target)?;
        Ok(target)
    }

    #[test]
    fn field_split() -> Fallible<()> {
        assert_eq!(ifs("", None)?, Vec::<String>::new());
        assert_eq!(ifs("", Some(""))?, vec!["".to_owned()]);

        assert_eq!(ifs("hello", None)?, vec!["hello".to_owned()]);
        assert_eq!(
            ifs("hello there\twoot\nyay", None)?,
            vec![
                "hello".to_owned(),
                "there".to_owned(),
                "woot".to_owned(),
                "yay".to_owned()
            ]
        );

        assert_eq!(
            ifs("hello\"quo ted\" world", None)?,
            vec!["hello\"quo ted\"".to_owned(), "world".to_owned()]
        );

        assert_eq!(
            ifs("hello\"quoted\" world", None)?,
            vec!["hello\"quoted\"".to_owned(), "world".to_owned()]
        );

        Ok(())
    }

    #[test]
    fn expandable_word() {
        assert_eq!(
            ExpandableWord::new(&"'hello'".into()).unwrap(),
            ExpandableWord {
                components: vec![WordComponent::Literal("hello".to_owned())]
            }
        );

        assert_eq!(
            ExpandableWord::new(&"hello".into()).unwrap(),
            ExpandableWord {
                components: vec![WordComponent::FieldSplittable("hello".to_owned())]
            }
        );

        assert_eq!(
            ExpandableWord::new(&"foo=hello".into()).unwrap(),
            ExpandableWord {
                components: vec![
                    WordComponent::AssignmentName("foo".to_owned()),
                    WordComponent::Literal("=".to_owned()),
                    WordComponent::FieldSplittable("hello".to_owned())
                ]
            }
        );

        assert_eq!(
            ExpandableWord::new(&"foo=~wez/hello".into()).unwrap(),
            ExpandableWord {
                components: vec![
                    WordComponent::AssignmentName("foo".to_owned()),
                    WordComponent::Literal("=".to_owned()),
                    WordComponent::TildeExpand(Some("wez".to_owned())),
                    WordComponent::FieldSplittable("/hello".to_owned()),
                ]
            }
        );

        assert_eq!(
            ExpandableWord::new(&"foo='~wez/hello'".into()).unwrap(),
            ExpandableWord {
                components: vec![
                    WordComponent::AssignmentName("foo".to_owned()),
                    WordComponent::Literal("=".to_owned()),
                    WordComponent::Literal("~wez/hello".to_owned()),
                ]
            }
        );

        assert_eq!(
            ExpandableWord::new(&"foo=\"~wez/hello\"".into()).unwrap(),
            ExpandableWord {
                components: vec![
                    WordComponent::AssignmentName("foo".to_owned()),
                    WordComponent::Literal("=".to_owned()),
                    WordComponent::FieldSplittable("~wez/hello".to_owned()),
                ]
            }
        );

        assert_eq!(
            ExpandableWord::new(&"~hello".into()).unwrap(),
            ExpandableWord {
                components: vec![WordComponent::TildeExpand(Some("hello".to_owned()))]
            }
        );

        assert_eq!(
            ExpandableWord::new(&"$hello there".into()).unwrap(),
            ExpandableWord {
                components: vec![
                    WordComponent::ParameterExpansion(ParamExpr::Get {
                        name: "hello".to_owned()
                    }),
                    WordComponent::FieldSplittable(" there".to_owned()),
                ]
            }
        );

        assert_eq!(
            ExpandableWord::new(&"\"$hello\" there".into()).unwrap(),
            ExpandableWord {
                components: vec![
                    WordComponent::ParameterExpansion(ParamExpr::Get {
                        name: "hello".to_owned()
                    }),
                    WordComponent::FieldSplittable(" there".to_owned()),
                ]
            }
        );
    }
}
