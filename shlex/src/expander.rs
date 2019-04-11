use crate::environment::Environment;
use crate::paramexp::ParamExpr;
use crate::parse_assignment_word;
use crate::string::ShellString;
use failure::{bail, Fallible};
use lazy_static::lazy_static;
use regex::{Captures, Regex};
use std::fmt::Write;

lazy_static! {
    static ref TILDE_RE: Regex =
        Regex::new(r"(~$|~/|~([a-zA-Z_][a-zA-Z0-9_]+))").expect("failed to compile TILDE_RE");
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

                // TODO: field splitting
                // TODO: pathname expansion

                Ok(vec![expanded.into()])
            }
        }
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
                    eprintln!("need to evaluate {:?}", expr);
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
}
