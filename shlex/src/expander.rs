use crate::environment::Environment;
use crate::parse_assignment_word;
use crate::string::ShellString;
use failure::Fallible;
use lazy_static::lazy_static;
use regex::{Captures, Regex};

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
        environment: &Environment,
    ) -> Fallible<ShellString>;

    fn expand_word(
        &self,
        word: &ShellString,
        environment: &Environment,
    ) -> Fallible<Vec<ShellString>> {
        let mut res = vec![];

        let word = self.tilde_expand(word, environment)?;

        res.push(word);

        Ok(res)
    }

    // Section 2.6.1
    fn tilde_expand(&self, word: &ShellString, environment: &Environment) -> Fallible<ShellString> {
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

    fn tilde_expand_helper(
        &self,
        caps: &Captures,
        environment: &Environment,
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
    fn tilde_expand_single(&self, s: &str, environment: &Environment) -> Fallible<String> {
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
    fn tilde_expand_assign(&self, s: &str, environment: &Environment) -> Fallible<String> {
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
            _environment: &Environment,
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
        let env = Environment::new();
        assert_eq!(
            expander.tilde_expand_assign("~", &env).unwrap(),
            "/home/wez".to_owned()
        );
        assert_eq!(
            expander.tilde_expand_assign("~/", &env).unwrap(),
            "/home/wez/".to_owned()
        );
        assert_eq!(
            expander.tilde_expand_assign("~:", &env).unwrap(),
            "/home/wez:".to_owned()
        );
        assert_eq!(
            expander.tilde_expand_single("~", &env).unwrap(),
            "/home/wez".to_owned()
        );
        assert_eq!(
            expander
                .tilde_expand_assign("~:~someone:~wez:~noone", &env)
                .unwrap(),
            "/home/wez:/home/someone:/home/wez:~noone".to_owned()
        );
        assert_eq!(
            expander
                .tilde_expand_assign("~:~someone\\:~wez:~noone", &env)
                .unwrap(),
            "/home/wez:/home/someone\\:~wez:~noone".to_owned()
        );
        assert_eq!(
            expander
                .tilde_expand_single("~:~someone:~wez:~noone", &env)
                .unwrap(),
            "~:~someone:~wez:~noone".to_owned()
        );
        assert_eq!(
            expander.tilde_expand_assign("hello~there", &env).unwrap(),
            "hello~there".to_owned()
        );
        assert_eq!(
            expander.tilde_expand_assign("hello~/there", &env).unwrap(),
            "hello~/there".to_owned()
        );
        assert_eq!(
            expander
                .tilde_expand_assign("hello~wez/there", &env)
                .unwrap(),
            "hello~wez/there".to_owned()
        );
        assert_eq!(
            expander
                .tilde_expand_assign("hello~someone/there", &env)
                .unwrap(),
            "hello~someone/there".to_owned()
        );

        assert_eq!(
            expander.tilde_expand(&"~".into(), &env).unwrap(),
            "/home/wez".into()
        );
        assert_eq!(
            expander.tilde_expand(&"THING=~".into(), &env).unwrap(),
            "THING=/home/wez".into()
        );
        assert_eq!(
            expander
                .tilde_expand(&"PATH=~/:~someone".into(), &env)
                .unwrap(),
            "PATH=/home/wez/:/home/someone".into()
        );
    }
}

/*
    pub fn parameter_expand(&mut self, _expander: &Expander, _environment: &Environment) {}
    pub fn command_substitution(&mut self, _expander: &Expander, _environment: &Environment) {}
    pub fn arithmetic_expansion(&mut self, _expander: &Expander, _environment: &Environment) {}
*/
