use crate::environment::Environment;
use crate::parse_assignment_word;
use std::ffi::{OsStr, OsString};

pub trait Expander {
    /// Look up the home directory for the specified user.
    /// If user is not specified, look it up for the current user.
    fn lookup_homedir(&self, user: Option<&OsStr>) -> Result<OsString, ()>;

    fn expand_word(&self, word: &OsStr, _environment: &Environment) -> Result<Vec<OsString>, ()> {
        let mut res = vec![];

        let word = self.tilde_expand(word)?;

        res.push(word);

        Ok(res)
    }

    fn tilde_expand(&self, word: &OsStr) -> Result<OsString, ()> {
        // Section 2.6.1
        Ok(word.to_os_string())
    }
}

/*
    pub fn parameter_expand(&mut self, _expander: &Expander, _environment: &Environment) {}
    pub fn command_substitution(&mut self, _expander: &Expander, _environment: &Environment) {}
    pub fn arithmetic_expansion(&mut self, _expander: &Expander, _environment: &Environment) {}
*/
