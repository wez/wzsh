use failure::{bail, Fallible};
use std::collections::HashMap;
use std::ffi::{OsStr, OsString};

#[derive(Debug, Clone, Default)]
pub struct Environment {
    // TODO: on windows, the key should be case insensitive
    map: HashMap<OsString, OsString>,
}

impl Environment {
    pub fn new() -> Self {
        let mut environ = Self::default();
        for (key, value) in std::env::vars_os() {
            environ.set(key, value);
        }
        environ
    }

    pub fn set<K: Into<OsString> + ?Sized, V: Into<OsString> + ?Sized>(
        &mut self,
        key: K,
        value: V,
    ) {
        self.map.insert(key.into(), value.into());
    }

    pub fn get<K: AsRef<OsStr>>(&self, key: K) -> Option<&OsStr> {
        self.map.get(key.as_ref()).map(OsString::as_os_str)
    }

    pub fn get_str<K: AsRef<OsStr> + std::fmt::Debug>(&self, key: K) -> Fallible<Option<&str>> {
        match self.map.get(key.as_ref()) {
            None => Ok(None),
            Some(v) => match v.to_str() {
                Some(s) => Ok(Some(s)),
                None => bail!(
                    "unable to convert environment value for {:?} to String",
                    key
                ),
            },
        }
    }

    pub fn unset<K: AsRef<OsStr>>(&mut self, key: K) {
        self.map.remove(key.as_ref());
    }

    pub fn iter(&self) -> impl Iterator<Item = (&OsString, &OsString)> {
        self.map.iter()
    }
}
