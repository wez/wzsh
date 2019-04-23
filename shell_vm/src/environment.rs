use failure::{bail, Fallible};
use std::collections::HashMap;
use std::ffi::{OsStr, OsString};

/// The environment represents the environmental variables
/// associated with the shell and the processes that it spawns.
#[derive(Clone, Debug)]
pub struct Environment {
    map: HashMap<OsString, OsString>,
}

impl Environment {
    pub fn new() -> Self {
        let mut environ = Self {
            map: HashMap::new(),
        };
        for (key, value) in std::env::vars_os() {
            environ.set(key, value);
        }
        environ
    }

    pub fn get_str<K: AsRef<OsStr> + std::fmt::Debug>(&self, key: K) -> Fallible<Option<&str>> {
        match self.get(key.as_ref()) {
            None => Ok(None),
            Some(v) => match v.to_str() {
                Some(s) => Ok(Some(s)),
                None => bail!(
                    "unable to convert environment value {:?} for key {:?} to String",
                    v,
                    key
                ),
            },
        }
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

    pub fn unset<K: AsRef<OsStr>>(&mut self, key: K) {
        self.map.remove(key.as_ref());
    }

    pub fn iter(&self) -> impl Iterator<Item = (&OsString, &OsString)> {
        self.map.iter()
    }
}
