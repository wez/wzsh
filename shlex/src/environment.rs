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

    pub fn set(&mut self, key: OsString, value: OsString) {
        self.map.insert(key, value);
    }

    pub fn get(&self, key: &OsStr) -> Option<&OsString> {
        self.map.get(key)
    }

    pub fn unset(&mut self, key: &OsStr) {
        self.map.remove(key);
    }

    pub fn iter(&self) -> impl Iterator<Item = (&OsString, &OsString)> {
        self.map.iter()
    }
}
