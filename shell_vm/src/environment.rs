use anyhow::bail;
use caseless::{canonical_caseless_match_str, Caseless};
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::ffi::{OsStr, OsString};
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone)]
struct CaseInsensitiveOsString(OsString);

impl CaseInsensitiveOsString {
    fn compare(&self, other: &OsStr) -> Ordering {
        match (self.0.to_str(), other.to_str()) {
            (Some(a), Some(b)) => {
                let mut a = a.chars().default_case_fold();
                let mut b = b.chars().default_case_fold();

                loop {
                    match (a.next(), b.next()) {
                        (None, None) => return Ordering::Equal,
                        (None, Some(_)) => return Ordering::Less,
                        (Some(_), None) => return Ordering::Greater,
                        (Some(a), Some(b)) => {
                            let ordering = a.cmp(&b);
                            if ordering != Ordering::Equal {
                                return ordering;
                            }
                        }
                    }
                }
            }
            _ => self.0.as_os_str().cmp(other),
        }
    }
}

impl PartialEq for CaseInsensitiveOsString {
    fn eq(&self, other: &CaseInsensitiveOsString) -> bool {
        match (self.0.to_str(), other.0.to_str()) {
            (Some(a), Some(b)) => canonical_caseless_match_str(a, b),
            _ => self.0.eq(&other.0),
        }
    }
}

impl PartialEq<OsStr> for CaseInsensitiveOsString {
    fn eq(&self, other: &OsStr) -> bool {
        match (self.0.to_str(), other.to_str()) {
            (Some(a), Some(b)) => canonical_caseless_match_str(a, b),
            _ => self.0.eq(&other),
        }
    }
}

impl PartialEq<&OsStr> for CaseInsensitiveOsString {
    fn eq(&self, other: &&OsStr) -> bool {
        match (self.0.to_str(), other.to_str()) {
            (Some(a), Some(b)) => canonical_caseless_match_str(a, b),
            _ => self.0.eq(other),
        }
    }
}

impl Eq for CaseInsensitiveOsString {}

impl Ord for CaseInsensitiveOsString {
    fn cmp(&self, other: &CaseInsensitiveOsString) -> Ordering {
        self.compare(other.0.as_os_str())
    }
}

impl PartialOrd for CaseInsensitiveOsString {
    fn partial_cmp(&self, other: &CaseInsensitiveOsString) -> Option<Ordering> {
        Some(self.compare(other.0.as_os_str()))
    }
}

impl PartialOrd<OsStr> for CaseInsensitiveOsString {
    fn partial_cmp(&self, other: &OsStr) -> Option<Ordering> {
        Some(self.compare(other))
    }
}

impl PartialOrd<&OsStr> for CaseInsensitiveOsString {
    fn partial_cmp(&self, other: &&OsStr) -> Option<Ordering> {
        Some(self.compare(other))
    }
}

impl Hash for CaseInsensitiveOsString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        if let Some(s) = self.0.to_str() {
            for c in s.chars().default_case_fold() {
                c.hash(state);
            }
        } else {
            self.0.hash(state);
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum EnvMap {
    Posix(BTreeMap<OsString, OsString>),
    Windows(BTreeMap<CaseInsensitiveOsString, OsString>),
}

impl Default for EnvMap {
    fn default() -> Self {
        if cfg!(windows) {
            Self::windows()
        } else {
            Self::posix()
        }
    }
}

impl EnvMap {
    fn posix() -> Self {
        EnvMap::Posix(BTreeMap::new())
    }

    fn windows() -> Self {
        EnvMap::Windows(BTreeMap::new())
    }

    fn set(&mut self, key: OsString, value: OsString) {
        match self {
            EnvMap::Posix(map) => map.insert(key, value),
            EnvMap::Windows(map) => map.insert(CaseInsensitiveOsString(key), value),
        };
    }

    fn get(&self, key: &OsStr) -> Option<&OsStr> {
        match self {
            EnvMap::Posix(map) => map.get(key),
            EnvMap::Windows(map) => map.get(&CaseInsensitiveOsString(key.to_os_string())),
        }
        .map(OsString::as_os_str)
    }

    fn unset(&mut self, key: &OsStr) {
        match self {
            EnvMap::Posix(map) => map.remove(key),
            EnvMap::Windows(map) => map.remove(&CaseInsensitiveOsString(key.to_os_string())),
        };
    }

    fn iter(&self) -> impl Iterator<Item = (&OsString, &OsString)> {
        // Using this technique to avoid incompatible match arms errors:
        // https://stackoverflow.com/a/54728634/149111
        let mut posix = None;
        let mut windows = None;
        match self {
            EnvMap::Posix(map) => posix = Some(map.iter()),
            EnvMap::Windows(map) => windows = Some(map.iter().map(|(k, v)| (&k.0, v))),
        };

        posix
            .into_iter()
            .flatten()
            .chain(windows.into_iter().flatten())
    }
}

/// The environment represents the environmental variables
/// associated with the shell and the processes that it spawns.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Environment {
    map: EnvMap,
}

impl Environment {
    pub fn new() -> Self {
        let mut environ = Self {
            map: Default::default(),
        };
        for (key, value) in std::env::vars_os() {
            environ.set(key, value);
        }
        environ
    }

    pub fn new_empty() -> Self {
        Self {
            map: Default::default(),
        }
    }

    pub fn get_str<K: AsRef<OsStr> + std::fmt::Debug>(
        &self,
        key: K,
    ) -> anyhow::Result<Option<&str>> {
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
        self.map.set(key.into(), value.into());
    }

    pub fn append_path<K: Into<OsString> + ?Sized, V: Into<OsString> + ?Sized>(
        &mut self,
        key: K,
        value: V,
    ) -> Result<(), std::env::JoinPathsError> {
        let key = key.into();
        let mut current_path: Vec<_> = self
            .get(&key)
            .map(|p| std::env::split_paths(p).collect())
            .unwrap_or_else(Vec::new);

        current_path.push(value.into().into());
        let new_path = std::env::join_paths(current_path.iter())?;
        self.set(key, new_path);
        Ok(())
    }

    pub fn prepend_path<K: Into<OsString> + ?Sized, V: Into<OsString> + ?Sized>(
        &mut self,
        key: K,
        value: V,
    ) -> Result<(), std::env::JoinPathsError> {
        let key = key.into();
        let mut current_path: Vec<_> = self
            .get(&key)
            .map(|p| std::env::split_paths(p).collect())
            .unwrap_or_else(Vec::new);

        current_path.insert(0, value.into().into());
        let new_path = std::env::join_paths(current_path.iter())?;
        self.set(key, new_path);
        Ok(())
    }

    pub fn get<K: AsRef<OsStr>>(&self, key: K) -> Option<&OsStr> {
        self.map.get(key.as_ref())
    }

    pub fn unset<K: AsRef<OsStr>>(&mut self, key: K) {
        self.map.unset(key.as_ref());
    }

    pub fn iter(&self) -> impl Iterator<Item = (&OsString, &OsString)> {
        self.map.iter()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn case_insensitive() {
        let foo = CaseInsensitiveOsString("foo".into());
        let food = CaseInsensitiveOsString("food".into());
        let big_foo = CaseInsensitiveOsString("FOO".into());
        assert_eq!(foo, big_foo);
        assert_ne!(foo, food);
        assert_eq!(foo.cmp(&big_foo), Ordering::Equal);
        assert_eq!(foo.cmp(&food), Ordering::Less);
        assert_eq!(food.cmp(&foo), Ordering::Greater);

        let foo_os_str = OsStr::new("foo");
        assert_eq!(foo, foo_os_str);
        assert_eq!(foo.partial_cmp(foo_os_str), Some(Ordering::Equal));
    }
}
