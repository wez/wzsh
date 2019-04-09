use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct Aliases {
    map: HashMap<String, String>,
}

impl Aliases {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn alias(&mut self, name: &str, value: &str) {
        self.map.insert(name.to_string(), value.to_string());
    }

    pub fn unalias(&mut self, name: &str) {
        self.map.remove(name);
    }

    pub fn lookup(&self, name: &str) -> Option<&String> {
        self.map.get(name)
    }
}
