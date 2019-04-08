use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct Aliases {
    map: HashMap<Vec<u8>, Vec<u8>>,
}

impl Aliases {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn alias(&mut self, name: &[u8], value: &[u8]) {
        self.map.insert(name.to_vec(), value.to_vec());
    }

    pub fn unalias(&mut self, name: &[u8]) {
        self.map.remove(name);
    }

    pub fn lookup(&self, name: &[u8]) -> Option<&[u8]> {
        self.map.get(name).map(Vec::as_slice)
    }
}
