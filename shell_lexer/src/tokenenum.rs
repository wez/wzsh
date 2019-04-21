use regex::Regex;
use std::collections::HashMap;

macro_rules! TokenEnum {
    ($Enum:ident, $Matcher:ident, $(
            $text:literal : $variant:ident
        ),+) => {

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum $Enum {
    $(
        $variant
    ),+
}

impl std::fmt::Display for $Enum {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
        $(
            $Enum::$variant => write!(fmt, "{}", $text)
        ),+
        }
    }
}

lazy_static::lazy_static! {
    static ref $Matcher: $crate::tokenenum::LiteralMatcher<$Enum> = {
        $crate::tokenenum::LiteralMatcher::new(&[
            $(
                ($text, $Enum::$variant)
            ),+
        ])
    };
}

    }
}

#[derive(Debug)]
pub struct LiteralMatcher<T: Copy> {
    re: Regex,
    map: HashMap<&'static str, T>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MatchResult<T: Copy> {
    Match(T, usize),
    No,
}

impl<T: Copy> LiteralMatcher<T> {
    pub fn new(literals: &[(&'static str, T)]) -> Self {
        let mut pattern = String::new();
        let mut map = HashMap::new();
        pattern.push_str("^(");
        for (idx, lit) in literals.iter().enumerate() {
            if idx > 0 {
                pattern.push('|');
            }
            pattern.push_str(&regex::escape(lit.0));
            map.insert(lit.0, lit.1);
        }
        pattern.push_str(")");

        Self {
            re: Regex::new(&pattern).unwrap(),
            map,
        }
    }

    pub fn lookup(&self, text: &str) -> Option<T> {
        self.map.get(text).map(|v| *v)
    }

    pub fn matches(&self, text: &str) -> MatchResult<T> {
        if let Some(m) = self.re.find(text) {
            MatchResult::Match(*self.map.get(m.as_str()).unwrap(), m.as_str().len())
        } else {
            MatchResult::No
        }
    }
}
