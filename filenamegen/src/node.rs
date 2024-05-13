use crate::token::Token;
use bstr::BStr;
#[cfg(windows)]
use bstr::BString;
use bstr::ByteSlice;
use regex::bytes::Regex;
use std::path::PathBuf;

#[derive(Debug)]
pub enum Node {
    LiteralComponents(PathBuf),
    RecursiveMatch,
    Regex(RegexAndTokens),
}

#[derive(Debug)]
pub struct RegexAndTokens {
    regex: Regex,
    tokens: Vec<Token>,
}

impl RegexAndTokens {
    pub fn new(regex: Regex, tokens: Vec<Token>) -> Self {
        Self { regex, tokens }
    }
}

#[cfg(windows)]
fn normalize_and_lower_case(s: &BStr) -> BString {
    let mut norm = BString::new();
    for c in s.chars() {
        if c == '/' {
            norm.push_char('\\');
        } else {
            for lower in c.to_lowercase() {
                norm.push_char(lower);
            }
        }
    }
    norm
}

impl Node {
    pub fn is_match(&self, s: &BStr) -> bool {
        match self {
            #[cfg(not(windows))]
            Node::LiteralComponents(p) => p.as_path() == s.to_path_lossy(),
            #[cfg(windows)]
            Node::LiteralComponents(p) => {
                if let Some(p) = BStr::from_path(p.as_path()) {
                    normalize_and_lower_case(p) == normalize_and_lower_case(s)
                } else {
                    // If we couldn't convert ourselves to a bstr, then it
                    // cannot possibly be compared to a bstr
                    false
                }
            }
            Node::RecursiveMatch => true,
            Node::Regex(RegexAndTokens { regex, .. }) => regex.is_match(s.as_bytes()),
        }
    }

    pub fn is_literal_prefix_component(&self) -> bool {
        match self {
            Node::LiteralComponents(p) => match p.components().next() {
                Some(std::path::Component::Prefix(_)) => true,
                _ => false,
            },

            _ => false,
        }
    }

    /// Convenience for testing whether Node is RecursiveMatch
    pub fn is_recursive(&self) -> bool {
        match self {
            Node::RecursiveMatch => true,
            _ => false,
        }
    }

    /// Append a regex representation of Node to the supplied pattern string
    pub fn append_regex(&self, pattern: &mut String) {
        match self {
            Node::LiteralComponents(path) => pattern.push_str(&regex::escape(
                path.to_str()
                    .expect("pattern to be convertible back to String"),
            )),
            #[cfg(windows)]
            Node::RecursiveMatch => pattern.push_str("([^./\\\\][^/\\\\]*[/\\\\]?)*"),
            #[cfg(not(windows))]
            Node::RecursiveMatch => pattern.push_str("([^./][^/]*/?)*"),
            Node::Regex(RegexAndTokens { tokens, .. }) => {
                for (i, token) in tokens.iter().enumerate() {
                    token.append_regex(pattern, i == 0);
                }
            }
        }
    }
}
