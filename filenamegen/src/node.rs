use crate::token::Token;
use bstr::BStr;
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

impl Node {
    pub fn is_match(&self, s: &BStr) -> bool {
        match self {
            Node::LiteralComponents(p) => p.as_path() == s.to_path_lossy(),
            Node::RecursiveMatch => true,
            Node::Regex(RegexAndTokens { regex, .. }) => regex.is_match(s.as_bytes()),
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
            Node::RecursiveMatch => pattern.push_str("([^./][^/]*/?)*"),
            Node::Regex(RegexAndTokens { tokens, .. }) => {
                for (i, token) in tokens.iter().enumerate() {
                    token.append_regex(pattern, i == 0);
                }
            }
        }
    }
}
