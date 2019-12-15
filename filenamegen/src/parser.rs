use crate::new_binary_pattern_string;
use crate::node::{Node, RegexAndTokens};
use crate::token::Token;
use anyhow::{anyhow, ensure};
use regex::bytes::Regex;

struct Parser<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    tokens: Vec<Token>,
    in_alternative: bool,
    in_class: usize,
}

impl<'a> Parser<'a> {
    fn parse(&mut self) -> anyhow::Result<()> {
        while let Some(c) = self.next() {
            match c {
                '\\' => {
                    if let Some(c) = self.next() {
                        self.tokens.push(Token::Literal(c));
                    } else {
                        self.tokens.push(Token::Literal('\\'));
                    }
                }
                '[' => {
                    if self.in_class == 0 {
                        self.tokens.push(Token::StartClass)
                    } else {
                        self.tokens.push(Token::ClassContent('['))
                    }
                    self.in_class += 1;
                }
                ']' if self.in_class > 0 => {
                    if self.in_class == 1 {
                        self.tokens.push(Token::EndClass);
                    } else {
                        self.tokens.push(Token::ClassContent(']'));
                    }
                    self.in_class -= 1;
                }
                '!' if self.in_class > 0 => self.tokens.push(Token::NegateClass),
                c if self.in_class > 0 => self.tokens.push(Token::ClassContent(c)),
                '{' if self.in_class == 0 => {
                    ensure!(
                        !self.in_alternative,
                        "cannot start an alternative inside an alternative"
                    );
                    self.in_alternative = true;
                    self.tokens.push(Token::StartAlternative)
                }
                ',' if self.in_alternative => self.tokens.push(Token::NextAlternative),
                '}' if self.in_alternative => {
                    ensure!(
                        self.in_alternative,
                        "cannot end an alternative when not already inside an alternative"
                    );
                    self.in_alternative = false;
                    self.tokens.push(Token::EndAlternative)
                }
                '?' => self.tokens.push(Token::Any),
                '*' => self.tokens.push(Token::ZeroOrMore),
                c => self.tokens.push(Token::Literal(c)),
            }
        }
        ensure!(!self.in_alternative, "missing closing alternative");
        ensure!(self.in_class == 0, "missing closing class");
        Ok(())
    }

    /// If the series of tokens is composed entirely of literals,
    /// returns them combined into a string
    fn collapse_literals(&mut self) -> Option<String> {
        let mut s = String::with_capacity(self.tokens.len());
        for t in &self.tokens {
            match t {
                Token::Literal(c) => s.push(*c),
                _ => return None,
            }
        }
        Some(s)
    }

    fn compile_to_regex(&mut self) -> anyhow::Result<Regex> {
        let mut pattern = new_binary_pattern_string();
        for (i, token) in self.tokens.iter().enumerate() {
            token.append_regex(&mut pattern, i == 0);
        }
        pattern.push('$');
        Regex::new(&pattern).map_err(|e| anyhow!("error compiling regex: {}: {}", pattern, e))
    }

    fn next(&mut self) -> Option<char> {
        self.chars.next()
    }

    #[allow(unused)]
    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|&ch| ch)
    }
}

/// Parse a pattern string into a Node.
pub fn parse(pattern: &str) -> anyhow::Result<Node> {
    let mut parser = Parser {
        chars: pattern.chars().peekable(),
        tokens: vec![],
        in_alternative: false,
        in_class: 0,
    };

    parser.parse()?;

    if let Some(literal) = parser.collapse_literals() {
        Ok(Node::LiteralComponents(literal.into()))
    } else {
        Ok(Node::Regex(RegexAndTokens::new(
            parser.compile_to_regex()?,
            parser.tokens,
        )))
    }
}
