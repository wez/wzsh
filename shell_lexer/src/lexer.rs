/// With reference to https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html
/// This module implements a lexer for the Shell Command Language.
use crate::errors::LexErrorKind;
use crate::position::{Pos, Span};
use crate::reader::{CharReader, Next, PositionedChar};
use crate::tokenenum::MatchResult;
use crate::{Operator, ReservedWord, OPERATORS, RESERVED_WORDS};
use failure::{bail, Fallible};
use lazy_static::lazy_static;
use regex::Regex;
use std::io::Read;

lazy_static! {
    static ref TILE_EXPAND_RE: Regex =
        Regex::new(r"^~([a-zA-Z_][a-zA-Z0-9_]+)?(/|$)").expect("failed to compile TILE_EXPAND_RE");
    static ref PARAM_RE: Regex =
        Regex::new(r"^([0-9]|[a-zA-Z_][a-zA-Z0-9_]+)").expect("failed to compile NAME_RE");
    static ref OPER_RE: Regex = Regex::new(r"^[%#:]?[%#-=?+]").expect("failed to compile OPER_RE");
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WordComponentKind {
    Literal(String),
    TildeExpand(Option<String>),
    ParamExpand(ParamExpr),
    CommandSubstitution(Vec<Token>),
}

impl WordComponentKind {
    pub fn literal(s: &str) -> WordComponentKind {
        WordComponentKind::Literal(s.to_owned())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WordComponent {
    pub kind: WordComponentKind,
    pub span: Span,
    pub splittable: bool,
    pub remove_backslash: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assignment {
    name: String,
    span: Span,
    value: Vec<WordComponent>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Word(Vec<WordComponent>),
    Operator(Operator, Span),
    Eof(Pos),
    Newline(Pos),
    IoNumber(usize, Span),
    Assignment(Assignment), // FIXME: lex both ways
    EndCommandSubst(Pos),
    EndParamSubst(Pos),
}

impl Token {
    /// If the token is a single literal word string, return a
    /// reference to that string.
    pub fn as_single_literal_word_string(&self) -> Option<&str> {
        if let Token::Word(ref word) = self {
            if word.len() == 1 {
                if let WordComponentKind::Literal(ref first_word) = &word[0].kind {
                    return Some(first_word);
                }
            }
        }
        None
    }

    pub fn is_reserved_word(&self, reserved: ReservedWord) -> bool {
        if let Some(literal) = self.as_single_literal_word_string() {
            if let Some(word) = RESERVED_WORDS.lookup(literal) {
                return word == reserved;
            }
        }
        false
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParamOper {
    /// `${NAME}` or `$NAME`, returns the value of parameter named NAME
    Get,
    /// `${#NAME}`, returns length of the value of parameter named NAME
    StringLength,
    /// `${NAME:-word}` returns the value of the named parameter unless
    /// it is unset or null, then return the expansion of word.
    /// `${NAME-word}` returns the value of named parameter unless it
    /// is unset, then return the expansion of word.
    GetDefault { allow_null: bool },
    /// `${NAME:=word}`.  If the named parameter is unset or null, then
    /// assign it the expansion of word.  Returns the final value of
    /// the named parameter.
    AssignDefault { allow_null: bool },
    /// `${NAME:?}` or `${NAME:?error message}`  Expands to the named
    /// parameter unless it is unset or null, in which case the optional
    /// message is expanded and printed and the script terminated.
    CheckSet { allow_null: bool },
    /// `${NAME:+word}`.  If the named value is unset or null, expands
    /// to null, otherwise expands to word.
    AlternativeValue { allow_null: bool },
    /// `${NAME%word}` word will be expanded and treated as a pattern.
    /// $NAME shall be expanded with the smallest portion of the suffix
    /// matched by the pattern deleted.
    RemoveSmallestSuffixPattern,
    /// `${NAME%%word}` word will be expanded and treated as a pattern.
    /// $NAME shall be expanded with the largest portion of the suffix
    /// matched by the pattern deleted.
    RemoveLargestSuffixPattern,
    /// `${NAME#word}` word will be expanded and treated as a pattern.
    /// $NAME shall be expanded with the smallest portion of the prefix
    /// matched by the pattern deleted.
    RemoveSmallestPrefixPattern,
    /// `${NAME##word}` word will be expanded and treated as a pattern.
    /// $NAME shall be expanded with the largest portion of the prefix
    /// matched by the pattern deleted.
    RemoveLargestPrefixPattern,
}

/// Represents a parameter expansion expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParamExpr {
    pub kind: ParamOper,
    pub name: String,
    pub word: Vec<Token>,
}

#[derive(Debug, PartialEq, Eq)]
enum State {
    Top,
    AssignmentWord,
    ParamExprWord,
    DoubleQuotes,
    CommandSubstitution(char),
}

#[derive(Debug)]
struct LexState {
    state: State,
    current_word: Option<Vec<WordComponent>>,
    open_paren_count: usize,
}

pub struct Lexer<R: Read> {
    reader: CharReader<R>,
    stack: Vec<LexState>,
    last_token: Option<Token>,
}

impl<R: Read> Lexer<R> {
    pub fn new(stream: R) -> Self {
        Self {
            reader: CharReader::new(stream),
            last_token: None,
            stack: vec![LexState {
                state: State::Top,
                current_word: None,
                open_paren_count: 0,
            }],
        }
    }

    pub fn next_token(&mut self) -> Fallible<Token> {
        match self.state().state {
            State::CommandSubstitution(_)
            | State::Top
            | State::AssignmentWord
            | State::ParamExprWord => self.top(),
            State::DoubleQuotes => bail!("invalid state for next_token {:?}", self.state().state),
        }
    }

    fn push_state(&mut self, state: State) {
        self.stack.push(LexState {
            state,
            current_word: None,
            open_paren_count: 0,
        });
    }

    fn pop_state(&mut self) {
        self.stack.pop();
    }

    fn unget_token(&mut self, token: Token) {
        assert!(self.last_token.is_none());
        self.last_token.replace(token);
    }

    fn top(&mut self) -> Fallible<Token> {
        loop {
            if let Some(token) = self.last_token.take() {
                return Ok(token);
            }

            if let MatchResult::Match(..) = self.reader.matches_literal(&OPERATORS)? {
                if let Some(token) = self.delimit_current_word() {
                    return Ok(token);
                }

                let (op, span) = self.reader.next_literal(&OPERATORS)?.unwrap();

                if let State::CommandSubstitution(')') = self.state().state {
                    let state = self.state();
                    match op {
                        Operator::LeftParen => state.open_paren_count += 1,
                        Operator::RightParen => {
                            state.open_paren_count -= 1;
                            if state.open_paren_count == 0 {
                                return Ok(Token::EndCommandSubst(span.start));
                            }
                        }
                        _ => {}
                    }
                }

                return Ok(Token::Operator(op, span));
            }

            if self.reader.matches_io_number()? {
                if let Some(token) = self.delimit_current_word() {
                    return Ok(token);
                }
                let (num, span) = self.reader.next_io_number()?.unwrap();
                return Ok(Token::IoNumber(num, span));
            }

            if self.reader.matches_assignment_word()? {
                if let Some(token) = self.delimit_current_word() {
                    return Ok(token);
                }
                let (name, span) = self.reader.next_assignment_word()?.unwrap();
                self.push_state(State::AssignmentWord);
                let value = match self.top()? {
                    Token::Word(value) => value,
                    token => {
                        self.unget_token(token);
                        vec![]
                    }
                };
                self.pop_state();
                return Ok(Token::Assignment(Assignment { name, span, value }));
            }

            match self.reader.next_char() {
                Next::Eof(pos) => {
                    if let Some(token) = self.delimit_current_word() {
                        return Ok(token);
                    }
                    return Ok(Token::Eof(pos));
                }
                Next::Error(err, pos) => return Err(err.context(pos).into()),
                Next::Char(c) => {
                    let is_param = self.state().state == State::ParamExprWord;

                    if c.c == ' ' || c.c == '\t' || c.c == '\r' {
                        if let Some(token) = self.delimit_current_word() {
                            return Ok(token);
                        }
                    } else if c.c == '\n' {
                        if let Some(token) = self.delimit_current_word() {
                            self.reader.unget(c);
                            return Ok(token);
                        }
                        return Ok(Token::Newline(c.pos));
                    } else if c.c == '\'' {
                        self.single_quotes(c.pos)?;
                    } else if c.c == '"' {
                        self.double_quotes(c.pos)?;
                    } else if c.c == '\\' {
                        // Nesting backtick command substitution makes this a bit gross :-/
                        match self.state().state {
                            // Are we in backtick command substitution?  If the closer is
                            // not a close paren, then we are.
                            State::CommandSubstitution(closer) if closer != ')' => {
                                let maybe_backtick = self
                                    .next_char_or_err(LexErrorKind::EofDuringCommandSubstitution)?;
                                // If we get "\\`" as a sequence then we are either starting
                                // or finishing a nested backtick sequence depending on our
                                // current state.
                                if maybe_backtick.c == '`' {
                                    if closer == '\\' {
                                        // Closing nested backtick
                                        if let Some(token) = self.delimit_current_word() {
                                            self.reader.unget(maybe_backtick);
                                            self.reader.unget(c);
                                            return Ok(token);
                                        }
                                        return Ok(Token::EndCommandSubst(maybe_backtick.pos));
                                    } else if closer == '`' {
                                        // Opening nested backtick
                                        self.command(
                                            c.pos,
                                            PositionedChar {
                                                c: '\\',
                                                pos: c.pos,
                                            },
                                        )?;
                                    } else {
                                        bail!("unexpected backtick closer {}", closer);
                                    }
                                } else {
                                    self.reader.unget(maybe_backtick);
                                    self.backslash(c)?;
                                }
                            }
                            _ => self.backslash(c)?,
                        };
                    } else if c.c == '$' {
                        self.dollar(c.pos)?;
                    } else if c.c == '`' {
                        if let State::CommandSubstitution('`') = self.state().state {
                            if let Some(token) = self.delimit_current_word() {
                                self.reader.unget(c);
                                return Ok(token);
                            }
                            return Ok(Token::EndCommandSubst(c.pos));
                        } else {
                            self.command(c.pos, c)?;
                        }
                    } else if is_param && c.c == '}' {
                        if let Some(token) = self.delimit_current_word() {
                            self.reader.unget(c);
                            return Ok(token);
                        }
                        return Ok(Token::EndParamSubst(c.pos));
                    } else {
                        self.add_char_to_word(c);
                    }
                }
            }
        }
    }

    fn next_char_or_err(&mut self, err: LexErrorKind) -> Fallible<PositionedChar> {
        match self.reader.next_char() {
            Next::Char(b) => Ok(b),
            Next::Eof(pos) => Err(err.at(pos.into()).into()),
            Next::Error(e, pos) => Err(e.context(pos).into()),
        }
    }

    fn state(&mut self) -> &mut LexState {
        self.stack.last_mut().unwrap()
    }

    fn delimit_current_word(&mut self) -> Option<Token> {
        let state = self.state();
        if let Some(mut word) = state.current_word.take() {
            // Check to see if we can apply tilde expansion before we return
            let last = if state.state == State::AssignmentWord {
                split_first_word_by_unquoted_colons(&mut word)
            } else {
                0
            };
            for i in (0..=last).rev() {
                apply_single_tilde_expansion(i, &mut word);
            }

            Some(Token::Word(word))
        } else {
            None
        }
    }

    fn backslash(&mut self, backslash: PositionedChar) -> Fallible<()> {
        let quoted = self.next_char_or_err(LexErrorKind::EofDuringBackslash)?;
        self.add_char_to_word(backslash);
        self.add_char_to_word(quoted);
        Ok(())
    }

    fn single_quotes(&mut self, start: Pos) -> Fallible<()> {
        let mut accumulator = String::new();
        let mut end;
        loop {
            let c = self.next_char_or_err(LexErrorKind::EofDuringSingleQuotedString)?;
            end = c.pos;
            if c.c == '\'' {
                break;
            }
            accumulator.push(c.c);
        }

        let word = WordComponent {
            kind: WordComponentKind::Literal(accumulator),
            span: Span::new(start, end),
            splittable: false,
            remove_backslash: false,
        };

        self.add_to_word(word);
        Ok(())
    }

    fn dollar(&mut self, start: Pos) -> Fallible<()> {
        let c = self.next_char_or_err(LexErrorKind::EofDuringParameterExpansion)?;
        if c.c == '(' {
            let maybe_paren = self.next_char_or_err(LexErrorKind::EofDuringParameterExpansion)?;
            if maybe_paren.c == '(' {
                self.arithmetic(start)
            } else {
                self.reader.unget(maybe_paren);
                self.command(start, c)
            }
        } else {
            self.parameter_expansion(start, c)
        }
    }

    fn command(&mut self, start: Pos, opener: PositionedChar) -> Fallible<()> {
        let closer = match opener.c {
            '(' => ')',
            '`' => '`',
            '\\' => '\\',
            _ => bail!("unhandled opener: {:?}", opener),
        };
        self.push_state(State::CommandSubstitution(closer));
        if opener.c == '(' {
            self.state().open_paren_count = 1;
        }
        let mut tokens = vec![];
        let mut end = start;
        while let Ok(token) = self.top() {
            match token {
                Token::EndCommandSubst(pos) => {
                    end = pos;
                    break;
                }
                Token::Eof(pos) => {
                    return Err(LexErrorKind::EofDuringCommandSubstitution
                        .at(pos.into())
                        .into());
                }
                token => tokens.push(token),
            }
        }
        self.pop_state();
        let word = WordComponent {
            kind: WordComponentKind::CommandSubstitution(tokens),
            span: Span::new(start, end),
            splittable: true,
            remove_backslash: true,
        };

        self.add_to_word(word);
        Ok(())
    }

    fn arithmetic(&mut self, _start: Pos) -> Fallible<()> {
        bail!("arithmetic not done");
    }

    fn parameter_expansion(&mut self, start: Pos, c: PositionedChar) -> Fallible<()> {
        let curlies = if c.c != '{' {
            self.reader.unget(c);
            false
        } else {
            true
        };

        let mut oper = if curlies {
            let hash = self.next_char_or_err(LexErrorKind::EofDuringParameterExpansion)?;
            if hash.c == '#' {
                Some(ParamOper::StringLength)
            } else {
                self.reader.unget(hash);
                None
            }
        } else {
            None
        };

        let (caps, name_pos) = match self.reader.matches_regex(&PARAM_RE)? {
            Some(tuple) => tuple,
            None => {
                self.reader.unget(c);
                self.add_char_to_word(PositionedChar { c: '$', pos: start });
                return Ok(());
            }
        };

        let name = caps.get(0).unwrap().as_str().to_string();
        self.reader.fixup_matched_length(name.len());
        let mut end = name_pos;
        end.col += name.len() - 1;

        if curlies && oper.is_none() {
            if let Some((caps, _oper_pos)) = self.reader.matches_regex(&OPER_RE)? {
                let oper_len = caps.get(0).unwrap().as_str().len();
                oper = Some(match caps.get(0).unwrap().as_str() {
                    ":-" => ParamOper::GetDefault { allow_null: false },
                    "-" => ParamOper::GetDefault { allow_null: true },
                    ":=" => ParamOper::AssignDefault { allow_null: false },
                    "=" => ParamOper::AssignDefault { allow_null: true },
                    ":?" => ParamOper::CheckSet { allow_null: false },
                    "?" => ParamOper::CheckSet { allow_null: true },
                    ":+" => ParamOper::AlternativeValue { allow_null: false },
                    "+" => ParamOper::AlternativeValue { allow_null: true },
                    "%" => ParamOper::RemoveSmallestSuffixPattern,
                    "%%" => ParamOper::RemoveLargestSuffixPattern,
                    "#" => ParamOper::RemoveSmallestPrefixPattern,
                    "##" => ParamOper::RemoveLargestPrefixPattern,
                    wat => bail!("unhandled operator type {}", wat),
                });
                self.reader.fixup_matched_length(oper_len);
            }
        }
        let oper = oper.unwrap_or(ParamOper::Get);

        let word = if curlies {
            self.push_state(State::ParamExprWord);
            let mut tokens = vec![];

            while let Ok(token) = self.top() {
                match token {
                    Token::EndParamSubst(pos) => {
                        end = pos;
                        break;
                    }
                    Token::Eof(pos) => {
                        return Err(LexErrorKind::EofDuringParameterExpansion
                            .at(pos.into())
                            .into());
                    }
                    token => tokens.push(token),
                }
            }
            self.pop_state();
            tokens
        } else {
            vec![]
        };

        self.add_to_word(WordComponent {
            kind: WordComponentKind::ParamExpand(ParamExpr {
                kind: oper,
                name,
                word,
            }),
            span: Span::new(start, end),
            splittable: true,
            remove_backslash: false,
        });

        Ok(())
    }

    fn double_quotes(&mut self, start: Pos) -> Fallible<()> {
        self.push_state(State::DoubleQuotes);

        let mut backslash = false;
        let end;
        loop {
            let c = self.next_char_or_err(LexErrorKind::EofDuringDoubleQuotedString)?;
            if c.c == '"' && !backslash {
                end = c.pos;
                break;
            } else if c.c == '$' && !backslash {
                self.dollar(c.pos)?;
            } else {
                backslash = false;
                if c.c == '\\' {
                    backslash = true;
                }
                self.add_char_to_word(c);
            }
        }

        let word = self.state().current_word.take();
        self.pop_state();

        if let Some(mut word) = word {
            word.first_mut().unwrap().span.start = start;
            word.last_mut().unwrap().span.end = end;

            // Quoted strings are not subject to field splitting
            for mut component in word {
                component.splittable = false;
                self.add_to_word(component);
            }
        }

        Ok(())
    }

    fn add_to_word(&mut self, word: WordComponent) {
        let state = self.state();
        if state.current_word.is_none() {
            state.current_word = Some(vec![]);
        }
        state.current_word.as_mut().unwrap().push(word);
    }

    fn add_char_to_word(&mut self, c: PositionedChar) {
        let state = self.state();
        if state.current_word.is_none() {
            state.current_word = Some(vec![]);
        }
        let current = state.current_word.as_mut().unwrap();
        match current.last_mut() {
            Some(WordComponent {
                kind: WordComponentKind::Literal(s),
                span,
                splittable: true,
                remove_backslash: true,
            }) => {
                s.push(c.c);
                span.end = c.pos;
            }
            _ => {
                current.push(WordComponent {
                    kind: WordComponentKind::Literal(c.c.to_string()),
                    span: Span::new(c.pos, c.pos),
                    splittable: true,
                    remove_backslash: true,
                });
            }
        }
    }
}

fn apply_single_tilde_expansion(word_idx: usize, words: &mut Vec<WordComponent>) {
    if let WordComponent {
        kind: WordComponentKind::Literal(first_word),
        splittable: true,
        remove_backslash: true,
        span,
    } = &words[word_idx]
    {
        if let Some(caps) = TILE_EXPAND_RE.captures(first_word) {
            let all = caps.get(0).unwrap();
            let trailer = caps.get(2).unwrap();
            let matched_len = if trailer.as_str().len() == 1 {
                // We want to include the `/` in the remainder
                all.end() - 1
            } else {
                all.end()
            };
            let remainder = first_word[matched_len..].to_string();
            let name = caps.get(1).map(|cap| cap.as_str().to_owned());
            let span = span.clone();

            words.remove(word_idx);

            let start = span.start;
            let mut end = start;
            end.col = start.col + name.as_ref().map(|n| n.len()).unwrap_or(0);

            words.insert(
                word_idx,
                WordComponent {
                    kind: WordComponentKind::TildeExpand(name),
                    splittable: false,
                    remove_backslash: false,
                    span: Span::new(start, end),
                },
            );

            if !remainder.is_empty() {
                let mut rem_start = end;
                rem_start.col += 1;
                let mut rem_end = rem_start;
                rem_end.col += remainder.len() - 1;

                words.insert(
                    word_idx + 1,
                    WordComponent {
                        kind: WordComponentKind::Literal(remainder),
                        splittable: true,
                        remove_backslash: true,
                        span: Span::new(rem_start, rem_end),
                    },
                );
            }
        }
    }
}

fn split_first_word_by_unquoted_colons(words: &mut Vec<WordComponent>) -> usize {
    if let WordComponent {
        kind: WordComponentKind::Literal(first_word),
        splittable: true,
        remove_backslash: true,
        span,
    } = &words[0]
    {
        let first_word = first_word.to_string();
        let mut start = span.start;
        words.remove(0);
        let mut prev = None;
        let mut result = 0;
        for (idx, element) in first_word
            .split(|c| match prev.replace(c) {
                Some('\\') => false,
                _ => c == ':',
            })
            .enumerate()
        {
            if idx > 0 {
                words.insert(
                    result,
                    WordComponent {
                        kind: WordComponentKind::literal(":"),
                        splittable: false,
                        remove_backslash: false,
                        span: Span::new(start, start),
                    },
                );
                result += 1;
                start.col += 1;
            }
            let mut end = start;
            end.col += element.len() - 1;
            words.insert(
                result,
                WordComponent {
                    kind: WordComponentKind::literal(element),
                    splittable: true,
                    remove_backslash: true,
                    span: Span::new(start, end),
                },
            );
            result += 1;
            start.col = end.col + 1;
        }

        result - 1
    } else {
        0
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    fn tokens(s: &str) -> Vec<Token> {
        let mut lexer = Lexer::new(s.as_bytes());
        let mut tokens = vec![];
        loop {
            match lexer.next_token().unwrap() {
                Token::Eof(_) => break,
                token @ _ => tokens.push(token),
            }
        }
        eprintln!("tokens: {:#?}", tokens);
        tokens
    }

    fn token_err(s: &str) -> String {
        let mut lexer = Lexer::new(s.as_bytes());
        loop {
            match lexer.next_token() {
                Err(e) => return format!("{}", e),
                Ok(Token::Eof(_)) => panic!("got to EOF rather than an Error"),
                _ => {}
            }
        }
    }

    #[test]
    fn operator() {
        assert_eq!(
            tokens("||"),
            vec![Token::Operator(Operator::OrIf, Span::new_to(0, 0, 1))]
        );
        assert_eq!(
            tokens("||&&"),
            vec![
                Token::Operator(Operator::OrIf, Span::new_to(0, 0, 1)),
                Token::Operator(Operator::AndIf, Span::new_to(0, 2, 3))
            ]
        );
        assert_eq!(
            tokens("|| &&"),
            vec![
                Token::Operator(Operator::OrIf, Span::new_to(0, 0, 1)),
                Token::Operator(Operator::AndIf, Span::new_to(0, 3, 4))
            ]
        );
        assert_eq!(
            tokens("||\n&&"),
            vec![
                Token::Operator(Operator::OrIf, Span::new_to(0, 0, 1)),
                Token::Newline(Pos::new(0, 2)),
                Token::Operator(Operator::AndIf, Span::new_to(1, 0, 1))
            ]
        );
    }

    #[test]
    fn single_quotes() {
        assert_eq!(
            tokens("'foo'\n&&"),
            vec![
                Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("foo"),
                    span: Span::new_to(0, 0, 4),
                    splittable: false,
                    remove_backslash: false
                },]),
                Token::Newline(Pos::new(0, 5)),
                Token::Operator(Operator::AndIf, Span::new_to(1, 0, 1))
            ]
        );
    }

    #[test]
    fn double_quotes() {
        assert_eq!(
            tokens("\"fo\\o\"bar"),
            vec![Token::Word(vec![
                WordComponent {
                    kind: WordComponentKind::literal("fo\\o"),
                    span: Span::new_to(0, 0, 5),
                    splittable: false,
                    remove_backslash: true
                },
                WordComponent {
                    kind: WordComponentKind::literal("bar"),
                    span: Span::new_to(0, 6, 8),
                    splittable: true,
                    remove_backslash: true
                },
            ]),]
        );
    }

    #[test]
    fn backslash() {
        assert_eq!(
            tokens("fo\\o"),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::literal("fo\\o"),
                span: Span::new_to(0, 0, 3),
                splittable: true,
                remove_backslash: true
            },]),]
        );
    }

    #[test]
    fn io_number() {
        assert_eq!(
            tokens("1<foo"),
            vec![
                Token::IoNumber(1, Span::new_to(0, 0, 1)),
                Token::Operator(Operator::Less, Span::new_to(0, 1, 1)),
                Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("foo"),
                    span: Span::new_to(0, 2, 4),
                    splittable: true,
                    remove_backslash: true
                },]),
            ]
        );
        assert_eq!(
            tokens("1<<foo"),
            vec![
                Token::IoNumber(1, Span::new_to(0, 0, 1)),
                Token::Operator(Operator::DoubleLess, Span::new_to(0, 1, 2)),
                Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("foo"),
                    span: Span::new_to(0, 3, 5),
                    splittable: true,
                    remove_backslash: true
                },]),
            ]
        );
        assert_eq!(
            tokens("0>foo"),
            vec![
                Token::IoNumber(0, Span::new_to(0, 0, 1)),
                Token::Operator(Operator::Great, Span::new_to(0, 1, 1)),
                Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("foo"),
                    span: Span::new_to(0, 2, 4),
                    splittable: true,
                    remove_backslash: true
                },]),
            ]
        );
    }

    #[test]
    fn assignment_word() {
        assert_eq!(
            tokens("FOO=bar"),
            vec![Token::Assignment(Assignment {
                name: "FOO".to_owned(),
                span: Span::new_to(0, 0, 4),
                value: vec![WordComponent {
                    kind: WordComponentKind::literal("bar"),
                    span: Span::new_to(0, 4, 6),
                    splittable: true,
                    remove_backslash: true
                }]
            })]
        );

        assert_eq!(
            tokens("FOO="),
            vec![Token::Assignment(Assignment {
                name: "FOO".to_owned(),
                span: Span::new_to(0, 0, 4),
                value: vec![],
            })]
        );

        assert_eq!(
            tokens("FOO=\n"),
            vec![
                Token::Assignment(Assignment {
                    name: "FOO".to_owned(),
                    span: Span::new_to(0, 0, 4),
                    value: vec![],
                }),
                Token::Newline(Pos::new(0, 4))
            ]
        );

        assert_eq!(
            tokens("FOO=||"),
            vec![
                Token::Assignment(Assignment {
                    name: "FOO".to_owned(),
                    span: Span::new_to(0, 0, 4),
                    value: vec![],
                }),
                Token::Operator(Operator::OrIf, Span::new_to(0, 4, 5)),
            ]
        );

        assert_eq!(
            tokens("FOO=~bar"),
            vec![Token::Assignment(Assignment {
                name: "FOO".to_owned(),
                span: Span::new_to(0, 0, 4),
                value: vec![WordComponent {
                    kind: WordComponentKind::TildeExpand(Some("bar".to_owned())),
                    span: Span::new_to(0, 4, 7),
                    splittable: false,
                    remove_backslash: false
                }]
            })]
        );

        assert_eq!(
            tokens("FOO=~bar:/somewhere:~foo/baz"),
            vec![Token::Assignment(Assignment {
                name: "FOO".to_owned(),
                span: Span::new_to(0, 0, 4),
                value: vec![
                    WordComponent {
                        kind: WordComponentKind::TildeExpand(Some("bar".to_owned())),
                        span: Span::new_to(0, 4, 7),
                        splittable: false,
                        remove_backslash: false
                    },
                    WordComponent {
                        kind: WordComponentKind::literal(":"),
                        span: Span::new_to(0, 8, 8),
                        splittable: false,
                        remove_backslash: false,
                    },
                    WordComponent {
                        kind: WordComponentKind::literal("/somewhere"),
                        span: Span::new_to(0, 9, 18),
                        splittable: true,
                        remove_backslash: true,
                    },
                    WordComponent {
                        kind: WordComponentKind::literal(":"),
                        span: Span::new_to(0, 19, 19),
                        splittable: false,
                        remove_backslash: false,
                    },
                    WordComponent {
                        kind: WordComponentKind::TildeExpand(Some("foo".to_owned())),
                        span: Span::new_to(0, 20, 23),
                        splittable: false,
                        remove_backslash: false
                    },
                    WordComponent {
                        kind: WordComponentKind::literal("/baz"),
                        span: Span::new_to(0, 24, 27),
                        splittable: true,
                        remove_backslash: true,
                    },
                ]
            })]
        );

        assert_eq!(
            tokens("FOO=bar baz"),
            vec![
                Token::Assignment(Assignment {
                    name: "FOO".to_owned(),
                    span: Span::new_to(0, 0, 4),
                    value: vec![WordComponent {
                        kind: WordComponentKind::literal("bar"),
                        span: Span::new_to(0, 4, 6),
                        splittable: true,
                        remove_backslash: true
                    }]
                }),
                Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("baz"),
                    span: Span::new_to(0, 8, 10),
                    splittable: true,
                    remove_backslash: true
                },]),
            ]
        );

        assert_eq!(
            tokens("FOO=\"bar baz\""),
            vec![Token::Assignment(Assignment {
                name: "FOO".to_owned(),
                span: Span::new_to(0, 0, 4),
                value: vec![WordComponent {
                    kind: WordComponentKind::literal("bar baz"),
                    span: Span::new_to(0, 4, 12),
                    splittable: false,
                    remove_backslash: true
                }]
            }),]
        );
    }

    #[test]
    fn tilde() {
        assert_eq!(
            tokens("~"),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::TildeExpand(None),
                span: Span::new_to(0, 0, 0),
                splittable: false,
                remove_backslash: false,
            }])]
        );
        assert_eq!(
            tokens("~wez"),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::TildeExpand(Some("wez".to_owned())),
                span: Span::new_to(0, 0, 3),
                splittable: false,
                remove_backslash: false,
            }])]
        );

        assert_eq!(
            tokens("~/"),
            vec![Token::Word(vec![
                WordComponent {
                    kind: WordComponentKind::TildeExpand(None),
                    span: Span::new_to(0, 0, 0),
                    splittable: false,
                    remove_backslash: false,
                },
                WordComponent {
                    kind: WordComponentKind::literal("/"),
                    span: Span::new_to(0, 1, 1),
                    splittable: true,
                    remove_backslash: true
                },
            ])]
        );
        assert_eq!(
            tokens("~wez/"),
            vec![Token::Word(vec![
                WordComponent {
                    kind: WordComponentKind::TildeExpand(Some("wez".to_owned())),
                    span: Span::new_to(0, 0, 3),
                    splittable: false,
                    remove_backslash: false,
                },
                WordComponent {
                    kind: WordComponentKind::literal("/"),
                    span: Span::new_to(0, 4, 4),
                    splittable: true,
                    remove_backslash: true
                },
            ])]
        );

        assert_eq!(
            tokens("~:"),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::literal("~:"),
                span: Span::new_to(0, 0, 1),
                splittable: true,
                remove_backslash: true
            },])]
        );
        assert_eq!(
            tokens("\\~"),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::literal("\\~"),
                span: Span::new_to(0, 0, 1),
                splittable: true,
                remove_backslash: true
            },])]
        );
    }

    #[test]
    fn dollarparamexp() {
        assert_eq!(
            tokens("\"$foo\""),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::ParamExpand(ParamExpr {
                    kind: ParamOper::Get,
                    name: "foo".to_owned(),
                    word: vec![]
                }),
                span: Span::new_to(0, 0, 5),
                splittable: false,
                remove_backslash: false,
            }])]
        );
        assert_eq!(
            tokens("\"${foo}\""),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::ParamExpand(ParamExpr {
                    kind: ParamOper::Get,
                    name: "foo".to_owned(),
                    word: vec![]
                }),
                span: Span::new_to(0, 0, 7),
                splittable: false,
                remove_backslash: false,
            }])]
        );
    }

    #[test]
    fn paramexp() {
        assert_eq!(
            tokens("$foo"),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::ParamExpand(ParamExpr {
                    kind: ParamOper::Get,
                    name: "foo".to_owned(),
                    word: vec![]
                }),
                span: Span::new_to(0, 0, 3),
                splittable: true,
                remove_backslash: false,
            }])]
        );
        assert_eq!(
            tokens("${foo}"),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::ParamExpand(ParamExpr {
                    kind: ParamOper::Get,
                    name: "foo".to_owned(),
                    word: vec![]
                }),
                span: Span::new_to(0, 0, 5),
                splittable: true,
                remove_backslash: false,
            }])]
        );
        assert_eq!(
            tokens("${#foo}"),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::ParamExpand(ParamExpr {
                    kind: ParamOper::StringLength,
                    name: "foo".to_owned(),
                    word: vec![]
                }),
                span: Span::new_to(0, 0, 6),
                splittable: true,
                remove_backslash: false,
            }])]
        );
        assert_eq!(
            tokens("${foo}bar"),
            vec![Token::Word(vec![
                WordComponent {
                    kind: WordComponentKind::ParamExpand(ParamExpr {
                        kind: ParamOper::Get,
                        name: "foo".to_owned(),
                        word: vec![]
                    }),
                    span: Span::new_to(0, 0, 5),
                    splittable: true,
                    remove_backslash: false,
                },
                WordComponent {
                    kind: WordComponentKind::literal("bar"),
                    span: Span::new_to(0, 6, 8),
                    splittable: true,
                    remove_backslash: true
                }
            ])]
        );

        assert_eq!(
            tokens("${foo:-hello there}bar"),
            vec![Token::Word(vec![
                WordComponent {
                    kind: WordComponentKind::ParamExpand(ParamExpr {
                        kind: ParamOper::GetDefault { allow_null: false },
                        name: "foo".to_owned(),
                        word: vec![
                            Token::Word(vec![WordComponent {
                                kind: WordComponentKind::literal("hello"),
                                span: Span::new_to(0, 7, 11),
                                splittable: true,
                                remove_backslash: true
                            }]),
                            Token::Word(vec![WordComponent {
                                kind: WordComponentKind::literal("there"),
                                span: Span::new_to(0, 13, 17),
                                splittable: true,
                                remove_backslash: true
                            }]),
                        ]
                    }),
                    span: Span::new_to(0, 0, 18),
                    splittable: true,
                    remove_backslash: false,
                },
                WordComponent {
                    kind: WordComponentKind::literal("bar"),
                    span: Span::new_to(0, 19, 21),
                    splittable: true,
                    remove_backslash: true
                }
            ])]
        );

        assert_eq!(
            tokens("${foo:-hello\nthere}bar"),
            vec![Token::Word(vec![
                WordComponent {
                    kind: WordComponentKind::ParamExpand(ParamExpr {
                        kind: ParamOper::GetDefault { allow_null: false },
                        name: "foo".to_owned(),
                        word: vec![
                            Token::Word(vec![WordComponent {
                                kind: WordComponentKind::literal("hello"),
                                span: Span::new_to(0, 7, 11),
                                splittable: true,
                                remove_backslash: true
                            }]),
                            Token::Newline(Pos::new(0, 12)),
                            Token::Word(vec![WordComponent {
                                kind: WordComponentKind::literal("there"),
                                span: Span::new_to(1, 0, 4),
                                splittable: true,
                                remove_backslash: true
                            }]),
                        ]
                    }),
                    span: Span::new(Pos::new(0, 0), Pos::new(1, 5)),
                    splittable: true,
                    remove_backslash: false,
                },
                WordComponent {
                    kind: WordComponentKind::literal("bar"),
                    span: Span::new_to(1, 6, 8),
                    splittable: true,
                    remove_backslash: true
                }
            ])]
        );

        assert_eq!(
            tokens("${foo:-${nest}}bar"),
            vec![Token::Word(vec![
                WordComponent {
                    kind: WordComponentKind::ParamExpand(ParamExpr {
                        kind: ParamOper::GetDefault { allow_null: false },
                        name: "foo".to_owned(),
                        word: vec![Token::Word(vec![WordComponent {
                            kind: WordComponentKind::ParamExpand(ParamExpr {
                                kind: ParamOper::Get,
                                name: "nest".to_owned(),
                                word: vec![]
                            }),
                            span: Span::new_to(0, 7, 13),
                            splittable: true,
                            remove_backslash: false,
                        }]),]
                    }),
                    span: Span::new_to(0, 0, 14),
                    splittable: true,
                    remove_backslash: false,
                },
                WordComponent {
                    kind: WordComponentKind::literal("bar"),
                    span: Span::new_to(0, 15, 17),
                    splittable: true,
                    remove_backslash: true
                }
            ])]
        );

        assert_eq!(
            tokens("${foo:-~wez}"),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::ParamExpand(ParamExpr {
                    kind: ParamOper::GetDefault { allow_null: false },
                    name: "foo".to_owned(),
                    word: vec![Token::Word(vec![WordComponent {
                        kind: WordComponentKind::TildeExpand(Some("wez".to_owned())),
                        span: Span::new_to(0, 7, 10),
                        splittable: false,
                        remove_backslash: false
                    }]),]
                }),
                span: Span::new_to(0, 0, 11),
                splittable: true,
                remove_backslash: false,
            },])]
        );
    }

    #[test]
    fn command() {
        assert_eq!(
            tokens("echo hello"),
            vec![
                Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("echo"),
                    span: Span::new_to(0, 0, 3),
                    splittable: true,
                    remove_backslash: true
                }]),
                Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("hello"),
                    span: Span::new_to(0, 5, 9),
                    splittable: true,
                    remove_backslash: true
                }]),
            ]
        );
    }

    #[test]
    fn command_subst() {
        assert_eq!(
            tokens("$(echo hello)"),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::CommandSubstitution(vec![
                    Token::Word(vec![WordComponent {
                        kind: WordComponentKind::literal("echo"),
                        span: Span::new_to(0, 2, 5),
                        splittable: true,
                        remove_backslash: true
                    }]),
                    Token::Word(vec![WordComponent {
                        kind: WordComponentKind::literal("hello"),
                        span: Span::new_to(0, 7, 11),
                        splittable: true,
                        remove_backslash: true
                    }]),
                ]),
                span: Span::new_to(0, 0, 12),
                splittable: true,
                remove_backslash: true,
            },])]
        );

        assert_eq!(
            tokens("$(echo\nhello)"),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::CommandSubstitution(vec![
                    Token::Word(vec![WordComponent {
                        kind: WordComponentKind::literal("echo"),
                        span: Span::new_to(0, 2, 5),
                        splittable: true,
                        remove_backslash: true
                    }]),
                    Token::Newline(Pos::new(0, 6)),
                    Token::Word(vec![WordComponent {
                        kind: WordComponentKind::literal("hello"),
                        span: Span::new_to(1, 0, 4),
                        splittable: true,
                        remove_backslash: true
                    }]),
                ]),
                span: Span::new(Pos::new(0, 0), Pos::new(1, 5)),
                splittable: true,
                remove_backslash: true,
            },])]
        );
    }

    #[test]
    fn command_subst_nest() {
        assert_eq!(
            tokens("$(echo $(ls))"),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::CommandSubstitution(vec![
                    Token::Word(vec![WordComponent {
                        kind: WordComponentKind::literal("echo"),
                        span: Span::new_to(0, 2, 5),
                        splittable: true,
                        remove_backslash: true
                    }]),
                    Token::Word(vec![WordComponent {
                        kind: WordComponentKind::CommandSubstitution(vec![Token::Word(vec![
                            WordComponent {
                                kind: WordComponentKind::literal("ls"),
                                span: Span::new_to(0, 9, 10),
                                splittable: true,
                                remove_backslash: true
                            }
                        ]),]),
                        span: Span::new_to(0, 7, 11),
                        splittable: true,
                        remove_backslash: true
                    }]),
                ]),
                span: Span::new_to(0, 0, 12),
                splittable: true,
                remove_backslash: true,
            },])]
        );
    }

    #[test]
    fn command_subst_backtick() {
        assert_eq!(
            tokens("`echo hello`"),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::CommandSubstitution(vec![
                    Token::Word(vec![WordComponent {
                        kind: WordComponentKind::literal("echo"),
                        span: Span::new_to(0, 1, 4),
                        splittable: true,
                        remove_backslash: true
                    }]),
                    Token::Word(vec![WordComponent {
                        kind: WordComponentKind::literal("hello"),
                        span: Span::new_to(0, 6, 10),
                        splittable: true,
                        remove_backslash: true
                    }]),
                ]),
                span: Span::new_to(0, 0, 11),
                splittable: true,
                remove_backslash: true,
            },])]
        );
    }

    #[test]
    fn command_subst_backtick_paren_nest() {
        assert_eq!(
            tokens("`echo $(ls)`"),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::CommandSubstitution(vec![
                    Token::Word(vec![WordComponent {
                        kind: WordComponentKind::literal("echo"),
                        span: Span::new_to(0, 1, 4),
                        splittable: true,
                        remove_backslash: true
                    }]),
                    Token::Word(vec![WordComponent {
                        kind: WordComponentKind::CommandSubstitution(vec![Token::Word(vec![
                            WordComponent {
                                kind: WordComponentKind::literal("ls"),
                                span: Span::new_to(0, 8, 9),
                                splittable: true,
                                remove_backslash: true
                            }
                        ]),]),
                        span: Span::new_to(0, 6, 10),
                        splittable: true,
                        remove_backslash: true
                    }]),
                ]),
                span: Span::new_to(0, 0, 11),
                splittable: true,
                remove_backslash: true,
            },])]
        );
    }

    #[test]
    fn command_subst_backtick_nest() {
        assert_eq!(
            tokens("`echo \\`ls\\``"),
            vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::CommandSubstitution(vec![
                    Token::Word(vec![WordComponent {
                        kind: WordComponentKind::literal("echo"),
                        span: Span::new_to(0, 1, 4),
                        splittable: true,
                        remove_backslash: true
                    }]),
                    Token::Word(vec![WordComponent {
                        kind: WordComponentKind::CommandSubstitution(vec![Token::Word(vec![
                            WordComponent {
                                kind: WordComponentKind::literal("ls"),
                                span: Span::new_to(0, 8, 9),
                                splittable: true,
                                remove_backslash: true
                            }
                        ]),]),
                        span: Span::new_to(0, 6, 11),
                        splittable: true,
                        remove_backslash: true
                    }]),
                ]),
                span: Span::new_to(0, 0, 12),
                splittable: true,
                remove_backslash: true,
            },])]
        );
    }

    #[test]
    fn arithmetic() {
        assert_eq!(token_err("$(($x-1))"), "arithmetic not done");
    }
}
