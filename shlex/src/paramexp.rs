use lazy_static::lazy_static;
use regex::Regex;

/// Represents a parameter expansion expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParamExpr {
    /// `${NAME}` or `$NAME`, returns the value of parameter named NAME
    Get { name: String },
    /// `${#NAME}`, returns length of the value of parameter named NAME
    StringLength { name: String },
    /// `${NAME:-word}` returns the value of the named parameter unless
    /// it is unset or null, then return the expansion of word.
    /// `${NAME-word}` returns the value of named parameter unless it
    /// is unset, then return the expansion of word.
    GetDefault {
        name: String,
        /// true when the `:` is not present.
        /// When true, allow expanding parameter to an empty string if
        /// it is set but empty.
        allow_null: bool,
        word: String,
    },
    /// `${NAME:=word}`.  If the named parameter is unset or null, then
    /// assign it the expansion of word.  Returns the final value of
    /// the named parameter.
    AssignDefault {
        name: String,
        /// true when the `:` is not present.
        /// When true, allow expanding parameter to an empty string if
        /// it is set but empty.
        allow_null: bool,
        word: String,
    },
    /// `${NAME:?}` or `${NAME:?error message}`  Expands to the named
    /// parameter unless it is unset or null, in which case the optional
    /// message is expanded and printed and the script terminated.
    CheckSet {
        name: String,
        /// true when the `:` is not present.
        /// When true, allow expanding parameter to an empty string if
        /// it is set but empty.
        allow_null: bool,
        word: String,
    },
    /// `${NAME:+word}`.  If the named value is unset or null, expands
    /// to null, otherwise expands to word.
    AlternativeValue {
        name: String,
        /// true when the `:` is not present.
        /// When true, if the value of the parameter is set but empty,
        /// expand to the value of word rather than null.
        allow_null: bool,
        word: String,
    },
    // TODO: Pattern matching notation
}

lazy_static! {
    static ref NAME_RE: Regex =
        Regex::new(r"^([0-9]|[a-zA-Z_][a-zA-Z0-9_]+)").expect("failed to compile NAME_RE");
    static ref OPER_RE: Regex = Regex::new(r"^:?[-=?+]").expect("failed to compile OPER_RE");
}

impl ParamExpr {
    /// Attempt to parse a parameter expansion expression from the start
    /// of the supplied slice.
    /// If successful, returns the expression and the remainder of the
    /// text after the closing delimiter of the expression.
    pub fn try_parse(s: &str) -> Option<(Self, &str)> {
        if !s.starts_with("$") {
            return None;
        }
        let s = &s[1..];
        if let Some(m) = NAME_RE.find(s) {
            let name = m.as_str().to_string();
            return Some((ParamExpr::Get { name }, &s[m.end()..]));
        }
        if !s.starts_with("{") {
            return None;
        }

        let s = &s[1..];
        let (is_len, s) = if s.starts_with("#") {
            (true, &s[1..])
        } else {
            (false, s)
        };

        let m = NAME_RE.find(s)?;
        let name = m.as_str().to_string();
        let remainder = &s[m.end()..];

        if is_len {
            let (empty, remain) = collect_word(remainder)?;
            if !empty.is_empty() {
                // trailing garbage
                return None;
            }
            return Some((ParamExpr::StringLength { name }, remain));
        }

        let mut iter = remainder.char_indices();
        let (first_idx, first_char) = iter.next()?;

        if first_char == '}' {
            // ${NAME}
            return Some((ParamExpr::Get { name }, &remainder[first_idx + 1..]));
        }

        let m = OPER_RE.find(remainder)?;
        let op_str = m.as_str();
        let op_len = op_str.len();
        let remainder = &remainder[op_len..];
        let (word, remainder) = collect_word(remainder)?;
        let word = word.to_string();
        match op_str {
            ":-" | "-" => {
                if word.is_empty() {
                    return None;
                }
                Some((
                    ParamExpr::GetDefault {
                        name,
                        allow_null: !op_str.starts_with(':'),
                        word,
                    },
                    remainder,
                ))
            }
            ":=" | "=" => {
                if word.is_empty() {
                    return None;
                }
                Some((
                    ParamExpr::AssignDefault {
                        name,
                        allow_null: !op_str.starts_with(':'),
                        word,
                    },
                    remainder,
                ))
            }
            ":?" | "?" => {
                // Word is allowed to be empty
                Some((
                    ParamExpr::CheckSet {
                        name,
                        allow_null: !op_str.starts_with(':'),
                        word,
                    },
                    remainder,
                ))
            }
            ":+" | "+" => {
                if word.is_empty() {
                    return None;
                }
                Some((
                    ParamExpr::AlternativeValue {
                        name,
                        allow_null: !op_str.starts_with(':'),
                        word,
                    },
                    remainder,
                ))
            }
            _ => None,
        }
    }
}

/// Given a sequence like "foo}bar" which is a sub-slice of
/// eg: "{foo}bar", returns Some("foo", "bar").
fn collect_word(remainder: &str) -> Option<(&str, &str)> {
    let mut close_stack = vec!['}'];
    let mut backslash = false;
    for (idx, c) in remainder.char_indices() {
        if backslash {
            backslash = false;
            continue;
        }
        match c {
            '\\' => {
                backslash = true;
                continue;
            }
            '{' => {
                close_stack.push('}');
                continue;
            }
            '(' => {
                close_stack.push(')');
                continue;
            }
            '[' => {
                close_stack.push(']');
                continue;
            }
            _ => {
                if c == *close_stack.last().unwrap() {
                    close_stack.pop();
                    if close_stack.is_empty() {
                        // We've found the end of the word parameter
                        return Some((&remainder[0..idx], &remainder[idx + 1..]));
                    }
                }
            }
        }
    }
    // We didn't find the closing brace before we ran out
    // of input :-/
    None
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_simple() {
        assert_eq!(ParamExpr::try_parse("$|"), None);
        assert_eq!(ParamExpr::try_parse("${"), None);
        assert_eq!(
            ParamExpr::try_parse("$0"),
            Some((
                ParamExpr::Get {
                    name: "0".to_string()
                },
                ""
            ))
        );
        assert_eq!(
            ParamExpr::try_parse("$12"),
            Some((
                ParamExpr::Get {
                    name: "1".to_string()
                },
                "2"
            ))
        );
        assert_eq!(
            ParamExpr::try_parse("$hello"),
            Some((
                ParamExpr::Get {
                    name: "hello".to_string()
                },
                ""
            ))
        );
        assert_eq!(
            ParamExpr::try_parse("${hello}world"),
            Some((
                ParamExpr::Get {
                    name: "hello".to_string()
                },
                "world"
            ))
        );

        assert_eq!(
            ParamExpr::try_parse("${#hello}"),
            Some((
                ParamExpr::StringLength {
                    name: "hello".to_string()
                },
                ""
            ))
        );
        assert_eq!(
            ParamExpr::try_parse("${#hello}trail"),
            Some((
                ParamExpr::StringLength {
                    name: "hello".to_string()
                },
                "trail"
            ))
        );
        assert_eq!(ParamExpr::try_parse("${#hello|}trail"), None,);

        assert_eq!(
            ParamExpr::try_parse("${hello:-word}trail"),
            Some((
                ParamExpr::GetDefault {
                    name: "hello".to_string(),
                    allow_null: false,
                    word: "word".to_string(),
                },
                "trail"
            ))
        );
        assert_eq!(
            ParamExpr::try_parse("${hello-word}trail"),
            Some((
                ParamExpr::GetDefault {
                    name: "hello".to_string(),
                    allow_null: true,
                    word: "word".to_string(),
                },
                "trail"
            ))
        );
        assert_eq!(ParamExpr::try_parse("${hello-}trail"), None,);

        assert_eq!(
            ParamExpr::try_parse("${hello:=word}trail"),
            Some((
                ParamExpr::AssignDefault {
                    name: "hello".to_string(),
                    allow_null: false,
                    word: "word".to_string(),
                },
                "trail"
            ))
        );
        assert_eq!(
            ParamExpr::try_parse("${hello=word}trail"),
            Some((
                ParamExpr::AssignDefault {
                    name: "hello".to_string(),
                    allow_null: true,
                    word: "word".to_string(),
                },
                "trail"
            ))
        );
        assert_eq!(ParamExpr::try_parse("${hello=}trail"), None,);

        assert_eq!(
            ParamExpr::try_parse("${hello:?word}trail"),
            Some((
                ParamExpr::CheckSet {
                    name: "hello".to_string(),
                    allow_null: false,
                    word: "word".to_string(),
                },
                "trail"
            ))
        );
        assert_eq!(
            ParamExpr::try_parse("${hello?word}trail"),
            Some((
                ParamExpr::CheckSet {
                    name: "hello".to_string(),
                    allow_null: true,
                    word: "word".to_string(),
                },
                "trail"
            ))
        );
        assert_eq!(
            ParamExpr::try_parse("${hello?}trail"),
            Some((
                ParamExpr::CheckSet {
                    name: "hello".to_string(),
                    allow_null: true,
                    word: "".to_string(),
                },
                "trail"
            ))
        );

        assert_eq!(
            ParamExpr::try_parse("${hello:+word}trail"),
            Some((
                ParamExpr::AlternativeValue {
                    name: "hello".to_string(),
                    allow_null: false,
                    word: "word".to_string(),
                },
                "trail"
            ))
        );
        assert_eq!(
            ParamExpr::try_parse("${hello+word}trail"),
            Some((
                ParamExpr::AlternativeValue {
                    name: "hello".to_string(),
                    allow_null: true,
                    word: "word".to_string(),
                },
                "trail"
            ))
        );
        assert_eq!(ParamExpr::try_parse("${hello+}trail"), None,);
    }
}
