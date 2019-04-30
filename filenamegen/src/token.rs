#[derive(Debug)]
pub enum Token {
    Literal(char),
    /// `?`
    Any,
    /// `*`
    ZeroOrMore,
    StartAlternative,
    NextAlternative,
    EndAlternative,
}

impl Token {
    /// Append a regex representation of Token to the supplied pattern string
    pub fn append_regex(&self, pattern: &mut String, is_first_in_component: bool) {
        match self {
            Token::Literal(c) => pattern.push_str(&regex::escape(&c.to_string())),
            // `?` matches any single character, except for `.` at the start of
            // a filename.
            Token::Any => {
                if is_first_in_component {
                    pattern.push_str("[^./]");
                } else {
                    pattern.push_str("[^/]");
                }
            }
            // `*` matches 0 or more of any character,
            // except for `.` at the start of a filename.
            Token::ZeroOrMore => {
                if is_first_in_component {
                    pattern.push_str("[^./][^/]*");
                } else {
                    pattern.push_str("[^/]*");
                }
            }
            Token::StartAlternative => pattern.push('('),
            Token::NextAlternative => pattern.push('|'),
            Token::EndAlternative => pattern.push(')'),
        }
    }
}
