use failure::Error;
use shell_lexer::{LexError, Span};
use shell_parser::ParseErrorKind;

fn extract_error_range(e: &Error) -> Option<Span> {
    if let Some(lex_err) = e.downcast_ref::<LexError>() {
        Some(lex_err.span)
    } else if let Some(parse_err) = e.downcast_ref::<ParseErrorKind>() {
        match parse_err {
            ParseErrorKind::UnexpectedToken(token, ..) => Some(token.span()),
        }
    } else {
        None
    }
}

pub fn print_error(e: &Error, input: &str) {
    for item in e.iter_chain() {
        eprintln!("wzsh: {}", item);
    }
    if let Some(span) = extract_error_range(e) {
        let lines: Vec<&str> = input.split('\n').collect();

        let start_line = &lines[span.start.line];
        let end_line = &lines[span.end.line];

        let mut indicator = String::new();
        let end_col = if span.start.line == span.end.line {
            span.end.col
        } else {
            start_line.len()
        };

        for _ in 0..span.start.col {
            indicator.push(' ');
        }

        indicator.push_str("\x1b[1m");
        for _ in span.start.col..=end_col {
            indicator.push('^');
        }
        indicator.push_str("\x1b[0m");

        eprintln!("{}", start_line);
        eprintln!("{}", indicator);

        if span.end.line != span.start.line {
            indicator.clear();
            indicator.push_str("\x1b[1m");
            for _ in 0..=span.end.col {
                indicator.push('^');
            }
            indicator.push_str("\x1b[0m");
            eprintln!("{}", end_line);
            eprintln!("{}", indicator);
        }
    }
}
