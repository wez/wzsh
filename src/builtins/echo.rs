use crate::builtins::Builtin;
use crate::shellhost::FunctionRegistry;
use cancel::Token;
use shell_vm::{Environment, IoEnvironment, Status, WaitableStatus};
use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;
use structopt::*;

#[derive(StructOpt)]
/// Write arguments to standard output
pub struct EchoCommand {
    /// Do not output the trailing newline
    #[structopt(short = "n")]
    no_newline: bool,

    /// Enable interpretation of backslash escapes
    #[structopt(short = "e", overrides_with = "disable_escapes")]
    enable_escapes: bool,

    /// Disable interpretation of backslash escapes (default)
    #[structopt(short = "E", overrides_with = "enable_escapes")]
    _disable_escapes: bool,

    /// The strings to output
    strings: Vec<String>,
}

fn maybe_octal(s: &str) -> Option<(char, usize)> {
    if s.len() >= 3 {
        if let Ok(num) = u8::from_str_radix(&s[..3], 8) {
            return Some((num as char, 3));
        }
    }
    if s.len() >= 2 {
        if let Ok(num) = u8::from_str_radix(&s[..2], 8) {
            return Some((num as char, 2));
        }
    }
    if s.len() >= 1 {
        if let Ok(num) = u8::from_str_radix(&s[..1], 8) {
            return Some((num as char, 1));
        }
    }
    None
}

fn maybe_hex(s: &str) -> Option<(char, usize)> {
    if s.len() >= 2 {
        if let Ok(num) = u8::from_str_radix(&s[..2], 16) {
            return Some((num as char, 2));
        }
    }
    if s.len() >= 1 {
        if let Ok(num) = u8::from_str_radix(&s[..1], 16) {
            return Some((num as char, 1));
        }
    }
    None
}

fn echo_escapes(mut s: &str) -> String {
    let mut result = String::new();

    while let Some(pos) = s.find('\\') {
        // Emit text preceding this
        if pos > 0 {
            result.push_str(&s[..pos]);
        }

        s = &s[pos..];

        if s.len() < 2 {
            break;
        }

        match s.chars().nth(1).unwrap() {
            '\\' => result.push('\\'),
            'a' => result.push('\x07'),
            'b' => result.push('\x08'),
            'c' => {
                // produce no further output!
                return result;
            }
            'e' => result.push('\x1b'),
            'f' => result.push('\x0c'),
            'n' => result.push('\x0a'),
            'r' => result.push('\x0d'),
            't' => result.push('\t'),
            'v' => result.push('\x0b'),
            '0' => {
                // Octal number with 1-3 digits
                if let Some((c, len)) = maybe_octal(&s[2..]) {
                    result.push(c);
                    s = &s[2 + len..];
                    continue;
                }
                // Wasn't a valid escape, so just emit
                // that portion as-is
                result.push_str(&s[..2]);
            }
            'x' => {
                // hex number with 1-2 digits
                if let Some((c, len)) = maybe_hex(&s[2..]) {
                    result.push(c);
                    s = &s[2 + len..];
                    continue;
                }
                // Wasn't a valid escape, so just emit
                // that portion as-is
                result.push_str(&s[..2]);
            }
            _ => {
                // Unknown escape
                result.push_str(&s[..2]);
            }
        }

        s = &s[2..];
    }
    result.push_str(s);

    result
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn escapes() {
        assert_eq!(echo_escapes("foo"), "foo");
        assert_eq!(echo_escapes("foo\\\\"), "foo\\");
        assert_eq!(echo_escapes("foo\\twoot"), "foo\twoot");
        assert_eq!(echo_escapes("foo\\cnot me"), "foo");
        assert_eq!(echo_escapes("foo\\x20"), "foo\x20");
        assert_eq!(echo_escapes("foo\\x2 "), "foo\x02 ");
        assert_eq!(echo_escapes("foo\\0003"), "foo\x03");
        assert_eq!(echo_escapes("foo\\003"), "foo\x03");
        assert_eq!(echo_escapes("foo\\03"), "foo\x03");

        // o777 is 511 and is out of range for ascii,
        // so the octal escape resolves as o77 which is
        // 63 in decimal -- the question mark.
        // That leaves the final 7 as the next char
        assert_eq!(echo_escapes("foo\\0777"), "foo?7");
    }
}

impl Builtin for EchoCommand {
    fn name() -> &'static str {
        "echo"
    }

    fn run(
        &mut self,
        _environment: &mut Environment,
        _current_directory: &mut PathBuf,
        io_env: &IoEnvironment,
        cancel: Arc<Token>,
        _functions: &Arc<FunctionRegistry>,
    ) -> anyhow::Result<WaitableStatus> {
        let joined = self.strings.join(" ");

        cancel.check_cancel()?;
        if self.enable_escapes {
            let escaped = echo_escapes(&joined);
            cancel.check_cancel()?;
            write!(io_env.stdout(), "{}", escaped)?;
        } else {
            write!(io_env.stdout(), "{}", joined)?;
        }

        if !self.no_newline {
            writeln!(io_env.stdout(), "")?;
        }
        Ok(Status::Complete(0.into()).into())
    }
}
