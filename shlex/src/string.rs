//! Helpers for working OsStr, OsString.
pub use std::ffi::{OsStr, OsString};

/// The ShellString type is used to represent a string used
/// by the shell.  It can represent rust native strings
/// (which are utf-8) or OsString values which are unspecified
/// operating system encoding.
/// ShellString is convertible from String, str, OsString and OsStr.
/// When converting from OsString or OsStr, if the string can be
/// interpreted as a valid utf-8 sequence, it is converted to a
/// rust string and tracked that way.  Otherwise, it is retained
/// as an OsString.
/// Note that this assignment/conversion has no knowledge of posix
/// locales and is thus not conforming to the various encoding and
/// locale related requirements of posix.
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum ShellString {
    String(String),
    Os(OsString),
}

impl ShellString {
    pub fn as_str(&self) -> Option<&str> {
        match self {
            ShellString::String(s) => Some(s.as_str()),
            _ => None,
        }
    }
}

impl AsRef<OsStr> for ShellString {
    fn as_ref(&self) -> &OsStr {
        match self {
            ShellString::String(s) => s.as_ref(),
            ShellString::Os(s) => s.as_os_str(),
        }
    }
}

impl From<String> for ShellString {
    fn from(s: String) -> ShellString {
        ShellString::String(s)
    }
}

impl From<&str> for ShellString {
    fn from(s: &str) -> ShellString {
        ShellString::String(s.to_owned())
    }
}

impl From<OsString> for ShellString {
    fn from(s: OsString) -> ShellString {
        // NOTE: this assumes that we *should* be treating
        // the input OsStr as UTF-8 on not(windows)
        if let Some(utf8) = s.to_str() {
            ShellString::String(utf8.to_string())
        } else {
            ShellString::Os(s)
        }
    }
}

impl From<&OsStr> for ShellString {
    fn from(s: &OsStr) -> ShellString {
        // NOTE: this assumes that we *should* be treating
        // the input OsStr as UTF-8 on not(windows)
        if let Some(utf8) = s.to_str() {
            ShellString::String(utf8.to_string())
        } else {
            ShellString::Os(s.to_os_string())
        }
    }
}

impl std::fmt::Debug for ShellString {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            ShellString::String(s) => s.fmt(fmt),
            #[cfg(not(windows))]
            ShellString::Os(s) => {
                use std::os::unix::ffi::OsStrExt;
                write!(fmt, "\"")?;
                for b in s.as_bytes() {
                    write!(fmt, "\\x{:02x}", *b)?;
                }
                write!(fmt, "\"")
            }
            #[cfg(windows)]
            ShellString::Os(s) => {
                use std::os::windows::ffi::OsStrExt;
                write!(fmt, "\"")?;
                for b in s.encode_wide() {
                    write!(fmt, "\\U{:04x}", *b)?;
                }
                write!(fmt, "\"")
            }
        }
    }
}

impl std::fmt::Display for ShellString {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            ShellString::String(s) => s.fmt(fmt),
            #[cfg(not(windows))]
            ShellString::Os(s) => {
                use std::os::unix::ffi::OsStrExt;
                for b in s.as_bytes() {
                    write!(fmt, "\\x{:02x}", *b)?;
                }
                Ok(())
            }
            #[cfg(windows)]
            ShellString::Os(s) => {
                use std::os::windows::ffi::OsStrExt;
                for b in s.encode_wide() {
                    write!(fmt, "\\u{:04x}", *b)?;
                }
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn format_string() {
        let s: ShellString = "foo".into();
        assert_eq!(format!("{}", s), "foo");
        assert_eq!(format!("{:?}", s), "\"foo\"");
    }

    #[test]
    fn format_os_string_compat() {
        let os: ShellString = OsStr::new("foo").into();
        assert_eq!(format!("{}", os), "foo");
        assert_eq!(format!("{:?}", os), "\"foo\"");
    }

    #[test]
    #[cfg(not(windows))]
    fn format_os_non_utf8() {
        use std::os::unix::ffi::OsStrExt;
        // Here, the values 0x66 and 0x6f correspond to 'f' and 'o'
        // respectively. The value 0x80 is a lone continuation byte, invalid
        // in a UTF-8 sequence.
        let source = [0x66, 0x6f, 0x80, 0x6f];
        let os_str = OsStr::from_bytes(&source[..]);
        let os: ShellString = os_str.into();
        assert_eq!(format!("{}", os), "\\x66\\x6f\\x80\\x6f");
        assert_eq!(format!("{:?}", os), "\"\\x66\\x6f\\x80\\x6f\"");
    }

    #[test]
    #[cfg(windows)]
    fn format_os_non_utf8() {
        use std::ffi::OsString;
        // Here the values 0x0066 and 0x006f correspond to 'f' and 'o'
        // respectively. The value 0xD800 is a lone surrogate half, invalid
        // in a UTF-16 sequence.
        let source = [0x0066, 0x006f, 0xD800, 0x006f];
        let os_string = OsString::from_wide(&source[..]);
        let os_str = os_string.as_os_str();
        let os: ShellString = os_str.into();
        assert_eq!(format!("{}", os), "\\U0066\\U006f\\Ud800\\U006f");
        assert_eq!(format!("{:?}", os), "\"\\U0066\\U006f\\Ud800\\U006f\"");
    }
}
