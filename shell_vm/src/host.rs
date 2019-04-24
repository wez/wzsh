use failure::Fallible;
use std::ffi::OsString;

pub trait ShellHost: std::fmt::Debug {
    /// Look up the home directory for the specified user.
    /// If user is not specified, look it up for the current user.
    fn lookup_homedir(&self, user: Option<&str>) -> Fallible<OsString>;
}
