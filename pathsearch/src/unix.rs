use crate::SimplePathSearcher;
use std::ffi::{OsStr, OsString};
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::path::{Path, PathBuf};

/// Returns true if the specified path has executable access permissions
/// for the current process.  The check is made using the access(2)
/// syscall.
pub fn is_executable(path: &Path) -> anyhow::Result<bool> {
    use libc::{access, X_OK};

    let cstr = std::ffi::CString::new(path.as_os_str().as_bytes().to_vec())?;
    let res = unsafe { access(cstr.as_ptr(), X_OK) };
    Ok(res == 0)
}

/// Returns an OsString composed from `a` with `b` appended
pub fn concat_osstr(a: &OsStr, b: &OsStr) -> OsString {
    let a = a.as_bytes();
    let b = b.as_bytes();
    let mut res = Vec::with_capacity(a.len() + b.len());
    res.extend_from_slice(a);
    res.extend_from_slice(b);
    OsStringExt::from_vec(res)
}

/// ExecutablePathSearcher is an iterator that yields candidate PathBuf instances
/// generated from searching the supplied path string following the
/// standard rules: explode path by the system path separator character
/// and then for each entry, concatenate the candidate command and test
/// whether that is an executable file.
pub struct ExecutablePathSearcher<'a>(SimplePathSearcher<'a>);

impl<'a> ExecutablePathSearcher<'a> {
    /// Create a new ExecutablePathSearcher that will yield candidate paths for
    /// the specified command.
    /// `path` should be the path string to be split and searched.  This is typically
    /// the value of the `PATH` environment variable.
    /// The `_path_ext` parameter is present for easier cross platform support
    /// when running on Windows.  It is ignored on unix systems, but on
    /// windows systems this is typically the value of the `PATHEXT` environment variable.
    pub fn new<T: AsRef<OsStr> + ?Sized>(
        command: &'a T,
        path: Option<&'a OsStr>,
        _path_ext: Option<&'a OsStr>,
    ) -> Self {
        Self(SimplePathSearcher::new(command, path))
    }
}

impl<'a> Iterator for ExecutablePathSearcher<'a> {
    type Item = PathBuf;

    /// Returns the next candidate executable file
    fn next(&mut self) -> Option<PathBuf> {
        while let Some(candidate) = self.0.next() {
            if let Ok(true) = is_executable(&candidate) {
                return Some(candidate);
            }
        }
        None
    }
}
