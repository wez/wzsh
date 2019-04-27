use failure::Fallible;
use shell_vm::Environment;
use std::ffi::{OsStr, OsString};
#[cfg(unix)]
use std::os::unix::ffi::{OsStrExt, OsStringExt};
#[cfg(windows)]
use std::os::windows::ffi::{OsStrExt, OsStringExt};
use std::path::{Path, PathBuf};

#[cfg(unix)]
fn is_executable(path: &Path) -> Fallible<bool> {
    use libc::{access, X_OK};

    let cstr = std::ffi::CString::new(path.as_os_str().as_bytes().to_vec())?;
    let res = unsafe { access(cstr.as_ptr(), X_OK) };
    Ok(res == 0)
}

#[cfg(windows)]
fn is_executable(_path: &Path) -> Fallible<bool> {
    Ok(true)
}

#[cfg(unix)]
fn concat_osstr(a: &OsStr, b: &OsStr) -> OsString {
    let a = a.as_bytes();
    let b = b.as_bytes();
    let mut res = Vec::with_capacity(a.len() + b.len());
    res.extend_from_slice(a);
    res.extend_from_slice(b);
    OsStringExt::from_vec(res)
}

#[cfg(windows)]
fn concat_osstr(a: &OsStr, b: &OsStr) -> OsString {
    let mut res: Vec<u16> = a.encode_wide().collect();
    for c in b.encode_wide() {
        res.push(c);
    }
    OsStringExt::from_wide(res)
}

#[cfg(unix)]
pub type PathSearcher<'a> = UnixPathSearcher<'a>;
#[cfg(windows)]
pub type PathSearcher<'a> = WindowsPathSearcher<'a>;

/// UnixPathSearcher is an iterator that yields candidate PathBuf instances
/// generated from searching the PATH environment variable following the
/// standard unix rules: explode PATH by the system path separator character
/// and then for each entry, concatenate the candidate command and test
/// whether that is an executable file.
#[allow(dead_code)]
pub struct UnixPathSearcher<'a> {
    path: &'a OsStr,
    path_iter: std::env::SplitPaths<'a>,
    command: &'a OsStr,
}

impl<'a> UnixPathSearcher<'a> {
    /// Create a new UnixPathSearcher that will yield candidate paths for
    /// the specified command
    #[allow(dead_code)]
    pub fn new<T: AsRef<OsStr> + ?Sized>(command: &'a T, env: &'a Environment) -> Self {
        let path = env.get("PATH").unwrap_or_else(|| OsStr::new(""));
        let path_iter = std::env::split_paths(path);
        let command = command.as_ref();
        Self {
            path,
            path_iter,
            command,
        }
    }
}

impl<'a> Iterator for UnixPathSearcher<'a> {
    type Item = PathBuf;

    fn next(&mut self) -> Option<PathBuf> {
        loop {
            let entry = self.path_iter.next()?;
            let candidate = entry.join(self.command);

            if candidate.is_file() {
                if let Ok(true) = is_executable(&candidate) {
                    return Some(candidate);
                }
            }
        }
    }
}

/// UnixPathSearcher is an iterator that yields candidate PathBuf instances
/// generated from searching the PATH environment variable following the
/// standard windows rules: explode PATH by the system path separator character
/// and then for each entry, concatenate the candidate command and test
/// whether that is a file.  Additional candidates are produced by taking
/// each of the filename extensions specified by the PATHEXT environment
/// variable and concatenating those with the command.
#[allow(dead_code)]
pub struct WindowsPathSearcher<'a> {
    path: &'a OsStr,
    path_ext: &'a OsStr,
    path_iter: std::env::SplitPaths<'a>,
    path_ext_iter: Option<std::env::SplitPaths<'a>>,
    command: &'a OsStr,
    candidate: Option<PathBuf>,
}

impl<'a> WindowsPathSearcher<'a> {
    #[allow(dead_code)]
    pub fn new<T: AsRef<OsStr> + ?Sized>(command: &'a T, env: &'a Environment) -> Self {
        let path = env.get("PATH").unwrap_or_else(|| OsStr::new(""));
        let path_ext = env.get("PATHEXT").unwrap_or_else(|| OsStr::new(".EXE"));
        let path_iter = std::env::split_paths(path);
        let path_ext_iter = None;
        let command = command.as_ref();
        let candidate = None;
        Self {
            path,
            path_iter,
            path_ext,
            path_ext_iter,
            command,
            candidate,
        }
    }
}

impl<'a> Iterator for WindowsPathSearcher<'a> {
    type Item = PathBuf;

    fn next(&mut self) -> Option<PathBuf> {
        loop {
            if let Some(iter) = self.path_ext_iter.as_mut() {
                loop {
                    if let Some(ext) = iter.next() {
                        let extended = PathBuf::from(concat_osstr(
                            self.candidate.as_ref().unwrap().as_os_str(),
                            ext.as_os_str(),
                        ));
                        if extended.is_file() {
                            return Some(extended);
                        }
                    }
                }
            }

            let entry = self.path_iter.next()?;
            let candidate = entry.join(self.command);
            self.candidate = Some(candidate.clone());
            self.path_ext_iter = Some(std::env::split_paths(self.path_ext));

            if candidate.is_file() {
                return Some(candidate);
            }
        }
    }
}
