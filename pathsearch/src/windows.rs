use failure::Fallible;
use std::ffi::{OsStr, OsString};
use std::os::windows::ffi::{OsStrExt, OsStringExt};
use std::path::{Path, PathBuf};

/// Returns an OsString composed from `a` with `b` appended
pub fn concat_osstr(a: &OsStr, b: &OsStr) -> OsString {
    let mut res: Vec<u16> = a.encode_wide().collect();
    for c in b.encode_wide() {
        res.push(c);
    }
    OsStringExt::from_wide(&res)
}

/// WindowsPathSearcher is an iterator that yields candidate PathBuf instances
/// generated from searching the PATH environment variable following the
/// standard windows rules: explode PATH by the system path separator character
/// and then for each entry, concatenate the candidate command and test
/// whether that is a file.  Additional candidates are produced by taking
/// each of the filename extensions specified by the PATHEXT environment
/// variable and concatenating those with the command.
pub struct WindowsPathSearcher<'a> {
    path_ext: &'a OsStr,
    path_iter: std::env::SplitPaths<'a>,
    path_ext_iter: Option<std::env::SplitPaths<'a>>,
    command: &'a OsStr,
    candidate: Option<PathBuf>,
}

impl<'a> WindowsPathSearcher<'a> {
    pub fn new<T: AsRef<OsStr> + ?Sized>(
        command: &'a T,
        path: Option<&'a OsStr>,
        path_ext: Option<&'a OsStr>,
    ) -> Self {
        let path = path.unwrap_or_else(|| OsStr::new(""));
        let path_ext = path_ext.unwrap_or_else(|| OsStr::new(".EXE"));
        let path_iter = std::env::split_paths(path);
        let path_ext_iter = None;
        let command = command.as_ref();
        let candidate = None;
        Self {
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

    /// Returns the next candidate executable file
    fn next(&mut self) -> Option<PathBuf> {
        loop {
            if let Some(iter) = self.path_ext_iter.as_mut() {
                while let Some(ext) = iter.next() {
                    let extended = PathBuf::from(concat_osstr(
                        self.candidate.as_ref().unwrap().as_os_str(),
                        ext.as_os_str(),
                    ));
                    if extended.is_file() {
                        return Some(extended);
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
