use crate::node::Node;
use crate::{new_binary_pattern_string, normalize_slashes};
use bstr::ByteSlice;
use regex::bytes::Regex;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct RecursiveWalker {
    /// The literal path components consumed before reaching the `**`,
    /// relative to the walk root of the glob.
    prefix: PathBuf,
    walk_root: PathBuf,
    walk: Option<walkdir::IntoIter>,
    regex: Regex,
}

impl RecursiveWalker {
    pub fn new<'a>(
        nodes: std::iter::Peekable<std::slice::Iter<'a, Node>>,
        root: &Path,
        prefix: PathBuf,
    ) -> Self {
        // `join` discards `root` when `prefix` is absolute, matching how an
        // absolute pattern resolves in the non-recursive walker.
        let walk_root = root.join(&prefix);
        let mut pattern = new_binary_pattern_string();
        Node::RecursiveMatch.append_regex(&mut pattern);
        for node in nodes {
            #[cfg(not(windows))]
            pattern.push_str("/?");
            #[cfg(windows)]
            pattern.push_str("[/\\\\]?");
            node.append_regex(&mut pattern);
        }
        pattern.push('$');
        let regex = Regex::new(&pattern).expect("regex to compile");

        Self {
            regex,
            prefix,
            walk_root,
            walk: None,
        }
    }

    pub(crate) fn next(&mut self) -> Option<PathBuf> {
        if self.walk.is_none() {
            self.walk = Some(
                walkdir::WalkDir::new(&self.walk_root)
                    .follow_links(true)
                    .into_iter(),
            );
        }

        while let Some(entry) = self.walk.as_mut().unwrap().next() {
            if let Ok(entry) = entry {
                let path = entry
                    .path()
                    .strip_prefix(&self.walk_root)
                    .expect("walk is always relative to self.walk_root");
                if self.is_match(path) {
                    // `path` is relative to `walk_root`. Rejoining `prefix`
                    // yields a result in the same shape the non-recursive
                    // walker produces, relative to the walk root of the glob.
                    return Some(normalize_slashes(self.prefix.join(path)));
                }
            }
        }

        None
    }

    fn is_match(&self, path: &Path) -> bool {
        let matched = if let Some(bytes) = <[u8]>::from_path(path) {
            self.regex.is_match(bytes)
        } else {
            false
        };
        matched
    }
}
