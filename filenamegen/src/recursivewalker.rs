use crate::node::Node;
use crate::{new_binary_pattern_string, Walker};
use bstr::BStr;
use regex::bytes::Regex;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct RecursiveWalker {
    walk_root: PathBuf,
    walk: Option<walkdir::IntoIter>,
    regex: Regex,
}

impl RecursiveWalker {
    pub fn new<'a>(
        nodes: std::iter::Peekable<std::slice::Iter<'a, Node>>,
        walk_root: PathBuf,
    ) -> Self {
        let mut pattern = new_binary_pattern_string();
        Node::RecursiveMatch.append_regex(&mut pattern);
        for node in nodes {
            #[cfg(not(windows))]
            pattern.push('/');
            #[cfg(windows)]
            pattern.push_str("[/\\\\]");
            node.append_regex(&mut pattern);
        }
        pattern.push('$');
        let regex = Regex::new(&pattern).expect("regex to compile");

        Self {
            regex,
            walk_root,
            walk: None,
        }
    }

    pub(crate) fn next<'a>(&mut self, walker: &mut Walker<'a>) -> Option<PathBuf> {
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
                    return Some(
                        entry
                            .path()
                            .strip_prefix(&walker.root)
                            .expect("walk is always relative to walker.root")
                            .to_path_buf(),
                    );
                }
            }
        }

        None
    }

    fn is_match(&self, path: &Path) -> bool {
        let matched = if let Some(bstr) = BStr::from_path(path) {
            self.regex.is_match(bstr.as_bytes())
        } else {
            false
        };
        matched
    }
}
