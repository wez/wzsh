use crate::node::Node;
use crate::normalize_slashes;
use crate::recursivewalker::RecursiveWalker;
use crate::Walker;
use bstr::BStr;
use std::ffi::OsStr;
use std::path::PathBuf;

/// A simple node-by-node walker
#[derive(Debug)]
pub struct NodeWalker<'a> {
    node: std::iter::Peekable<std::slice::Iter<'a, Node>>,
    node_to_match: Option<&'a Node>,
    current_dir: PathBuf,
    dir: Option<std::fs::ReadDir>,
    current_dir_has_literals: bool,
}

fn entry_may_be_dir(entry: &std::fs::DirEntry) -> bool {
    match entry.file_type() {
        // The entry is a regular file and can't be opened as a dir.
        Ok(file_type) if file_type.is_file() => false,
        // Could be a dir or a symlink.  For the symlink case
        // we won't know if the target is a file or a dir without
        // stating it.  In the case that that stat call tells us it
        // is a symlink we then need to perform an opendir() on it.
        // We can save the stat and just try to open the dir, so we
        // return true for both dir and symlink and let the kernel
        // tell us that the opendir failed.
        Ok(_) => true,
        // Failed to query the file type, which most likely means that
        // we lack access rights, so we skip it.
        _ => false,
    }
}

impl<'a> NodeWalker<'a> {
    pub fn new(nodes: &'a [Node]) -> Self {
        Self {
            node: nodes.iter().peekable(),
            node_to_match: None,
            current_dir: PathBuf::new(),
            dir: None,
            current_dir_has_literals: false,
        }
    }

    /// Fork off a new NodeWalker to follow a sub-dir
    fn fork(&self, child_dir: &OsStr) -> Self {
        Self {
            node: self.node.clone(),
            node_to_match: None,
            current_dir: self.current_dir.join(child_dir),
            dir: None,
            current_dir_has_literals: false,
        }
    }

    // Advance to the next directory component
    fn next_candidate_path(&mut self) -> Option<&'a Node> {
        while let Some(node) = self.node.next() {
            match node {
                Node::LiteralComponents(literal) => {
                    self.current_dir = self.current_dir.join(literal);
                    // Since we're combining a run of LiteralComponents
                    // together without returning a corresponding Node,
                    // we need to set a flag to remind ourselves to
                    // check the resultant path later.
                    self.current_dir_has_literals = true;
                }
                _ => return Some(node),
            }
        }
        None
    }

    /// Attempt to match the next entry by reading the dir
    fn next_from_dir(&mut self, walker: &mut Walker<'a>) -> Option<PathBuf> {
        while let Some(entry) = self.dir.as_mut().unwrap().next() {
            match entry {
                Err(_) => continue,
                Ok(entry) => {
                    let file_name = entry.path();
                    let base_name = file_name.file_name().unwrap();
                    if let Some(bstr) = BStr::from_os_str(base_name) {
                        if self.node_to_match.as_ref().unwrap().is_match(&bstr) {
                            let is_leaf = self.node.peek().is_none();
                            if is_leaf {
                                return Some(normalize_slashes(
                                    entry
                                        .path()
                                        .strip_prefix(&walker.root)
                                        .map(|p| p.to_path_buf())
                                        .unwrap_or_else(|_| entry.path()),
                                ));
                            } else if entry_may_be_dir(&entry) {
                                // We can only really match if this non-leaf node
                                // is a directory
                                let route = self.fork(base_name);
                                walker.stack.push_back(route);
                            }
                        }
                    }
                }
            }
        }
        return None;
    }

    pub(crate) fn next(&mut self, walker: &mut Walker<'a>) -> Option<PathBuf> {
        loop {
            if self.dir.is_some() {
                return self.next_from_dir(walker);
            }

            // Advance to the next directory component
            self.node_to_match = match self.next_candidate_path() {
                None => {
                    // If we walked a sequence of LiteralComponents at the end
                    // of the pattern we'll end up here without yielding a node.
                    // In this case current_dir is the candidate path to match.
                    if self.current_dir_has_literals {
                        self.current_dir_has_literals = false;
                        let candidate = walker.root.join(&self.current_dir);
                        if candidate.exists() {
                            return Some(candidate);
                        }
                    }
                    return None;
                }
                node => node,
            };

            if self.node_to_match.as_ref().unwrap().is_recursive() {
                if self.node.peek().is_some() {
                    walker.recursive.push_back(RecursiveWalker::new(
                        self.node.clone(),
                        walker.root.join(&self.current_dir),
                    ));
                    return None;
                }
                // Otherwise: a leaf recursive match is equivalent to ZeroOrMore
            }

            let name = walker.root.join(&self.current_dir);
            match std::fs::read_dir(&name) {
                Err(_) => return None,
                Ok(dir) => {
                    self.dir = Some(dir);
                }
            }
        }
    }
}
