//! Filename Generation, aka Globbing.
//! ## Why not the `glob` crate?
//! * Can match non-utf-8 filenames with wildcards
//! * Can generate paths relative to a specified dir rather than
//!   assuming the current working dir

use failure::Fallible;
use std::collections::VecDeque;
use std::path::{Component, Path, PathBuf};

mod node;
mod nodewalker;
mod parser;
mod recursivewalker;
mod token;
use node::Node;
use nodewalker::NodeWalker;
use parser::parse;
use recursivewalker::RecursiveWalker;

/// Represents a compiled glob expression.
/// Depending on the pattern, evaluating the glob may use a conservative
/// walker that tries to minimize the number of syscalls to just the
/// directories in which pattern matching needs to occur.
/// If the recursive glob `**` pattern is used then we have no choice
/// but to perform a full tree walk for the appropriate portions of
/// the filesystem.
#[derive(Debug)]
pub struct Glob {
    nodes: Vec<Node>,
}

impl Glob {
    /// Compile pattern into a `Glob`
    /// Special characters allowed in pattern:
    /// `?` match any single non-directory separator character,
    ///     except for a leading `.` in the directory entry name
    ///     (this allows hiding files using the unix convention
    ///     of a leading dot)
    ///
    /// `*` like `?` except matches 0 or more characters.
    ///
    /// `\` quotes the character that follows it, preventing it from
    ///     interpreted as a special character.
    ///
    /// `**`, when used in its own non-leaf directory component, acts as a
    ///     recursive wildcard, matching any number of directories.
    ///     When used in the leaf position it acts the same as `*`.
    ///
    /// `{foo,bar}.rs` matches both `foo.rs` and `bar.rs`.  The curly braces
    ///    define an alternation regex.
    pub fn new(pattern: &str) -> Fallible<Glob> {
        let mut nodes = vec![];
        for comp in Path::new(pattern).components() {
            let token = match comp {
                Component::Prefix(s) => {
                    nodes.clear();
                    Node::LiteralComponents(PathBuf::from(s.as_os_str()))
                }
                Component::RootDir => {
                    nodes.clear();
                    Node::LiteralComponents(PathBuf::from("/"))
                }
                Component::CurDir => continue,
                Component::ParentDir => Node::LiteralComponents(PathBuf::from("..")),
                Component::Normal(s) => {
                    let s = s.to_str().expect("str input to be representable as string");

                    // Let's see if this component contains a pattern
                    match s {
                        "**" => Node::RecursiveMatch,
                        _ => parse(s)?,
                    }
                }
            };

            // Collapse contiguous LiteralComponents into a single Node
            match (&token, nodes.last_mut()) {
                (
                    Node::LiteralComponents(ref literal),
                    Some(Node::LiteralComponents(ref mut path)),
                ) => *path = path.join(literal),
                _ => nodes.push(token),
            }
        }

        Ok(Glob { nodes })
    }

    /// Walk the filesystem starting at `path` and execute the glob.
    /// Returns all matching entries in sorted order.  The entries are
    /// relative to `path`.
    pub fn walk<P: AsRef<Path>>(&self, path: P) -> Vec<PathBuf> {
        let walker = Walker::new(path.as_ref(), &self.nodes);

        let mut results: Vec<PathBuf> = walker.collect();
        results.sort();

        results
    }
}

/// Disable unicode mode so that we can match non-utf8 filenames
fn new_binary_pattern_string() -> String {
    String::from(if cfg!(windows) { "^(?i-u)" } else { "^(?-u)" })
}

/// `Walker` is the iterator implementation that drives
/// executing the glob.  It tracks both the regular walker
/// and the recursive walkers.  The regular walkers are evaluated
/// before the recursive walkers.
struct Walker<'a> {
    root: &'a Path,
    stack: VecDeque<NodeWalker<'a>>,
    recursive: VecDeque<RecursiveWalker>,
}

impl<'a> Walker<'a> {
    fn new(root: &'a Path, nodes: &'a [Node]) -> Self {
        let route = NodeWalker::new(nodes);
        let mut stack = VecDeque::new();
        stack.push_back(route);
        Self {
            root,
            stack,
            recursive: VecDeque::new(),
        }
    }
}

impl<'a> Iterator for Walker<'a> {
    type Item = PathBuf;

    fn next(&mut self) -> Option<PathBuf> {
        while let Some(mut route) = self.stack.pop_front() {
            if let Some(path) = route.next(self) {
                self.stack.push_front(route);
                return Some(path);
            }
        }

        while let Some(mut route) = self.recursive.pop_front() {
            if let Some(path) = route.next(self) {
                self.recursive.push_front(route);
                return Some(path);
            }
        }

        None
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;
    use tempdir::TempDir;

    #[allow(unused)]
    fn make_dirs_in(root: &TempDir, dirs: &[&str]) -> Fallible<()> {
        for d in dirs {
            let p = root.path().join(d);
            std::fs::create_dir_all(p)?;
        }
        Ok(())
    }

    fn touch_file<P: AsRef<Path>>(path: P) -> Fallible<()> {
        eprintln!("touch_file {}", path.as_ref().display());
        let _file = std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(path.as_ref())?;
        Ok(())
    }

    fn touch_files_in(root: &TempDir, files: &[&str]) -> Fallible<()> {
        for f in files {
            let p = root.path().join(f);
            let d = p.parent().unwrap();
            std::fs::create_dir_all(d)?;
            touch_file(p)?;
        }
        Ok(())
    }

    fn make_fixture() -> Fallible<TempDir> {
        Ok(TempDir::new("filenamegen")?)
    }

    #[test]
    fn test_simple() -> Fallible<()> {
        let root = make_fixture()?;
        touch_files_in(&root, &["src/lib.rs"])?;
        let glob = Glob::new("src/*.rs")?;
        assert_eq!(glob.walk(root), vec![PathBuf::from("src/lib.rs")]);
        Ok(())
    }

    #[test]
    fn non_utf8_node_match() -> Fallible<()> {
        let node = parse("*.rs")?;
        use bstr::B;
        let pound = B(b"\xa3.rs");

        eprintln!("pound is {:?}", pound);
        eprintln!("node is {:?}", node);
        assert_eq!(node.is_match(&pound), true);

        Ok(())
    }

    #[test]
    #[cfg(windows)]
    fn case_insensitive() -> Fallible<()> {
        let node = parse("foo/bar.rs")?;
        use bstr::B;
        let upper = B(b"FOO/bAr.rs");

        assert_eq!(node.is_match(&upper), true);
        Ok(())
    }

    #[test]
    #[cfg(all(unix, not(target_os = "macos")))]
    fn test_non_utf8_on_disk() -> Fallible<()> {
        use bstr::B;
        let root = make_fixture()?;
        let pound = B(b"\xa3.rs").to_path()?;
        // on macos, the system won't allow us to create invalid utf8 names
        touch_file(root.path().join(&pound))?;
        let glob = Glob::new("*.rs")?;
        assert_eq!(glob.walk(root), vec![pound.to_path_buf()]);
        Ok(())
    }

    #[test]
    fn test_more() -> Fallible<()> {
        let root = make_fixture()?;
        touch_files_in(
            &root,
            &["foo/src/foo.rs", "bar/src/bar.rs", "bar/src/.bar.rs"],
        )?;
        let glob = Glob::new("*/src/*.rs")?;
        assert_eq!(
            glob.walk(&root),
            vec![
                PathBuf::from("bar/src/bar.rs"),
                PathBuf::from("foo/src/foo.rs")
            ]
        );

        let glob = Glob::new("foo/src/*.rs")?;
        assert_eq!(glob.walk(&root), vec![PathBuf::from("foo/src/foo.rs")]);

        let glob = Glob::new("*/src/.*.rs")?;
        assert_eq!(glob.walk(&root), vec![PathBuf::from("bar/src/.bar.rs")]);

        let glob = Glob::new("*")?;
        assert_eq!(
            glob.walk(&root),
            vec![PathBuf::from("bar"), PathBuf::from("foo")]
        );
        Ok(())
    }

    #[test]
    fn test_doublestar() -> Fallible<()> {
        let root = make_fixture()?;
        touch_files_in(
            &root,
            &[
                "foo/src/foo.rs",
                "bar/src/bar.rs",
                "woot/woot.rs",
                "woot/.woot.rs",
            ],
        )?;
        let glob = Glob::new("**/*.rs")?;
        assert_eq!(
            glob.walk(&root),
            vec![
                PathBuf::from("bar/src/bar.rs"),
                PathBuf::from("foo/src/foo.rs"),
                PathBuf::from("woot/woot.rs")
            ]
        );

        let glob = Glob::new("**")?;
        assert_eq!(
            glob.walk(&root),
            vec![
                PathBuf::from("bar"),
                PathBuf::from("foo"),
                PathBuf::from("woot")
            ]
        );
        Ok(())
    }

    #[test]
    fn glob_up() -> Fallible<()> {
        let root = make_fixture()?;
        touch_files_in(
            &root,
            &[
                "foo/src/foo.rs",
                "bar/src/bar.rs",
                "woot/woot.rs",
                "woot/.woot.rs",
            ],
        )?;
        let glob = Glob::new("../*/*.rs")?;
        assert_eq!(
            glob.walk(root.path().join("woot")),
            vec![PathBuf::from("../woot/woot.rs")]
        );
        Ok(())
    }

    #[test]
    fn alternative() -> Fallible<()> {
        let root = make_fixture()?;
        touch_files_in(&root, &["foo.rs", "bar.rs"])?;
        let glob = Glob::new("{foo,bar}.rs")?;
        assert_eq!(
            glob.walk(&root),
            vec![PathBuf::from("bar.rs"), PathBuf::from("foo.rs")]
        );
        Ok(())
    }

    #[test]
    fn bogus_alternative() -> Fallible<()> {
        assert_eq!(
            format!("{}", Glob::new("{{").unwrap_err()),
            "cannot start an alternative inside an alternative"
        );
        assert_eq!(
            format!("{}", Glob::new("{").unwrap_err()),
            "missing closing alternative"
        );
        Ok(())
    }

    #[test]
    fn class() -> Fallible<()> {
        let root = make_fixture()?;
        touch_files_in(&root, &["foo.o", "foo.a"])?;
        let glob = Glob::new("foo.[oa]")?;
        assert_eq!(
            glob.walk(&root),
            vec![PathBuf::from("foo.a"), PathBuf::from("foo.o")]
        );
        let glob = Glob::new("foo.[[:alnum:]]")?;
        assert_eq!(
            glob.walk(&root),
            vec![PathBuf::from("foo.a"), PathBuf::from("foo.o")]
        );
        let glob = Glob::new("foo.[![:alnum:]]")?;
        assert_eq!(glob.walk(&root), Vec::<PathBuf>::new());
        Ok(())
    }
}
