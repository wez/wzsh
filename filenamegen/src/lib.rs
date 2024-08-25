//! ## Filename Generation, aka Globbing.
//!
//! This crate implements shell style file name generation a.k.a.: globbing.
//! The provided globber can expand globs relative to a specified directory (or
//! just the current working directory).  `filenamegen` tries to avoid
//! walking down paths that will never match a glob in order to reduce
//! pressure on the underlying filesystem.
//!
//! This simple example recursively finds all of the rust source files under
//! the current directory.
//!
//! ```
//! use filenamegen::Glob;
//!
//! fn main() -> anyhow::Result<()> {
//!   let glob = Glob::new("**/*.rs")?;
//!   for path in glob.walk(std::env::current_dir()?) {
//!     println!("{}", path.display());
//!   }
//!   Ok(())
//! }
//! ```

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
    pub fn new(pattern: &str) -> anyhow::Result<Glob> {
        let mut nodes: Vec<Node> = vec![];
        for comp in Path::new(pattern).components() {
            let token = match comp {
                Component::Prefix(s) => {
                    nodes.clear();
                    Node::LiteralComponents(PathBuf::from(s.as_os_str()))
                }
                Component::RootDir => {
                    if nodes.len() == 1 && nodes[0].is_literal_prefix_component() {
                        // Retain any prefix component; it it logically part
                        // of this new RootDir
                    } else {
                        nodes.clear();
                    }
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

/// This is triply gross because the string needs to be translated
/// from WTF-8 to UCS-2, normalized, and then re-encoded back to WTF-8
#[cfg(windows)]
fn normalize_slashes(path: PathBuf) -> PathBuf {
    use std::ffi::OsString;
    use std::os::windows::ffi::{OsStrExt, OsStringExt};

    let mut normalized: Vec<u16> = path
        .into_os_string()
        .encode_wide()
        .map(|c| if c == b'\\' as u16 { b'/' as u16 } else { c })
        .collect();

    // Strip off the normalized long filename prefix.
    const LONG_FILE_NAME_PREFIX: [u16; 4] = [b'/' as u16, b'/' as u16, b'?' as u16, b'/' as u16];
    if normalized.starts_with(&LONG_FILE_NAME_PREFIX) {
        for _ in 0..LONG_FILE_NAME_PREFIX.len() {
            normalized.remove(0);
        }
    }

    OsString::from_wide(&normalized).into()
}

#[cfg(not(windows))]
fn normalize_slashes(path: PathBuf) -> PathBuf {
    path
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
    use tempfile::TempDir;

    #[allow(unused)]
    fn make_dirs_in(root: &TempDir, dirs: &[&str]) -> anyhow::Result<()> {
        for d in dirs {
            let p = root.path().join(d);
            std::fs::create_dir_all(p)?;
        }
        Ok(())
    }

    fn touch_file<P: AsRef<Path>>(path: P) -> anyhow::Result<()> {
        eprintln!("touch_file {}", path.as_ref().display());
        let _file = std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(path.as_ref())?;
        Ok(())
    }

    fn touch_files_in(root: &TempDir, files: &[&str]) -> anyhow::Result<()> {
        for f in files {
            let p = root.path().join(f);
            let d = p.parent().unwrap();
            std::fs::create_dir_all(d)?;
            touch_file(p)?;
        }
        Ok(())
    }

    fn make_fixture() -> anyhow::Result<TempDir> {
        #[cfg(unix)]
        {
            // Canonicalize the temp dir; on macos this location
            // is a symlink and that messes with some test assertions
            std::env::set_var("TMPDIR", std::env::temp_dir().canonicalize()?);
        }
        Ok(TempDir::new()?)
    }

    #[test]
    fn test_simple() -> anyhow::Result<()> {
        let root = make_fixture()?;
        touch_files_in(&root, &["src/lib.rs"])?;
        let glob = Glob::new("src/*.rs")?;
        assert_eq!(glob.walk(root), vec![PathBuf::from("src/lib.rs")]);
        Ok(())
    }

    #[test]
    fn test_non_relative() -> anyhow::Result<()> {
        let root = make_fixture()?;
        touch_files_in(&root, &["src/lib.rs"])?;
        let glob = Glob::new(&format!(
            "{}/src/*.rs",
            normalize_slashes(root.path().to_path_buf()).display()
        ))?;
        assert_eq!(
            glob.walk(&std::env::current_dir()?),
            vec![normalize_slashes(root.path().join("src/lib.rs"))]
        );
        Ok(())
    }

    #[test]
    fn non_utf8_node_match() -> anyhow::Result<()> {
        let node = parse("*.rs")?;
        use bstr::BStr;
        use bstr::B;
        let pound = BStr::new(B(b"\xa3.rs"));

        eprintln!("pound is {:?}", pound);
        eprintln!("node is {:?}", node);
        assert_eq!(node.is_match(&pound), true);

        Ok(())
    }

    #[test]
    fn spaces_and_parens() -> anyhow::Result<()> {
        let root = make_fixture()?;
        touch_files_in(&root, &["Program Files (x86)/Foo Bar/baz.exe"])?;

        let glob = Glob::new("Program Files (x86)/*")?;
        assert_eq!(
            glob.walk(&root.path()),
            vec![PathBuf::from("Program Files (x86)/Foo Bar")]
        );

        let glob = Glob::new(
            normalize_slashes(root.path().join("Program Files (x86)/*"))
                .to_str()
                .unwrap(),
        )?;

        assert_eq!(
            glob.walk(&root),
            vec![root.path().join("Program Files (x86)/Foo Bar")]
        );
        assert_eq!(
            glob.walk(&std::env::current_dir()?),
            vec![root.path().join("Program Files (x86)/Foo Bar")]
        );

        let glob = Glob::new(
            normalize_slashes(root.path().join("Program Files (x86)/*/baz.exe"))
                .to_str()
                .unwrap(),
        )?;
        assert_eq!(
            glob.walk(&std::env::current_dir()?),
            vec![root.path().join("Program Files (x86)/Foo Bar/baz.exe")]
        );
        Ok(())
    }

    #[test]
    #[cfg(windows)]
    fn case_insensitive() -> anyhow::Result<()> {
        let node = parse("foo/bar.rs")?;
        use bstr::B;
        use bstr::BStr;
        let upper = B(b"FOO/bAr.rs");

        assert_eq!(node.is_match(BStr::new(&upper)), true);
        Ok(())
    }

    #[test]
    #[cfg(all(unix, not(target_os = "macos")))]
    fn test_non_utf8_on_disk() -> anyhow::Result<()> {
        use bstr::ByteSlice;
        use bstr::B;

        if nix::sys::utsname::uname()
            .unwrap()
            .release()
            .to_str()
            .unwrap()
            .contains("Microsoft")
        {
            // If we're running on WSL the filesystem has
            // tigher restrictions
            return Ok(());
        }

        let root = make_fixture()?;
        let pound = B(b"\xa3.rs").to_path()?;
        // Some operating systems/filesystems won't allow us to create invalid utf8 names
        if touch_file(root.path().join(&pound)).is_ok() {
            let glob = Glob::new("*.rs")?;
            assert_eq!(glob.walk(root), vec![pound.to_path_buf()]);
        }
        Ok(())
    }

    #[test]
    fn test_lua_toml() -> anyhow::Result<()> {
        let root = make_fixture()?;
        touch_files_in(
            &root,
            &["simple.lua", "assets/policy-extras/bar.lua", "assets/policy-extras/shaping.toml"],
        )?;
        let glob = Glob::new("**/*.{lua,toml}")?;
        assert_eq!(
            glob.walk(&root),
            vec![
                PathBuf::from("assets/policy-extras/bar.lua"),
                PathBuf::from("assets/policy-extras/shaping.toml"),
                PathBuf::from("simple.lua"),
            ]
        );

        Ok(())
    }

    #[test]
    fn test_more() -> anyhow::Result<()> {
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
    fn test_doublestar() -> anyhow::Result<()> {
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
    fn glob_up() -> anyhow::Result<()> {
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
    fn alternative() -> anyhow::Result<()> {
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
    fn bogus_alternative() -> anyhow::Result<()> {
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
    fn class() -> anyhow::Result<()> {
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
