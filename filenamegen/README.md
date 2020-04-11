# filenamegen

### Filename Generation, aka Globbing.

This crate implements shell style file name generation a.k.a.: globbing.
The provided globber can expand globs relative to a specified directory (or
just the current working directory).  `filenamegen` tries to avoid
walking down paths that will never match a glob in order to reduce
pressure on the underlying filesystem.

This simple example recursively finds all of the rust source files under
the current directory.

```rust
use filenamegen::Glob;

fn main() -> anyhow::Result<()> {
  let glob = Glob::new("**/*.rs")?;
  for path in glob.walk(std::env::current_dir()?) {
    println!("{}", path.display());
  }
  Ok(())
}
```

License: MIT
