# pathsearch

This crate provides functions that can be used to search for an
executable based on the PATH environment on both POSIX and Windows
systems.

`find_executable_in_path` is the most convenient function exported
by this crate; given the name of an executable, it will yield the
absolute path of the first matching file.

```rust
use pathsearch::find_executable_in_path;

if let Some(exe) = find_executable_in_path("ls") {
  println!("Found ls at {}", exe.display());
}
```

`PathSearcher` is platform-independent struct that encompasses the
path searching algorithm used by `find_executable_in_path`.  Construct
it by passing in the PATH and PATHEXT (for Windows) environment variables
and iterate it to incrementally produce all candidate results.  This
is useful when implementing utilities such as `which` that want to show
all possible paths.

```rust
use pathsearch::PathSearcher;
use std::ffi::OsString;

let path = std::env::var_os("PATH");
let path_ext = std::env::var_os("PATHEXT");

for exe in PathSearcher::new(
    "zsh",
    path.as_ref().map(OsString::as_os_str),
    path_ext.as_ref().map(OsString::as_os_str),
) {
    println!("{}", exe.display());
}
```

`SimplePathSearcher` is a simple iterator that can be used to search
an arbitrary path for an arbitrary file that doesn't have to be executable.

License: MIT
