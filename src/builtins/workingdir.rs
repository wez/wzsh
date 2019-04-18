use crate::builtins::Builtin;
use crate::execenv::ExecutionEnvironment;
use crate::exitstatus::ExitStatus;
use failure::{err_msg, Fallible};
use std::path::{Component, Path, PathBuf};
use structopt::*;

#[derive(Debug, StructOpt)]
/// The pwd utility writes the absolute pathname of the current working directory to the standard output.
pub struct PwdCommand {
    /// Display the logical current working directory.
    /// This is the default behavior.
    #[structopt(short = "L", conflicts_with = "physical")]
    logical: bool,

    /// Display the physical current working directory (all symbolic links resolved).
    #[structopt(short = "P", conflicts_with = "logical")]
    physical: bool,
}

impl Builtin for PwdCommand {
    fn name() -> &'static str {
        "pwd"
    }

    fn run(&mut self, exe: &ExecutionEnvironment) -> Fallible<ExitStatus> {
        let pwd = if self.physical {
            exe.cwd().canonicalize()?
        } else {
            exe.cwd().to_path_buf()
        };
        writeln!(exe.stdout(), "{}", pwd.display())?;
        Ok(ExitStatus::ExitCode(0))
    }
}

// https://pubs.opengroup.org/onlinepubs/009695399/utilities/cd.html

#[derive(Debug, StructOpt)]
/// The cd utility changes the working directory of the current shell environment.
pub struct CdCommand {
    /// Handle the operand dot-dot logically; symbolic link components shall not be resolved before dot-dot components are processed
    #[structopt(short = "L", conflicts_with = "physical")]
    logical: bool,

    /// Handle the operand dot-dot physically; symbolic link components shall be resolved before dot-dot components are processed
    #[structopt(short = "P", conflicts_with = "logical")]
    physical: bool,

    /// The destination directory
    directory: Option<PathBuf>,
}

/// Normalize the path so that redundant components such as `.`
/// and `..` are appropriately removed.  This normalization does
/// not look at the filesystem; it is a logical rather than
/// canonical normalization that is unaware of symlinks.
fn normalize_path<P: AsRef<Path>>(p: P) -> PathBuf {
    let mut normalized = PathBuf::new();
    for component in p.as_ref().components() {
        match component {
            Component::RootDir => {
                normalized = PathBuf::new();
                normalized.push("/");
            }
            Component::Prefix(prefix) => {
                normalized = PathBuf::new();
                normalized.push(prefix.as_os_str());
            }
            Component::CurDir => {}
            Component::ParentDir => {
                normalized.pop();
            }
            Component::Normal(s) => {
                normalized.push(s);
            }
        }
    }
    normalized
}

fn canonicalize_path<P: AsRef<Path>>(p: P, physical: bool) -> Fallible<PathBuf> {
    if physical {
        p.as_ref().canonicalize().map_err(|e| e.into())
    } else {
        Ok(normalize_path(p))
    }
}

impl Builtin for CdCommand {
    fn name() -> &'static str {
        "cd"
    }

    fn run(&mut self, exe: &ExecutionEnvironment) -> Fallible<ExitStatus> {
        let directory = match self.directory.take() {
            Some(dir) => dir,
            None => {
                if let Some(home) = exe.env().get("HOME") {
                    PathBuf::from(home)
                } else {
                    writeln!(exe.stderr(), "$HOME is not set")?;
                    return Ok(ExitStatus::ExitCode(1));
                }
            }
        };

        let mut print = false;

        let curpath = if directory.is_absolute() {
            directory
        } else if directory == Path::new("-") {
            print = true;
            PathBuf::from(
                exe.env()
                    .get("OLDPWD")
                    .ok_or_else(|| err_msg("OLDPWD is not set"))?,
            )
        } else {
            exe.cwd().join(directory)
        };

        let cwd = canonicalize_path(curpath, self.physical)?;
        if !cwd.is_dir() {
            writeln!(
                exe.stderr(),
                "wzsh: cd: {} is not a directory",
                cwd.display()
            )?;
            return Ok(ExitStatus::ExitCode(1));
        }

        exe.chdir(cwd.clone());
        if print {
            writeln!(exe.stdout(), "{}", cwd.display())?;
        }
        Ok(ExitStatus::ExitCode(0))
    }
}
