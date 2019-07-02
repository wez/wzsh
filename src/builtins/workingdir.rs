use crate::builtins::Builtin;
use crate::shellhost::FunctionRegistry;
use cancel::Token;
use failure::{err_msg, Fallible};
use shell_vm::{Environment, IoEnvironment, Status, WaitableStatus};
use std::io::Write;
use std::path::{Component, Path, PathBuf};
use std::sync::Arc;
use structopt::*;

#[derive(Debug, StructOpt)]
/// The pwd utility writes the absolute pathname of the current working directory to the standard output.
pub struct PwdCommand {
    /// Display the logical current working directory.
    /// This is the default behavior.
    #[structopt(short = "L", overrides_with = "physical")]
    logical: bool,

    /// Display the physical current working directory (all symbolic links resolved).
    #[structopt(short = "P", overrides_with = "logical")]
    physical: bool,
}

impl Builtin for PwdCommand {
    fn name() -> &'static str {
        "pwd"
    }

    fn run(
        &mut self,
        _environment: &mut Environment,
        current_directory: &mut PathBuf,
        io_env: &IoEnvironment,
        _cancel: Arc<Token>,
        _functions: &Arc<FunctionRegistry>,
    ) -> Fallible<WaitableStatus> {
        let pwd = if self.physical {
            current_directory.canonicalize()?
        } else {
            current_directory.clone()
        };
        writeln!(io_env.stdout(), "{}", pwd.display())?;
        Ok(Status::Complete(0.into()).into())
    }
}

// https://pubs.opengroup.org/onlinepubs/009695399/utilities/cd.html

#[derive(Debug, StructOpt)]
/// The cd utility changes the working directory of the current shell environment.
pub struct CdCommand {
    /// Handle the operand dot-dot logically; symbolic link components shall not be resolved before dot-dot components are processed
    #[structopt(short = "L", overrides_with = "physical")]
    logical: bool,

    /// Handle the operand dot-dot physically; symbolic link components shall be resolved before dot-dot components are processed
    #[structopt(short = "P", overrides_with = "logical")]
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

    fn run(
        &mut self,
        environment: &mut Environment,
        current_directory: &mut PathBuf,
        io_env: &IoEnvironment,
        _cancel: Arc<Token>,
        _functions: &Arc<FunctionRegistry>,
    ) -> Fallible<WaitableStatus> {
        let directory = match self.directory.take() {
            Some(dir) => dir,
            None => {
                if let Some(home) = environment.get("HOME") {
                    PathBuf::from(home)
                } else {
                    writeln!(io_env.stderr(), "$HOME is not set")?;
                    return Ok(Status::Complete(1.into()).into());
                }
            }
        };

        let mut print = false;

        let curpath = if directory.is_absolute() {
            directory
        } else if directory == Path::new("-") {
            print = true;
            PathBuf::from(
                environment
                    .get("OLDPWD")
                    .ok_or_else(|| err_msg("OLDPWD is not set"))?,
            )
        } else {
            current_directory.join(directory)
        };

        let cwd = canonicalize_path(curpath, self.physical)?;
        if !cwd.is_dir() {
            writeln!(
                io_env.stderr(),
                "wzsh: cd: {} is not a directory",
                cwd.display()
            )?;
            return Ok(Status::Complete(1.into()).into());
        }

        *current_directory = cwd.clone();
        if print {
            writeln!(io_env.stdout(), "{}", cwd.display())?;
        }
        return Ok(Status::Complete(0.into()).into());
    }
}
