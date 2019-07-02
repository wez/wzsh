use crate::builtins::Builtin;
use crate::shellhost::FunctionRegistry;
use cancel::Token;
use failure::Fallible;
use shell_vm::{Environment, IoEnvironment, Status, WaitableStatus};
use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;
use structopt::*;

#[derive(StructOpt)]
/// Set the export attribute for variables (note that this is always
/// set in the current version of wzsh)
pub struct ExportCommand {
    names: Vec<String>,
    /// Print exported variables in a syntax compatible with the shell
    #[structopt(short = "p", conflicts_with = "names")]
    print: bool,
}

impl Builtin for ExportCommand {
    fn name() -> &'static str {
        "export"
    }

    fn run(
        &mut self,
        environment: &mut Environment,
        _current_directory: &mut PathBuf,
        io_env: &IoEnvironment,
        cancel: Arc<Token>,
        _functions: &Arc<FunctionRegistry>,
    ) -> Fallible<WaitableStatus> {
        if self.print {
            for (k, v) in environment.iter() {
                cancel.check_cancel()?;
                match (k.to_str(), v.to_str()) {
                    (Some(k), Some(v)) => writeln!(io_env.stdout(), "export {}={}", k, v)?,
                    _ => writeln!(
                        io_env.stderr(),
                        "{:?}={:?} cannot be formatted as utf-8",
                        k,
                        v
                    )?,
                }
            }
        } else {
            for name in &self.names {
                // parse `name=value` and assign
                let split: Vec<&str> = name.splitn(2, '=').collect();
                if split.len() == 2 {
                    environment.set(split[0], split[1]);
                }
            }
        }
        Ok(Status::Complete(0.into()).into())
    }
}

#[derive(StructOpt)]
/// Unset a variable from the environment
pub struct UnsetCommand {
    names: Vec<String>,
}

impl Builtin for UnsetCommand {
    fn name() -> &'static str {
        "unset"
    }

    fn run(
        &mut self,
        environment: &mut Environment,
        _current_directory: &mut PathBuf,
        _io_env: &IoEnvironment,
        _cancel: Arc<Token>,
        _functions: &Arc<FunctionRegistry>,
    ) -> Fallible<WaitableStatus> {
        for name in &self.names {
            environment.unset(name);
        }
        Ok(Status::Complete(0.into()).into())
    }
}
