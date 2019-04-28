use crate::builtins::Builtin;
use failure::Fallible;
use shell_vm::{Environment, IoEnvironment, Status, WaitableStatus};
use std::path::PathBuf;
use structopt::*;

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
    ) -> Fallible<WaitableStatus> {
        for name in &self.names {
            environment.unset(name);
        }
        Ok(Status::Complete(0.into()).into())
    }
}
