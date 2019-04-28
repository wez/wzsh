use crate::builtins::Builtin;
use failure::Fallible;
use shell_vm::{Environment, IoEnvironment, Status, WaitableStatus};
use std::path::PathBuf;
use structopt::*;

#[derive(StructOpt)]
/// The `:` command only expand command arguments. It is used when a command is needed, as in the
/// then condition of an if command, but nothing is done by the command.
pub struct ColonCommand {}

impl Builtin for ColonCommand {
    fn name() -> &'static str {
        ":"
    }

    fn run(
        &mut self,
        _environment: &mut Environment,
        _current_directory: &mut PathBuf,
        _io_env: &IoEnvironment,
    ) -> Fallible<WaitableStatus> {
        Ok(Status::Complete(0.into()).into())
    }
}
