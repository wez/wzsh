use crate::builtins::Builtin;
use crate::execenv::ExecutionEnvironment;
use crate::exitstatus::ExitStatus;
use failure::Fallible;
use structopt::*;

#[derive(StructOpt)]
/// The `:` command only expand command arguments. It is used when a command is needed, as in the
/// then condition of an if command, but nothing is done by the command.
pub struct ColonCommand {}

impl Builtin for ColonCommand {
    fn name() -> &'static str {
        ":"
    }

    fn run(&mut self, _exe: &ExecutionEnvironment) -> Fallible<ExitStatus> {
        Ok(ExitStatus::ExitCode(0))
    }
}
