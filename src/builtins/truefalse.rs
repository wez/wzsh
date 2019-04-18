use crate::builtins::Builtin;
use crate::execenv::ExecutionEnvironment;
use crate::exitstatus::ExitStatus;
use failure::Fallible;
use structopt::*;

#[derive(StructOpt)]
/// The true utility shall return with exit code zero.
pub struct TrueCommand {}

impl Builtin for TrueCommand {
    fn name() -> &'static str {
        "true"
    }

    fn run(&mut self, _exe: &ExecutionEnvironment) -> Fallible<ExitStatus> {
        Ok(ExitStatus::ExitCode(0))
    }
}

#[derive(StructOpt)]
/// The false utility shall return with a non-zero exit code.
pub struct FalseCommand {}

impl Builtin for FalseCommand {
    fn name() -> &'static str {
        "false"
    }

    fn run(&mut self, _exe: &ExecutionEnvironment) -> Fallible<ExitStatus> {
        Ok(ExitStatus::ExitCode(1))
    }
}
