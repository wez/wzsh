use crate::builtins::Builtin;
use failure::Fallible;
use shell_vm::{Environment, IoEnvironment, Status, WaitableStatus};
use std::path::PathBuf;
use structopt::*;

#[derive(StructOpt)]
/// The true utility shall return with exit code zero.
pub struct TrueCommand {}

impl Builtin for TrueCommand {
    fn name() -> &'static str {
        "true"
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

#[derive(StructOpt)]
/// The false utility shall return with a non-zero exit code.
pub struct FalseCommand {}

impl Builtin for FalseCommand {
    fn name() -> &'static str {
        "false"
    }

    fn run(
        &mut self,
        _environment: &mut Environment,
        _current_directory: &mut PathBuf,
        _io_env: &IoEnvironment,
    ) -> Fallible<WaitableStatus> {
        Ok(Status::Complete(1.into()).into())
    }
}
