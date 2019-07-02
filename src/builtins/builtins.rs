use crate::builtins::Builtin;
use crate::shellhost::FunctionRegistry;
use cancel::Token;
use failure::Fallible;
use shell_vm::{Environment, IoEnvironment, Status, WaitableStatus};
use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;
use structopt::*;

#[derive(Debug, StructOpt)]
/// List builtin commands
pub struct BuiltinsCommand {}
impl Builtin for BuiltinsCommand {
    fn name() -> &'static str {
        "builtins"
    }

    fn run(
        &mut self,
        _environment: &mut Environment,
        _current_directory: &mut PathBuf,
        io_env: &IoEnvironment,
        _cancel: Arc<Token>,
        _functions: &Arc<FunctionRegistry>,
    ) -> Fallible<WaitableStatus> {
        let mut builtins: Vec<&'static str> = super::BUILTINS.iter().map(|(k, _)| *k).collect();
        builtins.sort_unstable();
        for k in builtins {
            writeln!(io_env.stdout(), "{}", k)?;
        }
        Ok(Status::Complete(0.into()).into())
    }
}
