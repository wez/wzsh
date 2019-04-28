use crate::builtins::Builtin;
use crate::pathsearch::PathSearcher;
use failure::Fallible;
use shell_vm::{Environment, IoEnvironment, Status, WaitableStatus};
use std::io::Write;
use std::path::PathBuf;
use structopt::*;

#[derive(StructOpt)]
/// Search the path for a command; if found, print out the path
/// to that command.
pub struct WhichCommand {
    /// Find all possible matches in the path, rather than stopping
    /// at the first one
    #[structopt(short = "a")]
    all: bool,

    /// The command to find
    command: PathBuf,
}

impl Builtin for WhichCommand {
    fn name() -> &'static str {
        "which"
    }

    fn run(
        &mut self,
        environment: &mut Environment,
        _current_directory: &mut PathBuf,
        io_env: &IoEnvironment,
    ) -> Fallible<WaitableStatus> {
        let mut found = false;
        for path in PathSearcher::new(&self.command, environment) {
            found = true;
            writeln!(io_env.stdout(), "{}", path.display())?;
            if !self.all {
                break;
            }
        }
        Ok(Status::Complete(if found { 0 } else { 1 }.into()).into())
    }
}
