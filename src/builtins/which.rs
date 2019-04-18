use crate::builtins::Builtin;
use crate::execenv::ExecutionEnvironment;
use crate::exitstatus::ExitStatus;
use crate::pathsearch::PathSearcher;
use failure::Fallible;
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

    fn run(&mut self, exe: &ExecutionEnvironment) -> Fallible<ExitStatus> {
        let mut found = false;
        for path in PathSearcher::new(&self.command, &*exe.env()) {
            found = true;
            writeln!(exe.stdout(), "{}", path.display())?;
            if !self.all {
                break;
            }
        }
        Ok(ExitStatus::ExitCode(if found { 0 } else { 1 }))
    }
}
