use crate::builtins::{lookup_builtin, Builtin};
use crate::shellhost::FunctionRegistry;
use cancel::Token;
use pathsearch::PathSearcher;
use shell_vm::{Environment, IoEnvironment, Status, Value, WaitableStatus};
use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;
use structopt::*;

#[derive(StructOpt)]
/// Search the path for a command; if found, print out the path
/// to that command.
pub struct WhichCommand {
    /// Find all possible matches in the path, rather than stopping
    /// at the first one
    #[structopt(short = "a")]
    all: bool,

    /// Don't output anything; only indicate success/failure through
    /// the exit status.
    /// This is a non-standard extension for wzsh.
    #[structopt(short = "q")]
    quiet: bool,

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
        cancel: Arc<Token>,
        functions: &Arc<FunctionRegistry>,
    ) -> anyhow::Result<WaitableStatus> {
        let mut found = false;

        if let Some(name) = self.command.to_str() {
            if let Some(_) = functions.lookup_function(name) {
                found = true;
                if !self.quiet {
                    writeln!(io_env.stdout(), "{}: shell function", name)?;
                }
            }
        }
        if let Some(_) = lookup_builtin(&Value::OsString(self.command.as_os_str().to_os_string())) {
            found = true;
            if !self.quiet {
                writeln!(
                    io_env.stdout(),
                    "{}: shell built-in command",
                    self.command.display()
                )?;
            }
        }
        if !found || self.all {
            for path in PathSearcher::new(
                &self.command,
                environment.get("PATH"),
                environment.get("PATHEXT"),
            ) {
                cancel.check_cancel()?;
                found = true;
                if !self.quiet {
                    writeln!(io_env.stdout(), "{}", path.display())?;
                }
                if !self.all {
                    break;
                }
            }
        }
        Ok(Status::Complete(if found { 0 } else { 1 }.into()).into())
    }
}
