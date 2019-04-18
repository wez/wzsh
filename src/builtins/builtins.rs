use crate::builtins::Builtin;
use crate::execenv::ExecutionEnvironment;
use crate::exitstatus::ExitStatus;
use failure::Fallible;
use structopt::*;

#[derive(Debug, StructOpt)]
/// List builtin commands
pub struct BuiltinsCommand {}
impl Builtin for BuiltinsCommand {
    fn name() -> &'static str {
        "builtins"
    }

    fn run(&mut self, exe: &ExecutionEnvironment) -> Fallible<ExitStatus> {
        let mut builtins: Vec<&'static str> = super::BUILTINS.iter().map(|(k, _)| *k).collect();
        builtins.sort_unstable();
        for k in builtins {
            writeln!(exe.stdout(), "{}", k)?;
        }
        Ok(ExitStatus::ExitCode(0))
    }
}
