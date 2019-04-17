use crate::builtins::Builtin;
use crate::execenv::ExecutionEnvironment;
use crate::exitstatus::ExitStatus;
use failure::Fallible;
use structopt::*;

#[derive(Debug, StructOpt)]
/// The pwd utility writes the absolute pathname of the current working directory to the standard output.
pub struct PwdCommand {
    /// Display the logical current working directory.
    /// This is the default behavior.
    #[structopt(short = "L")]
    logical: bool,

    /// Display the physical current working directory (all symbolic links resolved).
    #[structopt(short = "P")]
    physical: bool,
}

impl Builtin for PwdCommand {
    fn name() -> &'static str {
        "pwd"
    }

    fn run(&mut self, exe: &ExecutionEnvironment) -> Fallible<ExitStatus> {
        if self.logical && self.physical {
            writeln!(
                exe.stderr(),
                "pwd: cannot print both physical and logical at the same time"
            )?;
            Ok(ExitStatus::ExitCode(1))
        } else if self.physical {
            let pwd = std::env::current_dir()?;
            writeln!(exe.stdout(), "{}", pwd.display())?;
            Ok(ExitStatus::ExitCode(0))
        } else {
            let env = exe.env();
            if let Some(pwd) = env.get("PWD") {
                writeln!(exe.stdout(), "{}", std::path::Path::new(pwd).display())?;
            } else {
                // Fall back to just printing the physical path
                let pwd = std::env::current_dir()?;
                writeln!(exe.stdout(), "{}", pwd.display())?;
            }
            Ok(ExitStatus::ExitCode(0))
        }
    }
}
