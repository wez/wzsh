use failure::{bail, format_err, Error, Fallible};

use shlex::string::ShellString;
use shlex::{Environment, Expander};

mod errorprint;
mod execenv;
mod exitstatus;
mod filedescriptor;
mod parse;
mod repl;

use execenv::ExecutionEnvironment;
use repl::repl;

pub struct ShellExpander {}
impl Expander for ShellExpander {
    fn lookup_homedir(
        &self,
        user: Option<&str>,
        environment: &mut Environment,
    ) -> Fallible<ShellString> {
        if user.is_none() {
            let home = environment
                .get("HOME")
                .ok_or_else(|| format_err!("HOME is not set"))?;
            Ok(home.as_os_str().into())
        } else {
            bail!("ze goggles");
        }
    }
}

fn main() -> Result<(), Error> {
    let env = ExecutionEnvironment::new()?;
    let expander = ShellExpander {};
    repl(env, expander)
}
