use failure::Fallible;

mod errorprint;
mod execenv;
mod exitstatus;
mod expander;
mod filedescriptor;
mod job;
mod parse;
mod repl;

use execenv::ExecutionEnvironment;
use expander::ShellExpander;
use repl::repl;

fn main() -> Fallible<()> {
    let env = ExecutionEnvironment::new()?;
    let expander = ShellExpander {};
    repl(env, expander)
}
