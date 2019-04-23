use failure::Fallible;

mod builtins;
mod errorprint;
mod execenv;
mod exitstatus;
mod expander;
mod job;
mod parse;
mod pathsearch;
mod repl;
mod vm;

use execenv::ExecutionEnvironment;
use expander::ShellExpander;
use repl::repl;

fn main() -> Fallible<()> {
    let env = ExecutionEnvironment::new()?;
    let expander = ShellExpander {};
    repl(env, expander)
}
