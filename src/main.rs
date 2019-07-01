use failure::Fallible;
use shell_vm::Environment;

mod builtins;
mod errorprint;
mod exitstatus;
mod job;
mod repl;
mod shellhost;

fn main() -> Fallible<()> {
    let cwd = std::env::current_dir()?;
    let env = Environment::new();
    repl::repl(cwd, env)
}
