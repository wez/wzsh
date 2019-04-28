use failure::Fallible;

mod builtins;
mod errorprint;
mod exitstatus;
mod job;
mod pathsearch;
mod repl;
mod shellhost;

fn main() -> Fallible<()> {
    repl::repl()
}
