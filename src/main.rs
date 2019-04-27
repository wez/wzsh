use failure::Fallible;

//mod builtins;
mod errorprint;
//mod execenv;
mod exitstatus;
//mod job;
//mod parse;
mod pathsearch;
mod repl;

fn main() -> Fallible<()> {
    repl::repl()
}
