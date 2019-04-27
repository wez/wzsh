use failure::Fallible;

//mod builtins;
mod errorprint;
//mod execenv;
//mod exitstatus;
//mod expander;
//mod job;
//mod parse;
//mod pathsearch;
mod repl;
//mod vm;

fn main() -> Fallible<()> {
    repl::repl()
}
