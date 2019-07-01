use crate::errorprint::print_error_path;
use failure::Fallible;
use shell_vm::Environment;
use std::path::PathBuf;

mod builtins;
mod errorprint;
mod exitstatus;
mod job;
mod repl;
mod script;
mod shellhost;

fn config_dir() -> PathBuf {
    dirs::home_dir()
        .expect("can't find HOME dir")
        .join(".config")
        .join("wzsh")
}

fn main() -> Fallible<()> {
    let mut cwd = std::env::current_dir()?;
    let mut env = Environment::new();

    let startup_script = config_dir().join("startup.wzsh");
    if startup_script.exists() {
        if let Err(err) = script::compile_and_run_script_file(&startup_script, &mut cwd, &mut env) {
            print_error_path(&err, &startup_script);
            eprintln!("wzsh: ignoring error during startup processing.");
        }
    }

    repl::repl(cwd, env)
}
