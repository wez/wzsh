use crate::errorprint::print_error_path;
use crate::shellhost::FunctionRegistry;
use shell_vm::Environment;
use std::path::PathBuf;
use std::sync::Arc;
use structopt::StructOpt;

mod builtins;
mod errorprint;
mod exitstatus;
mod job;
mod repl;
mod script;
mod shellhost;

#[derive(Debug, StructOpt)]
#[structopt(about = "Wez's Shell\nhttp://github.com/wez/wzsh")]
#[structopt(raw(
    global_setting = "structopt::clap::AppSettings::ColoredHelp",
    version = r#"env!("VERGEN_SEMVER_LIGHTWEIGHT")"#,
))]
struct Opt {
    /// Skip loading startup.wzsh
    #[structopt(long = "no-startup")]
    skip_startup: bool,

    /// Instead of starting the interactive REPL, load script
    /// from file and execute it
    file: Option<PathBuf>,
}

fn config_dir() -> PathBuf {
    dirs::home_dir()
        .expect("can't find HOME dir")
        .join(".config")
        .join("wzsh")
}

fn main() -> anyhow::Result<()> {
    let mut cwd = std::env::current_dir()?;
    let mut env = Environment::new();
    let funcs = Arc::new(FunctionRegistry::new());

    let opts = Opt::from_args();
    if !opts.skip_startup {
        let startup_script = config_dir().join("startup.wzsh");
        if startup_script.exists() {
            if let Err(err) =
                script::compile_and_run_script_file(&startup_script, &mut cwd, &mut env, &funcs)
            {
                print_error_path(&err, &startup_script);
                eprintln!("wzsh: ignoring error during startup processing.");
            }
        }
    }

    if let Some(file) = opts.file.as_ref() {
        if let Err(err) =
            script::compile_and_run_script_file(file, &mut cwd, &mut env, &funcs)
        {
            print_error_path(&err, file);
            std::process::exit(1);
        }
        Ok(())
    } else {
        repl::repl(cwd, env, &funcs)
    }
}
