use crate::errorprint::{print_error, print_error_path};
use crate::shellhost::FunctionRegistry;
use shell_vm::Environment;
use std::io::Read;
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
    dirs_next::home_dir()
        .expect("can't find HOME dir")
        .join(".config")
        .join("wzsh")
}

fn main() -> anyhow::Result<()> {
    let mut cwd = std::env::current_dir()?;
    let mut env = Environment::new();
    let mut exe_dir = None;

    // We want to pick up our shell utility executables.
    // In the source tree they are emitted alongside the wzsh
    // executable. In a deployed package they will also be
    // located alongside the executable, so resolve the executable
    // path and add it to the current environment.
    if let Ok(exe) = std::env::current_exe() {
        if let Some(bindir) = exe.parent() {
            // Allow startup scripts to explicitly reference this
            // location so that they can set up aliases if they
            // prefer to use these utilities over others that
            // might be in their path
            env.set("WZSH_BIN_DIR", bindir);
            env.append_path("PATH", bindir)?;

            exe_dir.replace(bindir.to_path_buf());
        }
    }

    let funcs = Arc::new(FunctionRegistry::new());

    let opts = Opt::from_args();
    if !opts.skip_startup {
        let startup_paths = &[
            #[cfg(windows)]
            exe_dir.as_ref().unwrap().join(".wzshrc"),
            config_dir().join("startup.wzsh"),
            dirs_next::home_dir().unwrap().join(".wzshrc"),
        ];

        for startup_script in startup_paths {
            if startup_script.exists() {
                if let Err(err) =
                    script::compile_and_run_script_file(&startup_script, &mut cwd, &mut env, &funcs)
                {
                    print_error_path(&err, &startup_script);
                    eprintln!("wzsh: ignoring error during startup processing.");
                }
                break;
            }
        }
    }

    if let Some(file) = opts.file.as_ref() {
        if let Err(err) = script::compile_and_run_script_file(file, &mut cwd, &mut env, &funcs) {
            print_error_path(&err, file);
            std::process::exit(1);
        }
        Ok(())
    } else if atty::isnt(atty::Stream::Stdin) {
        let mut stdin = String::new();
        std::io::stdin().lock().read_to_string(&mut stdin)?;

        if let Err(err) =
            script::compile_and_run_script(stdin.as_bytes(), "stdin", &mut cwd, &mut env, &funcs)
        {
            print_error(&err, &stdin);
            std::process::exit(1);
        }
        Ok(())
    } else {
        repl::repl(cwd, env, &funcs)
    }
}
