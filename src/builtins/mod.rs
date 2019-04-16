use crate::execenv::ExecutionEnvironment;
use crate::exitstatus::ExitStatus;
use failure::{bail, Fallible};
use lazy_static::lazy_static;
use shlex::string::ShellString;
use std::collections::HashMap;
use structopt::*;

pub trait Builtin: StructOpt {
    fn eval(argv: Vec<ShellString>, exe: &ExecutionEnvironment) -> Fallible<ExitStatus>
    where
        Self: Sized,
    {
        let name = match &argv[0] {
            ShellString::String(s) => s,
            _ => bail!("somehow dispatched to builtin with non-String name?"),
        };
        let app = Self::clap()
            .global_setting(structopt::clap::AppSettings::ColoredHelp)
            .global_setting(structopt::clap::AppSettings::DisableVersion)
            .name(name.to_owned());
        let mut args = Self::from_clap(&app.get_matches_from_safe(argv.iter())?);
        args.run(exe)
    }

    fn run(&mut self, exe: &ExecutionEnvironment) -> Fallible<ExitStatus>;
}

pub type BuiltinFunc =
    fn(argv: Vec<ShellString>, exe: &ExecutionEnvironment) -> Fallible<ExitStatus>;

pub fn lookup_builtin(name: &ShellString) -> Option<BuiltinFunc> {
    match &name {
        ShellString::String(s) => BUILTINS.get(s.as_str()).map(|f| *f),
        _ => None,
    }
}

#[derive(Debug, StructOpt)]
#[structopt(about = "Place a background job into the foreground")]
struct FgCommand {}
impl Builtin for FgCommand {
    fn run(&mut self, exe: &ExecutionEnvironment) -> Fallible<ExitStatus> {
        let mut jobs = exe.job_list().jobs();
        if let Some(mut job) = jobs.pop() {
            writeln!(
                exe.stderr(),
                "wzsh: putting [{}] {} into fg",
                job.process_group_id(),
                job
            )?;
            job.put_in_foreground()?;
            let status = job.wait()?;
            writeln!(exe.stderr(), "wzsh: after fg, wait returned {}", status)?;
            Ok(ExitStatus::ExitCode(0))
        } else {
            writeln!(exe.stderr(), "wzsh: fg: no jobs to put in foreground")?;
            Ok(ExitStatus::ExitCode(1))
        }
    }
}

#[derive(Debug, StructOpt)]
#[structopt(about = "list known jobs")]
struct JobsCommand {}
impl Builtin for JobsCommand {
    fn run(&mut self, exe: &ExecutionEnvironment) -> Fallible<ExitStatus> {
        let mut jobs = exe.job_list().jobs();
        for job in &mut jobs {
            match job.try_wait() {
                Ok(Some(status)) => writeln!(
                    exe.stdout(),
                    "[{}] - {} {}",
                    job.process_group_id(),
                    status,
                    job
                )?,
                Ok(None) => writeln!(
                    exe.stdout(),
                    "[{}] - <nochange> {}", // TODO: be smarter about stopped status
                    job.process_group_id(),
                    job
                )?,
                Err(e) => writeln!(exe.stderr(), "wzsh: wait failed for job {}", e)?,
            }
        }
        Ok(ExitStatus::ExitCode(0))
    }
}

lazy_static! {
    static ref BUILTINS: HashMap<&'static str, BuiltinFunc> = {
        let builtins: [(&'static str, BuiltinFunc); 2] = [("jobs", JobsCommand::eval), ("fg", FgCommand::eval)];

        // This identity helper effectively casts away the per-function
        // type information that would otherwise cause a type mismatch
        // when populating the hashmap
        fn identity(f: BuiltinFunc) -> BuiltinFunc {
            f
        }
        builtins.into_iter().map(|(k, v)| (*k, identity(*v))).collect()
    };
}
