use crate::builtins::Builtin;
use crate::job::JOB_LIST;
use cancel::Token;
use failure::{err_msg, Fallible};
use shell_vm::{Environment, IoEnvironment, Status, WaitableStatus};
use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;
use structopt::*;

#[derive(Debug, StructOpt)]
/// Place a background job into the foreground
pub struct FgCommand {}
impl Builtin for FgCommand {
    fn name() -> &'static str {
        "fg"
    }

    fn run(
        &mut self,
        _environment: &mut Environment,
        _current_directory: &mut PathBuf,
        io_env: &IoEnvironment,
        _cancel: Arc<Token>,
    ) -> Fallible<WaitableStatus> {
        let mut jobs = JOB_LIST.jobs();
        if let Some(mut job) = jobs.pop() {
            writeln!(
                io_env.stderr(),
                "wzsh: putting [{}] {} into fg",
                job.process_group_id(),
                job
            )?;
            job.put_in_foreground()?;
            let status = job
                .wait()
                .ok_or_else(|| err_msg("job.wait returned None?"))?;
            writeln!(
                io_env.stderr(),
                "wzsh: after fg, wait returned {:?}",
                status
            )?;
            Ok(Status::Complete(0.into()).into())
        } else {
            writeln!(io_env.stderr(), "wzsh: fg: no jobs to put in foreground")?;
            Ok(Status::Complete(1.into()).into())
        }
    }
}

#[derive(Debug, StructOpt, Default)]
/// list known jobs
pub struct JobsCommand {}
impl Builtin for JobsCommand {
    fn name() -> &'static str {
        "jobs"
    }

    fn run(
        &mut self,
        _environment: &mut Environment,
        _current_directory: &mut PathBuf,
        io_env: &IoEnvironment,
        _cancel: Arc<Token>,
    ) -> Fallible<WaitableStatus> {
        let mut jobs = JOB_LIST.jobs();
        for job in &mut jobs {
            match job.poll() {
                Some(status) => writeln!(
                    io_env.stdout(),
                    "[{}] - {:?} {}",
                    job.process_group_id(),
                    status,
                    job
                )?,
                None => writeln!(
                    io_env.stdout(),
                    "[{}] - <nochange> {}", // TODO: be smarter about stopped status
                    job.process_group_id(),
                    job
                )?,
            }
        }
        Ok(Status::Complete(0.into()).into())
    }
}
