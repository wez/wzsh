use crate::builtins::Builtin;
use crate::execenv::ExecutionEnvironment;
use crate::exitstatus::ExitStatus;
use failure::Fallible;
use structopt::*;

#[derive(Debug, StructOpt)]
#[structopt(about = "Place a background job into the foreground")]
pub struct FgCommand {}
impl Builtin for FgCommand {
    fn name() -> &'static str {
        "fg"
    }

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

#[derive(Debug, StructOpt, Default)]
#[structopt(about = "list known jobs")]
pub struct JobsCommand {}
impl Builtin for JobsCommand {
    fn name() -> &'static str {
        "jobs"
    }

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
