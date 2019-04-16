use crate::execenv::ExecutionEnvironment;
use crate::exitstatus::ExitStatus;
use failure::Fallible;
use lazy_static::lazy_static;
use shlex::string::ShellString;
use std::collections::HashMap;

pub type BuiltinFunc = fn(argv: &[ShellString], exe: &ExecutionEnvironment) -> Fallible<ExitStatus>;

pub struct BuiltinCommand {
    argv: Vec<ShellString>,
    func: BuiltinFunc,
}

impl BuiltinCommand {
    pub fn new(func: BuiltinFunc, argv: Vec<ShellString>) -> Self {
        Self { func, argv }
    }

    pub fn run(&self, exe: &ExecutionEnvironment) -> Fallible<ExitStatus> {
        (self.func)(&self.argv, exe)
    }
}

pub fn lookup_builtin(name: &ShellString) -> Option<BuiltinFunc> {
    match &name {
        ShellString::String(s) => BUILTINS.get(s.as_str()).map(|f| *f),
        _ => None,
    }
}

fn jobs(_argv: &[ShellString], exe: &ExecutionEnvironment) -> Fallible<ExitStatus> {
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
                "[{}] - running {}",
                job.process_group_id(),
                job
            )?,
            Err(e) => writeln!(exe.stderr(), "wzsh: wait failed for job {}", e)?,
        }
    }
    Ok(ExitStatus::ExitCode(0))
}

lazy_static! {
    static ref BUILTINS: HashMap<&'static str, BuiltinFunc> = {
        let builtins = [("jobs", jobs)];

        // This identity helper effectively casts away the per-function
        // type information that would otherwise cause a type mismatch
        // when populating the hashmap
        fn identity(f: BuiltinFunc) -> BuiltinFunc {
            f
        }
        builtins.into_iter().map(|(k, v)| (*k, identity(*v))).collect()
    };
}
