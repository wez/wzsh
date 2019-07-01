use crate::job::Job;
use crate::shellhost::Host;
use failure::Fallible;
use shell_compiler::Compiler;
use shell_parser::Parser;
use shell_vm::{Environment, Machine, Program, Status};
use std::path::{Path, PathBuf};
use std::sync::Arc;

pub fn compile_and_run_script_file(
    path: &Path,
    cwd: &mut PathBuf,
    env: &mut Environment,
) -> Fallible<Status> {
    let job = Job::new_empty(path.to_string_lossy().to_string());
    let file = std::fs::File::open(path)?;
    let mut parser = Parser::new(file);

    let command = parser.parse()?;
    let mut compiler = Compiler::new();
    compiler.compile_command(&command)?;
    let prog = compiler.finish()?;

    let mut machine = Machine::new(&Program::new(prog), Some(env.clone()))?;
    machine.set_host(Arc::new(Host::new(job)));
    let status = machine.run();

    let (new_cwd, new_env) = machine.top_environment();
    *cwd = new_cwd;
    *env = new_env;

    status
}
