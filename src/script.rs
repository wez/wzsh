use crate::job::Job;
use crate::shellhost::{FunctionRegistry, Host};
use shell_compiler::Compiler;
use shell_parser::Parser;
use shell_vm::{Environment, Machine, Program, Status};
use std::path::{Path, PathBuf};
use std::sync::Arc;

pub fn compile_and_run_script<R: std::io::Read>(
    file: R,
    file_name: &str,
    cwd: &mut PathBuf,
    env: &mut Environment,
    funcs: &Arc<FunctionRegistry>,
) -> anyhow::Result<Status> {
    let job = Job::new_empty(file_name.to_string());
    let mut parser = Parser::new(file);

    let command = parser.parse()?;
    let mut compiler = Compiler::new();
    compiler.compile_command(&command)?;
    let prog = compiler.finish()?;

    let mut machine = Machine::new(&Program::new(prog), Some(env.clone()), &cwd)?;
    machine.set_host(Arc::new(Host::new(job, funcs)));
    let status = machine.run();

    let (new_cwd, new_env) = machine.top_environment();
    *cwd = new_cwd;
    *env = new_env;

    status
}

pub fn compile_and_run_script_file(
    path: &Path,
    cwd: &mut PathBuf,
    env: &mut Environment,
    funcs: &Arc<FunctionRegistry>,
) -> anyhow::Result<Status> {
    let file_name = path.to_string_lossy();
    let file = std::fs::File::open(path)?;
    compile_and_run_script(file, &file_name, cwd, env, funcs)
}
