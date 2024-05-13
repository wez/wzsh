use crate::builtins::lookup_builtin;
use crate::exitstatus::ChildProcess;
#[cfg(unix)]
use crate::job::{add_to_process_group, make_foreground_process_group};
use crate::job::{Job, JOB_LIST};
use anyhow::{anyhow, bail, Context};
use cancel::Token;
use pathsearch::PathSearcher;
use shell_vm::{
    Environment, IoEnvironment, Machine, Program, ShellHost, Status, Value, WaitableStatus,
};
use std::collections::HashMap;
use std::ffi::OsString;
use std::io::Write;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

#[derive(Debug)]
pub struct FunctionRegistry {
    functions: Mutex<HashMap<String, Arc<Program>>>,
}

impl FunctionRegistry {
    pub fn new() -> Self {
        Self {
            functions: Mutex::new(HashMap::new()),
        }
    }

    pub fn define_function(&self, name: &str, program: &Arc<Program>) {
        let mut funcs = self.functions.lock().unwrap();
        funcs.insert(name.to_owned(), Arc::clone(program));
    }

    pub fn lookup_function(&self, name: &str) -> Option<Arc<Program>> {
        let funcs = self.functions.lock().unwrap();
        funcs.get(name).map(Arc::clone)
    }
}

#[derive(Debug)]
pub struct Host {
    job: Mutex<Job>,
    job_control_enabled: bool,
    funcs: Arc<FunctionRegistry>,
}

impl Host {
    pub fn new(job: Job, funcs: &Arc<FunctionRegistry>) -> Self {
        Self {
            job: Mutex::new(job),
            job_control_enabled: false,
            funcs: Arc::clone(funcs),
        }
    }

    pub fn with_job_control(job: Job, funcs: &Arc<FunctionRegistry>) -> Self {
        Self {
            job: Mutex::new(job),
            job_control_enabled: true,
            funcs: Arc::clone(funcs),
        }
    }
}

impl ShellHost for Host {
    fn lookup_homedir(&self, user: Option<&str>) -> anyhow::Result<OsString> {
        if user.is_none() {
            if let Some(home) = dirs_next::home_dir() {
                if let Some(s) = home.to_str() {
                    // Urgh for windows.
                    Ok(s.replace("\\", "/").into())
                } else {
                    Ok(home.into())
                }
            } else {
                bail!("failed to resolve own home dir");
            }
        } else {
            bail!("lookup_homedir for specific user not implemented");
        }
    }

    fn spawn_command(
        &self,
        argv: &Vec<Value>,
        environment: &mut Environment,
        current_directory: &mut PathBuf,
        io_env: &IoEnvironment,
    ) -> anyhow::Result<WaitableStatus> {
        if argv.is_empty() {
            return Ok(Status::Complete(0.into()).into());
        }

        let (search_builtin, search_path, argv) = if argv[0].as_str() == Some("command") {
            (false, true, &argv[1..])
        } else if argv[0].as_str() == Some("builtin") {
            (true, false, &argv[1..])
        } else {
            (true, true, &argv[..])
        };

        if let Some(name) = argv[0].as_str() {
            if let Some(prog) = self.funcs.lookup_function(name) {
                // Execute the function.
                let job = Job::new_empty(name.to_string());
                let mut machine =
                    Machine::new(&prog, Some(environment.clone()), &current_directory)?;
                machine.set_host(Arc::new(Host::with_job_control(job, &self.funcs)));

                machine.set_positional(argv.to_vec());

                let status = machine.run();

                let (new_cwd, new_env) = machine.top_environment();
                *current_directory = new_cwd;
                *environment = new_env;

                return status.map(Into::into);
            }
        }

        if search_builtin {
            if let Some(builtin) = lookup_builtin(&argv[0]) {
                // Create a token for cancellation.
                // This is currently useless; I just wanted to establish this in
                // the builtins interface.
                // This needs to be connected to CTRL-C from the REPL or from a
                // signal handler.
                let token = Arc::new(Token::new());
                return builtin(
                    &argv[..],
                    environment,
                    current_directory,
                    io_env,
                    token,
                    &self.funcs,
                );
            }
        }

        if search_path {
            if let Some(exe) = PathSearcher::new(
                argv[0]
                    .as_os_str()
                    .ok_or_else(|| anyhow!("argv0 is not convertible to OsStr"))?,
                environment.get("PATH"),
                environment.get("PATHEXT"),
            )
            .next()
            {
                let mut child_cmd = std::process::Command::new(&exe);
                for (i, arg) in argv.iter().enumerate().skip(1) {
                    child_cmd.arg(
                        arg.as_os_str()
                            .ok_or_else(|| anyhow!("argv{} is not convertible to OsStr", i))?,
                    );
                }
                child_cmd.env_clear();
                child_cmd.envs(environment.iter());
                child_cmd.current_dir(&current_directory);

                child_cmd.stdin(io_env.fd_as_stdio(0)?);
                child_cmd.stdout(io_env.fd_as_stdio(1)?);
                child_cmd.stderr(io_env.fd_as_stdio(2)?);

                let process_group_id = self.job.lock().unwrap().process_group_id();

                #[cfg(unix)]
                unsafe {
                    use std::os::unix::process::CommandExt;
                    let job_control = self.job_control_enabled;
                    child_cmd.pre_exec(move || {
                        let pid = libc::getpid();
                        if job_control {
                            if process_group_id == 0 {
                                /*
                                if asynchronous {
                                    make_own_process_group(pid);
                                } else {}
                                */
                                make_foreground_process_group(pid);
                            } else {
                                add_to_process_group(pid, process_group_id);
                            }
                        }
                        for s in &[
                            libc::SIGINT,
                            libc::SIGQUIT,
                            libc::SIGTSTP,
                            libc::SIGTTIN,
                            libc::SIGTTOU,
                            libc::SIGCHLD,
                        ] {
                            libc::signal(*s, libc::SIG_DFL);
                        }

                        Ok(())
                    });
                }

                let child = child_cmd
                    .spawn()
                    .with_context(|| format!("spawning {:?}", argv))?;
                let child = ChildProcess::new(child);

                if self.job_control_enabled {
                    // To avoid a race condition with starting up the child, we
                    // need to also munge the process group assignment here in
                    // the parent.  Note that the loser of the race will experience
                    // errors in attempting this block, so we willfully ignore
                    // the return values here: we cannot do anything about them.
                    #[cfg(unix)]
                    {
                        let pid = child.pid();
                        if process_group_id == 0 {
                            /*
                            if asynchronous {
                                make_own_process_group(pid);
                            } else {}
                            */
                            make_foreground_process_group(pid);
                        } else {
                            add_to_process_group(pid, process_group_id);
                        }
                    }
                }

                self.job.lock().unwrap().add(child.clone())?;

                if process_group_id == 0 {
                    JOB_LIST.add(self.job.lock().unwrap().clone());
                }

                return Ok(WaitableStatus::new(Arc::new(child)));
            }
        }

        if let Some(s) = argv[0].as_str() {
            writeln!(io_env.stderr(), "wzsh: {} not found", s)?;
        } else {
            writeln!(io_env.stderr(), "wzsh: {:?} not found", &argv[0])?;
        }
        Ok(Status::Complete(127.into()).into())
    }

    fn define_function(&self, name: &str, program: &Arc<Program>) -> anyhow::Result<()> {
        self.funcs.define_function(name, program);
        Ok(())
    }
}
