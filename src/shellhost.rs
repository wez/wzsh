use crate::builtins::lookup_builtin;
use crate::exitstatus::UnixChild;
use crate::job::{add_to_process_group, make_foreground_process_group, Job, JOB_LIST};
use crate::pathsearch::PathSearcher;
use failure::{bail, err_msg, format_err, Fallible};
use shell_vm::{Environment, IoEnvironment, ShellHost, Status, Value, WaitableStatus};
use std::ffi::OsString;
use std::io::Write;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

#[derive(Debug)]
pub struct Host {
    job: Mutex<Job>,
}

impl Host {
    pub fn new(job: Job) -> Self {
        Self {
            job: Mutex::new(job),
        }
    }
}

impl ShellHost for Host {
    fn lookup_homedir(&self, _user: Option<&str>) -> Fallible<OsString> {
        bail!("lookup_homedir not implemented");
    }
    fn spawn_command(
        &self,
        argv: &Vec<Value>,
        environment: &mut Environment,
        current_directory: &mut PathBuf,
        io_env: &IoEnvironment,
    ) -> Fallible<WaitableStatus> {
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

        if search_builtin {
            if let Some(builtin) = lookup_builtin(&argv[0]) {
                return builtin(&argv[..], environment, current_directory, io_env);
            }
        }

        if search_path {
            if let Some(exe) = PathSearcher::new(
                argv[0]
                    .as_os_str()
                    .ok_or_else(|| err_msg("argv0 is not convertible to OsStr"))?,
                environment,
            )
            .next()
            {
                let mut child_cmd = std::process::Command::new(&exe);
                for (i, arg) in argv.iter().enumerate().skip(1) {
                    child_cmd.arg(
                        arg.as_os_str()
                            .ok_or_else(|| format_err!("argv{} is not convertible to OsStr", i))?,
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
                    child_cmd.pre_exec(move || {
                        let pid = libc::getpid();
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

                let child = child_cmd.spawn()?;

                let child = UnixChild::new(child);

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
        Ok(Status::Complete(1.into()).into())
    }
}
