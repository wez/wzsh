use crate::exitstatus::{ExitStatus, WaitableExitStatus};
use failure::{Fail, Fallible};
use std::sync::{Arc, Mutex};

pub fn put_shell_in_foreground() {
    unsafe {
        let pgrp = libc::getpgid(libc::getpid());
        libc::tcsetpgrp(0, pgrp);
    }
}

pub fn make_own_process_group(pid: i32) {
    #[cfg(unix)]
    unsafe {
        // Put the process into its own process group
        libc::setpgid(pid, pid);
    }
}

pub fn make_foreground_process_group(pid: i32) {
    make_own_process_group(pid);
    #[cfg(unix)]
    unsafe {
        // Grant that process group foreground control
        // over the terminal
        let pty_fd = 0;
        libc::tcsetpgrp(pty_fd, pid);
    }
}

#[derive(Debug)]
struct Inner {
    processes: Vec<WaitableExitStatus>,
    process_group_id: libc::pid_t,
    background: bool,
    stopped: bool,
}

impl Inner {
    fn add(&mut self, proc: WaitableExitStatus) {
        self.processes.push(proc);
    }
}

#[derive(Clone, Debug)]
pub struct Job {
    inner: Arc<Mutex<Inner>>,
}

#[derive(Default, Debug)]
pub struct JobList {
    pub jobs: Mutex<Vec<Arc<Job>>>,
}

impl Job {
    pub fn new(proc: WaitableExitStatus, background: bool) -> Self {
        let process_group_id = match proc {
            WaitableExitStatus::Child(ref proc) => proc.id() as _,
            _ => 0,
        };
        Self {
            inner: Arc::new(Mutex::new(Inner {
                processes: vec![proc],
                process_group_id,
                background,
                stopped: false,
            })),
        }
    }

    pub fn is_background(&self) -> bool {
        let inner = self.inner.lock().unwrap();
        inner.background
    }

    pub fn put_in_background(&mut self) -> Fallible<()> {
        let inner = self.inner.lock().unwrap();
        unsafe {
            if libc::kill(-inner.process_group_id, libc::SIGCONT) != 0 {
                let err = std::io::Error::last_os_error();
                Err(err
                    .context(format!("SIGCONT progress group {}", inner.process_group_id))
                    .into())
            } else {
                Ok(())
            }
        }
    }

    pub fn put_in_foreground(&mut self) {
        let inner = self.inner.lock().unwrap();
        unsafe {
            let pty_fd = 0;
            libc::tcsetpgrp(pty_fd, inner.process_group_id);
        }
    }

    pub fn unwrap(job: Option<Job>) -> Job {
        job.unwrap_or_else(|| Job::new(WaitableExitStatus::Done(ExitStatus::new_ok()), false))
    }

    pub fn add_to_job_or_create(
        job: Option<Job>,
        proc: WaitableExitStatus,
        background: bool,
    ) -> Job {
        match job {
            Some(job) => {
                {
                    let mut inner = job.inner.lock().unwrap();
                    inner.add(proc);
                }
                job.clone()
            }
            None => Job::new(proc, background),
        }
    }

    pub fn wait(&mut self) -> Fallible<ExitStatus> {
        let mut inner = self.inner.lock().unwrap();
        inner.processes.last_mut().unwrap().wait()
    }
}
