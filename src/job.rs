use crate::exitstatus::{ExitStatus, WaitableExitStatus};
use crate::parse::Command;
use failure::{Fail, Fallible};
use std::collections::HashMap;
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
    label: String,
}

#[derive(Clone, Debug)]
pub struct Job {
    inner: Arc<Mutex<Inner>>,
}

#[derive(Default, Debug)]
pub struct JobList {
    pub jobs: Mutex<HashMap<i32, Job>>,
}

impl std::fmt::Display for Job {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        let inner = self.inner.lock().unwrap();
        write!(fmt, "{}", inner.label)
    }
}

impl Job {
    pub fn new_empty(cmd: &Command) -> Self {
        Self {
            inner: Arc::new(Mutex::new(Inner {
                processes: vec![],
                process_group_id: 0,
                background: cmd.asynchronous,
                label: format!("{}", cmd),
            })),
        }
    }

    pub fn add(&mut self, mut proc: WaitableExitStatus, background: bool) -> Fallible<()> {
        let process_group_id = match &proc {
            WaitableExitStatus::Child(ref proc) => proc.id() as _,
            WaitableExitStatus::UnixChild(ref proc) => proc.pid(),
            WaitableExitStatus::Done(_) => 0,
        };

        let mut inner = self.inner.lock().unwrap();
        if inner.process_group_id == 0 {
            inner.process_group_id = process_group_id;
        }

        if !background {
            inner.processes.push(WaitableExitStatus::Done(proc.wait()?));
        } else {
            inner.processes.push(proc);
        }
        Ok(())
    }

    pub fn is_background(&self) -> bool {
        let inner = self.inner.lock().unwrap();
        inner.background
    }

    pub fn process_group_id(&self) -> i32 {
        let inner = self.inner.lock().unwrap();
        inner.process_group_id
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

    pub fn unwrap_or_create(job: Option<Job>, cmd: &Command) -> Job {
        job.unwrap_or_else(|| Job::new_empty(cmd))
    }

    pub fn wait(&mut self) -> Fallible<ExitStatus> {
        let mut inner = self.inner.lock().unwrap();
        inner.processes.last_mut().unwrap().wait()
    }
    pub fn try_wait(&mut self) -> Fallible<Option<ExitStatus>> {
        let mut inner = self.inner.lock().unwrap();
        inner.processes.last_mut().unwrap().try_wait()
    }
}

impl JobList {
    pub fn add(&self, job: Job) -> Job {
        let id = job.process_group_id();
        let mut jobs = self.jobs.lock().unwrap();
        jobs.insert(id, job.clone());
        eprintln!("tracking {} jobs", jobs.len());
        job
    }

    pub fn check_and_print_status(&self) {
        let mut jobs = self.jobs.lock().unwrap();
        eprintln!("doing status check");
        let mut terminated = vec![];
        for (id, job) in jobs.iter_mut() {
            eprintln!("consider id {}", *id);
            match job.try_wait() {
                Ok(Some(status)) => {
                    eprintln!("{} {}", status, job);
                    if status.terminated() {
                        terminated.push(*id);
                    }
                }
                Ok(_) => {}
                Err(e) => eprintln!("wzsh: wait failed for job {} {}", id, e),
            }
        }

        for id in terminated {
            jobs.remove(&id);
        }
    }
}
