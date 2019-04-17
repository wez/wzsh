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

fn send_cont(pid: libc::pid_t) -> Fallible<()> {
    unsafe {
        if libc::kill(pid, libc::SIGCONT) != 0 {
            let err = std::io::Error::last_os_error();
            Err(err.context(format!("SIGCONT pid {}", pid)).into())
        } else {
            Ok(())
        }
    }
}

#[derive(Debug)]
struct Inner {
    processes: Vec<WaitableExitStatus>,
    process_group_id: libc::pid_t,
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
                label: format!("{}", cmd),
            })),
        }
    }

    pub fn add(&mut self, proc: WaitableExitStatus) -> Fallible<()> {
        let process_group_id = match &proc {
            WaitableExitStatus::Child(ref proc) => proc.id() as _,
            WaitableExitStatus::UnixChild(ref proc) => proc.pid(),
            WaitableExitStatus::Done(_) => 0,
        };

        let mut inner = self.inner.lock().unwrap();
        if inner.process_group_id == 0 {
            inner.process_group_id = process_group_id;
        }

        inner.processes.push(proc);
        Ok(())
    }

    pub fn is_background(&self) -> bool {
        let inner = self.inner.lock().unwrap();
        let pgrp = unsafe { libc::tcgetpgrp(0) };
        inner.process_group_id != pgrp
    }

    pub fn process_group_id(&self) -> i32 {
        let inner = self.inner.lock().unwrap();
        inner.process_group_id
    }

    pub fn put_in_background(&mut self) -> Fallible<()> {
        let inner = self.inner.lock().unwrap();
        send_cont(-inner.process_group_id).ok();
        Ok(())
    }

    pub fn put_in_foreground(&mut self) -> Fallible<()> {
        let inner = self.inner.lock().unwrap();
        if inner.process_group_id == 0 {
            return Ok(());
        }
        unsafe {
            let pty_fd = 0;
            libc::tcsetpgrp(pty_fd, inner.process_group_id)
        };
        send_cont(-inner.process_group_id).ok();

        Ok(())
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
        job
    }

    pub fn jobs(&self) -> Vec<Job> {
        let jobs = self.jobs.lock().unwrap();
        jobs.iter().map(|(_, v)| v.clone()).collect()
    }

    pub fn check_and_print_status(&self) {
        let mut jobs = self.jobs.lock().unwrap();
        let mut terminated = vec![];
        for (id, job) in jobs.iter_mut() {
            match job.try_wait() {
                Ok(Some(status)) => {
                    /* FIXME: only print if it wasn't the most recent fg command
                    if job.is_background() {
                        eprintln!("[{}] - {} {}", id, status, job);
                    }
                    */
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
