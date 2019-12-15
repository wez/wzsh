use crate::exitstatus::{ChildProcess, Pid};
use lazy_static::lazy_static;
use shell_vm::{Status, WaitForStatus};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

lazy_static! {
    pub static ref JOB_LIST: JobList = JobList::default();
}

pub fn put_shell_in_foreground() {
    #[cfg(unix)]
    unsafe {
        let pgrp = libc::getpgid(libc::getpid());
        libc::tcsetpgrp(0, pgrp);
    }
}

#[cfg(unix)]
pub fn make_own_process_group(pid: i32) {
    unsafe {
        // Put the process into its own process group
        libc::setpgid(pid, pid);
    }
}

#[cfg(unix)]
pub fn add_to_process_group(pid: i32, process_group_id: i32) {
    unsafe {
        libc::setpgid(pid, process_group_id);
    }
}

#[cfg(unix)]
pub fn make_foreground_process_group(pid: i32) {
    make_own_process_group(pid);
    unsafe {
        // Grant that process group foreground control
        // over the terminal
        let pty_fd = 0;
        libc::tcsetpgrp(pty_fd, pid);
    }
}

#[cfg(unix)]
fn send_cont(pid: libc::pid_t) -> anyhow::Result<()> {
    unsafe {
        use anyhow::Context;
        if libc::kill(pid, libc::SIGCONT) != 0 {
            let err = std::io::Error::last_os_error();
            Err(err).with_context(|| format!("SIGCONT pid {}", pid))
        } else {
            Ok(())
        }
    }
}

#[derive(Debug)]
struct Inner {
    processes: Vec<ChildProcess>,
    process_group_id: Pid,
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
    pub fn new_empty(label: String) -> Self {
        Self {
            inner: Arc::new(Mutex::new(Inner {
                processes: vec![],
                process_group_id: 0,
                label,
            })),
        }
    }

    pub fn add(&mut self, proc: ChildProcess) -> anyhow::Result<()> {
        let process_group_id = proc.pid();

        let mut inner = self.inner.lock().unwrap();
        if inner.process_group_id == 0 {
            inner.process_group_id = process_group_id;
        }

        inner.processes.push(proc);
        Ok(())
    }

    pub fn is_background(&self) -> bool {
        #[cfg(unix)]
        {
            let inner = self.inner.lock().unwrap();
            let pgrp = unsafe { libc::tcgetpgrp(0) };
            inner.process_group_id != pgrp
        }
        #[cfg(windows)]
        false
    }

    pub fn process_group_id(&self) -> i32 {
        #[cfg(unix)]
        {
            let inner = self.inner.lock().unwrap();
            inner.process_group_id
        }
        #[cfg(windows)]
        0
    }

    pub fn put_in_background(&mut self) -> anyhow::Result<()> {
        #[cfg(unix)]
        {
            let inner = self.inner.lock().unwrap();
            send_cont(-inner.process_group_id).ok();
        }
        Ok(())
    }

    pub fn put_in_foreground(&mut self) -> anyhow::Result<()> {
        #[cfg(unix)]
        {
            let inner = self.inner.lock().unwrap();
            if inner.process_group_id == 0 {
                return Ok(());
            }
            unsafe {
                let pty_fd = 0;
                libc::tcsetpgrp(pty_fd, inner.process_group_id)
            };
            send_cont(-inner.process_group_id).ok();
        }

        Ok(())
    }

    pub fn wait(&mut self) -> Option<Status> {
        let mut inner = self.inner.lock().unwrap();
        inner.processes.last_mut().unwrap().wait()
    }
    pub fn poll(&mut self) -> Option<Status> {
        let mut inner = self.inner.lock().unwrap();
        inner.processes.last_mut().unwrap().poll()
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
            if let Some(Status::Complete(_status)) = job.poll() {
                /* FIXME: only print if it wasn't the most recent fg command
                if job.is_background() {
                    eprintln!("[{}] - {} {}", id, status, job);
                }
                */
                terminated.push(*id);
            }
        }

        for id in terminated {
            jobs.remove(&id);
        }
    }
}
