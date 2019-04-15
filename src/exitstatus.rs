use failure::{Fail, Fallible};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct ExitStatus {
    code: i32,
}

#[derive(Debug)]
pub enum WaitableExitStatus {
    Child(std::process::Child),
    Done(ExitStatus),
}

#[cfg(unix)]
fn wait_child(child: &mut std::process::Child) -> Fallible<ExitStatus> {
    let pid = child.id() as libc::pid_t;

    unsafe {
        let mut status = 0i32;
        let res = libc::waitpid(pid, &mut status, libc::WUNTRACED);
        if res != pid {
            let err = std::io::Error::last_os_error();
            return Err(err.context(format!("waiting for child pid {}", pid)))?;
        }
        // Put the shell back in the foreground
        let pgrp = libc::getpgid(libc::getpid());
        libc::tcsetpgrp(0, pgrp);

        Ok(ExitStatus { code: status })
    }
}

#[cfg(not(unix))]
fn wait_child(child: &mut std::process::Child) -> Fallible<ExitStatus> {
    let pid = child.id();
    child
        .wait()
        .map_err(|e| e.context(format!("waiting for child pid {}", pid)))
}

impl WaitableExitStatus {
    pub fn with_child(asynchronous: bool, mut child: std::process::Child) -> Fallible<Self> {
        if asynchronous {
            // TODO: should really distinguish between async procs and
            // async jobs as part of job control.
            // For now, if we're async we just ignore the child status
            // and pretend that it is good.
            //Ok(WaitableExitStatus::Child(child))
            Ok(WaitableExitStatus::Done(ExitStatus::new_ok()))
        } else {
            let status = wait_child(&mut child)?;
            Ok(WaitableExitStatus::Done(status.into()))
        }
    }

    pub fn wait(self) -> Fallible<ExitStatus> {
        match self {
            WaitableExitStatus::Child(mut child) => Ok(child.wait()?.into()),
            WaitableExitStatus::Done(status) => Ok(status),
        }
    }
}

impl ExitStatus {
    pub fn new_ok() -> Self {
        Self { code: 0 }
    }
    pub fn new_fail() -> Self {
        Self { code: 1 }
    }

    pub fn is_ok(&self) -> bool {
        self.code == 0
    }

    pub fn code(&self) -> i32 {
        self.code
    }

    pub fn invert(&self) -> ExitStatus {
        if self.is_ok() {
            ExitStatus { code: 1 }
        } else {
            ExitStatus { code: 0 }
        }
    }
}

impl From<std::process::ExitStatus> for ExitStatus {
    fn from(s: std::process::ExitStatus) -> ExitStatus {
        if let Some(code) = s.code() {
            ExitStatus { code }
        } else {
            // FIXME: do something smarter about reporting signals
            ExitStatus { code: 127 }
        }
    }
}
