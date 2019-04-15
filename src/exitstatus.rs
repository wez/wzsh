use crate::job::put_shell_in_foreground;
use failure::{Fail, Fallible};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct ExitStatus {
    code: i32,
}

pub enum WaitableExitStatus {
    Child(std::process::Child),
    Done(ExitStatus),
}

impl std::fmt::Debug for WaitableExitStatus {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            WaitableExitStatus::Child(ref child) => fmt
                .debug_struct("WaitableExitStatus::Child")
                .field("pid", &child.id())
                .finish(),
            WaitableExitStatus::Done(status) => fmt
                .debug_struct("WaitableExitStatus::Done")
                .field("code", &status.code)
                .finish(),
        }
    }
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
        put_shell_in_foreground();

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
    pub fn wait(&mut self) -> Fallible<ExitStatus> {
        match self {
            WaitableExitStatus::Child(ref mut child) => {
                let status = wait_child(child)?;
                Ok(status.into())
            }
            WaitableExitStatus::Done(status) => Ok(*status),
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
