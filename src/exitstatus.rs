use crate::job::put_shell_in_foreground;
use failure::{Fail, Fallible};
use std::borrow::Cow;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ExitStatus {
    Running,
    Stopped,
    ExitCode(i32),
    Signalled(i32),
}

// libc doesn't provide an NSIG value, so we guess; this
// value is likely larger than reality but it is safe because
// we only use signal numbers produced from a process exit
// status to index into the sys_signame and sys_siglist
// globals, and those are therefore valid signal offsets.
const NSIG: usize = 1024;
extern "C" {
    static sys_signame: [*const i8; NSIG];
    static sys_siglist: [*const i8; NSIG];
}

fn signame(n: i32) -> Cow<'static, str> {
    unsafe {
        let c_str = sys_signame[n as usize];
        std::ffi::CStr::from_ptr(c_str).to_string_lossy()
    }
}

fn sigdesc(n: i32) -> Cow<'static, str> {
    unsafe {
        let c_str = sys_siglist[n as usize];
        std::ffi::CStr::from_ptr(c_str).to_string_lossy()
    }
}

impl std::fmt::Display for ExitStatus {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            ExitStatus::Running => write!(fmt, "running"),
            ExitStatus::Stopped => write!(fmt, "stopped"),
            ExitStatus::ExitCode(n) => write!(fmt, "exit code {}", n),
            ExitStatus::Signalled(n) => {
                let name = signame(*n);
                let desc = sigdesc(*n);
                write!(fmt, "signal {} sig{}: {}", n, name, desc)
            }
        }
    }
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
                .field("code", &status)
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

        let status = if libc::WIFSTOPPED(status) {
            ExitStatus::Stopped
        } else if libc::WIFSIGNALED(status) {
            ExitStatus::Signalled(libc::WTERMSIG(status))
        } else if libc::WIFEXITED(status) {
            ExitStatus::ExitCode(libc::WEXITSTATUS(status))
        } else {
            ExitStatus::Running
        };

        Ok(status)
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
        ExitStatus::ExitCode(0)
    }
    pub fn new_fail() -> Self {
        ExitStatus::ExitCode(1)
    }

    pub fn is_ok(&self) -> bool {
        match self {
            ExitStatus::ExitCode(0) => true,
            _ => false,
        }
    }

    pub fn invert(&self) -> ExitStatus {
        match self {
            ExitStatus::ExitCode(0) => ExitStatus::ExitCode(1),
            ExitStatus::ExitCode(_) | ExitStatus::Signalled(_) => ExitStatus::ExitCode(0),
            _ => ExitStatus::ExitCode(1),
        }
    }
}
