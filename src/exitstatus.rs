use crate::job::put_shell_in_foreground;
use failure::{err_msg, Fail, Fallible};
use std::borrow::Cow;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ExitStatus {
    Running,
    Stopped,
    ExitCode(i32),
    Signalled(i32),
}

impl From<std::process::ExitStatus> for ExitStatus {
    fn from(status: std::process::ExitStatus) -> ExitStatus {
        if let Some(code) = status.code() {
            ExitStatus::ExitCode(code)
        } else if status.success() {
            ExitStatus::ExitCode(0)
        } else {
            ExitStatus::ExitCode(1)
        }
    }
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

#[derive(Debug)]
pub struct UnixChild {
    pid: libc::pid_t,
    final_status: Option<ExitStatus>,
}

pub enum WaitableExitStatus {
    Child(std::process::Child),
    UnixChild(UnixChild),
    Done(ExitStatus),
}

impl std::fmt::Debug for WaitableExitStatus {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            WaitableExitStatus::Child(ref child) => fmt
                .debug_struct("WaitableExitStatus::Child")
                .field("pid", &child.id())
                .finish(),
            WaitableExitStatus::UnixChild(ref child) => fmt
                .debug_struct("WaitableExitStatus::UnixChild")
                .field("child", child)
                .finish(),
            WaitableExitStatus::Done(status) => fmt
                .debug_struct("WaitableExitStatus::Done")
                .field("code", &status)
                .finish(),
        }
    }
}

#[cfg(unix)]
impl UnixChild {
    pub fn pid(&self) -> libc::pid_t {
        self.pid
    }

    fn wait(&mut self, blocking: bool) -> Fallible<Option<ExitStatus>> {
        if let Some(status) = self.final_status {
            return Ok(Some(status));
        }
        unsafe {
            let mut status = 0i32;
            let res = libc::waitpid(
                self.pid,
                &mut status,
                libc::WUNTRACED | if blocking { 0 } else { libc::WNOHANG },
            );
            if res != self.pid {
                if !blocking {
                    return Ok(None);
                }
                let err = std::io::Error::last_os_error();
                return Err(err.context(format!("waiting for child pid {}", self.pid)))?;
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

            if status.terminated() {
                self.final_status = Some(status);
            }

            Ok(Some(status))
        }
    }
}

fn wait_child(child: &mut std::process::Child, blocking: bool) -> Fallible<Option<ExitStatus>> {
    let pid = child.id();
    let res = if blocking {
        child.wait().map(|s| Some(s.into()))
    } else {
        child.try_wait().map(|s| match s {
            Some(s) => Some(s.into()),
            None => None,
        })
    };
    res.map_err(|e| e.context(format!("waiting for child pid {}", pid)).into())
}

impl WaitableExitStatus {
    #[cfg(unix)]
    pub fn with_child(child: std::process::Child) -> Self {
        WaitableExitStatus::UnixChild(UnixChild {
            pid: child.id() as _,
            final_status: None,
        })
    }

    #[cfg(not(unix))]
    pub fn with_child(child: std::process::Child) -> Self {
        WaitableExitStatus::Child(child)
    }

    pub fn wait(&mut self) -> Fallible<ExitStatus> {
        match self {
            WaitableExitStatus::Child(ref mut child) => {
                let status = wait_child(child, true)?
                    .ok_or_else(|| err_msg("wait_child returned None in blocking mode"))?;
                Ok(status.into())
            }
            WaitableExitStatus::UnixChild(ref mut child) => {
                let status = child
                    .wait(true)?
                    .ok_or_else(|| err_msg("wait_child returned None in blocking mode"))?;
                Ok(status.into())
            }
            WaitableExitStatus::Done(status) => Ok(*status),
        }
    }

    pub fn try_wait(&mut self) -> Fallible<Option<ExitStatus>> {
        match self {
            WaitableExitStatus::Child(ref mut child) => {
                let status = wait_child(child, false)?;
                Ok(status.into())
            }
            WaitableExitStatus::UnixChild(ref mut child) => {
                let status = child.wait(false)?;
                Ok(status.into())
            }
            WaitableExitStatus::Done(status) => Ok(Some(*status)),
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

    pub fn terminated(&self) -> bool {
        match self {
            ExitStatus::ExitCode(_) | ExitStatus::Signalled(_) => true,
            ExitStatus::Stopped | ExitStatus::Running => false,
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
