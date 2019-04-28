use failure::{err_msg, Fail, Fallible};
use shell_vm::{Status, WaitForStatus};
use std::borrow::Cow;
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ExitStatus {
    Running,
    Stopped,
    ExitCode(i32),
    Signalled(i32),
}

impl ExitStatus {
    pub fn terminated(&self) -> bool {
        match self {
            ExitStatus::ExitCode(_) | ExitStatus::Signalled(_) => true,
            ExitStatus::Stopped | ExitStatus::Running => false,
        }
    }
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

impl From<ExitStatus> for Status {
    fn from(status: ExitStatus) -> Status {
        match status {
            ExitStatus::Running => Status::Running,
            ExitStatus::Stopped => Status::Stopped,
            ExitStatus::ExitCode(n) => Status::Complete((n as isize).into()),
            ExitStatus::Signalled(n) => Status::Complete((128 + n as isize).into()),
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
struct UnixChildInner {
    pid: libc::pid_t,
    final_status: Option<ExitStatus>,
}

impl UnixChildInner {
    fn wait(&mut self, blocking: bool) -> Option<ExitStatus> {
        if let Some(status) = self.final_status {
            return Some(status);
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
                    return None;
                }
                let err = std::io::Error::last_os_error();
                eprintln!("error waiting for child pid {} {}", self.pid, err);

                let status = ExitStatus::ExitCode(1);
                self.final_status = Some(status);
                return Some(status);
            }

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

            Some(status)
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnixChild {
    inner: Arc<Mutex<UnixChildInner>>,
}

#[cfg(unix)]
impl UnixChild {
    pub fn pid(&self) -> libc::pid_t {
        self.inner.lock().unwrap().pid
    }

    pub fn new(child: std::process::Child) -> Self {
        let pid = child.id() as _;
        Self {
            inner: Arc::new(Mutex::new(UnixChildInner {
                pid,
                final_status: None,
            })),
        }
    }

    fn wait(&self, blocking: bool) -> Option<ExitStatus> {
        self.inner.lock().unwrap().wait(blocking)
    }
}

#[cfg(unix)]
impl WaitForStatus for UnixChild {
    fn wait(&self) -> Option<Status> {
        UnixChild::wait(self, true).map(Into::into)
    }
    fn poll(&self) -> Option<Status> {
        UnixChild::wait(self, false).map(Into::into)
    }
}
