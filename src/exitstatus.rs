#[cfg(windows)]
use filedescriptor::OwnedHandle;
use shell_vm::{Status, WaitForStatus};
use std::borrow::Cow;
#[cfg(windows)]
use std::os::windows::io::{AsRawHandle, IntoRawHandle};
use std::sync::{Arc, Mutex};

#[cfg(unix)]
pub type Pid = libc::pid_t;
#[cfg(windows)]
pub type Pid = u32;

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
#[cfg(unix)]
const NSIG: usize = 1024;
#[cfg(unix)]
extern "C" {
    static sys_signame: [*const i8; NSIG];
    static sys_siglist: [*const i8; NSIG];
}

fn signame(n: i32) -> Cow<'static, str> {
    #[cfg(unix)]
    unsafe {
        let c_str = sys_signame[n as usize];
        std::ffi::CStr::from_ptr(c_str).to_string_lossy()
    }
    #[cfg(windows)]
    unreachable!(n);
}

fn sigdesc(n: i32) -> Cow<'static, str> {
    #[cfg(unix)]
    unsafe {
        let c_str = sys_siglist[n as usize];
        std::ffi::CStr::from_ptr(c_str).to_string_lossy()
    }
    #[cfg(windows)]
    unreachable!(n);
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
struct ChildProcessInner {
    pid: Pid,
    last_status: ExitStatus,
    #[cfg(windows)]
    process: OwnedHandle,
}

impl ChildProcessInner {
    fn wait(&mut self, blocking: bool) -> Option<ExitStatus> {
        if self.last_status.terminated() {
            return Some(self.last_status);
        }
        #[cfg(unix)]
        unsafe {
            let mut status = 0i32;
            let res = libc::waitpid(
                self.pid,
                &mut status,
                libc::WUNTRACED | if blocking { 0 } else { libc::WNOHANG },
            );
            if res != self.pid {
                if !blocking {
                    return Some(self.last_status);
                }
                let err = std::io::Error::last_os_error();
                eprintln!("error waiting for child pid {} {}", self.pid, err);

                let status = ExitStatus::ExitCode(1);
                self.last_status = status;
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

            self.last_status = status;
            Some(status)
        }
        #[cfg(windows)]
        {
            use winapi::shared::winerror::WAIT_TIMEOUT;
            use winapi::um::processthreadsapi::GetExitCodeProcess;
            use winapi::um::synchapi::*;
            use winapi::um::winbase::WAIT_OBJECT_0;
            if blocking {
                let res = unsafe { WaitForSingleObject(self.process.as_raw_handle(), 0) };
                match res {
                    WAIT_OBJECT_0 => {}
                    WAIT_TIMEOUT => return None,
                    _ => {
                        let err = std::io::Error::last_os_error();
                        eprintln!("error waiting for child pid {} {}", self.pid, err);
                        let status = ExitStatus::ExitCode(1);
                        self.last_status = status;
                        return Some(status);
                    }
                };
            }

            let mut exit_code: winapi::shared::minwindef::DWORD = 0;
            let status =
                if unsafe { GetExitCodeProcess(self.process.as_raw_handle(), &mut exit_code) } != 0
                {
                    ExitStatus::ExitCode(exit_code as i32)
                } else {
                    let err = std::io::Error::last_os_error();
                    eprintln!("error getting exit code for child pid {} {}", self.pid, err);
                    ExitStatus::ExitCode(1)
                };
            self.last_status = status;
            Some(status)
        }
    }
}

#[derive(Debug, Clone)]
pub struct ChildProcess {
    inner: Arc<Mutex<ChildProcessInner>>,
}

impl ChildProcess {
    pub fn pid(&self) -> Pid {
        self.inner.lock().unwrap().pid
    }

    pub fn new(child: std::process::Child) -> Self {
        let pid = child.id() as _;
        Self {
            inner: Arc::new(Mutex::new(ChildProcessInner {
                pid,
                last_status: ExitStatus::Running,
                #[cfg(windows)]
                process: OwnedHandle::new(child.into_raw_handle()),
            })),
        }
    }

    fn wait(&self, blocking: bool) -> Option<ExitStatus> {
        self.inner.lock().unwrap().wait(blocking)
    }
}

impl WaitForStatus for ChildProcess {
    fn wait(&self) -> Option<Status> {
        ChildProcess::wait(self, true).map(Into::into)
    }
    fn poll(&self) -> Option<Status> {
        ChildProcess::wait(self, false).map(Into::into)
    }
}
