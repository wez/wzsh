use failure::{bail, Fallible};
use std::os::unix::prelude::*;

pub struct FileDescriptor {
    fd: RawFd,
}

impl Drop for FileDescriptor {
    fn drop(&mut self) {
        unsafe {
            libc::close(self.fd);
        }
    }
}

impl AsRawFd for FileDescriptor {
    fn as_raw_fd(&self) -> RawFd {
        self.fd
    }
}

fn dup(fd: RawFd) -> Fallible<FileDescriptor> {
    let duped = unsafe { libc::dup(fd) };
    if duped == -1 {
        bail!(
            "dup of fd {} failed: {:?}",
            fd,
            std::io::Error::last_os_error()
        )
    } else {
        let mut owned = FileDescriptor { fd: duped };
        owned.cloexec()?;
        Ok(owned)
    }
}

pub struct Pipes {
    pub read: FileDescriptor,
    pub write: FileDescriptor,
}

impl FileDescriptor {
    pub fn dup<F: AsRawFd>(f: F) -> Fallible<Self> {
        dup(f.as_raw_fd())
    }

    pub fn pipe() -> Fallible<Pipes> {
        let mut fds = [-1i32; 2];
        let res = unsafe { libc::pipe(fds.as_mut_ptr()) };
        if res == -1 {
            bail!(
                "failed to create a pipe: {:?}",
                std::io::Error::last_os_error()
            )
        } else {
            let mut read = FileDescriptor { fd: fds[0] };
            let mut write = FileDescriptor { fd: fds[1] };
            read.cloexec()?;
            write.cloexec()?;
            Ok(Pipes { read, write })
        }
    }

    /// Helper function to set the close-on-exec flag for a raw descriptor
    fn cloexec(&mut self) -> Fallible<()> {
        let flags = unsafe { libc::fcntl(self.fd, libc::F_GETFD) };
        if flags == -1 {
            bail!(
                "fcntl to read flags failed: {:?}",
                std::io::Error::last_os_error()
            );
        }
        let result = unsafe { libc::fcntl(self.fd, libc::F_SETFD, flags | libc::FD_CLOEXEC) };
        if result == -1 {
            bail!(
                "fcntl to set CLOEXEC failed: {:?}",
                std::io::Error::last_os_error()
            );
        }
        Ok(())
    }

    pub fn as_stdio(&self) -> Fallible<std::process::Stdio> {
        let duped = dup(self.fd)?;
        let fd = duped.fd;
        let stdio = unsafe { std::process::Stdio::from_raw_fd(fd) };
        std::mem::forget(duped); // don't drop; stdio now owns it
        Ok(stdio)
    }
}