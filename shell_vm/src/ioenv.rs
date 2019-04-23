use failure::{format_err, Fallible};
use filedescriptor::{dup, FileDescriptor};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

#[derive(Clone)]
pub struct IoEnvironment {
    fds: HashMap<usize, Arc<Mutex<FileDescriptor>>>,
}

pub struct Readable {
    fd: Arc<Mutex<FileDescriptor>>,
}

impl std::io::Read for Readable {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, std::io::Error> {
        self.fd.lock().unwrap().read(buf)
    }
}

pub struct Writable {
    fd: Arc<Mutex<FileDescriptor>>,
}

impl std::io::Write for Writable {
    fn write(&mut self, buf: &[u8]) -> Result<usize, std::io::Error> {
        self.fd.lock().unwrap().write(buf)
    }

    fn flush(&mut self) -> Result<(), std::io::Error> {
        self.fd.lock().unwrap().flush()
    }
}

impl IoEnvironment {
    pub fn new() -> Fallible<Self> {
        let mut fds = HashMap::new();

        macro_rules! stdio {
            ($fd:literal, $func:path) => {
                fds.insert($fd, Arc::new(Mutex::new(dup($func())?)));
            };
        }

        stdio!(0, std::io::stdin);
        stdio!(1, std::io::stdout);
        stdio!(2, std::io::stderr);

        Ok(Self { fds })
    }

    pub fn stdin(&self) -> Readable {
        let fd = Arc::clone(self.fds.get(&0).expect("stdin fd is missing"));
        Readable { fd }
    }

    pub fn stdout(&self) -> Writable {
        let fd = Arc::clone(self.fds.get(&1).expect("stdout fd is missing"));
        Writable { fd }
    }

    pub fn stderr(&self) -> Writable {
        let fd = Arc::clone(self.fds.get(&2).expect("stdout fd is missing"));
        Writable { fd }
    }

    pub fn assign_fd(&mut self, fd_number: usize, fd: FileDescriptor) {
        self.fds.insert(fd_number, Arc::new(Mutex::new(fd)));
    }

    pub fn duplicate_to(&mut self, src_fd: usize, dest_fd: usize) -> Fallible<()> {
        let fd = self
            .fds
            .get(&src_fd)
            .ok_or_else(|| format_err!("duplicate_to: src_fd {} not present", src_fd))?;
        self.fds.insert(dest_fd, Arc::clone(fd));
        Ok(())
    }
}
