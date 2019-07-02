use crate::{Environment, IoEnvironment, Program, Status, Value};
use failure::Fallible;
use std::ffi::OsString;
use std::path::PathBuf;
use std::sync::Arc;

/// The WaitForStatus trait allows waiting on a spawned command.
/// Since the command could be a child process, some action
/// running in a another thread, or perhaps even be an inline
/// or immediately ready thing, the trait gives some flexibility
/// in waiting on whatever that implementation may be.
pub trait WaitForStatus: std::fmt::Debug {
    /// Non-blocking check for the status of the item
    fn poll(&self) -> Option<Status>;
    /// Block until the status of the item changes from Running
    /// to some other status.  It is possible that this may be
    /// subject to a spurious wakeup and that the returned
    /// status still shows as Running.
    fn wait(&self) -> Option<Status>;
}

/// Status is always immediately ready with its own value.
impl WaitForStatus for Status {
    fn wait(&self) -> Option<Status> {
        Some(self.clone())
    }

    fn poll(&self) -> Option<Status> {
        self.wait()
    }
}

impl From<Status> for WaitableStatus {
    fn from(status: Status) -> WaitableStatus {
        WaitableStatus::new(Arc::new(status))
    }
}

/// The WaitableStatus type is a little wrapper around the WaitForStatus
/// trait that allows embedding a concrete type into the Value enum
/// so that the status is visible to the vm.
#[derive(Clone, Debug)]
pub struct WaitableStatus {
    waiter: Arc<WaitForStatus>,
}

/// PartialEq is required by the Value enum.  This is a simple test for
/// equality based on the polled Status.
impl PartialEq for WaitableStatus {
    fn eq(&self, rhs: &WaitableStatus) -> bool {
        let lhs = self.poll();
        let rhs = rhs.poll();
        lhs == rhs
    }
}

/// Eq is required by the Value enum
impl Eq for WaitableStatus {}

impl WaitableStatus {
    pub fn new(waiter: Arc<WaitForStatus>) -> Self {
        Self { waiter }
    }

    /// Non-blocking check for the status of the item
    pub fn poll(&self) -> Option<Status> {
        self.waiter.poll()
    }

    /// Block until the status of the item changes from Running
    /// to some other status.  It is possible that this may be
    /// subject to a spurious wakeup and that the returned
    /// status still shows as Running.
    pub fn wait(&self) -> Option<Status> {
        self.waiter.wait()
    }
}

pub trait ShellHost: std::fmt::Debug {
    /// Look up the home directory for the specified user.
    /// If user is not specified, look it up for the current user.
    fn lookup_homedir(&self, user: Option<&str>) -> Fallible<OsString>;

    /// Spawn a command.
    /// The argument vector is pre-built, and the cwd, IO and environment
    /// variables are set up according to any redirection or other overrides
    /// specified by the user, so the role of the ShellHost in dispatching
    /// this method is to determine how to spawn the command.
    /// Most shells offer one of the following mechanisms, depending on
    /// the command:
    /// * Spawning a child process
    /// * Execution a function defined by the shell language script
    /// * Executing a builtin function.
    /// This interface allows for any of these and for others to
    /// occur by allowing the host to initiate running the command,
    /// but not requiring that it be complete upon returning from
    /// this method.   Instead, the WaitableStatus type allows the
    /// shell VM to poll or block until the command is complete.
    ///
    /// argv:
    /// The argument vector.  Element 0 contains the name of
    /// the command to be run.  The elements should generally
    /// be either String or OsString, but we allow other types
    /// for potential future flexibility (eg: passing a list directly
    /// to a function without joining/splitting).
    ///
    /// environment:
    /// The set of environment variables to propagate to the
    /// command.  Note that this is mutable in order to allow
    /// builtins such as `cd` to update the environment accordingly.
    ///
    /// current_directory:
    /// The current working directory.  This is mutable to
    /// support the `cd` builtin.
    ///
    /// io_env:
    /// The io environment allows the command to read or write
    /// to the stdio streams, or other defined descriptor numbers.
    fn spawn_command(
        &self,
        argv: &Vec<Value>,
        environment: &mut Environment,
        current_directory: &mut PathBuf,
        io_env: &IoEnvironment,
    ) -> Fallible<WaitableStatus>;

    fn define_function(&self, name: &str, program: &Arc<Program>) -> Fallible<()>;
}
