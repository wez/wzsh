use crate::Status;
use failure::Fallible;
use std::ffi::OsString;
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
}
