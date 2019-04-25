#![allow(dead_code)]
use failure::{bail, err_msg, Fallible};
use std::collections::VecDeque;
use std::ffi::{OsStr, OsString};
use std::path::PathBuf;
use std::sync::Arc;

mod environment;
mod host;
mod ioenv;

pub mod op;
pub use environment::*;
pub use host::*;
pub use ioenv::*;
pub use op::Operation;
use op::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    None,
    String(String),
    OsString(OsString),
    List(Vec<Value>),
    Integer(isize),
    WaitableStatus(WaitableStatus),
}

impl Value {
    fn as_os_str(&self) -> Option<&OsStr> {
        match self {
            Value::String(s) => Some(s.as_ref()),
            Value::OsString(s) => Some(s.as_os_str()),
            _ => None,
        }
    }

    fn truthy(&self) -> bool {
        match self {
            Value::None => false,
            Value::String(s) => !s.is_empty(),
            Value::OsString(s) => !s.is_empty(),
            Value::List(list) => !list.is_empty(),
            Value::Integer(n) => *n != 0,
            Value::WaitableStatus(status) => {
                match status.poll() {
                    // Invert for the program return code: 0 is success
                    Some(Status::Complete(value)) => !value.truthy(),
                    _ => false,
                }
            }
        }
    }
}

impl<T: ?Sized + AsRef<str>> From<&T> for Value {
    fn from(s: &T) -> Value {
        Value::String(s.as_ref().to_owned())
    }
}

impl From<String> for Value {
    fn from(s: String) -> Value {
        Value::String(s)
    }
}

impl From<OsString> for Value {
    fn from(s: OsString) -> Value {
        Value::OsString(s)
    }
}

impl From<isize> for Value {
    fn from(s: isize) -> Value {
        Value::Integer(s)
    }
}

impl From<Vec<Value>> for Value {
    fn from(s: Vec<Value>) -> Value {
        Value::List(s)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operand {
    /// A value known at compilation time
    Immediate(Value),
    /// A value located from the stack at runtime.
    /// The number is relative to the current frame
    /// pointer.
    FrameRelative(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum InstructionAddress {
    /// Relative to the start of the program
    Absolute(usize),
    /// Relative to the current program position
    Relative(isize),
}

#[derive(Debug, Default)]
pub struct Program {
    opcodes: Vec<Operation>,
}

impl Program {
    pub fn new(opcodes: Vec<Operation>) -> Arc<Program> {
        Arc::new(Self { opcodes })
    }
}

#[derive(Debug, Default)]
pub struct Frame {
    /// Absolute index to the top of the stack including the
    /// data required for this frame.  FrameRelative addressing
    /// is relative to this position.
    frame_pointer: usize,

    /// The number of stack entries occupied by this frame.
    /// When the frame is popped, the stack is trimmed to
    /// (frame_pointer-frame_size) entries.
    frame_size: usize,
}

#[derive(Debug, Default)]
pub struct Machine {
    stack: VecDeque<Value>,
    frames: VecDeque<Frame>,
    environment: VecDeque<Environment>,
    io_env: VecDeque<IoEnvironment>,
    cwd: PathBuf,
    host: Option<Arc<ShellHost>>,

    program: Arc<Program>,
    program_counter: usize,
}

/// This enum is essentially why this vm exists; it allows stepping
/// through a program and returning control to the host application
/// in the event that a process being waited upon is stopped.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Status {
    /// The program is still running; call step() again to make
    /// or check for progress.
    Running,
    /// The program is waiting on a process that has been put into
    /// the background and stopped.  Calling step() again will
    /// continue to return Stopped until the status of that process
    /// has changed.
    Stopped,
    /// The program has completed and yielded a value.
    Complete(Value),
}

fn split_by_ifs<'a>(value: &'a str, ifs: &str) -> Vec<&'a str> {
    let ifs: std::collections::HashSet<char> = ifs.chars().collect();
    let mut split = vec![];
    let mut run_start = None;

    for (idx, c) in value.char_indices() {
        if ifs.contains(&c) {
            if let Some(start) = run_start.take() {
                if idx > start + 1 {
                    split.push(&value[start..idx]);
                }
            }
            continue;
        }
        if run_start.is_none() {
            run_start = Some(idx);
        }
    }

    if let Some(start) = run_start.take() {
        split.push(&value[start..]);
    }

    split
}

impl Machine {
    pub fn new(program: &Arc<Program>) -> Fallible<Self> {
        let mut environment = VecDeque::new();
        environment.push_back(Environment::new());

        let mut io_env = VecDeque::new();
        io_env.push_back(IoEnvironment::new()?);

        let cwd = std::env::current_dir()?;

        Ok(Self {
            program: Arc::clone(program),
            environment,
            io_env,
            cwd,
            ..Default::default()
        })
    }

    fn environment(&self) -> Fallible<&Environment> {
        self.environment
            .back()
            .ok_or_else(|| err_msg("no current environment"))
    }

    fn environment_mut(&mut self) -> Fallible<&mut Environment> {
        self.environment
            .back_mut()
            .ok_or_else(|| err_msg("no current environment"))
    }

    /// Compute the effective value of IFS
    fn ifs(&self) -> Fallible<&str> {
        Ok(self.environment()?.get_str("IFS")?.unwrap_or(" \t\n"))
    }

    fn io_env(&self) -> Fallible<&IoEnvironment> {
        self.io_env
            .back()
            .ok_or_else(|| err_msg("no current IoEnvironment"))
    }

    fn io_env_mut(&mut self) -> Fallible<&mut IoEnvironment> {
        self.io_env
            .back_mut()
            .ok_or_else(|| err_msg("no current IoEnvironment"))
    }

    /// Attempt to make a single step of progress with the program.
    pub fn step(&mut self) -> Fallible<Status> {
        let program = Arc::clone(&self.program);
        let op = program
            .opcodes
            .get(self.program_counter)
            .ok_or_else(|| err_msg("walked off the end of the program"))?;
        self.program_counter += 1;
        match op.dispatch(self) {
            status @ Ok(Status::Stopped) => {
                // Rewind so that we retry this same op next time around
                self.program_counter -= 1;
                status
            }
            status => status,
        }
    }

    /// Continually invoke step() while the status == Running.
    /// Returns either Stopped or Complete at the appropriate time.
    pub fn run(&mut self) -> Fallible<Status> {
        loop {
            match self.step()? {
                Status::Running => continue,
                done => return Ok(done),
            }
        }
    }

    /// Resolve an operand for write.
    pub fn operand_mut(&mut self, operand: &Operand) -> Fallible<&mut Value> {
        match operand {
            Operand::Immediate(_) => bail!("cannot mutably reference an Immediate operand"),
            Operand::FrameRelative(offset) => self
                .stack
                .get_mut(
                    self.frames
                        .back()
                        .ok_or_else(|| err_msg("no frame?"))?
                        .frame_pointer
                        - offset,
                )
                .ok_or_else(|| err_msg("FrameRelative offset out of range")),
        }
    }

    /// Resolve an operand for read.
    pub fn operand<'a>(&'a self, operand: &'a Operand) -> Fallible<&'a Value> {
        match operand {
            Operand::Immediate(value) => Ok(value),
            Operand::FrameRelative(offset) => self
                .stack
                .get(
                    self.frames
                        .back()
                        .ok_or_else(|| err_msg("no frame?"))?
                        .frame_pointer
                        - offset,
                )
                .ok_or_else(|| err_msg("FrameRelative offset out of range")),
        }
    }

    pub fn operand_as_os_str<'a>(&'a self, operand: &'a Operand) -> Fallible<&'a OsStr> {
        self.operand(operand)?
            .as_os_str()
            .ok_or_else(|| err_msg("operand is not representable as OsStr"))
    }

    /// Resolve an operand for read, and return true if its value
    /// evaluates as true in a trutihness test.
    pub fn operand_truthy(&self, operand: &Operand) -> Fallible<bool> {
        Ok(self.operand(operand)?.truthy())
    }

    fn push_with_glob(list: &mut Vec<Value>, _glob: bool, v: Value) {
        // FIXME: implement glob.  We need to lookup the cwd for correctness.
        list.push(v);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    fn prog(ops: &[Operation]) -> Arc<Program> {
        Program::new(ops.to_vec())
    }

    fn machine(ops: &[Operation]) -> Machine {
        Machine::new(&prog(ops)).unwrap()
    }

    fn run_err(m: &mut Machine) -> String {
        format!("{}", m.run().unwrap_err())
    }

    #[test]
    fn test_exit() -> Fallible<()> {
        let mut m = machine(&[Operation::Exit(Exit {
            value: Operand::Immediate(Value::None),
        })]);
        assert_eq!(m.step()?, Status::Complete(Value::None));
        Ok(())
    }

    #[test]
    fn test_read_invalid_operand_no_frame() {
        let mut m = machine(&[Operation::Exit(Exit {
            value: Operand::FrameRelative(0),
        })]);
        assert_eq!(run_err(&mut m), "no frame?");
    }

    #[test]
    fn test_pop_too_many_frames() {
        let mut m = machine(&[Operation::PopFrame(PopFrame {})]);
        assert_eq!(run_err(&mut m), "frame underflow");
    }

    #[test]
    fn test_read_invalid_operand() {
        let mut m = machine(&[
            Operation::PushFrame(PushFrame { size: 1 }),
            Operation::Exit(Exit {
                value: Operand::FrameRelative(0),
            }),
        ]);
        assert_eq!(run_err(&mut m), "FrameRelative offset out of range");
    }

    #[test]
    fn test_write_invalid_operand() {
        let mut m = machine(&[Operation::Copy(Copy {
            source: Operand::Immediate(Value::None),
            destination: Operand::Immediate(Value::Integer(1)),
        })]);
        assert_eq!(
            run_err(&mut m),
            "cannot mutably reference an Immediate operand"
        );
    }

    #[test]
    fn test_unterminated() {
        let mut m = machine(&[]);
        assert_eq!(run_err(&mut m), "walked off the end of the program");
    }

    #[test]
    fn test_copy() -> Fallible<()> {
        let mut m = machine(&[
            Operation::PushFrame(PushFrame { size: 1 }),
            Operation::Copy(Copy {
                source: Operand::Immediate(Value::Integer(42)),
                destination: Operand::FrameRelative(1),
            }),
            Operation::Exit(Exit {
                value: Operand::FrameRelative(1),
            }),
        ]);
        assert_eq!(m.run()?, Status::Complete(Value::Integer(42)));
        Ok(())
    }

    #[test]
    fn test_split_by_ifs() {
        let ifs = " \t\n";
        assert_eq!(split_by_ifs("foo", ifs), vec!["foo"]);
        assert_eq!(split_by_ifs("foo bar", ifs), vec!["foo", "bar"]);
        assert_eq!(split_by_ifs("foo  bar ", ifs), vec!["foo", "bar"]);
        assert_eq!(split_by_ifs("\t foo  bar ", ifs), vec!["foo", "bar"]);
    }
}
