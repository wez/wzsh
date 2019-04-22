#![allow(dead_code)]
use failure::{bail, err_msg, Fallible};
use std::collections::VecDeque;
use std::ffi::OsString;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    None,
    String(String),
    OsString(OsString),
    List(Vec<Value>),
    Integer(isize),
}

impl<T: ?Sized + AsRef<str>> From<&T> for Value {
    fn from(s: &T) -> Value {
        Value::String(s.as_ref().to_owned())
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstructionAddress {
    /// Relative to the start of the program
    Absolute(usize),
    /// Relative to the current program position
    Relative(isize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operation {
    /// Copy the source to the destination, replacing the
    /// destination value.
    Copy {
        source: Operand,
        destination: Operand,
    },

    /// Terminate the program and return the specified value
    Exit { value: Operand },

    /// Append the string value from the source to
    /// the string value at the destination.
    StringAppend {
        source: Operand,
        destination: Operand,
    },

    /// Append the value from the source to the list
    /// value at the destination.
    ListAppend { value: Operand, list: Operand },

    /// Insert the source value to the destination list
    /// at the specified index.
    ListInsert {
        value: Operand,
        list: Operand,
        insertion_index: Operand,
    },

    /// Remove the value from the destination list at
    /// the specified index.
    ListRemove { list: Operand, index: Operand },

    /// destination = a + b
    Add {
        a: Operand,
        b: Operand,
        destination: Operand,
    },

    /// destination = a - b
    Subtract {
        a: Operand,
        b: Operand,
        destination: Operand,
    },

    /// destination = a * b
    Multiply {
        a: Operand,
        b: Operand,
        destination: Operand,
    },

    /// destination = a / b
    Divide {
        a: Operand,
        b: Operand,
        destination: Operand,
    },

    /// Unconditional jump
    Jump { target: InstructionAddress },

    /// If the operand is zero, jump.
    JumpIfZero {
        condition: Operand,
        target: InstructionAddress,
    },

    /// If the operand is non-zero, jump
    JumpIfNonZero {
        condition: Operand,
        target: InstructionAddress,
    },

    /// Reserve space for and push a new frame
    PushFrame { size: usize },

    /// Pop the current frame
    PopFrame,
}

#[derive(Debug, Default)]
pub struct Program {
    opcodes: Vec<Operation>,
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

impl Machine {
    pub fn new(program: &Arc<Program>) -> Self {
        Self {
            program: Arc::clone(program),
            ..Default::default()
        }
    }

    /// Attempt to make a single step of progress with the program.
    pub fn step(&mut self) -> Fallible<Status> {
        let program = Arc::clone(&self.program);
        let op = program
            .opcodes
            .get(self.program_counter)
            .ok_or_else(|| err_msg("walked off the end of the program"))?;
        self.program_counter += 1;
        match op {
            Operation::PushFrame { size } => {
                let new_size = self.stack.len() + size;
                self.stack.resize(new_size, Value::None);

                self.frames.push_back(Frame {
                    frame_pointer: new_size,
                    frame_size: *size,
                });
            }
            Operation::PopFrame => {
                let frame = self
                    .frames
                    .pop_back()
                    .ok_or_else(|| err_msg("frame underflow"))?;
                let new_size = frame.frame_pointer - frame.frame_size;
                self.stack.resize(new_size, Value::None);
            }
            Operation::Copy {
                source,
                destination,
            } => {
                let copy = self.operand(source)?.clone();
                *self.operand_mut(destination)? = copy;
            }
            Operation::Exit { value } => {
                return Ok(Status::Complete(self.operand(value)?.clone()));
            }
            _ => bail!("unhandled op: {:?}", op),
        };
        Ok(Status::Running)
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
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    fn prog(ops: &[Operation]) -> Arc<Program> {
        Arc::new(Program {
            opcodes: ops.to_vec(),
        })
    }

    fn machine(ops: &[Operation]) -> Machine {
        Machine::new(&prog(ops))
    }

    fn run_err(m: &mut Machine) -> String {
        format!("{}", m.run().unwrap_err())
    }

    #[test]
    fn test_exit() -> Fallible<()> {
        let mut m = machine(&[Operation::Exit {
            value: Operand::Immediate(Value::None),
        }]);
        assert_eq!(m.step()?, Status::Complete(Value::None));
        Ok(())
    }

    #[test]
    fn test_read_invalid_operand_no_frame() {
        let mut m = machine(&[Operation::Exit {
            value: Operand::FrameRelative(0),
        }]);
        assert_eq!(run_err(&mut m), "no frame?");
    }

    #[test]
    fn test_pop_too_many_frames() {
        let mut m = machine(&[Operation::PopFrame]);
        assert_eq!(run_err(&mut m), "frame underflow");
    }

    #[test]
    fn test_read_invalid_operand() {
        let mut m = machine(&[
            Operation::PushFrame { size: 1 },
            Operation::Exit {
                value: Operand::FrameRelative(0),
            },
        ]);
        assert_eq!(run_err(&mut m), "FrameRelative offset out of range");
    }

    #[test]
    fn test_write_invalid_operand() {
        let mut m = machine(&[Operation::Copy {
            source: Operand::Immediate(Value::None),
            destination: Operand::Immediate(Value::Integer(1)),
        }]);
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
            Operation::PushFrame { size: 1 },
            Operation::Copy {
                source: Operand::Immediate(Value::Integer(42)),
                destination: Operand::FrameRelative(1),
            },
            Operation::Exit {
                value: Operand::FrameRelative(1),
            },
        ]);
        assert_eq!(m.run()?, Status::Complete(Value::Integer(42)));
        Ok(())
    }
}
