use super::*;
use failure::{bail, ensure, format_err, ResultExt};
use filedescriptor::FileDescriptor;

/// The Dispatch trait is implemented by the individual operation
/// types, and via the Operation enum that encompasses all possible
/// operations.
/// The program counter has been pre-incremented which means that
/// relative Jump instructions need to take that into account.
/// If the return value is Status::Stopped, the step implementation
/// will rewind the program counter such that that same operation
/// will be retried on a subsequent step call.
pub trait Dispatch {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<Status>;
}

macro_rules! op {
    (
    $(
        $(#[$outer:meta])*
        $name:ident {
            $(
                $(#[$inner:meta])*
                $field:ident: $ty:ty
            ),* $(,)?
        }
        $(,)?
    )*
    ) => {
$(
$(#[$outer])*
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct $name {
    $(
    $(#[$inner])*
    pub $field: $ty,
    )*
}

impl Into<Operation> for $name {
    fn into(self) -> Operation {
        Operation::$name(self)
    }
}

)*

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operation {
$(
    $name($name),
)*
}

impl Dispatch for Operation {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<Status> {
        match self {
            $(
                Operation::$name(inner) => inner.dispatch(machine),
            )*
        }
    }
}
    };
}

op!(
    /// Copy the source to the destination, replacing the
    /// destination value.
    Copy {
        source: Operand,
        destination: Operand,
    },
    /// Append the string value from the source to
    /// the string value at the destination.
    /// Appending Value::None is allowed and is a NOP.
    StringAppend {
        source: Operand,
        destination: Operand
    },
    /// Terminate the program and return the specified value
    Exit { value: Operand },
    /// Append the value from the source to the list
    /// value at the destination.
    /// If split is true, split value using the current IFS value
    /// before appending, and append the generated elements instead.
    /// If glob is true and the element(s) are
    /// subject to filename generation (which may yield additional fields)
    /// prior to being appended to the list.
    ListAppend {
        value: Operand,
        list: Operand,
        split: bool,
        glob: bool,
    },
    /// Insert the source value to the destination list
    /// at the specified index.
    ListInsert {
        value: Operand,
        list: Operand,
        insertion_index: Operand,
    },
    /// Remove the value from the destination list at
    /// the specified index.
    ListRemove {
        list: Operand,
        index: Operand,
    },
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
    Jump {
        target: InstructionAddress,
    },
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
    PopFrame {},
    PushIo {},
    PopIo {},
    DupFd {
        src_fd: usize,
        dest_fd: usize,
    },
    OpenFile {
        /// The file to open.  Can either be an immediate string
        /// value, or a list.  If a list, only a list with a single
        /// string element is permitted.
        name: Operand,
        fd_number: usize,
        input: bool,
        output: bool,
        clobber: bool,
        append: bool,
    },
    /// Clone the current output and environment variables and
    /// push them on the environment stack.  Subsequent command
    /// invocations will use the top of the environment stack.
    PushEnvironment {},
    /// Pop the top of the environment stack
    PopEnvironment {},
    /// Set a variable in the current environment
    SetEnv {
        name: Operand,
        value: Operand,
    },
    /// Get a variable from the current environment and store it
    /// into the destination.  If the variable isn't present,
    /// Value::None is stored instead.
    GetEnv {
        name: Operand,
        target: Operand,
    },
    /// Perform tilde expansion on the input and store in the output.
    TildeExpand {
        name: Operand,
        destination: Operand,
    },
    /// Test whether the source operand is Value::None.  If so, stores
    /// Integer(1) into destination, else stores Integer(0).
    IsNone {
        source: Operand,
        destination: Operand,
    },
    /// Test whether the source operand is Value::None, or is Value::String("").
    /// If so, stores Integer(1) into destination, else stores Integer(0).
    IsNoneOrEmptyString {
        source: Operand,
        destination: Operand,
    },
    /// The target list is joined into a single string using the first character
    /// of the IFS variable in the current environment.  The result is placed
    /// in destination
    JoinList {
        list: Operand,
        destination: Operand,
    },
);

impl Dispatch for Copy {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<Status> {
        let copy = machine.operand(&self.source)?.clone();
        *machine.operand_mut(&self.destination)? = copy;
        Ok(Status::Running)
    }
}

impl Dispatch for PushFrame {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<Status> {
        let new_size = machine.stack.len() + self.size;
        machine.stack.resize(new_size, Value::None);

        machine.frames.push_back(Frame {
            frame_pointer: new_size,
            frame_size: self.size,
        });
        Ok(Status::Running)
    }
}

impl Dispatch for PopFrame {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<Status> {
        let frame = machine
            .frames
            .pop_back()
            .ok_or_else(|| err_msg("frame underflow"))?;
        let new_size = frame.frame_pointer - frame.frame_size;
        machine.stack.resize(new_size, Value::None);
        Ok(Status::Running)
    }
}

impl Dispatch for PushEnvironment {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<Status> {
        let cloned = machine.environment()?.clone();
        machine.environment.push_back(cloned);
        Ok(Status::Running)
    }
}

impl Dispatch for PopEnvironment {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<Status> {
        machine
            .environment
            .pop_back()
            .ok_or_else(|| err_msg("environment underflow"))?;
        Ok(Status::Running)
    }
}

impl Dispatch for PushIo {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<Status> {
        let cloned = machine.io_env()?.clone();
        machine.io_env.push_back(cloned);
        Ok(Status::Running)
    }
}

impl Dispatch for PopIo {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<Status> {
        machine
            .io_env
            .pop_back()
            .ok_or_else(|| err_msg("IoEnvironment underflow"))?;
        Ok(Status::Running)
    }
}

impl Dispatch for GetEnv {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<Status> {
        let value = machine
            .environment()?
            .get(machine.operand_as_os_str(&self.name)?)
            .map(|x| Value::OsString(x.into()))
            .unwrap_or(Value::None);
        *machine.operand_mut(&self.target)? = value;
        Ok(Status::Running)
    }
}

impl Dispatch for SetEnv {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<Status> {
        let name = machine.operand_as_os_str(&self.name)?.to_os_string();
        let value = machine.operand_as_os_str(&self.value)?.to_os_string();
        machine.environment_mut()?.set(name, value);
        Ok(Status::Running)
    }
}

impl Dispatch for Exit {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<Status> {
        Ok(Status::Complete(machine.operand(&self.value)?.clone()))
    }
}

impl Dispatch for StringAppend {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<Status> {
        let src = match machine.operand(&self.source)? {
            Value::String(s) => s.clone(),
            Value::None => return Ok(Status::Running),
            _ => bail!("cannot StringAppend from non-string"),
        };
        match machine.operand_mut(&self.destination)? {
            Value::String(dest) => dest.push_str(&src),
            _ => bail!("cannot StringAppend to non-string"),
        }
        Ok(Status::Running)
    }
}

impl Dispatch for ListAppend {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<Status> {
        let ifs = machine
            .environment()?
            .get_str("IFS")?
            .unwrap_or(" \t\n")
            .to_owned();
        let src = machine.operand(&self.value)?.clone();
        let list = match machine.operand_mut(&self.list)? {
            Value::List(dest) => dest,
            _ => bail!("cannot ListAppend to non-list"),
        };

        if self.split && !ifs.is_empty() {
            match src {
                Value::String(src) => {
                    for word in split_by_ifs(&src, &ifs) {
                        Machine::push_with_glob(list, self.glob, word.into());
                    }
                }
                _ => Machine::push_with_glob(list, self.glob, src),
            };
        } else {
            Machine::push_with_glob(list, self.glob, src);
        }
        Ok(Status::Running)
    }
}

impl Dispatch for DupFd {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<Status> {
        machine
            .io_env_mut()?
            .duplicate_to(self.src_fd, self.dest_fd)?;
        Ok(Status::Running)
    }
}

impl Dispatch for OpenFile {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<Status> {
        let file_name = match machine.operand(&self.name)? {
            Value::OsString(s) => s.clone(),
            Value::String(s) => OsStr::new(s).to_os_string(),
            Value::List(list) if list.is_empty() => bail!("redirection matched 0 items"),
            Value::List(list) if list.len() > 1 => bail!("ambiguous redirection"),
            Value::List(list) => {
                if let Some(os) = list[0].as_os_str() {
                    os.to_os_string()
                } else {
                    bail!("invalid redirection to {:?}", list)
                }
            }
            invalid => bail!("invalid redirection to {:?}", invalid),
        };
        let file_name = PathBuf::from(file_name);
        let file_name = if file_name.is_absolute() {
            file_name
        } else {
            machine.cwd.join(file_name)
        };
        let mut options = std::fs::OpenOptions::new();
        options
            .read(self.input)
            .write(self.output || self.append || self.clobber)
            .append(self.append)
            .truncate(self.output && !self.append)
            // TODO: if a noclobber option is set, and !self.clobber,
            // then we should look at .create_new() instead
            .create(self.output || self.append || self.clobber);
        let file = options.open(&file_name).context(format!(
            "opening '{}' using {:#?}",
            file_name.display(),
            options
        ))?;
        let fd = FileDescriptor::dup(file).context(format!(
            "duplicating open file handle from '{}",
            file_name.display()
        ))?;

        machine.io_env_mut()?.assign_fd(self.fd_number, fd);
        Ok(Status::Running)
    }
}

/// Calculate the new program counter value after applying target.
fn compute_jump_target(machine: &mut Machine, target: InstructionAddress) -> Fallible<usize> {
    // we need to account for the fact that the
    // program counter is pre-incremented prior to calling Dispatch::dispatch.
    let pc = machine.program_counter - 1;
    let dest = match target {
        InstructionAddress::Absolute(dest) => dest,
        InstructionAddress::Relative(offset) if offset >= 0 => {
            pc.checked_add(offset as usize).ok_or_else(|| {
                format_err!(
                    "overflow while computing jump target; PC={} target={:?}",
                    pc,
                    target
                )
            })?
        }
        InstructionAddress::Relative(offset) => {
            pc.checked_sub(offset as usize).ok_or_else(|| {
                format_err!(
                    "overflow while computing jump target; PC={} target={:?}",
                    pc,
                    target
                )
            })?
        }
    };

    ensure!(
        dest < machine.program.opcodes.len(),
        "jump target walks off the end of the program. PC={} target={:?} dest={}",
        pc,
        target,
        dest
    );
    Ok(dest)
}

impl Dispatch for Jump {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<Status> {
        let target = compute_jump_target(machine, self.target)?;
        machine.program_counter = target;
        Ok(Status::Running)
    }
}

impl Dispatch for JumpIfZero {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<Status> {
        if !machine.operand_truthy(&self.condition)? {
            let target = compute_jump_target(machine, self.target)?;
            machine.program_counter = target;
        }
        Ok(Status::Running)
    }
}

impl Dispatch for JumpIfNonZero {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<Status> {
        if machine.operand_truthy(&self.condition)? {
            let target = compute_jump_target(machine, self.target)?;
            machine.program_counter = target;
        }
        Ok(Status::Running)
    }
}

impl Dispatch for IsNone {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<Status> {
        let is_none = match machine.operand(&self.source)? {
            Value::None => 1,
            _ => 0,
        };

        *machine.operand_mut(&self.destination)? = is_none.into();

        Ok(Status::Running)
    }
}

impl Dispatch for IsNoneOrEmptyString {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<Status> {
        let is_none = match machine.operand(&self.source)? {
            Value::None => 1,
            Value::String(s) if s.is_empty() => 1,
            _ => 0,
        };

        *machine.operand_mut(&self.destination)? = is_none.into();

        Ok(Status::Running)
    }
}

macro_rules! notyet {
    ($($name:ty),* $(,)?) => {
        $(
impl Dispatch for $name {
    fn dispatch(&self, _machine: &mut Machine) -> Fallible<Status> {
        bail!("dispatch not impl for {:?}", self)
    }
}

        )*
    }
}

notyet!(
    Add,
    Divide,
    JoinList,
    ListInsert,
    ListRemove,
    Multiply,
    Subtract,
    TildeExpand,
);
