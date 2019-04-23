use super::*;

pub trait Dispatch {
    fn dispatch(&self, machine: &mut Machine) -> Fallible<()>;
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
