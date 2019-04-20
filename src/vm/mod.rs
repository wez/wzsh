#![allow(dead_code)]
use crate::exitstatus::ExitStatus;
use shlex::string::ShellString;
pub mod codegen;
pub mod registeralloc;

pub type ProgramOffset = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    None,
    String(ShellString),
    Status(ExitStatus),
    Array(Vec<ShellString>),
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Program {
    program_address: ProgramOffset,
    opcodes: Vec<Opcode>,
    labels: Vec<ProgramOffset>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProgramAddress {
    /// An offset from the start of the program
    Absolute(ProgramOffset),

    /// An offset from the current program address
    Relative(isize),

    /// A label defines an address that is not known
    /// at that stage of compilation and that is filled
    /// in later.
    Label(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RegisterAddress {
    /// An offset from the bottom of the register stack
    Absolute(usize),

    /// An offset from the current top of the register stack
    FrameRelative(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operand {
    /// A value in a register
    Register(RegisterAddress),

    /// Some location in the program
    ProgramAddress(ProgramAddress),

    /// A literal value known at compile time
    Literal(Value),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Opcode {
    /// Halt execution of the program, yielding the operand
    Exit(Operand),

    /// Allocate the specified amount of additional space
    /// on the top of the stack and move the frame pointer
    /// by that amount.  The new stack entries initially hold
    /// null values.
    StackAllocate(usize),

    /// Unconditionally set the program address
    Jump(ProgramAddress),

    /// Resolve the source operand and copy its value
    /// to the destination operand.
    Copy {
        destination: Operand,
        source: Operand,
    },

    /// Evaluates the truthiness of the value specified
    /// by condition.  If it is true then Jump to the
    /// address_if_true address.
    ConditionalBranch {
        condition: Operand,
        address_if_true: ProgramAddress,
    },

    /// Concatenate the value of the source operand with the
    /// string operand specified by the destination.
    StringConcat {
        destination: Operand,
        source: Operand,
    },

    /// Append the value of the source operand to the
    /// array operand specified by the destination
    ArrayAppend {
        destination: Operand,
        source: Operand,
    },

    /// Set name=value in the current environment
    SetEnv { name: Operand, value: Operand },

    /// Clear name from the current environment
    UnsetEnv { name: Operand },
}
