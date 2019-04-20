use super::registeralloc::RegisterAllocator;
use super::{Opcode, Operand, Value};
use crate::parse::{Command, CommandType};
use failure::{bail, Fallible};

#[derive(Debug)]
pub struct CodeGenerator {
    functions: Vec<FunctionBuilder>,
}

#[derive(Debug, Default)]
pub struct FunctionBuilder {
    register_allocator: RegisterAllocator,
    opcodes: Vec<Opcode>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        let func = FunctionBuilder::default();
        Self {
            functions: vec![func],
        }
    }

    fn current_func(&mut self) -> &mut FunctionBuilder {
        self.functions.last_mut().expect("no current function!?")
    }

    fn push_opcode(&mut self, opcode: Opcode) -> usize {
        let func = self.current_func();
        let idx = func.opcodes.len();
        func.opcodes.push(opcode);
        idx
    }

    pub fn generate(&mut self, command: &Command) -> Fallible<()> {
        match &command.command {
            CommandType::SimpleCommand(cmd) => {
                for t in &cmd.assignments {
                    if let Some((key, value)) = t.kind.parse_assignment_word() {
                        // FIXME: generic expansion on value before we assign it
                        self.push_opcode(Opcode::SetEnv {
                            name: Operand::Literal(Value::String(key.into())),
                            value: Operand::Literal(Value::String(value.into())),
                        });
                    }
                }

                Ok(())
            }
            _ => bail!("unhandled {:?}", command),
        }
    }
}
