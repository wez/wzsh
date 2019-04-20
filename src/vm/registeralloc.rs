use super::RegisterAddress;

#[derive(Debug, Clone, Default)]
pub struct RegisterAllocator {
    /// Total number of registers allocated
    num_allocated: usize,

    /// Simple free list of registers that have
    /// been returned to us
    free_list: Vec<usize>,
}

impl RegisterAllocator {
    pub fn new() -> Self {
        Default::default()
    }

    /// Returns the size of the required register frame
    pub fn frame_size(&self) -> usize {
        self.num_allocated
    }

    /// Allocate a register.
    /// This may re-use a previously free register.
    #[must_use = "allocated a RegisterAddress which must be used"]
    pub fn allocate(&mut self) -> RegisterAddress {
        if let Some(idx) = self.free_list.pop() {
            RegisterAddress::FrameRelative(idx)
        } else {
            let idx = self.num_allocated;
            self.num_allocated += 1;
            RegisterAddress::FrameRelative(idx)
        }
    }

    /// Free a register, allowing it to potentially be re-used.
    pub fn free(&mut self, reg: RegisterAddress) {
        match reg {
            RegisterAddress::FrameRelative(idx) => self.free_list.push(idx),
            _ => panic!("cannot free register {:?}", reg),
        }
    }
}
