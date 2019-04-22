/// A very simple register allocation scheme
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
    pub fn allocate(&mut self) -> usize {
        if let Some(idx) = self.free_list.pop() {
            idx
        } else {
            let idx = self.num_allocated;
            self.num_allocated += 1;
            // Since the returned index will be used relative to
            // the top, we can't return a 0 here.  Instead we
            // always add one so that we are always below the
            // top of the stack.
            idx + 1
        }
    }

    /// Free a register, allowing it to potentially be re-used.
    pub fn free(&mut self, reg: usize) {
        self.free_list.push(reg);
    }
}
