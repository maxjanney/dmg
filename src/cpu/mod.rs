pub mod reg;

use super::mem;
use reg::Registers;

pub struct Cpu {
    regs: Registers,
}

impl Cpu {
    pub fn new() -> Self {
        Self {
            regs: Registers::new(),
        }
    }

    pub fn exec(&mut self, mem: &mut mem::Memory) {}
}
