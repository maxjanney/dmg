pub mod isa;
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

    pub fn step(&mut self, mem: &mut mem::Memory) -> u32 {
        0
    }
}
