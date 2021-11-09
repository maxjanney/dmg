use super::cpu::Cpu;
use super::mem::Memory;

pub struct Gameboy {
    cpu: Cpu,
    mem: Memory,
}

impl Gameboy {
    pub fn new() -> Self {
        let mut gb = Gameboy {
            cpu: Cpu::new(),
            mem: Memory::new(),
        };
        gb.mem.power_up();
        gb
    }
}
