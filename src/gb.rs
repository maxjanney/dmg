use crate::{cpu::Cpu, mem::Memory};

// The gameboy runs 70224 clock cycles a frame
const FRAME_CYCLES: u32 = 70224;

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

    pub fn frame(&mut self) {}
}
