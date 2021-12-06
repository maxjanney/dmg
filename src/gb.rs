use crate::{cpu::Cpu, mem::Memory};

// The gameboy runs 70224 clock cycles a frame
const FRAME_CYCLES: u32 = 70224;

pub struct Gameboy {
    cpu: Cpu,
    mem: Memory,
}

#[derive(Clone, Copy)]
pub enum Interrupt {
    VBlank = 1 << 0,
    LCD = 1 << 1,
    Timer = 1 << 2,
    Serial = 1 << 3,
    Joypad = 1 << 4,
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
