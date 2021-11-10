use std::fmt;

pub const Z: u8 = 1 << 7;
pub const N: u8 = 1 << 6;
pub const H: u8 = 1 << 5;
pub const C: u8 = 1 << 4;

#[derive(Debug)]
pub struct Registers {
    pub halted: bool,
    pub stopped: bool,
    pub ime: bool,
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub f: u8,
    pub h: u8,
    pub l: u8,
    pub pc: u16,
    sp: u16,
}

impl Registers {
    pub fn new() -> Self {
        Self {
            halted: false,
            stopped: false,
            ime: false,
            a: 0x01,
            b: 0x00,
            c: 0x13,
            d: 0x00,
            e: 0xd8,
            f: 0,
            h: 0x01,
            l: 0x4d,
            sp: 0xfffe,
            pc: 0x0100,
        }
    }

    #[inline]
    pub fn af(&self) -> u16 {
        ((self.a as u16) << 8) | (self.f as u16)
    }

    #[inline]
    pub fn bc(&self) -> u16 {
        ((self.b as u16) << 8) | (self.c as u16)
    }

    #[inline]
    pub fn de(&self) -> u16 {
        ((self.d as u16) << 8) | (self.e as u16)
    }

    #[inline]
    pub fn hl(&self) -> u16 {
        ((self.h as u16) << 8) | (self.l as u16)
    }

    pub fn bump(&mut self) -> u16 {
        let pc = self.pc;
        self.pc += 1;
        pc
    }
}

impl fmt::Display for Registers {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "a: {:02x}, b: {:02x}, c: {:02x}, d: {:02x}, e: {:02x}, f: {:02x}, h: {:02x}, l: {:02x} sp: {:04x}, pc: {:04x}",
            self.a, self.b, self.c, self.d, self.e, self.f, self.h, self.l, self.sp, self.pc,
        )
    }
}
