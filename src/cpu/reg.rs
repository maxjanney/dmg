#[derive(Clone, Copy)]
pub enum Flag {
    Z = 1 << 7,
    N = 1 << 6,
    H = 1 << 5,
    C = 1 << 4,
}

#[derive(Debug)]
pub struct Registers {
    pub(super) halted: bool,
    pub(super) stopped: bool,
    pub(super) ime: bool,
    pub(super) a: u8,
    pub(super) b: u8,
    pub(super) c: u8,
    pub(super) d: u8,
    pub(super) e: u8,
    pub(super) f: u8,
    pub(super) h: u8,
    pub(super) l: u8,
    pub(super) pc: u16,
    pub(super) sp: u16,
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
            f: 0x00,
            h: 0x01,
            l: 0x4d,
            sp: 0xfffe,
            pc: 0x0100,
        }
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

    pub fn inc_hl(&mut self) {
        self.l = self.l.wrapping_add(1);
        if self.l == 0 {
            self.h += 1;
        }
    }

    pub fn dec_hl(&mut self) {
        self.l = self.l.wrapping_sub(1);
        if self.l == 0xff {
            self.h -= 1;
        }
    }

    #[inline]
    pub fn get_flag(&self, f: Flag) -> u8 {
        ((self.f & f as u8) != 0) as u8
    }

    #[inline]
    pub fn set_flag(&mut self, f: Flag, val: bool) {
        if val {
            self.f |= f as u8;
        } else {
            self.f &= !(f as u8);
        }
    }
}
