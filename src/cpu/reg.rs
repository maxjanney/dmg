use bitflags::bitflags;

bitflags! {
    #[derive(Default)]
    pub struct Flag: u8 {
        const C = 0b0001_0000;
        const H = 0b0010_0000;
        const N = 0b0100_0000;
        const Z = 0b1000_0000;
    }
}

#[derive(Default)]
pub struct Registers {
    pub(super) stopped: bool,
    pub(super) halted: bool,
    pub(super) a: u8,
    pub(super) b: u8,
    pub(super) c: u8,
    pub(super) d: u8,
    pub(super) e: u8,
    pub(super) h: u8,
    pub(super) l: u8,
    pub(super) pc: u16,
    pub(super) sp: u16,
    pub(super) f: Flag,
}

impl Registers {
    pub fn new() -> Self {
        Self {
            a: 0x01,
            c: 0x13,
            e: 0xd8,
            h: 0x01,
            l: 0x4d,
            sp: 0xfffe,
            pc: 0x0100,
            ..Default::default()
        }
    }

    pub fn bc(&self) -> u16 {
        ((self.b as u16) << 8) | (self.c as u16)
    }

    pub fn de(&self) -> u16 {
        ((self.d as u16) << 8) | (self.e as u16)
    }

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

    pub fn flag_bits(&self) -> u8 {
        self.f.bits()
    }

    pub fn contains(&self, f: Flag) -> bool {
        self.f.contains(f)
    }

    pub fn toggle(&mut self, f: Flag) {
        self.f.toggle(f);
    }

    pub fn set(&mut self, f: Flag, val: bool) {
        self.f.set(f, val);
    }

    pub fn insert(&mut self, f: Flag) {
        self.f.insert(f);
    }

    pub fn remove(&mut self, f: Flag) {
        self.f.remove(f);
    }
}
