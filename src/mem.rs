enum Mbc {
    Mbc1,
    Mbc2,
    Mbc3,
    Mbc5,
}

pub struct Memory {
    rom: Vec<u8>,
    ram: Vec<u8>,
    rom_bank: u16,
    ram_enabled: bool,
    mbc: Mbc,
}

impl Memory {
    // state of the gb after the boot rom
    // see https://bgb.bircd.org/pandocs.htm#powerupsequence
    fn power_up(&mut self) {
        self.wb(0xff05, 0x00); // TIMA
        self.wb(0xff06, 0x00); // TMA
        self.wb(0xff07, 0x00); // TAC
        self.wb(0xff10, 0x80); // NR10
        self.wb(0xff11, 0xbf); // NR11
        self.wb(0xff12, 0xf3); // NR12
        self.wb(0xff14, 0xbf); // NR14
        self.wb(0xff16, 0x3f); // NR21
        self.wb(0xff17, 0x00); // NR22
        self.wb(0xff19, 0xbf); // NR24
        self.wb(0xff1a, 0x7f); // NR30
        self.wb(0xff1b, 0xff); // NR31
        self.wb(0xff1c, 0x9f); // NR32
        self.wb(0xff1e, 0xbf); // NR33
        self.wb(0xff20, 0xff); // NR41
        self.wb(0xff21, 0x00); // NR42
        self.wb(0xff22, 0x00); // NR43
        self.wb(0xff23, 0xbf); // NR30
        self.wb(0xff24, 0x77); // NR50
        self.wb(0xff25, 0xf3); // NR51
        self.wb(0xff26, 0xf1); // NR52
        self.wb(0xff40, 0x91); // LCDC
        self.wb(0xff42, 0x00); // SCY
        self.wb(0xff43, 0x00); // SCX
        self.wb(0xff45, 0x00); // LYC
        self.wb(0xff47, 0xfc); // BGP
        self.wb(0xff48, 0xff); // OBP0
        self.wb(0xff49, 0xff); // OBP1
        self.wb(0xff4a, 0x00); // WY
        self.wb(0xff4b, 0x00); // WX
        self.wb(0xffff, 0x00); // IE
    }

    // Write a byte at the specified address
    pub fn wb(&mut self, addr: u16, val: u8) {
        match addr {
            0x0000..=0x1fff => match self.mbc {
                Mbc::Mbc1 | Mbc::Mbc3 | Mbc::Mbc5 => self.ram_enabled = val & 0x0f == 0x0a,
                Mbc::Mbc2 => {
                    if addr & 0x100 == 0 {
                        self.ram_enabled = !self.ram_enabled;
                    }
                }
            },
            0x2000..=0x3fff => match self.mbc {
                let val = val as u16;
                Mbc::Mbc1 => {
                    self.rom_bank = (self.rom_bank & 0x60) | (val & 0x1f);
                    if self.rom_bank == 0 {
                        self.rom_bank = 1;
                    }
                }
            },
        }
    }
}
