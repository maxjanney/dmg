#[derive(PartialEq, Eq)]
enum Mbc {
    Mbc1,
    Mbc2,
    Mbc3,
    Mbc5,
}

enum BankMode {
    Rom,
    Ram,
}

impl BankMode {
    fn from_u8(val: u8) -> Self {
        if val & 1 != 0 {
            Self::Ram
        } else {
            Self::Rom
        }
    }
}

pub struct Memory {
    rom: Vec<u8>,
    ram: Vec<u8>,
    rom_bank: u16,
    ram_bank: u8,
    ram_enabled: bool,
    mode: BankMode,
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
            // 0000-1FFF - RAM and Timer Enable (Write Only)
            0x0000..=0x1fff => match self.mbc {
                Mbc::Mbc1 | Mbc::Mbc3 | Mbc::Mbc5 => self.ram_enabled = val & 0xf == 0xa,
                Mbc::Mbc2 => {
                    if addr & 0x100 == 0 {
                        self.ram_enabled = !self.ram_enabled;
                    }
                }
            },
            // 2000-3FFF - ROM Bank Number (Write Only)
            0x2000..=0x3fff => {
                let val = val as u16;
                match self.mbc {
                    Mbc::Mbc1 => {
                        self.rom_bank = (self.rom_bank & 0x60) | (val & 0x1f);
                        if self.rom_bank == 0 {
                            self.rom_bank = 1;
                        }
                    }
                    Mbc::Mbc2 => {
                        if addr & 0x100 != 0 {
                            self.rom_bank = val & 0xf;
                        }
                    }
                    Mbc::Mbc3 => {
                        let val = if val == 0 { 1 } else { val };
                        self.rom_bank = val & 0x7f;
                    }
                    Mbc::Mbc5 => {
                        if addr >> 12 == 0x2 {
                            // 2000 - 2FFF (ROM Bank Low)
                            self.rom_bank = (self.rom_bank & 0xff00) | val;
                        } else {
                            // $3000 - $3FFF (ROM Bank High)
                            let val = val & 1;
                            self.rom_bank = (self.rom_bank & 0x00ff) | (val << 8);
                        }
                    }
                }
            }
            // 4000-5FFF - RAM Bank Number - or - Upper Bits of ROM Bank Number
            0x4000..=0x5fff => match self.mbc {
                Mbc::Mbc1 => match self.mode {
                    BankMode::Rom => {
                        let val = (val & 0x3) as u16;
                        self.rom_bank = (self.rom_bank & 0x1f) | (val << 5);
                    }
                    BankMode::Ram => {
                        self.ram_bank = val & 0x3;
                    }
                },
                Mbc::Mbc3 => {
                    self.ram_bank = val & 0x3;
                    todo!("Rtc mapping");
                }
                Mbc::Mbc5 => {
                    self.ram_bank = val & 0xf;
                }
                _ => {}
            },
            // 6000-7FFF - ROM/RAM Mode Select (Write Only)
            0x6000..=0x7fff => match self.mbc {
                Mbc::Mbc1 => self.mode = BankMode::from_u8(val),
                Mbc::Mbc3 => todo!("Rtc latch"),
                _ => {}
            },
            // A000-BFFF - RAM Bank 00-03, if any (Read/Write)
            0xa000..=0xbfff => {
                if self.ram_enabled {
                    // todo!("check if rtc is currently mapped to this region for MBC2");
                    let val = if self.mbc == Mbc::Mbc2 {
                        val & 0xf
                    } else {
                        val
                    };
                    let offset = (self.ram_bank as u16) * 0x2000;
                    self.ram[((addr - 0xa000) + offset) as usize] = val;
                }
            }
            _ => {}
        }
    }
}
