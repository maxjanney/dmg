use crate::{input::Input, ppu::Ppu, timer::Timer};

const WRAM: usize = 0x2000;
const HRAM: usize = 0x7f;

#[derive(PartialEq, Eq)]
enum Mbc {
    Mbc1,
    Mbc2,
    Mbc3,
    Mbc5,
    Unknown,
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
    if_: u8,
    ie_: u8,
    timer: Timer,
    input: Input,
    ppu: Ppu,
    rom: Vec<u8>,
    ram: Vec<u8>,
    wram: Box<[u8; WRAM]>,
    hram: Box<[u8; HRAM]>,
    rom_bank: u16,
    ram_bank: u8,
    ram_enabled: bool,
    mode: BankMode,
    mbc: Mbc,
}

impl Memory {
    pub fn new() -> Self {
        Self {
            if_: 0,
            ie_: 0,
            timer: Timer::new(),
            input: Input::new(),
            ppu: Ppu::new(),
            rom: Vec::new(),
            ram: Vec::new(),
            wram: Box::new([0u8; WRAM]),
            hram: Box::new([0u8; HRAM]),
            rom_bank: 1,
            ram_bank: 0,
            ram_enabled: false,
            mode: BankMode::Rom,
            mbc: Mbc::Unknown,
        }
    }

    pub fn load_rom(&mut self, rom: Vec<u8>) {}

    // state of the gb after the boot rom finishes
    // see https://bgb.bircd.org/pandocs.htm#powerupsequence
    pub fn power_up(&mut self) {
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

    // Read a byte
    pub fn rb(&self, addr: u16) -> u8 {
        match addr {
            // 0000-3FFF - ROM Bank 00 (Read Only)
            0x0000..=0x3fff => self.rom[addr as usize],
            // 4000-7FFF - ROM Bank 01-7F (Read Only)
            0x4000..=0x7fff => {
                let offset = (self.rom_bank as u32) * 0x4000;
                self.rom[((addr as u32) - 0x4000 + offset) as usize]
            }
            // 8000 - 9FFF VRAM
            0x8000..=0x9fff => self.ppu.vram[(addr & 0x7fff) as usize],
            // A000-BFFF - RAM Bank 00-03, if any (Read/Write)
            0xa000..=0xbfff => {
                if !self.ram_enabled {
                    0xff
                } else {
                    // TODO: Rtc
                    let offset = (self.ram_bank as u16) * 0x2000;
                    self.ram[((addr - 0xa000) + offset) as usize]
                }
            }
            // 4KB Work RAM Bank 0 (WRAM)
            // E000 - EFFF mirrors C000 - CFFF
            0xc000..=0xcfff | 0xe000..=0xefff => self.wram[(addr & 0xfff) as usize],
            // 4KB Work RAM Bank 1 (WRAM)
            // F000 - FDFF mirrors D000 - DFFF
            0xd000..=0xdfff | 0xf000..=0xfdff => self.wram[((addr & 0xfff) + 0x1000) as usize],
            // FE00 - FE9F Sprite attribute table (OAM)
            0xfe00..=0xfe9f => self.ppu.oam[(addr & 0xff) as usize],
            // FF00-FF7F I/O Ports
            0xff00..=0xff7f => self.io_rb(addr),
            // FF80 - FFFE HRAM
            0xff80..=0xfffe => self.hram[(addr & 0x7f) as usize],
            // FFFF Interrupt enable register
            0xffff => self.ie_,
            _ => 0xff,
        }
    }

    // Read a word
    pub fn rw(&self, addr: u16) -> u16 {
        (self.rb(addr) as u16) | ((self.rb(addr + 1) as u16) << 8)
    }

    // Read byte from I/O register
    fn io_rb(&self, addr: u16) -> u8 {
        match (addr >> 4) & 0xf {
            0x0 => match addr & 0xf {
                0x0 => self.input.rb(),
                0x4 => self.timer.div,
                0x5 => self.timer.tima,
                0x6 => self.timer.tma,
                0x7 => self.timer.tac,
                _ => 0xff,
            },
            0x4 => self.ppu.rb(addr),
            _ => 0xff,
        }
    }

    // Write a byte
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
                _ => {}
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
                            // 3000 - 3FFF (ROM Bank High)
                            let val = val & 1;
                            self.rom_bank = (self.rom_bank & 0x00ff) | (val << 8);
                        }
                    }
                    _ => {}
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
                    // TODO: Rtc
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
                    // TODO: Rtc
                    let val = if self.mbc == Mbc::Mbc2 {
                        val & 0xf
                    } else {
                        val
                    };
                    let offset = (self.ram_bank as u16) * 0x2000;
                    self.ram[((addr - 0xa000) + offset) as usize] = val;
                }
            }
            // 4KB Work RAM Bank 0 (WRAM)
            // E000 - EFFF mirrors C000 - CFFF
            0xc000..=0xcfff | 0xe000..=0xefff => self.wram[(addr & 0xfff) as usize] = val,
            // 4KB Work RAM Bank 1 (WRAM)
            // F000 - FDFF mirrors D000 - DFFF
            0xd000..=0xdfff | 0xf000..=0xfdff => {
                self.wram[((addr & 0xfff) + 0x1000) as usize] = val;
            }
            // FE00 - FE9F Sprite attribute table (OAM)
            0xfe00..=0xfe9f => self.ppu.oam[(addr & 0xff) as usize] = val,
            // FF00-FF7F I/O Ports
            0xff00..=0xff7f => self.io_wb(addr, val),
            // FF80 - FFFE HRAM
            0xff80..=0xfffe => self.hram[(addr & 0x7f) as usize] = val,
            // FFFF Interrupt enable register
            0xffff => self.ie_ = val,
            _ => {}
        }
    }

    // Write a word
    pub fn ww(&mut self, addr: u16, val: u16) {
        self.wb(addr, val as u8);
        self.wb(addr + 1, (val >> 8) as u8);
    }

    // Write byte to I/O register
    fn io_wb(&mut self, addr: u16, val: u8) {
        match (addr >> 4) & 0xf {
            0x0 => match addr & 0xf {
                0x4 => self.timer.div = 0,
                0x5 => self.timer.tima = val,
                0x6 => self.timer.tma = val,
                0x7 => self.timer.update(val),
                0xf => self.if_ = val,
                _ => {}
            },
            _ => {}
        }
    }
}
