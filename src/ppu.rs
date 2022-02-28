use bitflags::bitflags;

const VRAM_SIZE: usize = 0x2000;
pub const OAM_SIZE: usize = 0xa0;

pub const WIDTH: usize = 160;
pub const HEIGHT: usize = 140;

const NUM_TILES: usize = 384;

type Color = (u8, u8, u8);
type Tile = [[u8; 8]; 8];

const PALETTE: [Color; 4] = [
    // White
    (255, 255, 255),
    // Light gray
    (192, 192, 192),
    // Dark gray
    (96, 96, 96),
    // Black
    (0, 0, 0),
];

#[derive(Copy, Clone, Debug)]
enum Mode {
    HBlank,
    VBlank,
    Searching,
    Transferring,
}

struct Palette {
    // Background and window tiles
    bgp: [Color; 4],
    // Sprite palette 0
    sp0: [Color; 4],
    // Sprite palette 1
    sp1: [Color; 4],
}

struct Tiles {
    data: [Tile; NUM_TILES],
    to_update: [bool; NUM_TILES],
    needs_update: bool,
}

bitflags! {
    struct Lcdc: u8 {
        // LCD and PPU enable
        const LE = 1 << 7;
        // Window tile map area
        const WA = 1 << 6;
        // Window enable
        const WE = 1 << 5;
        // BG and Window tile data area
        const BWA = 1 << 4;
        // BG tile map area
        const BA = 1 << 3;
        // OBJ Size
        const OS = 1 << 2;
        // OBJ Enagle
        const OE = 1 << 1;
        // BG and Window enable/priority
        const BWE = 1 << 0;
    }
}

bitflags! {
    struct Lcds: u8 {
        // LYC=LY STAT Interrupt source
        const LY = 1 << 6;
        // Mode 2 OAM STAT Interrupt source
        const M2 = 1 << 5;
        // // Mode 1 VBlank STAT Interrupt source
        const M1 = 1 << 4;
        // Mode 0 HBlank STAT Interrupt source
        const M0 = 1 << 3;
    }
}

pub struct Ppu {
    pub vram: [u8; VRAM_SIZE],
    pub oam: [u8; OAM_SIZE],
    pub buf: [u8; 4 * WIDTH * HEIGHT],
    // Ticks
    ticks: u32,
    // Cached tiles
    tiles: Tiles,
    // Palettes
    pal: Palette,
    // LCD Control register at 0xff40
    lcdc: Lcdc,
    // Mode flag
    mode: Mode,
    // LCD Status register at 0xff41
    lcds: Lcds,
    // FF42 - SCY (Scroll Y) (R/W)
    scy: u8,
    // FF43 - SCX (Scroll X) (R/W)
    scx: u8,
    // FF44 - LY (LCD Y Coordinate) (R)
    ly: u8,
    // FF45 - LYC (LY Compare) (R/W)
    lyc: u8,
    // FF4A - WY (Window Y Position) (R/W)
    wy: u8,
    // FF4B - WX (Window X Position + 7) (R/W)
    wx: u8,
    // FF47 - BGP (BG Palette Data) (R/W)
    bgp: u8,
    // FF48 - OBP0 (OBJ Palette 0 Data) (R/W)
    obp0: u8,
    // FF49 - OBP1 (OBJ Palette 1 Data) (R/W)
    obp1: u8,
}

impl Ppu {
    pub fn new() -> Self {
        Self {
            vram: [0u8; VRAM_SIZE],
            oam: [0u8; OAM_SIZE],
            buf: [255; 4 * WIDTH * HEIGHT],
            ticks: 0,
            tiles: Tiles {
                data: [[[0; 8]; 8]; NUM_TILES],
                to_update: [false; NUM_TILES],
                needs_update: false,
            },
            pal: Palette {
                bgp: [(0, 0, 0); 4],
                sp0: [(0, 0, 0); 4],
                sp1: [(0, 0, 0); 4],
            },
            lcdc: Lcdc::empty(),
            mode: Mode::VBlank,
            lcds: Lcds::empty(),
            scy: 0,
            scx: 0,
            ly: 0,
            lyc: 0,
            wy: 0,
            wx: 0,
            bgp: 0,
            obp0: 0,
            obp1: 0,
        }
    }

    pub fn rb(&self, addr: u16) -> u8 {
        match addr & 0xff {
            0x40 => self.lcdc.bits(),
            0x41 => {
                self.lcds.bits() | (((self.lyc == self.ly) as u8) << 2) | ((self.mode as u8) << 0)
            }
            0x42 => self.scy,
            0x43 => self.scx,
            0x44 => self.ly,
            0x45 => self.lyc,
            // 0x46 write only
            0x47 => self.bgp,
            0x48 => self.obp0,
            0x49 => self.obp1,
            0x4a => self.wy,
            0x4b => self.wx,
            _ => 0xff,
        }
    }

    pub fn wb(&mut self, addr: u16, val: u8) {
        match addr & 0xff {
            0x40 => self.lcdc = Lcdc::from_bits_truncate(val),
            0x41 => self.lcds = Lcds::from_bits_truncate(val),
            0x42 => self.scy = val,
            0x43 => self.scx = val,
            // 0x44 Read only
            0x45 => self.lyc = val,
            // 0x46 oam/dma transfer in mem
            0x47 => {
                self.bgp = val;
                update_palette(&mut self.pal.bgp, val);
            }
            0x48 => {
                self.obp0 = val;
                update_palette(&mut self.pal.sp0, val);
            }
            0x49 => {
                self.obp1 = val;
                update_palette(&mut self.pal.sp1, val);
            }
            0x4a => self.wy = val,
            0x4b => self.wx = val,
            _ => {}
        }
    }

    // Update the given tile
    pub fn update_tile(&mut self, addr: u16) {
        let tile = (addr & 0x1fff) / 16;
        self.tiles.to_update[tile as usize] = true;
        self.tiles.needs_update = true;
    }

    // Update the tileset
    fn update_tileset(&mut self) {
        self.tiles.needs_update = false;
        let iter = self.tiles.to_update.iter_mut();
        for (tile, b) in iter.enumerate().filter(|&(_, &mut i)| i) {
            *b = false;
            for i in 0..8 {
                let addr = 16 * tile + 2 * i;
                let (mut lo, mut hi) = (self.vram[addr], self.vram[addr + 1]);
                for j in (0..8).rev() {
                    let v = ((hi & 1) << 1) | (lo & 1);
                    self.tiles.data[tile][i][j] = v;
                    lo >>= 1;
                    hi >>= 1;
                }
            }
        }
    }
}

fn update_palette(pal: &mut [Color; 4], val: u8) {
    pal[0] = PALETTE[((val >> 0) & 0x3) as usize];
    pal[1] = PALETTE[((val >> 2) & 0x3) as usize];
    pal[2] = PALETTE[((val >> 4) & 0x3) as usize];
    pal[3] = PALETTE[((val >> 6) & 0x3) as usize];
}
