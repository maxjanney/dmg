const VRAM_SIZE: usize = 0x2000;
const OAM_SIZE: usize = 0xa0;

type Color = [u8; 4];

const PALETTE: [Color; 4] = [
    // White
    [255, 255, 255, 255],
    // Light gray
    [211, 211, 211, 255],
    // Dark gray
    [169, 169, 169, 255],
    // Black
    [0, 0, 0, 255],
];

struct Palette {
    // Background and window tiles
    bgp: [Color; 4],
    // Sprite palette 0
    sp0: [Color; 4],
    // Sprite palette 1
    sp1: [Color; 4],
}

#[derive(Copy, Clone, Debug)]
enum Mode {
    HBlank,
    VBlank,
    Searching,
    Transferring,
}

pub struct Ppu {
    pub vram: [u8; VRAM_SIZE],
    pub oam: [u8; OAM_SIZE],
    pal: Palette,
    // LCD Control at 0xff40
    // LCD and PPU enable
    lcd_enabled: bool,
    // Window tile map area
    // 0=9800-9BFF, 1=9C00-9FFF
    win_map: bool,
    // Window enable
    win_enabled: bool,
    // BG and Window tile data area
    // 0=8800-97FF, 1=8000-8FFF
    tile_dat: bool,
    // BG tile map are
    // 0=9800-9BFF, 1=9C00-9FFF
    bg_map: bool,
    // OBJ size
    // 0=8x8, 1=8x16
    obj_size: bool,
    // OBJ enable
    obj_enabled: bool,
    // BG and Window enable/priority
    bg_win_enabled: bool,
    // LCD Status register at 0xff41
    // Mode flag
    mode: Mode,
    // Mode 0 HBlank STAT Interrupt source
    mode0int: bool,
    // Mode 1 VBlank STAT Interrupt source
    mode1int: bool,
    // Mode 2 OAM STAT Interrupt source
    mode2int: bool,
    // LYC=LY STAT Interrupt source
    lycint: bool,
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
            pal: Palette {
                bgp: [[0; 4]; 4],
                sp0: [[0; 4]; 4],
                sp1: [[0; 4]; 4],
            },
            lcd_enabled: false,
            win_map: false,
            win_enabled: false,
            tile_dat: false,
            bg_map: false,
            obj_size: false,
            obj_enabled: false,
            bg_win_enabled: false,
            mode: Mode::VBlank,
            mode0int: false,
            mode1int: false,
            mode2int: false,
            lycint: false,
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
            0x40 => {
                ((self.lcd_enabled as u8) << 7)
                    | ((self.win_map as u8) << 6)
                    | ((self.win_enabled as u8) << 5)
                    | ((self.tile_dat as u8) << 4)
                    | ((self.bg_map as u8) << 3)
                    | ((self.obj_size as u8) << 2)
                    | ((self.obj_enabled as u8) << 1)
                    | ((self.bg_win_enabled as u8) << 0)
            }
            0x41 => {
                ((self.lycint as u8) << 6)
                    | ((self.mode2int as u8) << 5)
                    | ((self.mode1int as u8) << 4)
                    | ((self.mode0int as u8) << 3)
                    | (((self.lyc == self.ly) as u8) << 2)
                    | ((self.mode as u8) << 0)
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
            0x40 => {
                self.lcd_enabled = (val >> 7) & 1 != 0;
                self.win_map = (val >> 6) & 1 != 0;
                self.win_enabled = (val >> 5) & 1 != 0;
                self.tile_dat = (val >> 4) & 1 != 0;
                self.bg_map = (val >> 3) & 1 != 0;
                self.obj_size = (val >> 2) & 1 != 0;
                self.obj_enabled = (val >> 1) & 1 != 0;
                self.bg_win_enabled = (val >> 0) & 1 != 0;
            }
            0x41 => {
                self.lycint = (val >> 6) & 1 != 0;
                self.mode2int = (val >> 5) & 1 != 0;
                self.mode1int = (val >> 4) & 1 != 0;
                self.mode0int = (val >> 3) & 1 != 0;
                // Rest are read only
            }
            0x42 => self.scy = val,
            0x43 => self.scx = val,
            // 0x44 Read only
            0x45 => self.lyc = val,
            // 0x46 TODO: oam/dma transfer
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
}

fn update_palette(pal: &mut [Color; 4], val: u8) {
    pal[0] = PALETTE[((val >> 0) & 0x3) as usize];
    pal[1] = PALETTE[((val >> 2) & 0x3) as usize];
    pal[2] = PALETTE[((val >> 4) & 0x3) as usize];
    pal[3] = PALETTE[((val >> 6) & 0x3) as usize];
}
