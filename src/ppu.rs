const VRAM_SIZE: usize = 0x2000;
const OAM_SIZE: usize = 0xa0;

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
}
