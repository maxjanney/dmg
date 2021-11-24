enum Selection {
    Button,
    Direction,
    Both,
}

pub struct Input {
    buttons: u8,
    directions: u8,
    sel: Selection,
}

impl Input {
    pub fn new() -> Self {
        // 0 = on
        Self {
            buttons: 0xff,
            directions: 0xff,
            sel: Selection::Direction,
        }
    }

    pub fn rb(&self) -> u8 {
        match self.sel {
            Selection::Button => self.buttons,
            Selection::Direction => self.directions,
            Selection::Both => unimplemented!("Not sure what this does"),
        }
    }

    pub fn wb(&mut self, val: u8) {
        match !val & 0x30 {
            0x00 => self.sel = Selection::Both,
            0x10 => self.sel = Selection::Direction,
            0x20 => self.sel = Selection::Button,
            _ => {}
        }
    }
}
