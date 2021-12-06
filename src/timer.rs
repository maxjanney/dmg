use crate::gb::Interrupt;

pub struct Timer {
    // Divider register
    pub div: u8,
    // Timer counter
    pub tima: u8,
    // Timer modulo
    pub tma: u8,
    // Timer control
    pub tac: u8,
    // Speed tima is incremented
    speed: u32,
    clock: Clock,
}

struct Clock {
    div: u32,
    tima: u32,
}

impl Timer {
    pub fn new() -> Self {
        Self {
            div: 0,
            tima: 0,
            tma: 0,
            tac: 0,
            speed: 256,
            clock: Clock { div: 0, tima: 0 },
        }
    }

    // Update the speed tima gets incremented
    pub fn update(&mut self, val: u8) {
        self.tac = val;
        self.speed = match val & 0x3 {
            0x0 => 256,
            0x1 => 4,
            0x2 => 16,
            0x3 => 64,
            _ => self.speed,
        };
    }

    // Advance the timer and return whether an interrupt occured
    pub fn step(&mut self, if_: &mut u8, ticks: u32) {
        let ticks = ticks / 4;

        self.clock.div += ticks;
        while self.clock.div >= 64 {
            self.div += 1;
            self.clock.div -= 64;
        }

        if self.timer_enabled() {
            self.clock.tima += ticks;
            // Decrement tima at the specified speed
            while self.clock.tima >= self.speed {
                self.tima = self.tima.wrapping_add(1);
                // When tima overflows, we load the contents of
                // tma into it and request an interrupt
                if self.tima == 0 {
                    self.tima = self.tma;
                    *if_ |= Interrupt::Timer as u8;
                }
                self.clock.tima -= self.speed;
            }
        }
    }

    pub fn timer_enabled(&self) -> bool {
        self.tac & 0x4 != 0
    }
}
