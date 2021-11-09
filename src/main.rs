mod cpu;
mod gb;
mod mem;

use cpu::reg::Registers;

fn main() {
    let r = Registers::new();
    println!("{}", r);
}
