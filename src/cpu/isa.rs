use crate::mem::Memory;

use super::reg::{Flag, Registers};

pub fn exec(ins: u8, regs: &mut Registers, mem: &mut Memory) -> u32 {
    macro_rules! ld_rn {
        ($reg:ident) => {{
            regs.$reg = mem.rb(regs.bump());
            8
        }};
    }

    macro_rules! ld_rr {
        ($r1:ident, $r2:ident) => {{
            regs.$r1 = regs.$r2;
            4
        }};
    }

    macro_rules! ld_rr_nn {
        ($r1:ident, $r2:ident) => {{
            regs.$r2 = mem.rb(regs.bump());
            regs.$r1 = mem.rb(regs.bump());
            12
        }};
    }

    macro_rules! ld_r_hl {
        ($reg:ident) => {{
            regs.$reg = mem.rb(regs.hl());
            8
        }};
    }

    macro_rules! ld_hl_r {
        ($reg:ident) => {{
            mem.wb(regs.hl(), regs.$reg);
            8
        }};
    }

    macro_rules! add_a {
        ($n:expr, $c:expr) => {{
            let a = regs.a;
            let n = $n;
            regs.set_flag(Flag::N, false);
            regs.set_flag(Flag::H, (a & 0xf) + (n & 0xf) > 0xf);
            regs.set_flag(Flag::C, a as u16 + n as u16 > 0xff);
            regs.a = a.wrapping_add(n);
            regs.set_flag(Flag::Z, regs.a == 0);
            $c
        }};
    }

    macro_rules! adc_a {
        ($n:expr, $c:expr) => {{
            let a = regs.a;
            let n = $n;
            let f = regs.get_flag(Flag::C);
            regs.set_flag(Flag::N, false);
            regs.set_flag(Flag::H, (a & 0xf) + (n & 0xf) + f > 0xf);
            regs.set_flag(Flag::C, a as u16 + n as u16 + f as u16 > 0xff);
            regs.a = a.wrapping_add(n).wrapping_add(f);
            regs.set_flag(Flag::Z, regs.a == 0);
            $c
        }};
    }

    macro_rules! push_rr {
        ($r1:ident, $r2:ident) => {{
            mem.wb(regs.sp - 1, regs.$r1);
            mem.wb(regs.sp - 2, regs.$r2);
            regs.sp -= 2;
            16
        }};
    }

    macro_rules! pop_rr {
        ($r1:ident, $r2:ident) => {{
            regs.$r1 = mem.rb(regs.sp);
            regs.$r2 = mem.rb(regs.sp + 1);
            regs.sp += 2;
            12
        }};
    }

    match ins {
        0x00 => 4,               // NOP
        0x01 => ld_rr_nn!(b, c), // LD BC, nn
        0x02 => {
            // LD (BC), A
            mem.wb(regs.bc(), regs.a);
            8
        }
        0x06 => ld_rn!(b), // LD B, n
        0x08 => {
            // LD
            let n = mem.rw(regs.pc);
            mem.ww(n, regs.sp);
            regs.pc += 2;
            20
        }
        0x0a => {
            // LD A, (BC)
            regs.a = mem.rb(regs.bc());
            8
        }
        0x0e => ld_rn!(c),       // LD C, n
        0x11 => ld_rr_nn!(d, e), // LD DE, nn
        0x12 => {
            // LD (DE), A
            mem.wb(regs.de(), regs.a);
            8
        }
        0x16 => ld_rn!(d), // LD D, n
        0x1a => {
            // LD A,(DE)
            regs.a = mem.rb(regs.de());
            8
        }
        0x1e => ld_rn!(e),       // LD E, n
        0x21 => ld_rr_nn!(h, l), // LD HL, nn
        0x22 => {
            // LD (HL+), A
            mem.wb(regs.hl(), regs.a);
            regs.inc_hl();
            8
        }
        0x26 => ld_rn!(h), // LD H ,n
        0x2a => {
            // LD A, (HL+)
            regs.a = mem.rb(regs.hl());
            regs.inc_hl();
            8
        }
        0x2e => ld_rn!(l), // LD L, n
        0x31 => {
            // LD SP, nn
            regs.sp = mem.rw(regs.pc);
            regs.pc += 1;
            12
        }
        0x32 => {
            // LD (HL-), A
            mem.wb(regs.hl(), regs.a);
            regs.dec_hl();
            8
        }
        0x36 => {
            // LD (HL), n
            mem.wb(regs.hl(), mem.rb(regs.bump()));
            12
        }
        0x3a => {
            // LDD A, (HL-)
            regs.a = mem.rb(regs.hl());
            regs.dec_hl();
            8
        }
        0x3e => ld_rn!(a),                      // LD A, n
        0x40 => ld_rr!(b, b),                   // LD B, B
        0x41 => ld_rr!(b, c),                   // LD B, C
        0x42 => ld_rr!(b, d),                   // LD B, D
        0x43 => ld_rr!(b, e),                   // LD B, E
        0x44 => ld_rr!(b, h),                   // LD B, H
        0x45 => ld_rr!(b, l),                   // LD B, L
        0x46 => ld_r_hl!(b),                    // LD B, (HL)
        0x47 => ld_rr!(b, a),                   // LD B, A
        0x48 => ld_rr!(c, b),                   // LD C, B
        0x49 => ld_rr!(c, c),                   // LD C, C
        0x4a => ld_rr!(c, d),                   // LD C, D
        0x4b => ld_rr!(c, e),                   // LD C, E
        0x4c => ld_rr!(c, h),                   // LD C, H
        0x4d => ld_rr!(c, l),                   // LD C, L
        0x4e => ld_r_hl!(c),                    // LD C, (HL)
        0x4f => ld_rr!(c, a),                   // LD C, A
        0x50 => ld_rr!(d, b),                   // LD D, B
        0x51 => ld_rr!(d, c),                   // LD D, C
        0x52 => ld_rr!(d, d),                   // LD D, D
        0x53 => ld_rr!(d, e),                   // LD D, E
        0x54 => ld_rr!(d, h),                   // LD D, H
        0x55 => ld_rr!(d, l),                   // LD D, L
        0x56 => ld_r_hl!(d),                    // LD D, (HL)
        0x57 => ld_rr!(d, a),                   // LD D, A
        0x58 => ld_rr!(e, b),                   // LD E, B
        0x59 => ld_rr!(e, c),                   // LD E, C
        0x5a => ld_rr!(e, d),                   // LD E, D
        0x5b => ld_rr!(e, e),                   // LD E, E
        0x5c => ld_rr!(e, h),                   // LD E, H
        0x5d => ld_rr!(e, l),                   // LD E, L
        0x5e => ld_r_hl!(e),                    // LD E, (HL)
        0x5f => ld_rr!(e, a),                   // LD E, A
        0x60 => ld_rr!(h, b),                   // LD H, B
        0x61 => ld_rr!(h, c),                   // LD H, C
        0x62 => ld_rr!(h, d),                   // LD H, D
        0x63 => ld_rr!(h, e),                   // LD H, E
        0x64 => ld_rr!(h, h),                   // LD H, H
        0x65 => ld_rr!(h, l),                   // LD H, L
        0x66 => ld_r_hl!(h),                    // LD H, (HL)
        0x67 => ld_rr!(h, a),                   // LD H, A
        0x68 => ld_rr!(l, b),                   // LD L, B
        0x69 => ld_rr!(l, c),                   // LD L, C
        0x6a => ld_rr!(l, d),                   // LD L, D
        0x6b => ld_rr!(l, e),                   // LD L, E
        0x6c => ld_rr!(l, h),                   // LD L, H
        0x6d => ld_rr!(l, l),                   // LD L, L
        0x6e => ld_r_hl!(l),                    // LD L, (HL)
        0x6f => ld_rr!(l, a),                   // LD L, A
        0x70 => ld_hl_r!(b),                    // LD (HL), B
        0x71 => ld_hl_r!(c),                    // LD (HL), C
        0x72 => ld_hl_r!(d),                    // LD (HL), D
        0x73 => ld_hl_r!(e),                    // LD (HL), E
        0x74 => ld_hl_r!(h),                    // LD (HL), H
        0x75 => ld_hl_r!(l),                    // LD (HL), L
        0x77 => ld_hl_r!(a),                    // LD (HL), A
        0x78 => ld_rr!(a, b),                   // LD A, B
        0x79 => ld_rr!(a, c),                   // LD A, C
        0x7a => ld_rr!(a, d),                   // LD A, D
        0x7b => ld_rr!(a, e),                   // LD A, E
        0x7c => ld_rr!(a, h),                   // LD A, H
        0x7d => ld_rr!(a, l),                   // LD A, L
        0x7e => ld_r_hl!(a),                    // LD A, (HL)
        0x7f => ld_rr!(a, a),                   // LD A, A
        0x80 => add_a!(regs.b, 4),              // ADD A, B
        0x81 => add_a!(regs.c, 4),              // ADD A, C
        0x82 => add_a!(regs.d, 4),              // ADD A, D
        0x83 => add_a!(regs.e, 4),              // ADD A, E
        0x84 => add_a!(regs.h, 4),              // ADD A, H
        0x85 => add_a!(regs.l, 4),              // ADD A, L
        0x86 => add_a!(mem.rb(regs.hl()), 8),   // ADD A, (HL)
        0x87 => add_a!(regs.a, 4),              // ADD A, A
        0x88 => adc_a!(regs.b, 4),              // ADC A, B
        0x89 => adc_a!(regs.c, 4),              // ADC A, C
        0x8a => adc_a!(regs.d, 4),              // ADC A, D
        0x8b => adc_a!(regs.e, 4),              // ADC A, E
        0x8c => adc_a!(regs.h, 4),              // ADC A, H
        0x8d => adc_a!(regs.l, 4),              // ADC A, L
        0x8e => adc_a!(mem.rb(regs.hl()), 8),   // ADC A, (HL)
        0x8f => adc_a!(regs.a, 4),              // ADC A, A
        0xc1 => pop_rr!(b, c),                  // POP BC
        0xc5 => push_rr!(b, c),                 // PUSH BC
        0xc6 => add_a!(mem.rb(regs.bump()), 8), // ADD A, n
        0xce => add_a!(mem.rb(regs.bump()), 8), // ADC A, n
        0xd1 => push_rr!(d, e),                 // POP DE
        0xd5 => push_rr!(d, e),                 // PUSH DE
        0xe0 => {
            // LD (n), A
            let n = mem.rb(regs.bump()) as u16;
            mem.wb(0xff00 | n, regs.a);
            12
        }
        0xe1 => pop_rr!(h, l), // POP HL
        0xe2 => {
            // LD (FF00+C), A
            mem.wb(0xff00 | (regs.c as u16), regs.a);
            8
        }
        0xe5 => push_rr!(h, l), // PUSH HL
        0xea => {
            // LD (nn), A
            mem.wb(mem.rw(regs.pc), regs.a);
            regs.pc += 2;
            16
        }
        0xf0 => {
            // LD A, (n)
            let n = regs.bump();
            regs.a = mem.rb(0xff00 | (mem.rb(n) as u16));
            12
        }
        0xf1 => pop_rr!(a, f), // POP AF
        0xf2 => {
            // LD A, (C)
            regs.a = mem.rb(0xff00 | (regs.c as u16));
            8
        }
        0xf5 => push_rr!(a, f),       // PUSH AF
        0xf8 => ld_hl_spn(regs, mem), // LDHL SP,n
        0xf9 => {
            // LD SP, HL
            regs.sp = regs.hl();
            8
        }
        0xfa => {
            // LD A, (nn)
            regs.a = mem.rb(mem.rw(regs.pc));
            regs.pc += 2;
            16
        }
        _ => 0,
    }
}

// LDHL HL, SP+i8
fn ld_hl_spn(regs: &mut Registers, mem: &mut Memory) -> u32 {
    // sign extend n
    let n = mem.rb(regs.bump()) as i8 as i16 as u16;
    let res = regs.sp + n;
    regs.h = (res >> 8) as u8;
    regs.l = res as u8;
    // TODO: update flags
    12
}
