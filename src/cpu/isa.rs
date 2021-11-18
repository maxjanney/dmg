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
            regs.remove(Flag::N);
            regs.set(Flag::H, (a & 0xf) + (n & 0xf) > 0xf);
            regs.set(Flag::C, (a as u16) + (n as u16) > 0xff);
            regs.a = a.wrapping_add(n);
            regs.set(Flag::Z, regs.a == 0);
            $c
        }};
    }

    macro_rules! adc_a {
        ($n:expr, $c:expr) => {{
            let a = regs.a;
            let n = $n;
            let f = regs.contains(Flag::C) as u8;
            regs.remove(Flag::N);
            regs.set(Flag::H, (a & 0xf) + (n & 0xf) + f > 0xf);
            regs.f
                .set(Flag::C, (a as u16) + (n as u16) + (f as u16) > 0xff);
            regs.a = a.wrapping_add(n).wrapping_add(f);
            regs.set(Flag::Z, regs.a == 0);
            $c
        }};
    }

    macro_rules! sub_a {
        ($n:expr, $c:expr) => {{
            let a = regs.a;
            let n = $n;
            regs.insert(Flag::N);
            regs.set(Flag::H, (a & 0xf) < (n & 0xf));
            regs.set(Flag::C, a < n);
            regs.a = a.wrapping_sub(n);
            regs.set(Flag::Z, regs.a == 0);
            $c
        }};
    }

    macro_rules! sbc_a {
        ($n:expr, $c:expr) => {{
            let a = regs.a;
            let n = $n;
            let f = regs.contains(Flag::C) as u8;
            regs.insert(Flag::N);
            regs.set(Flag::H, (a & 0xf) < (n & 0xf) + f);
            regs.set(Flag::C, (a as u16) < (n as u16) + (f as u16));
            regs.a = a.wrapping_sub(n).wrapping_sub(f);
            regs.set(Flag::Z, regs.a == 0);
            $c
        }};
    }

    macro_rules! and_a {
        ($n:expr, $c:expr) => {{
            regs.a &= $n;
            regs.set(Flag::Z, regs.a == 0);
            regs.remove(Flag::N | Flag::C);
            regs.insert(Flag::H);
            $c
        }};
    }

    macro_rules! or_n {
        ($n:expr, $c:expr) => {{
            regs.a |= $n;
            regs.set(Flag::Z, regs.a == 0);
            regs.remove(Flag::N | Flag::H | Flag::C);
            $c
        }};
    }

    macro_rules! xor_a {
        ($n:expr, $c:expr) => {{
            regs.a ^= $n;
            regs.set(Flag::Z, regs.a == 0);
            regs.remove(Flag::N | Flag::H | Flag::C);
            $c
        }};
    }

    macro_rules! cp_a {
        ($n:expr, $c:expr) => {{
            let n = $n;
            regs.set(Flag::Z, regs.a == n);
            regs.insert(Flag::N);
            regs.set(Flag::C, regs.a < n);
            regs.set(Flag::H, (regs.a & 0xf) < (n & 0xf));
            $c
        }};
    }

    macro_rules! inc_r {
        ($reg:ident) => {{
            regs.$reg = regs.$reg.wrapping_add(1);
            regs.set(Flag::Z, regs.$reg == 0);
            regs.remove(Flag::N);
            regs.set(Flag::H, regs.$reg & 0xf == 0);
            4
        }};
    }

    macro_rules! dec_r {
        ($reg:ident) => {{
            regs.insert(Flag::N);
            regs.set(Flag::H, regs.$reg.trailing_zeros() >= 4);
            regs.$reg = regs.$reg.wrapping_sub(1);
            regs.set(Flag::Z, regs.$reg == 0);
            4
        }};
    }

    macro_rules! add_hl_rr {
        ($n:expr) => {{
            let hl = regs.hl();
            let n = $n;
            regs.remove(Flag::N);
            regs.set(Flag::H, (hl & 0xfff) + (n & 0xfff) > 0xfff);
            regs.set(Flag::C, hl > 0xfff - n);
            let r = hl.wrapping_add(n);
            regs.l = r as u8;
            regs.h = (r >> 8) as u8;
            8
        }};
    }

    macro_rules! inc_rr {
        ($r1:ident, $r2:ident) => {{
            regs.$r2 = regs.$r2.wrapping_add(1);
            if regs.$r2 == 0 {
                regs.$r1 += 1;
            }
            8
        }};
    }

    macro_rules! dec_rr {
        ($r1:ident, $r2:ident) => {{
            regs.$r2 = regs.$r2.wrapping_sub(1);
            if regs.$r2 == 0xff {
                regs.$r1 -= 1;
            }
            8
        }};
    }

    macro_rules! jp_rr {
        () => {{
            regs.pc = mem.rw(regs.pc);
            16
        }};
    }

    macro_rules! jp_cc_nn {
        ($c:expr) => {
            if $c {
                jp_rr!()
            } else {
                regs.pc += 2;
                12
            }
        };
    }

    macro_rules! jr_n {
        () => {{
            let n = mem.rb(regs.bump()) as i8 as i16;
            regs.pc = n.wrapping_add(regs.pc as i16) as u16;
            12
        }};
    }

    macro_rules! jr_cc_n {
        ($c:expr) => {{
            if $c {
                jr_n!()
            } else {
                regs.pc += 2;
                8
            }
        }};
    }

    macro_rules! call_nn {
        () => {{
            regs.sp -= 2;
            mem.ww(regs.sp, regs.pc + 2);
            regs.pc = mem.rw(regs.pc);
            24
        }};
    }

    macro_rules! call_cc_nn {
        ($c:expr) => {{
            if $c {
                call_nn!()
            } else {
                regs.pc += 2;
                12
            }
        }};
    }

    macro_rules! rst_n {
        ($n:expr) => {{
            regs.sp -= 2;
            mem.ww(regs.sp, regs.pc);
            regs.pc = $n;
            16
        }};
    }

    macro_rules! ret {
        () => {{
            regs.pc = mem.rw(regs.pc);
            regs.sp += 2;
            16
        }};
    }

    macro_rules! ret_cc {
        ($c:expr) => {{
            if $c {
                ret!() + 4
            } else {
                8
            }
        }};
    }

    macro_rules! push_rr {
        ($r1:expr, $r2:expr) => {{
            mem.wb(regs.sp - 1, $r1);
            mem.wb(regs.sp - 2, $r2);
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
        0x03 => inc_rr!(b, c), // INC BC
        0x04 => inc_r!(b),     // INC B
        0x05 => dec_r!(b),     // DEC B
        0x06 => ld_rn!(b),     // LD B, n
        0x07 => {
            // RLCA
            let c = (regs.a & 0x80 != 0) as u8;
            regs.a = (regs.a << 1) | c;
            regs.set(Flag::C, c != 0);
            regs.set(Flag::Z, regs.a == 0);
            regs.remove(Flag::N | Flag::H);
            4
        }
        0x08 => {
            // LD
            let n = mem.rw(regs.pc);
            mem.ww(n, regs.sp);
            regs.pc += 2;
            20
        }
        0x09 => add_hl_rr!(regs.bc()), // ADD HL, BC
        0x0a => {
            // LD A, (BC)
            regs.a = mem.rb(regs.bc());
            8
        }
        0x0b => dec_rr!(b, c), // DEC BC
        0x0c => inc_r!(c),     // INC C
        0x0d => dec_r!(c),     // DEC C
        0x0e => ld_rn!(c),     // LD C, n
        0x0f => {
            // RRCA
            let c = regs.a & 0x01;
            regs.a = (regs.a >> 1) | (c << 7);
            regs.set(Flag::C, c != 0);
            regs.set(Flag::Z, regs.a == 0);
            regs.remove(Flag::N | Flag::H);
            4
        }
        0x11 => ld_rr_nn!(d, e), // LD DE, nn
        0x12 => {
            // LD (DE), A
            mem.wb(regs.de(), regs.a);
            8
        }
        0x13 => inc_rr!(d, e), // INC DE
        0x14 => inc_r!(d),     // INC D
        0x15 => dec_r!(d),     // DEC D
        0x16 => ld_rn!(d),     // LD D, n
        0x17 => {
            // RLA
            let c = regs.a & 0x80 != 0;
            regs.a = (regs.a << 1) | (regs.contains(Flag::C) as u8);
            regs.set(Flag::C, c);
            regs.set(Flag::Z, regs.a == 0);
            regs.remove(Flag::N | Flag::H);
            4
        }
        0x18 => jr_n!(),               // JR n
        0x19 => add_hl_rr!(regs.de()), // ADD HL, DE
        0x1a => {
            // LD A,(DE)
            regs.a = mem.rb(regs.de());
            8
        }
        0x1b => dec_rr!(d, e), // DEC DE
        0x1c => inc_r!(e),     // INC E
        0x1d => dec_r!(e),     // DEC E
        0x1e => ld_rn!(e),     // LD E, n
        0x1f => {
            // RRA
            let c = (regs.contains(Flag::C) as u8) << 7;
            let zero_bit = regs.a & 0x1 != 0;
            regs.a = (regs.a >> 1) | c;
            regs.set(Flag::C, zero_bit);
            regs.set(Flag::Z, regs.a == 0);
            regs.remove(Flag::N | Flag::H);
            4
        }
        0x20 => jr_cc_n!(!regs.contains(Flag::Z)), // JR NZ, n
        0x21 => ld_rr_nn!(h, l),                   // LD HL, nn
        0x22 => {
            // LD (HL+), A
            mem.wb(regs.hl(), regs.a);
            regs.inc_hl();
            8
        }
        0x23 => inc_rr!(h, l),                    // INC HL
        0x24 => inc_r!(h),                        // INC H
        0x25 => dec_r!(h),                        // DEC H
        0x26 => ld_rn!(h),                        // LD H ,n
        0x27 => daa(regs),                        // DAA
        0x28 => jr_cc_n!(regs.contains(Flag::Z)), // JR Z, n
        0x29 => add_hl_rr!(regs.hl()),            // ADD HL, HL
        0x2a => {
            // LD A, (HL+)
            regs.a = mem.rb(regs.hl());
            regs.inc_hl();
            8
        }
        0x2b => dec_rr!(h, l), // DEC HL
        0x2c => inc_r!(l),     // INC L
        0x2d => dec_r!(l),     // DEC L
        0x2e => ld_rn!(l),     // LD L, n
        0x2f => {
            // CPL
            regs.a = !regs.a;
            regs.insert(Flag::N | Flag::H);
            4
        }
        0x30 => jr_cc_n!(!regs.contains(Flag::C)), // JR NC, n
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
        0x33 => {
            // INC SP
            regs.sp = regs.sp.wrapping_add(1);
            8
        }
        0x35 => dec_hln(regs, mem),
        0x36 => {
            // LD (HL), n
            mem.wb(regs.hl(), mem.rb(regs.bump()));
            12
        }
        0x37 => {
            // SCF
            regs.remove(Flag::N | Flag::H);
            regs.insert(Flag::C);
            4
        }
        0x38 => jr_cc_n!(regs.contains(Flag::C)), // JR C, n
        0x39 => add_hl_rr!(regs.sp),              // ADD HL, SP
        0x3a => {
            // LDD A, (HL-)
            regs.a = mem.rb(regs.hl());
            regs.dec_hl();
            8
        }
        0x3b => {
            // DEC SP
            regs.sp = regs.sp.wrapping_sub(1);
            8
        }
        0x3c => inc_r!(a), // INC A
        0x3d => dec_r!(a), // DEC A
        0x3e => ld_rn!(a), // LD A, n
        0x3f => {
            // CCF
            regs.toggle(Flag::C);
            regs.remove(Flag::N | Flag::H);
            4
        }
        0x40 => ld_rr!(b, b),                            // LD B, B
        0x41 => ld_rr!(b, c),                            // LD B, C
        0x42 => ld_rr!(b, d),                            // LD B, D
        0x43 => ld_rr!(b, e),                            // LD B, E
        0x44 => ld_rr!(b, h),                            // LD B, H
        0x45 => ld_rr!(b, l),                            // LD B, L
        0x46 => ld_r_hl!(b),                             // LD B, (HL)
        0x47 => ld_rr!(b, a),                            // LD B, A
        0x48 => ld_rr!(c, b),                            // LD C, B
        0x49 => ld_rr!(c, c),                            // LD C, C
        0x4a => ld_rr!(c, d),                            // LD C, D
        0x4b => ld_rr!(c, e),                            // LD C, E
        0x4c => ld_rr!(c, h),                            // LD C, H
        0x4d => ld_rr!(c, l),                            // LD C, L
        0x4e => ld_r_hl!(c),                             // LD C, (HL)
        0x4f => ld_rr!(c, a),                            // LD C, A
        0x50 => ld_rr!(d, b),                            // LD D, B
        0x51 => ld_rr!(d, c),                            // LD D, C
        0x52 => ld_rr!(d, d),                            // LD D, D
        0x53 => ld_rr!(d, e),                            // LD D, E
        0x54 => ld_rr!(d, h),                            // LD D, H
        0x55 => ld_rr!(d, l),                            // LD D, L
        0x56 => ld_r_hl!(d),                             // LD D, (HL)
        0x57 => ld_rr!(d, a),                            // LD D, A
        0x58 => ld_rr!(e, b),                            // LD E, B
        0x59 => ld_rr!(e, c),                            // LD E, C
        0x5a => ld_rr!(e, d),                            // LD E, D
        0x5b => ld_rr!(e, e),                            // LD E, E
        0x5c => ld_rr!(e, h),                            // LD E, H
        0x5d => ld_rr!(e, l),                            // LD E, L
        0x5e => ld_r_hl!(e),                             // LD E, (HL)
        0x5f => ld_rr!(e, a),                            // LD E, A
        0x60 => ld_rr!(h, b),                            // LD H, B
        0x61 => ld_rr!(h, c),                            // LD H, C
        0x62 => ld_rr!(h, d),                            // LD H, D
        0x63 => ld_rr!(h, e),                            // LD H, E
        0x64 => ld_rr!(h, h),                            // LD H, H
        0x65 => ld_rr!(h, l),                            // LD H, L
        0x66 => ld_r_hl!(h),                             // LD H, (HL)
        0x67 => ld_rr!(h, a),                            // LD H, A
        0x68 => ld_rr!(l, b),                            // LD L, B
        0x69 => ld_rr!(l, c),                            // LD L, C
        0x6a => ld_rr!(l, d),                            // LD L, D
        0x6b => ld_rr!(l, e),                            // LD L, E
        0x6c => ld_rr!(l, h),                            // LD L, H
        0x6d => ld_rr!(l, l),                            // LD L, L
        0x6e => ld_r_hl!(l),                             // LD L, (HL)
        0x6f => ld_rr!(l, a),                            // LD L, A
        0x70 => ld_hl_r!(b),                             // LD (HL), B
        0x71 => ld_hl_r!(c),                             // LD (HL), C
        0x72 => ld_hl_r!(d),                             // LD (HL), D
        0x73 => ld_hl_r!(e),                             // LD (HL), E
        0x74 => ld_hl_r!(h),                             // LD (HL), H
        0x75 => ld_hl_r!(l),                             // LD (HL), L
        0x77 => ld_hl_r!(a),                             // LD (HL), A
        0x78 => ld_rr!(a, b),                            // LD A, B
        0x79 => ld_rr!(a, c),                            // LD A, C
        0x7a => ld_rr!(a, d),                            // LD A, D
        0x7b => ld_rr!(a, e),                            // LD A, E
        0x7c => ld_rr!(a, h),                            // LD A, H
        0x7d => ld_rr!(a, l),                            // LD A, L
        0x7e => ld_r_hl!(a),                             // LD A, (HL)
        0x7f => ld_rr!(a, a),                            // LD A, A
        0x80 => add_a!(regs.b, 4),                       // ADD A, B
        0x81 => add_a!(regs.c, 4),                       // ADD A, C
        0x82 => add_a!(regs.d, 4),                       // ADD A, D
        0x83 => add_a!(regs.e, 4),                       // ADD A, E
        0x84 => add_a!(regs.h, 4),                       // ADD A, H
        0x85 => add_a!(regs.l, 4),                       // ADD A, L
        0x86 => add_a!(mem.rb(regs.hl()), 8),            // ADD A, (HL)
        0x87 => add_a!(regs.a, 4),                       // ADD A, A
        0x88 => adc_a!(regs.b, 4),                       // ADC A, B
        0x89 => adc_a!(regs.c, 4),                       // ADC A, C
        0x8a => adc_a!(regs.d, 4),                       // ADC A, D
        0x8b => adc_a!(regs.e, 4),                       // ADC A, E
        0x8c => adc_a!(regs.h, 4),                       // ADC A, H
        0x8d => adc_a!(regs.l, 4),                       // ADC A, L
        0x8e => adc_a!(mem.rb(regs.hl()), 8),            // ADC A, (HL)
        0x8f => adc_a!(regs.a, 4),                       // ADC A, A
        0x90 => sub_a!(regs.b, 4),                       // SUB B
        0x91 => sub_a!(regs.c, 4),                       // SUB C
        0x92 => sub_a!(regs.d, 4),                       // SUB D
        0x93 => sub_a!(regs.e, 4),                       // SUB E
        0x94 => sub_a!(regs.h, 4),                       // SUB H
        0x95 => sub_a!(regs.l, 4),                       // SUB L
        0x96 => sub_a!(mem.rb(regs.hl()), 8),            // SUB (HL)
        0x97 => sub_a!(regs.a, 4),                       // SUB A
        0x98 => sbc_a!(regs.b, 4),                       // SBC A, B
        0x99 => sbc_a!(regs.c, 4),                       // SBC A, C
        0x9a => sbc_a!(regs.d, 4),                       // SBC A, D
        0x9b => sbc_a!(regs.e, 4),                       // SBC A, E
        0x9c => sbc_a!(regs.h, 4),                       // SBC A, H
        0x9d => sbc_a!(regs.l, 4),                       // SBC A, L
        0x9e => sbc_a!(mem.rb(regs.hl()), 8),            // SBC A, (HL)
        0x9f => sbc_a!(regs.a, 4),                       // SBC A, A
        0xa0 => and_a!(regs.b, 4),                       // AND B
        0xa1 => and_a!(regs.c, 4),                       // AND C
        0xa2 => and_a!(regs.d, 4),                       // AND D
        0xa3 => and_a!(regs.e, 4),                       // AND E
        0xa4 => and_a!(regs.h, 4),                       // AND H
        0xa5 => and_a!(regs.l, 4),                       // AND L
        0xa6 => and_a!(mem.rb(regs.hl()), 8),            // AND (HL)
        0xa7 => and_a!(regs.a, 4),                       // AND A
        0xa8 => xor_a!(regs.b, 4),                       // XOR B
        0xa9 => xor_a!(regs.c, 4),                       // XOR C
        0xaa => xor_a!(regs.d, 4),                       // XOR D
        0xab => xor_a!(regs.e, 4),                       // XOR E
        0xac => xor_a!(regs.h, 4),                       // XOR H
        0xad => xor_a!(regs.l, 4),                       // XOR L
        0xae => xor_a!(mem.rb(regs.hl()), 8),            // XOR (HL)
        0xaf => xor_a!(regs.a, 4),                       // XOR A
        0xb0 => or_n!(regs.b, 4),                        // OR B
        0xb1 => or_n!(regs.c, 4),                        // OR C
        0xb2 => or_n!(regs.d, 4),                        // OR D
        0xb3 => or_n!(regs.e, 4),                        // OR E
        0xb4 => or_n!(regs.h, 4),                        // OR H
        0xb5 => or_n!(regs.l, 4),                        // OR L
        0xb6 => or_n!(mem.rb(regs.hl()), 8),             // OR (HL)
        0xb7 => or_n!(regs.a, 4),                        // OR A
        0xb8 => cp_a!(regs.b, 4),                        // CP B
        0xb9 => cp_a!(regs.c, 4),                        // CP C
        0xba => cp_a!(regs.d, 4),                        // CP D
        0xbb => cp_a!(regs.e, 4),                        // CP E
        0xbc => cp_a!(regs.h, 4),                        // CP H
        0xbd => cp_a!(regs.l, 4),                        // CP L
        0xbe => cp_a!(mem.rb(regs.hl()), 8),             // CP (HL)
        0xbf => cp_a!(regs.a, 4),                        // CP A
        0xc0 => ret_cc!(!regs.contains(Flag::Z)),        // RET NZ
        0xc1 => pop_rr!(b, c),                           // POP BC
        0xc2 => jp_cc_nn!(!regs.contains(Flag::Z)),      // JP NZ, nn
        0xc3 => jp_rr!(),                                // JP nn
        0xc4 => call_cc_nn!(!regs.contains(Flag::Z)),    // CALL NZ, nn
        0xc5 => push_rr!(regs.b, regs.c),                // PUSH BC
        0xc6 => add_a!(mem.rb(regs.bump()), 8),          // ADD A, n
        0xc7 => rst_n!(0x00),                            // RST 00h
        0xc8 => ret_cc!(regs.contains(Flag::Z)),         // RET Z
        0xc9 => ret!(),                                  // RET
        0xca => jp_cc_nn!(regs.contains(Flag::Z)),       // JP Z, nn
        0xcb => exec_cb(mem.rb(regs.bump()), regs, mem), // CBh
        0xcc => call_cc_nn!(regs.contains(Flag::Z)),     // CALL Z, nn
        0xcd => call_nn!(),                              // CALL nn
        0xce => add_a!(mem.rb(regs.bump()), 8),          // ADC A, n
        0xcf => rst_n!(0x08),                            // RST 08h
        0xd0 => ret_cc!(!regs.contains(Flag::C)),        // RET NC
        0xd1 => push_rr!(regs.d, regs.e),                // POP DE
        0xd2 => jp_cc_nn!(!regs.contains(Flag::C)),      // JP NC, nn
        0xd4 => call_cc_nn!(!regs.contains(Flag::C)),    // CALL NC, nn
        0xd5 => push_rr!(regs.d, regs.e),                // PUSH DE
        0xd6 => sub_a!(mem.rb(regs.bump()), 8),          // SUB n
        0xd7 => rst_n!(0x10),                            // RST 10h
        0xd8 => ret_cc!(regs.contains(Flag::C)),         // RET C
        0xda => jp_cc_nn!(regs.contains(Flag::C)),       // JP C, nn
        0xdc => call_cc_nn!(regs.contains(Flag::C)),     // CALL C, nn
        0xde => sbc_a!(mem.rb(regs.bump()), 8),          // SBC A, n
        0xdf => rst_n!(0x18),                            // RST 18h
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
        0xe5 => push_rr!(regs.h, regs.l),       // PUSH HL
        0xe6 => and_a!(mem.rb(regs.bump()), 8), // AND n
        0xe7 => rst_n!(0x20),                   // RST 20h
        0xe8 => add_sp_n(regs, mem),            // ADD SP, n
        0xe9 => {
            // JP (HL)
            regs.pc = regs.hl();
            4
        }
        0xea => {
            // LD (nn), A
            mem.wb(mem.rw(regs.pc), regs.a);
            regs.pc += 2;
            16
        }
        0xee => xor_a!(mem.rb(regs.bump()), 8), // XOR n
        0xef => rst_n!(0x28),                   // RST 28h
        0xf0 => {
            // LD A, (n)
            let n = regs.bump();
            regs.a = mem.rb(0xff00 | (mem.rb(n) as u16));
            12
        }
        0xf1 => {
            // POP AF
            regs.a = mem.rb(regs.sp);
            regs.f = unsafe { Flag::from_bits_unchecked(mem.rb(regs.sp + 1)) };
            regs.sp += 2;
            12
        }
        0xf2 => {
            // LD A, (C)
            regs.a = mem.rb(0xff00 | (regs.c as u16));
            8
        }
        0xf5 => push_rr!(regs.a, regs.flag_bits()), // PUSH A, F
        0xf6 => or_n!(mem.rb(regs.bump()), 8),      // OR n
        0xf7 => rst_n!(0x30),                       // RST 30h
        0xf8 => ld_hl_spn(regs, mem),               // LDHL SP,n
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
        0xff => rst_n!(0x38), // RST 38h
        _ => 0,
    }
}

fn exec_cb(ins: u8, regs: &mut Registers, mem: &mut Memory) -> u32 {
    macro_rules! bit {
        ($r:expr, $b:expr, $c:expr) => {{
            let is_set = ($r & (1 << $b)) != 0;
            regs.set(Flag::Z, !is_set);
            regs.remove(Flag::N);
            regs.insert(Flag::H);
            $c
        }};
    }

    match ins {
        0x40 => bit!(regs.b, 0, 8),             // BIT 0, B
        0x41 => bit!(regs.c, 0, 8),             // BIT 0, C
        0x42 => bit!(regs.d, 0, 8),             // BIT 0, D
        0x43 => bit!(regs.e, 0, 8),             // BIT 0, E
        0x44 => bit!(regs.h, 0, 8),             // BIT 0, H
        0x45 => bit!(regs.l, 0, 8),             // BIT 0, L
        0x46 => bit!(mem.rb(regs.hl()), 0, 12), // BIT 0, (HL)
        0x47 => bit!(regs.a, 0, 8),             // BIT 0, A
        0x48 => bit!(regs.b, 1, 8),             // BIT 1, B
        0x49 => bit!(regs.c, 1, 8),             // BIT 1, C
        0x4a => bit!(regs.d, 1, 8),             // BIT 1, D
        0x4b => bit!(regs.e, 1, 8),             // BIT 1, E
        0x4c => bit!(regs.h, 1, 8),             // BIT 1, H
        0x4d => bit!(regs.l, 1, 8),             // BIT 1, L
        0x4e => bit!(mem.rb(regs.hl()), 1, 12), // BIT 1, (HL)
        0x4f => bit!(regs.a, 1, 8),             // BIT 1, A
        0x50 => bit!(regs.b, 2, 8),             // BIT 2, B
        0x51 => bit!(regs.c, 2, 8),             // BIT 2, C
        0x52 => bit!(regs.d, 2, 8),             // BIT 2, D
        0x53 => bit!(regs.e, 2, 8),             // BIT 2, E
        0x54 => bit!(regs.h, 2, 8),             // BIT 2, H
        0x55 => bit!(regs.l, 2, 8),             // BIT 2, L
        0x56 => bit!(mem.rb(regs.hl()), 2, 12), // BIT 2, (HL)
        0x57 => bit!(regs.a, 2, 8),             // BIT 2, A
        0x58 => bit!(regs.b, 3, 8),             // BIT 3, B
        0x59 => bit!(regs.c, 3, 8),             // BIT 3, C
        0x5a => bit!(regs.d, 3, 8),             // BIT 3, D
        0x5b => bit!(regs.e, 3, 8),             // BIT 3, E
        0x5c => bit!(regs.h, 3, 8),             // BIT 3, H
        0x5d => bit!(regs.l, 3, 8),             // BIT 3, L
        0x5e => bit!(mem.rb(regs.hl()), 3, 12), // BIT 3, (HL)
        0x5f => bit!(regs.a, 3, 8),             // BIT 3, A
        0x60 => bit!(regs.b, 4, 8),             // BIT 4, B
        0x61 => bit!(regs.c, 4, 8),             // BIT 4, C
        0x62 => bit!(regs.d, 4, 8),             // BIT 4, D
        0x63 => bit!(regs.e, 4, 8),             // BIT 4, E
        0x64 => bit!(regs.h, 4, 8),             // BIT 4, H
        0x65 => bit!(regs.l, 4, 8),             // BIT 4, L
        0x66 => bit!(mem.rb(regs.hl()), 4, 12), // BIT 4, (HL)
        0x67 => bit!(regs.a, 4, 8),             // BIT 4, A
        0x68 => bit!(regs.b, 5, 8),             // BIT 5, B
        0x69 => bit!(regs.c, 5, 8),             // BIT 5, C
        0x6a => bit!(regs.d, 5, 8),             // BIT 5, D
        0x6b => bit!(regs.e, 5, 8),             // BIT 5, E
        0x6c => bit!(regs.h, 5, 8),             // BIT 5, H
        0x6d => bit!(regs.l, 5, 8),             // BIT 5, L
        0x6e => bit!(mem.rb(regs.hl()), 5, 12), // BIT 5, (HL)
        0x6f => bit!(regs.a, 5, 8),             // BIT 5, A
        0x70 => bit!(regs.b, 6, 8),             // BIT 6, B
        0x71 => bit!(regs.c, 6, 8),             // BIT 6, C
        0x72 => bit!(regs.d, 6, 8),             // BIT 6, D
        0x73 => bit!(regs.e, 6, 8),             // BIT 6, E
        0x74 => bit!(regs.h, 6, 8),             // BIT 6, H
        0x75 => bit!(regs.l, 6, 8),             // BIT 6, L
        0x76 => bit!(mem.rb(regs.hl()), 6, 12), // BIT 6, (HL)
        0x77 => bit!(regs.a, 6, 8),             // BIT 6, A
        0x78 => bit!(regs.b, 7, 8),             // BIT 7, B
        0x79 => bit!(regs.c, 7, 8),             // BIT 7, C
        0x7a => bit!(regs.d, 7, 8),             // BIT 7, D
        0x7b => bit!(regs.e, 7, 8),             // BIT 7, E
        0x7c => bit!(regs.h, 7, 8),             // BIT 7, H
        0x7d => bit!(regs.l, 7, 8),             // BIT 7, L
        0x7e => bit!(mem.rb(regs.hl()), 7, 12), // BIT 7, (HL)
        0x7f => bit!(regs.a, 7, 8),             // BIT 7, A
        _ => 0,
    }
}

fn ld_hl_spn(regs: &mut Registers, mem: &mut Memory) -> u32 {
    // sign extend n
    let n = mem.rb(regs.bump()) as i8 as i16 as u16;
    regs.remove(Flag::Z | Flag::N);
    regs.set(Flag::H, (regs.sp & 0xf) + (n & 0xf) > 0xf);
    regs.set(Flag::C, (regs.sp & 0xff) + (n & 0xff) > 0xff);
    let r = regs.sp.wrapping_add(n);
    regs.l = r as u8;
    regs.h = (r >> 8) as u8;
    12
}

fn add_sp_n(regs: &mut Registers, mem: &mut Memory) -> u32 {
    // sign extend n
    let n = mem.rb(regs.bump()) as i8 as i16 as u16;
    regs.remove(Flag::Z | Flag::N);
    regs.set(Flag::H, (regs.sp & 0xf) + (n & 0xf) > 0xf);
    regs.set(Flag::C, (regs.sp & 0xff) + (n & 0xff) > 0xff);
    regs.sp = regs.sp.wrapping_add(n);
    16
}

fn dec_hln(regs: &mut Registers, mem: &mut Memory) -> u32 {
    let hl = regs.hl();
    let n = mem.rb(hl).wrapping_sub(1);
    mem.wb(hl, n);
    regs.set(Flag::Z, n == 0);
    regs.insert(Flag::N);
    regs.set(Flag::H, n & 0xf == 0xf);
    12
}

fn daa(regs: &mut Registers) -> u32 {
    // WTF is the DAA instruction??
    regs.remove(Flag::H);
    4
}
