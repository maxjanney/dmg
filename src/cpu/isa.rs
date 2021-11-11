use crate::mem::Memory;

use super::reg::Registers;

pub fn exec(ins: u8, regs: &mut Registers, mem: &mut Memory) -> u32 {
    macro_rules! ld_n {
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

    match ins {
        0x00 => 4, // NOP

        0x02 => {
            // LD (BC), A
            mem.wb(regs.bc(), regs.a);
            8
        }
        0x12 => {
            // LD (DE), A
            mem.wb(regs.de(), regs.a);
            8
        }

        0x06 => ld_n!(b), // LD B, n
        0x0e => ld_n!(c), // LD C, n
        0x16 => ld_n!(d), // LD D, n
        0x1e => ld_n!(e), // LD E, n
        0x26 => ld_n!(h), // LD H ,n
        0x2e => ld_n!(l), // LD L, n

        0x7f => ld_rr!(a, a), // LD A, A
        0x78 => ld_rr!(a, b), // LD A, B
        0x79 => ld_rr!(a, c), // LD A, C
        0x7a => ld_rr!(a, d), // LD A, D
        0x7b => ld_rr!(a, e), // LD A, E
        0x7c => ld_rr!(a, h), // LD A, H
        0x7d => ld_rr!(a, l), // LD A, L
        0x7e => ld_r_hl!(a),  // LD A, (HL)
        0x3a => {
            // LDD A, (HL-)
            regs.a = mem.rb(regs.hl());
            regs.dec_hl();
            8
        }
        0x40 => ld_rr!(b, b), // LD B, B
        0x41 => ld_rr!(b, c), // LD B, C
        0x42 => ld_rr!(b, d), // LD B, D
        0x43 => ld_rr!(b, e), // LD B, E
        0x44 => ld_rr!(b, h), // LD B, H
        0x45 => ld_rr!(b, l), // LD B, L
        0x46 => ld_r_hl!(b),  // LD B, (HL)
        0x47 => ld_rr!(b, a), // LD B, A
        0x48 => ld_rr!(c, b), // LD C, B
        0x49 => ld_rr!(c, c), // LD C, C
        0x4a => ld_rr!(c, d), // LD C, D
        0x4b => ld_rr!(c, e), // LD C, E
        0x4c => ld_rr!(c, h), // LD C, H
        0x4d => ld_rr!(c, l), // LD C, L
        0x4e => ld_r_hl!(c),  // LD C, (HL)
        0x4f => ld_rr!(c, a), // LD C, A
        0x50 => ld_rr!(d, b), // LD D, B
        0x51 => ld_rr!(d, c), // LD D, C
        0x52 => ld_rr!(d, d), // LD D, D
        0x53 => ld_rr!(d, e), // LD D, E
        0x54 => ld_rr!(d, h), // LD D, H
        0x55 => ld_rr!(d, l), // LD D, L
        0x56 => ld_r_hl!(d),  // LD D, (HL)
        0x57 => ld_rr!(d, a), // LD D, A
        0x58 => ld_rr!(e, b), // LD E, B
        0x59 => ld_rr!(e, c), // LD E, C
        0x5a => ld_rr!(e, d), // LD E, D
        0x5b => ld_rr!(e, e), // LD E, E
        0x5c => ld_rr!(e, h), // LD E, H
        0x5d => ld_rr!(e, l), // LD E, L
        0x5e => ld_r_hl!(e),  // LD E, (HL)
        0x5f => ld_rr!(e, a), // LD E, A
        0x60 => ld_rr!(h, b), // LD H, B
        0x61 => ld_rr!(h, c), // LD H, C
        0x62 => ld_rr!(h, d), // LD H, D
        0x63 => ld_rr!(h, e), // LD H, E
        0x64 => ld_rr!(h, h), // LD H, H
        0x65 => ld_rr!(h, l), // LD H, L
        0x66 => ld_r_hl!(h),  // LD H, (HL)
        0x67 => ld_rr!(h, a), // LD H, A
        0x68 => ld_rr!(l, b), // LD L, B
        0x69 => ld_rr!(l, c), // LD L, C
        0x6a => ld_rr!(l, d), // LD L, D
        0x6b => ld_rr!(l, e), // LD L, E
        0x6c => ld_rr!(l, h), // LD L, H
        0x6d => ld_rr!(l, l), // LD L, L
        0x6e => ld_r_hl!(l),  // LD L, (HL)
        0x6f => ld_rr!(l, a), // LD L, A
        0x70 => ld_hl_r!(b),  // LD (HL), B
        0x71 => ld_hl_r!(c),  // LD (HL), C
        0x72 => ld_hl_r!(d),  // LD (HL), D
        0x73 => ld_hl_r!(e),  // LD (HL), E
        0x74 => ld_hl_r!(h),  // LD (HL), H
        0x75 => ld_hl_r!(l),  // LD (HL), L
        0x77 => ld_hl_r!(a),  // LD (HL), A
        0x36 => {
            // LD (HL), n
            mem.wb(regs.hl(), mem.rb(regs.bump()));
            12
        }

        0x0a => {
            // LD A, (BC)
            regs.a = mem.rb(regs.bc());
            8
        }
        0x1a => {
            // LD A,(DE)
            regs.a = mem.rb(regs.de());
            8
        }
        0xfa => {
            // LD A, (nn)
            regs.a = mem.rb(mem.rw(regs.pc));
            regs.pc += 2;
            16
        }
        0xe2 => {
            mem.wb(0xff00 | (regs.c as u16), regs.a);
            8
        }
        0xea => {
            // LD (nn), A
            mem.wb(mem.rw(regs.pc), regs.a);
            regs.pc += 2;
            16
        }
        0xf2 => {
            // LD A, (C)
            regs.a = mem.rb(0xff00 | (regs.c as u16));
            8
        }
        _ => 0,
    }
}
