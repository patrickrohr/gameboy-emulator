// Copyright Patrick Rohr, 2023

extern crate num;
#[macro_use]
extern crate num_derive;

use crate::num::FromPrimitive;
use std::env;
use std::fs;
use std::result;

struct MemMapper {
    // TODO: implement memory mapping properly
    game_rom: Vec<u8>,
    ram: [u8; 0x8000],
    // TODO: switchable
    //cartridge_switch: [u8; 0x4000],
    //vram_switch: [u8; 0x2000],
    //ext_ram_switch: [u8; 0x2000],
    //wram: [u8; 0x1000],
    //wram_switch: [u8; 0x1000],
}

impl MemMapper {
    fn new(game_rom: Vec<u8>) -> MemMapper {
        MemMapper {
            game_rom,
            ram: [0; 0x8000],
        }
    }

    fn get(&self, addr: u16) -> u8 {
        if addr < self.game_rom.len() as u16 {
            return *self
                .game_rom
                .get(addr as usize)
                .expect(&format!("address out of range {:#06x}", addr));
        } else if addr >= 0x8000 {
            return self.ram[(addr - 0x8000) as usize];
        } else {
            // consider panicking here...
            return 0;
        }
    }

    fn set(&mut self, addr: u16, val: u8) {
        if addr >= 0x8000 {
            self.ram[(addr - 0x8000) as usize] = val;
        }
        // consider panicking here...
        // do nothing for now.
    }

    fn setw(&mut self, addr: u16, val: u16) {
        let bytes = val.to_le_bytes();
        if addr >= 0x8000 {
            self.ram[(addr - 0x8000) as usize] = bytes[0];
            self.ram[(addr - 0x8000 + 1) as usize] = bytes[1];
        }
        // consider panicking here...
        // do nothing for now.
    }
}

// LOAD
#[derive(Debug)]
enum LoadType {}

// JUMP
#[derive(Debug)]
enum JumpCondition {
    True,
    ZeroFlag,
    NotZeroFlag,
    CarryFlag,
    NotCarryFlag,
}

#[derive(Debug)]
enum JumpTarget {
    Immediate, // u16, absolute
    HL,        // u16, absolute
    Offset,    // i8, relative
}

// Arithmetic / Logical instructions
#[derive(Debug)]
enum Operator {
    ADD,
    ADC,
    SUB,
    SBC,
    AND,
    XOR,
    OR,
    CP,
    INC,
    DEC,
}

#[derive(Debug)]
enum Operand {
    B,
    C,
    D,
    E,
    H,
    L,
    atHL, // (HL)
    A,
    Immediate,
}

// LD8
#[derive(Debug)]
enum Ld8Location {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    atBC,
    atDE,
    atHL,
    atHLInc,
    atHLDec,
    atC,
    Address8,
    Address16,
    Immediate,
}

// LD16
// TODO: should registers all be stored in one enum?
#[derive(Debug)]
enum Ld16Target {
    BC,
    DE,
    HL,
    SP,
    Address, // a16
}

#[derive(Debug)]
enum Ld16Source {
    Immediate, // d16
    SP,
    SPOffset, // SP+s8
    HL,
}

#[derive(Debug)]
enum Instruction {
    NOP,
    LD(LoadType),
    JP(JumpTarget, JumpCondition),
    OP(Operator, Operand),
    CCF, // complement carry flag
    SCF, // set carry flag
    DAA, // decimal adjust accumulator
    CPL, // complement accumulator
    LD8(Ld8Location /* target */, Ld8Location /* source */),
    LD16(Ld16Target, Ld16Source),
}

impl Instruction {
    fn from_opcode(ext_opcode: u16) -> Instruction {
        match ext_opcode {
            // nop
            0x00 => Instruction::NOP,
            // LD BC, d16
            0x01 => Instruction::LD16(Ld16Target::BC, Ld16Source::Immediate),
            // LD (BC), A
            0x02 => Instruction::LD8(Ld8Location::atBC, Ld8Location::A),
            // INC B
            0x04 => Instruction::OP(Operator::INC, Operand::B),
            // DEC B
            0x05 => Instruction::OP(Operator::DEC, Operand::B),
            // LD B, d8
            0x06 => Instruction::LD8(Ld8Location::B, Ld8Location::Immediate),
            // LD (a16), SP
            0x08 => Instruction::LD16(Ld16Target::Address, Ld16Source::SP),
            // LD A, (BC)
            0x0a => Instruction::LD8(Ld8Location::A, Ld8Location::atBC),
            // INC C
            0x0c => Instruction::OP(Operator::INC, Operand::C),
            // DEC C
            0x0d => Instruction::OP(Operator::DEC, Operand::C),
            // LD C, d8
            0x0e => Instruction::LD8(Ld8Location::C, Ld8Location::Immediate),

            // LD DE, d16
            0x11 => Instruction::LD16(Ld16Target::DE, Ld16Source::Immediate),
            // LD (DE), A
            0x12 => Instruction::LD8(Ld8Location::atDE, Ld8Location::A),
            // INC D
            0x14 => Instruction::OP(Operator::INC, Operand::D),
            // DEC D
            0x15 => Instruction::OP(Operator::DEC, Operand::D),
            // LD D, d8
            0x16 => Instruction::LD8(Ld8Location::D, Ld8Location::Immediate),
            // JR i8
            0x18 => Instruction::JP(JumpTarget::Offset, JumpCondition::True),
            // LD A, (DE)
            0x1a => Instruction::LD8(Ld8Location::A, Ld8Location::atDE),
            // INC E
            0x1c => Instruction::OP(Operator::INC, Operand::E),
            // DEC E
            0x1d => Instruction::OP(Operator::DEC, Operand::E),
            // LD E, d8
            0x1e => Instruction::LD8(Ld8Location::E, Ld8Location::Immediate),

            // JR NZ,i8
            0x20 => Instruction::JP(JumpTarget::Offset, JumpCondition::NotZeroFlag),
            // LD HL, d16
            0x21 => Instruction::LD16(Ld16Target::HL, Ld16Source::Immediate),
            // LD (HL+), A
            0x22 => Instruction::LD8(Ld8Location::atHLInc, Ld8Location::A),
            // INC H
            0x24 => Instruction::OP(Operator::INC, Operand::H),
            // DEC H
            0x25 => Instruction::OP(Operator::DEC, Operand::H),
            // LD H, d8
            0x26 => Instruction::LD8(Ld8Location::H, Ld8Location::Immediate),
            // DAA
            0x27 => Instruction::DAA,
            // JR Z,i8
            0x28 => Instruction::JP(JumpTarget::Offset, JumpCondition::ZeroFlag),
            // LD A, (HL+)
            0x2a => Instruction::LD8(Ld8Location::A, Ld8Location::atHLInc),
            // INC L
            0x2c => Instruction::OP(Operator::INC, Operand::L),
            // DEC L
            0x2d => Instruction::OP(Operator::DEC, Operand::L),
            // LD L, d8
            0x2e => Instruction::LD8(Ld8Location::L, Ld8Location::Immediate),
            // CPL
            0x2f => Instruction::CPL,

            // JR NC,i8
            0x30 => Instruction::JP(JumpTarget::Offset, JumpCondition::NotCarryFlag),
            // LD SP, d16
            0x31 => Instruction::LD16(Ld16Target::SP, Ld16Source::Immediate),
            // LD (HL-), A
            0x32 => Instruction::LD8(Ld8Location::atHLDec, Ld8Location::A),
            // INC (HL)
            0x34 => Instruction::OP(Operator::INC, Operand::atHL),
            // DEC (HL)
            0x35 => Instruction::OP(Operator::DEC, Operand::atHL),
            // LD (HL), d8
            0x36 => Instruction::LD8(Ld8Location::atHL, Ld8Location::Immediate),
            // SCF
            0x37 => Instruction::SCF,
            // JR C,i8
            0x38 => Instruction::JP(JumpTarget::Offset, JumpCondition::CarryFlag),
            // LD A, (HL-)
            0x3a => Instruction::LD8(Ld8Location::A, Ld8Location::atHLDec),
            // INC A
            0x3c => Instruction::OP(Operator::INC, Operand::A),
            // DEC A
            0x3d => Instruction::OP(Operator::DEC, Operand::A),
            // LD A, d8
            0x3e => Instruction::LD8(Ld8Location::A, Ld8Location::Immediate),
            // CCF
            0x3f => Instruction::CCF,

            // LD B, B
            0x40 => Instruction::LD8(Ld8Location::B, Ld8Location::B),
            // LD B, C
            0x41 => Instruction::LD8(Ld8Location::B, Ld8Location::C),
            // LD B, D
            0x42 => Instruction::LD8(Ld8Location::B, Ld8Location::D),
            // LD B, E
            0x43 => Instruction::LD8(Ld8Location::B, Ld8Location::E),
            // LD B, H
            0x44 => Instruction::LD8(Ld8Location::B, Ld8Location::H),
            // LD B, L
            0x45 => Instruction::LD8(Ld8Location::B, Ld8Location::L),
            // LD B, (HL)
            0x46 => Instruction::LD8(Ld8Location::B, Ld8Location::atHL),
            // LD B, A
            0x47 => Instruction::LD8(Ld8Location::B, Ld8Location::A),
            // LD C, B
            0x48 => Instruction::LD8(Ld8Location::C, Ld8Location::B),
            // LD C, C
            0x49 => Instruction::LD8(Ld8Location::C, Ld8Location::C),
            // LD C, D
            0x4a => Instruction::LD8(Ld8Location::C, Ld8Location::D),
            // LD C, E
            0x4b => Instruction::LD8(Ld8Location::C, Ld8Location::E),
            // LD C, H
            0x4c => Instruction::LD8(Ld8Location::C, Ld8Location::H),
            // LD C, L
            0x4d => Instruction::LD8(Ld8Location::C, Ld8Location::L),
            // LD C, (HL)
            0x4e => Instruction::LD8(Ld8Location::C, Ld8Location::atHL),
            // LD C, A
            0x4f => Instruction::LD8(Ld8Location::C, Ld8Location::A),

            // LD D, B
            0x50 => Instruction::LD8(Ld8Location::D, Ld8Location::B),
            // LD D, C
            0x51 => Instruction::LD8(Ld8Location::D, Ld8Location::C),
            // LD D, D
            0x52 => Instruction::LD8(Ld8Location::D, Ld8Location::D),
            // LD D, E
            0x53 => Instruction::LD8(Ld8Location::D, Ld8Location::E),
            // LD D, H
            0x54 => Instruction::LD8(Ld8Location::D, Ld8Location::H),
            // LD D, L
            0x55 => Instruction::LD8(Ld8Location::D, Ld8Location::L),
            // LD D, (HL)
            0x56 => Instruction::LD8(Ld8Location::D, Ld8Location::atHL),
            // LD D, A
            0x57 => Instruction::LD8(Ld8Location::D, Ld8Location::A),
            // LD E, B
            0x58 => Instruction::LD8(Ld8Location::E, Ld8Location::B),
            // LD E, C
            0x59 => Instruction::LD8(Ld8Location::E, Ld8Location::C),
            // LD E, D
            0x5a => Instruction::LD8(Ld8Location::E, Ld8Location::D),
            // LD E, E
            0x5b => Instruction::LD8(Ld8Location::E, Ld8Location::E),
            // LD E, H
            0x5c => Instruction::LD8(Ld8Location::E, Ld8Location::H),
            // LD E, L
            0x5d => Instruction::LD8(Ld8Location::E, Ld8Location::L),
            // LD E, (HL)
            0x5e => Instruction::LD8(Ld8Location::E, Ld8Location::atHL),
            // LD E, A
            0x5f => Instruction::LD8(Ld8Location::E, Ld8Location::A),

            // LD H, B
            0x60 => Instruction::LD8(Ld8Location::H, Ld8Location::B),
            // LD H, C
            0x61 => Instruction::LD8(Ld8Location::H, Ld8Location::C),
            // LD H, D
            0x62 => Instruction::LD8(Ld8Location::H, Ld8Location::D),
            // LD H, E
            0x63 => Instruction::LD8(Ld8Location::H, Ld8Location::E),
            // LD H, H
            0x64 => Instruction::LD8(Ld8Location::H, Ld8Location::H),
            // LD H, L
            0x65 => Instruction::LD8(Ld8Location::H, Ld8Location::L),
            // LD H, (HL)
            0x66 => Instruction::LD8(Ld8Location::H, Ld8Location::atHL),
            // LD H, A
            0x67 => Instruction::LD8(Ld8Location::H, Ld8Location::A),
            // LD L, B
            0x68 => Instruction::LD8(Ld8Location::L, Ld8Location::B),
            // LD L, C
            0x69 => Instruction::LD8(Ld8Location::L, Ld8Location::C),
            // LD L, D
            0x6a => Instruction::LD8(Ld8Location::L, Ld8Location::D),
            // LD L, E
            0x6b => Instruction::LD8(Ld8Location::L, Ld8Location::E),
            // LD L, H
            0x6c => Instruction::LD8(Ld8Location::L, Ld8Location::H),
            // LD L, L
            0x6d => Instruction::LD8(Ld8Location::L, Ld8Location::L),
            // LD L, (HL)
            0x6e => Instruction::LD8(Ld8Location::L, Ld8Location::atHL),
            // LD L, A
            0x6f => Instruction::LD8(Ld8Location::L, Ld8Location::A),

            // LD (HL), B
            0x70 => Instruction::LD8(Ld8Location::atHL, Ld8Location::B),
            // LD (HL), C
            0x71 => Instruction::LD8(Ld8Location::atHL, Ld8Location::C),
            // LD (HL), D
            0x72 => Instruction::LD8(Ld8Location::atHL, Ld8Location::D),
            // LD (HL), E
            0x73 => Instruction::LD8(Ld8Location::atHL, Ld8Location::E),
            // LD (HL), H
            0x74 => Instruction::LD8(Ld8Location::atHL, Ld8Location::H),
            // LD (HL), L
            0x75 => Instruction::LD8(Ld8Location::atHL, Ld8Location::L),
            // HALT
            // 0x76 => Instruction::HALT,
            // LD (HL), A
            0x77 => Instruction::LD8(Ld8Location::atHL, Ld8Location::A),
            // LD A, B
            0x78 => Instruction::LD8(Ld8Location::A, Ld8Location::B),
            // LD A, C
            0x79 => Instruction::LD8(Ld8Location::A, Ld8Location::C),
            // LD A, D
            0x7a => Instruction::LD8(Ld8Location::A, Ld8Location::D),
            // LD A, E
            0x7b => Instruction::LD8(Ld8Location::A, Ld8Location::E),
            // LD A, H
            0x7c => Instruction::LD8(Ld8Location::A, Ld8Location::H),
            // LD A, L
            0x7d => Instruction::LD8(Ld8Location::A, Ld8Location::L),
            // LD A, (HL)
            0x7e => Instruction::LD8(Ld8Location::A, Ld8Location::atHL),
            // LD A, A
            0x7f => Instruction::LD8(Ld8Location::A, Ld8Location::A),

            // ADD A, B
            0x80 => Instruction::OP(Operator::ADD, Operand::B),
            // ADD A, C
            0x81 => Instruction::OP(Operator::ADD, Operand::C),
            // ADD A, D
            0x82 => Instruction::OP(Operator::ADD, Operand::D),
            // ADD A, E
            0x83 => Instruction::OP(Operator::ADD, Operand::E),
            // ADD A, H
            0x84 => Instruction::OP(Operator::ADD, Operand::H),
            // ADD A, L
            0x85 => Instruction::OP(Operator::ADD, Operand::L),
            // ADD A, (HL)
            0x86 => Instruction::OP(Operator::ADD, Operand::atHL),
            // ADD A, A
            0x87 => Instruction::OP(Operator::ADD, Operand::A),
            // ADC A, B
            0x88 => Instruction::OP(Operator::ADC, Operand::B),
            // ADC A, C
            0x89 => Instruction::OP(Operator::ADC, Operand::C),
            // ADC A, D
            0x8a => Instruction::OP(Operator::ADC, Operand::D),
            // ADC A, E
            0x8b => Instruction::OP(Operator::ADC, Operand::E),
            // ADC A, H
            0x8c => Instruction::OP(Operator::ADC, Operand::H),
            // ADC A, L
            0x8d => Instruction::OP(Operator::ADC, Operand::L),
            // ADC A, (HL)
            0x8e => Instruction::OP(Operator::ADC, Operand::atHL),
            // ADC A, A
            0x8f => Instruction::OP(Operator::ADC, Operand::A),

            // SUB A, B
            0x90 => Instruction::OP(Operator::SUB, Operand::B),
            // SUB A, C
            0x91 => Instruction::OP(Operator::SUB, Operand::C),
            // SUB A, D
            0x92 => Instruction::OP(Operator::SUB, Operand::D),
            // SUB A, E
            0x93 => Instruction::OP(Operator::SUB, Operand::E),
            // SUB A, H
            0x94 => Instruction::OP(Operator::SUB, Operand::H),
            // SUB A, L
            0x95 => Instruction::OP(Operator::SUB, Operand::L),
            // SUB A, (HL)
            0x96 => Instruction::OP(Operator::SUB, Operand::atHL),
            // SUB A, A
            0x97 => Instruction::OP(Operator::SUB, Operand::A),
            // SBC A, B
            0x98 => Instruction::OP(Operator::SBC, Operand::B),
            // SBC A, C
            0x99 => Instruction::OP(Operator::SBC, Operand::C),
            // SBC A, D
            0x9a => Instruction::OP(Operator::SBC, Operand::D),
            // SBC A, E
            0x9b => Instruction::OP(Operator::SBC, Operand::E),
            // SBC A, H
            0x9c => Instruction::OP(Operator::SBC, Operand::H),
            // SBC A, L
            0x9d => Instruction::OP(Operator::SBC, Operand::L),
            // SBC A, (HL)
            0x9e => Instruction::OP(Operator::SBC, Operand::atHL),
            // SBC A, A
            0x9f => Instruction::OP(Operator::SBC, Operand::A),

            // AND A, B
            0xa0 => Instruction::OP(Operator::AND, Operand::B),
            // AND A, C
            0xa1 => Instruction::OP(Operator::AND, Operand::C),
            // AND A, D
            0xa2 => Instruction::OP(Operator::AND, Operand::D),
            // AND A, E
            0xa3 => Instruction::OP(Operator::AND, Operand::E),
            // AND A, H
            0xa4 => Instruction::OP(Operator::AND, Operand::H),
            // AND A, L
            0xa5 => Instruction::OP(Operator::AND, Operand::L),
            // AND A, (HL)
            0xa6 => Instruction::OP(Operator::AND, Operand::atHL),
            // AND A, A
            0xa7 => Instruction::OP(Operator::AND, Operand::A),
            // XOR B
            0xa8 => Instruction::OP(Operator::XOR, Operand::B),
            // XOR C
            0xa9 => Instruction::OP(Operator::XOR, Operand::C),
            // XOR D
            0xaa => Instruction::OP(Operator::XOR, Operand::D),
            // XOR E
            0xab => Instruction::OP(Operator::XOR, Operand::E),
            // XOR H
            0xac => Instruction::OP(Operator::XOR, Operand::H),
            // XOR L
            0xad => Instruction::OP(Operator::XOR, Operand::L),
            // XOR (HL)
            0xae => Instruction::OP(Operator::XOR, Operand::atHL),
            // XOR A
            0xaf => Instruction::OP(Operator::XOR, Operand::A),

            // OR A, B
            0xb0 => Instruction::OP(Operator::OR, Operand::B),
            // OR A, C
            0xb1 => Instruction::OP(Operator::OR, Operand::C),
            // OR A, D
            0xb2 => Instruction::OP(Operator::OR, Operand::D),
            // OR A, E
            0xb3 => Instruction::OP(Operator::OR, Operand::E),
            // OR A, H
            0xb4 => Instruction::OP(Operator::OR, Operand::H),
            // OR A, L
            0xb5 => Instruction::OP(Operator::OR, Operand::L),
            // OR A, (HL)
            0xb6 => Instruction::OP(Operator::OR, Operand::atHL),
            // OR A, A
            0xb7 => Instruction::OP(Operator::OR, Operand::A),
            // CP A, B
            0xb8 => Instruction::OP(Operator::CP, Operand::B),
            // CP A, C
            0xb9 => Instruction::OP(Operator::CP, Operand::C),
            // CP A, D
            0xba => Instruction::OP(Operator::CP, Operand::D),
            // CP A, E
            0xbb => Instruction::OP(Operator::CP, Operand::E),
            // CP A, H
            0xbc => Instruction::OP(Operator::CP, Operand::H),
            // CP A, L
            0xbd => Instruction::OP(Operator::CP, Operand::L),
            // CP A, (HL)
            0xbe => Instruction::OP(Operator::CP, Operand::atHL),
            // CP A, A
            0xbf => Instruction::OP(Operator::CP, Operand::A),

            // JP NZ,a16
            0xc2 => Instruction::JP(JumpTarget::Immediate, JumpCondition::NotZeroFlag),
            // JP a16
            0xc3 => Instruction::JP(JumpTarget::Immediate, JumpCondition::True),
            // ADD A, d8
            0xc6 => Instruction::OP(Operator::ADD, Operand::Immediate),
            // JP Z,a16
            0xca => Instruction::JP(JumpTarget::Immediate, JumpCondition::ZeroFlag),
            // ADC A, d8
            0xce => Instruction::OP(Operator::ADC, Operand::Immediate),

            // JP NC,a16
            0xd2 => Instruction::JP(JumpTarget::Immediate, JumpCondition::NotCarryFlag),
            // SUB A, d8
            0xd6 => Instruction::OP(Operator::SUB, Operand::Immediate),
            // JP C,a16
            0xda => Instruction::JP(JumpTarget::Immediate, JumpCondition::CarryFlag),
            // SBC A, d8
            0xde => Instruction::OP(Operator::SBC, Operand::Immediate),

            // LD (a8), A
            0xe0 => Instruction::LD8(Ld8Location::Address8, Ld8Location::A),
            // LD (C), A
            0xe2 => Instruction::LD8(Ld8Location::atC, Ld8Location::A),
            // AND A, d8
            0xe6 => Instruction::OP(Operator::AND, Operand::Immediate),
            // JP HL
            0xe9 => Instruction::JP(JumpTarget::HL, JumpCondition::True),
            // LD (a16), A
            0xea => Instruction::LD8(Ld8Location::Address16, Ld8Location::A),
            // XOR d8
            0xee => Instruction::OP(Operator::XOR, Operand::Immediate),

            // LD A, (a8)
            0xf0 => Instruction::LD8(Ld8Location::A, Ld8Location::Address8),
            // LD A, (C)
            0xf2 => Instruction::LD8(Ld8Location::A, Ld8Location::atC),
            // OR A, d8
            0xf6 => Instruction::OP(Operator::OR, Operand::Immediate),
            // LD HL, SP+s8
            0xf8 => Instruction::LD16(Ld16Target::HL, Ld16Source::SPOffset),
            // LD SP, HL
            0xf9 => Instruction::LD16(Ld16Target::SP, Ld16Source::HL),
            // LD A, (a16)
            0xfa => Instruction::LD8(Ld8Location::A, Ld8Location::Address16),
            // CP A, d8
            0xfe => Instruction::OP(Operator::CP, Operand::Immediate),

            _ => panic!("not implemented opcode {:#06x}", ext_opcode),
        }
    }
}

struct Cpu {
    // On little-endian architecture, [0] is the low order byte, while [1] is the high order byte.
    // e.g. for register AF: A@[1], F@[C].
    reg_af: [u8; 2],
    reg_bc: [u8; 2],
    reg_de: [u8; 2],
    reg_hl: [u8; 2],
    reg_sp: [u8; 2],
    reg_pc: [u8; 2],
    mem: MemMapper,

    cycles: usize,
}

impl Cpu {
    fn new(mem: MemMapper) -> Cpu {
        let mut cpu = Cpu {
            reg_af: [0, 0],
            reg_bc: [0, 0],
            reg_de: [0, 0],
            reg_hl: [0, 0],
            reg_sp: [0, 0],
            // Program counter is initialized at 0x100.
            reg_pc: u16::to_le_bytes(0x100),
            mem,
            cycles: 0,
        };
        return cpu;
    }

    fn get_a(&self) -> u8 {
        self.reg_af[1]
    }

    fn get_b(&self) -> u8 {
        self.reg_bc[1]
    }

    fn get_c(&self) -> u8 {
        self.reg_bc[0]
    }

    fn get_d(&self) -> u8 {
        self.reg_de[1]
    }

    fn get_e(&self) -> u8 {
        self.reg_de[0]
    }

    fn get_h(&self) -> u8 {
        self.reg_hl[1]
    }

    fn get_l(&self) -> u8 {
        self.reg_hl[0]
    }

    fn get_af(&self) -> u16 {
        u16::from_le_bytes(self.reg_af)
    }

    fn get_bc(&self) -> u16 {
        u16::from_le_bytes(self.reg_bc)
    }

    fn get_de(&self) -> u16 {
        u16::from_le_bytes(self.reg_de)
    }

    fn get_hl(&self) -> u16 {
        u16::from_le_bytes(self.reg_hl)
    }

    fn get_sp(&self) -> u16 {
        u16::from_le_bytes(self.reg_sp)
    }

    fn get_pc(&self) -> u16 {
        u16::from_le_bytes(self.reg_pc)
    }

    fn is_z_flag(&self) -> bool {
        return self.reg_af[0] & 0x80 == 0x80;
    }

    fn is_n_flag(&self) -> bool {
        return self.reg_af[0] & 0x40 == 0x40;
    }

    fn is_h_flag(&self) -> bool {
        return self.reg_af[0] & 0x20 == 0x20;
    }

    fn is_c_flag(&self) -> bool {
        return self.reg_af[0] & 0x10 == 0x10;
    }

    fn set_bits(&mut self, set: bool, mask: u8) {
        let mut reg = self.reg_af[0];
        if (set) {
            reg |= mask;
        } else {
            reg &= !mask;
        }
        self.reg_af[0] = reg;
    }

    fn set_z_flag(&mut self, set: bool) {
        self.set_bits(set, 0x80);
    }

    fn set_n_flag(&mut self, set: bool) {
        self.set_bits(set, 0x40);
    }

    fn set_h_flag(&mut self, set: bool) {
        self.set_bits(set, 0x20);
    }

    fn set_c_flag(&mut self, set: bool) {
        self.set_bits(set, 0x10);
    }

    fn set_a(&mut self, val: u8) {
        self.reg_af[1] = val;
    }

    fn set_b(&mut self, val: u8) {
        self.reg_bc[1] = val;
    }

    fn set_c(&mut self, val: u8) {
        self.reg_bc[0] = val;
    }

    fn set_d(&mut self, val: u8) {
        self.reg_de[1] = val;
    }

    fn set_e(&mut self, val: u8) {
        self.reg_de[0] = val;
    }

    fn set_h(&mut self, val: u8) {
        self.reg_hl[1] = val;
    }

    fn set_l(&mut self, val: u8) {
        self.reg_hl[0] = val;
    }

    fn set_af(&mut self, val: u16) {
        self.reg_af = u16::to_le_bytes(val);
    }

    fn set_bc(&mut self, val: u16) {
        self.reg_bc = u16::to_le_bytes(val);
    }

    fn set_de(&mut self, val: u16) {
        self.reg_de = u16::to_le_bytes(val);
    }

    fn set_hl(&mut self, val: u16) {
        self.reg_hl = u16::to_le_bytes(val);
    }

    fn set_sp(&mut self, val: u16) {
        self.reg_sp = u16::to_le_bytes(val);
    }

    fn set_pc(&mut self, val: u16) {
        self.reg_pc = u16::to_le_bytes(val);
    }

    fn cycle(&mut self) {
        self.cycles += 1;
    }

    fn pc_fetch(&mut self) -> u8 {
        self.cycle();

        let pc = self.get_pc();
        let byte = self.mem.get(pc);
        // Increment program counter
        self.set_pc(pc + 1);
        return byte;
    }

    fn jump(&mut self, addr: u16, conditionMet: bool) {
        if (conditionMet) {
            self.set_pc(addr);
        }
    }

    fn op_add(&mut self, val: u8, carry: Option<u8>) {
        let carry = match carry {
            Some(c) => c,
            None => 0,
        };

        let a = self.get_a(); 
        let (result, c1) = a.overflowing_add(val);
        let (result, c2) = result.overflowing_add(carry);
        let c = c1 || c2;
        // half-carry bit
        let h = ((a & 0x0f) + (val & 0x0f) + (carry & 0x0f)) & 0x10 == 0x10;

        self.set_z_flag(result == 0);
        self.set_n_flag(false);
        self.set_h_flag(h);
        self.set_c_flag(c);

        self.set_a(result);
    }

    fn op_sub(&mut self, val: u8, carry: Option<u8>) {
        let carry = match carry {
            Some(c) => c,
            None => 0,
        };

        let a = self.get_a(); 
        let (result, c1) = a.overflowing_sub(val);
        let (result, c2) = result.overflowing_sub(carry);
        let c = c1 || c2;
        // half-carry bit
        let h = ((a & 0x0f) - (val & 0x0f) - (carry & 0x0f)) & 0x10 == 0x10;

        self.set_z_flag(result == 0);
        self.set_n_flag(true);
        self.set_h_flag(h);
        self.set_c_flag(c);

        self.set_a(result);
    }

    fn op_cp(&mut self, val: u8) {
        // almost identical to SUB, but does not update register A.
        let a = self.get_a();
        self.op_sub(val, None);
        self.set_a(a);
    }

    fn op_incdec(&mut self, val: u8, operand: Operand, inc: bool) {
        let result = if inc {
            val.wrapping_add(1)
        } else {
            val.wrapping_sub(1)
        };
        let h = if inc {
            ((0x0f & val) + 1) & 0x10 == 0x10
        } else {
            ((0x0f & val) - 1) & 0x10 == 0x10
        };

        self.set_z_flag(result == 0);
        self.set_n_flag(!inc); // inc: false, dec: true
        self.set_h_flag(h);

        match(operand) {
            Operand::B => self.set_b(result),
            Operand::C => self.set_c(result),
            Operand::D => self.set_d(result),
            Operand::E => self.set_e(result),
            Operand::H => self.set_h(result),
            Operand::L => self.set_l(result),
            Operand::atHL => {
                let addr = self.get_hl();
                // TODO: should cycle() be part of mem.get()?
                self.cycle();
                self.mem.set(addr, result);
            }
            Operand::A => self.set_a(result),
            Operand::Immediate => panic!("INC/DEC do not operate on immediate"),
        };
    }

    fn op_daa(&mut self) {
        let mut a = self.get_a();
        if self.is_n_flag() { // after subtraction
            if self.is_c_flag() { // top nibble
                a -= 0x60;
            }
            if self.is_h_flag() { // bottom nibble
                a -= 0x06;
            }
        } else { // after addition
            if self.is_c_flag() || a > 0x99 { // top nibble
                a += 0x60;
                self.set_c_flag(true);
            }
            if self.is_h_flag() || (a & 0x0f) > 0x09 { // bottom nibble
                a += 0x06;
            }
        }
        self.set_a(a);
    }

    // returns cycles
    fn step(&mut self) -> Result<(), ()> {
        // snapshot pc for logging below.
        let pc = self.get_pc();
        let opcode = self.pc_fetch();

        // Extended opcodes start with 0xCB
        let mut ext_opcode: u16 = opcode as u16;
        if (opcode == 0xCB) {
            ext_opcode = u16::from_le_bytes([opcode, self.pc_fetch()]);
        }

        let instruction = Instruction::from_opcode(ext_opcode);
        println!(
            "Execute opcode {:#06x} at {:#04x}: {:?}",
            ext_opcode, pc, instruction
        );
        match instruction {
            // NOP
            Instruction::NOP => (/* do nothing */),
            // JP
            Instruction::JP(target, condition) => {
                let addr: u16 = match target {
                    JumpTarget::Immediate => {
                        // JP nn takes 4 cycles but only performs 3 pc_fetch'es.
                        // TODO: could this be a function? Does using a 2 byte immediate always
                        // cost 3 machine cycles?
                        self.cycle();
                        u16::from_le_bytes([self.pc_fetch(), self.pc_fetch()])
                    }
                    JumpTarget::HL => self.get_hl(),
                    // read pc_fetch (PC++) before PC value.
                    JumpTarget::Offset => {
                        // JR e takes 3 cycles but only performs 2 pc_fetch'es.
                        self.cycle();
                        // order matters.
                        let e = (self.pc_fetch() as i8) as i32;
                        let pc = self.get_pc() as i32;
                        (pc + e) as u16
                    }
                };
                let conditionMet: bool = match condition {
                    JumpCondition::True => true,
                    // TODO!
                    _ => false,
                };
                self.jump(addr, conditionMet);
            },
            Instruction::OP(operator, operand) => {
                let op_val = match operand {
                    Operand::B => self.get_b(),
                    Operand::C => self.get_c(),
                    Operand::D => self.get_d(),
                    Operand::E => self.get_e(),
                    Operand::H => self.get_h(),
                    Operand::L => self.get_l(),
                    Operand::atHL => {
                        let addr = self.get_hl();
                        // TODO: should cycle() be part of mem.get()?
                        self.cycle();
                        self.mem.get(addr)
                    }
                    Operand::A => self.get_a(),
                    Operand::Immediate => self.pc_fetch(),
                };

                let result = match operator {
                    Operator::ADD => self.op_add(op_val, None),
                    Operator::ADC => self.op_add(op_val, Some(self.is_c_flag().into())),
                    Operator::SUB => self.op_sub(op_val, None),
                    Operator::SBC => self.op_sub(op_val, Some(self.is_c_flag().into())),
                    Operator::AND => {
                        let result = self.get_a() & op_val;
                        self.set_z_flag(result == 0);
                        self.set_n_flag(false);
                        self.set_h_flag(true);
                        self.set_c_flag(false);
                        self.set_a(result);
                    },
                    Operator::OR => {
                        let result = self.get_a() | op_val;
                        self.set_z_flag(result == 0);
                        self.set_n_flag(false);
                        self.set_h_flag(false);
                        self.set_c_flag(false);
                        self.set_a(result);
                    }
                    Operator::XOR => {
                        let result = self.get_a() ^ op_val;
                        self.set_z_flag(result == 0);
                        self.set_n_flag(false);
                        self.set_h_flag(false);
                        self.set_c_flag(false);
                        self.set_a(result);
                    }
                    Operator::CP => self.op_cp(op_val),
                    Operator::INC => self.op_incdec(op_val, operand, true /* inc */),
                    Operator::DEC => self.op_incdec(op_val, operand, false /* inc */),
                };
            }
            Instruction::CCF => {
                self.set_n_flag(false);
                self.set_h_flag(false);
                self.set_c_flag(!self.is_c_flag());
            }
            Instruction::SCF => {
                self.set_n_flag(false);
                self.set_h_flag(false);
                self.set_c_flag(true);
            }
            Instruction::SCF => {
                self.set_n_flag(false);
                self.set_h_flag(false);
                self.set_c_flag(true);
            }
            Instruction::DAA => self.op_daa(),
            Instruction::CPL => {
                let a = self.get_a();
                self.set_a(!a);
                self.set_n_flag(true);
                self.set_h_flag(true);
            }
            Instruction::LD16(target, source) => {
                let val = match source {
                    Ld16Source::Immediate => u16::from_le_bytes([self.pc_fetch(), self.pc_fetch()]),
                    Ld16Source::SP => self.get_sp(),
                    Ld16Source::SPOffset => {
                        // SP+s8
                        // Cast to signed int, perform sign-extension, then cast back to uint.
                        let offset = self.pc_fetch() as i8 as i16 as u16;
                        self.get_sp().wrapping_add(offset)
                    }
                    Ld16Source::HL => self.get_hl(),
                };

                match target {
                    Ld16Target::HL => self.set_hl(val),
                    Ld16Target::BC => self.set_bc(val),
                    Ld16Target::DE => self.set_de(val),
                    Ld16Target::HL => self.set_hl(val),
                    Ld16Target::SP => self.set_pc(val),
                    Ld16Target::Address => {
                        // only supported for LD (a16), SP, so it is safe to fetch next byte.
                        let addr = u16::from_le_bytes([self.pc_fetch(), self.pc_fetch()]);
                        self.cycle(); // ?
                        self.mem.setw(addr, val);
                    } // a16
                };
            }
            Instruction::LD8(target, source) => {
                // TODO: fix cycles for memory accesses.
                // Only either source or target can contain an immediate / address value.
                // Therefore, it is safe to process source before target.
                let val = match source {
                    Ld8Location::A => self.get_a(),
                    Ld8Location::B => self.get_b(),
                    Ld8Location::C => self.get_c(),
                    Ld8Location::D => self.get_d(),
                    Ld8Location::E => self.get_e(),
                    Ld8Location::H => self.get_h(),
                    Ld8Location::L => self.get_l(),
                    Ld8Location::atBC => self.mem.get(self.get_bc()),
                    Ld8Location::atDE => self.mem.get(self.get_de()),
                    Ld8Location::atHL => self.mem.get(self.get_hl()),
                    Ld8Location::atHLInc => {
                        // TODO: cycle
                        let hl = self.get_hl();
                        self.set_hl(hl.wrapping_add(1));
                        self.mem.get(hl)
                    }
                    Ld8Location::atHLDec => {
                        // TODO: cycle
                        let hl = self.get_hl();
                        self.set_hl(hl.wrapping_sub(1));
                        self.mem.get(hl)
                    }
                    Ld8Location::atC => self.mem.get(self.get_c() as u16),
                    Ld8Location::Address8 => {
                        let addr = self.pc_fetch() as u16;
                        self.mem.get(addr)
                    }
                    Ld8Location::Address16 => {
                        let addr = u16::from_le_bytes([self.pc_fetch(), self.pc_fetch()]);
                        self.mem.get(addr)
                    }
                    Ld8Location::Immediate => self.pc_fetch(),
                };

                match target {
                    Ld8Location::A => self.set_a(val),
                    Ld8Location::B => self.set_b(val),
                    Ld8Location::C => self.set_c(val),
                    Ld8Location::D => self.set_d(val),
                    Ld8Location::E => self.set_e(val),
                    Ld8Location::H => self.set_h(val),
                    Ld8Location::L => self.set_l(val),
                    Ld8Location::atBC => self.mem.set(self.get_bc(), val),
                    Ld8Location::atDE => self.mem.set(self.get_de(), val),
                    Ld8Location::atHL => self.mem.set(self.get_hl(), val),
                    Ld8Location::atHLInc => {
                        let hl = self.get_hl();
                        self.mem.set(hl, val);
                        self.set_hl(hl.wrapping_add(1));
                    }
                    Ld8Location::atHLDec => {
                        let hl = self.get_hl();
                        self.mem.set(hl, val);
                        self.set_hl(hl.wrapping_sub(1));
                    }
                    Ld8Location::atC => self.mem.set(self.get_c() as u16, val),
                    Ld8Location::Address8 => {
                        let addr = self.pc_fetch() as u16;
                        self.mem.set(addr, val);
                    }
                    Ld8Location::Address16 => {
                        let addr = u16::from_le_bytes([self.pc_fetch(), self.pc_fetch()]);
                        self.mem.set(addr, val);
                    }
                    Ld8Location::Immediate => panic!("Invalid LD8 target"),
                }
            }

            _ => return Err(()),
        };

        Ok(())
    }
}

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();
    let game = &args[1];

    let game_rom: Vec<u8> = fs::read(game)?;

    let mem = MemMapper::new(game_rom);
    let mut cpu = Cpu::new(mem);

    while cpu.step() == Ok(()) {
        // TODO: sleep...
    }

    // clock_init();
    // onClockTick(|| { });
    //    while let Some(instruction) = cpu.load_instruction() {
    //        println!("Execute instruction: {:?}", instruction);
    //        instruction.execute(&mut cpu);
    //        // TODO: sleep based on instruction.cycles()
    //        // TODO: handle interrupts.
    //    }

    Ok(())
}
