import { MMU } from './mmu'
import {
    uint8,
    uint16,
    int8,
    addCarriesByte,
    addCarriesWord,
    addHalfCarriesByte,
    subHalfCarriesByte,
    addHalfCarriesWord
} from './utils'
import { prefixedOpcodeTable, unprefixedOpcodeTable } from './opcodes'

// 8 bit registers
export enum R8 {
    A, B, C, D, E, H, L
}

// 16 bit registers
export enum R16 {
    AF, BC, DE, HL, SP
}

export class CPU {
    A: number = 0
    B: number = 0
    C: number = 0
    D: number = 0
    E: number = 0
    H: number = 0
    L: number = 0

    F: { 
        z: boolean // zero flag
        n: boolean // negative flag
        h: boolean // half carry flag
        c: boolean // carry flag
        toUint8: () => number
    } = {
        z: false,
        n: false,
        h: false,
        c: false,
        toUint8: function() {
            let f = 0;
            if (this.z) f |= 0x80 // bit 7
            if (this.n) f |= 0x40 // bit 6
            if (this.h) f |= 0x20 // bit 5
            if (this.c) f |= 0x10 // bit 4
            return f
        }
    }

    SP: number = 0
    PC: number = 0

    halted: boolean = false
    stopped: boolean = false

    jumped: boolean = false // records whether prev instruction was a jump
    
    ime: boolean = false; // Interrupt Master Enable
    setIMENext: boolean = false; // Set IME on next instruction; used by Ei() command

    mmu: MMU

    constructor(mmu: MMU) {
        this.mmu = mmu
    }

    getR8(reg8: R8): number {
        switch(reg8) {
            case R8.A: return this.A
            case R8.B: return this.B
            case R8.C: return this.C
            case R8.D: return this.D
            case R8.E: return this.E
            case R8.H: return this.H
            case R8.L: return this.L
        }
    }

    setR8(reg8: R8, val: number) {
        switch(reg8) {
            case R8.A: this.A = val; return
            case R8.B: this.B = val; return
            case R8.C: this.C = val; return
            case R8.D: this.D = val; return
            case R8.E: this.E = val; return
            case R8.H: this.H = val; return
            case R8.L: this.L = val; return
        }
    }

    getR16(reg16: R16): number {
        switch(reg16) {
            case R16.AF: return this.getAF()
            case R16.BC: return this.getBC()
            case R16.DE: return this.getDE()
            case R16.HL: return this.getHL()
            case R16.SP: return this.SP
        }
    }

    setR16(reg16: R16, val: number) {
        switch(reg16) {
            case R16.AF: console.warn("set AF not implemented")
            case R16.BC: this.setBC(val); return
            case R16.DE: this.setDE(val); return
            case R16.HL: this.setHL(val); return
            case R16.SP: this.SP = val; return
        }
    }

    getAF(): number {
        return (this.A << 8) | this.F.toUint8()
    }

    getBC(): number {
        return (this.B << 8) | this.C
    }
    getDE(): number {
        return (this.D << 8) | this.E
    }
    getHL(): number {
        return (this.H << 8) | this.L
    }
    setBC(word: number) {
        this.B = word & 0xFF00 >> 8 
        this.C = word & 0x00FF
    }
    setDE(word: number) {
        this.D = word & 0xFF00 >> 8 
        this.E = word & 0x00FF
    }
    setHL(word: number) {
        this.H = word & 0xFF00 >> 8 
        this.L = word & 0x00FF
    }

    // executes the next instruction and returns the number of cycles consumed.
    step(): number {
        this.jumped = false

        let opcode = this.mmu.rb(this.PC)
        let prefixed = false
        if (opcode === 0xCB) {
            this.PC += 1
            opcode = this.mmu.rb(this.PC)
            prefixed = true
        }

        const opcData = prefixed ? prefixedOpcodeTable[opcode] : unprefixedOpcodeTable[opcode]

        this.execute(opcode, prefixed)

        if (this.jumped) {
            return opcData.cycles
        }
        if (opcData.cyclesIfNoJump) {
            return opcData.cyclesIfNoJump
        }
        return opcData.cycles
    }

    execute(instr: number, prefixed: boolean): void {
        if (!prefixed) {
            switch(instr) {
                case 0x00: // NOP [----]
                    // nothin
                    break
                case 0x01: // LD BC d16 [----]
                    this.setBC(this.mmu.rw(this.PC + 1))
                    break
                case 0x02: // LD (BC) A [----]
                    this.A = this.mmu.rw(this.getBC())
                    break
                case 0x03: // INC BC [----]
                    this.setBC(this.getBC() + 1)
                    break
                case 0x04: // INC B  [Z0H-]
                    this.inc_r8(R8.B)
                    break
                case 0x05: // DEC B  [Z1H-]
                    this.dec_r8(R8.B)
                    break
                case 0x06: // LD B d8 [----]
                    this.ld_r8_d8(R8.B)
            
                case 0x7: // RLCA   [000C]
                
                case 0x8: // LD (a16) SP [----]
                
                case 0x9: // ADD HL BC [-0HC]
                
                case 0xa: // LD A (BC) [----]
                
                case 0xb: // DEC BC  [----]
                
                case 0xc: // INC C  [Z0H-]
                
                case 0xd: // DEC C  [Z1H-]
                
                case 0xe: // LD C d8 [----]
                
                case 0xf: // RRCA   [000C]
                
                case 0x10: // STOP 0  [----]
                
                case 0x11: // LD DE d16 [----]
                
                case 0x12: // LD (DE) A [----]
                
                case 0x13: // INC DE  [----]
                
                case 0x14: // INC D  [Z0H-]
                
                case 0x15: // DEC D  [Z1H-]
                
                case 0x16: // LD D d8 [----]
                
                case 0x17: // RLA   [000C]
                
                case 0x18: // JR r8  [----]
                
                case 0x19: // ADD HL DE [-0HC]
                
                case 0x1a: // LD A (DE) [----]
                
                case 0x1b: // DEC DE  [----]
                
                case 0x1c: // INC E  [Z0H-]
                
                case 0x1d: // DEC E  [Z1H-]
                
                case 0x1e: // LD E d8 [----]
                
                case 0x1f: // RRA   [000C]
                
                case 0x20: // JR NZ r8 [----]
                
                case 0x21: // LD HL d16 [----]
                
                case 0x22: // LD (HL+) A [----]
                
                case 0x23: // INC HL  [----]
                
                case 0x24: // INC H  [Z0H-]
                
                case 0x25: // DEC H  [Z1H-]
                
                case 0x26: // LD H d8 [----]
                
                case 0x27: // DAA   [Z-0C]
                
                case 0x28: // JR Z r8 [----]
                
                case 0x29: // ADD HL HL [-0HC]
                
                case 0x2a: // LD A (HL+) [----]
                
                case 0x2b: // DEC HL  [----]
                
                case 0x2c: // INC L  [Z0H-]
                
                case 0x2d: // DEC L  [Z1H-]
                
                case 0x2e: // LD L d8 [----]
                
                case 0x2f: // CPL   [-11-]
                
                case 0x30: // JR NC r8 [----]
                
                case 0x31: // LD SP d16 [----]
                
                case 0x32: // LD (HL-) A [----]
                
                case 0x33: // INC SP  [----]
                
                case 0x34: // INC (HL)  [Z0H-]
                
                case 0x35: // DEC (HL)  [Z1H-]
                
                case 0x36: // LD (HL) d8 [----]
                
                case 0x37: // SCF   [-001]
                
                case 0x38: // JR C r8 [----]
                
                case 0x39: // ADD HL SP [-0HC]
                
                case 0x3a: // LD A (HL-) [----]
                
                case 0x3b: // DEC SP  [----]
                
                case 0x3c: // INC A  [Z0H-]
                
                case 0x3d: // DEC A  [Z1H-]
                
                case 0x3e: // LD A d8 [----]
                
                case 0x3f: // CCF   [-00C]
                
                case 0x40: // LD B B [----]
                
                case 0x41: // LD B C [----]
                
                case 0x42: // LD B D [----]
                
                case 0x43: // LD B E [----]
                
                case 0x44: // LD B H [----]
                
                case 0x45: // LD B L [----]
                
                case 0x46: // LD B (HL) [----]
                
                case 0x47: // LD B A [----]
                
                case 0x48: // LD C B [----]
                
                case 0x49: // LD C C [----]
                
                case 0x4a: // LD C D [----]
                
                case 0x4b: // LD C E [----]
                
                case 0x4c: // LD C H [----]
                
                case 0x4d: // LD C L [----]
                
                case 0x4e: // LD C (HL) [----]
                
                case 0x4f: // LD C A [----]
                
                case 0x50: // LD D B [----]
                
                case 0x51: // LD D C [----]
                
                case 0x52: // LD D D [----]
                
                case 0x53: // LD D E [----]
                
                case 0x54: // LD D H [----]
                
                case 0x55: // LD D L [----]
                
                case 0x56: // LD D (HL) [----]
                
                case 0x57: // LD D A [----]
                
                case 0x58: // LD E B [----]
                
                case 0x59: // LD E C [----]
                
                case 0x5a: // LD E D [----]
                
                case 0x5b: // LD E E [----]
                
                case 0x5c: // LD E H [----]
                
                case 0x5d: // LD E L [----]
                
                case 0x5e: // LD E (HL) [----]
                
                case 0x5f: // LD E A [----]
                
                case 0x60: // LD H B [----]
                
                case 0x61: // LD H C [----]
                
                case 0x62: // LD H D [----]
                
                case 0x63: // LD H E [----]
                
                case 0x64: // LD H H [----]
                
                case 0x65: // LD H L [----]
                
                case 0x66: // LD H (HL) [----]
                
                case 0x67: // LD H A [----]
                
                case 0x68: // LD L B [----]
                
                case 0x69: // LD L C [----]
                
                case 0x6a: // LD L D [----]
                
                case 0x6b: // LD L E [----]
                
                case 0x6c: // LD L H [----]
                
                case 0x6d: // LD L L [----]
                
                case 0x6e: // LD L (HL) [----]
                
                case 0x6f: // LD L A [----]
                
                case 0x70: // LD (HL) B [----]
                
                case 0x71: // LD (HL) C [----]
                
                case 0x72: // LD (HL) D [----]
                
                case 0x73: // LD (HL) E [----]
                
                case 0x74: // LD (HL) H [----]
                
                case 0x75: // LD (HL) L [----]
                
                case 0x76: // HALT   [----]
                
                case 0x77: // LD (HL) A [----]
                
                case 0x78: // LD A B [----]
                
                case 0x79: // LD A C [----]
                
                case 0x7a: // LD A D [----]
                
                case 0x7b: // LD A E [----]
                
                case 0x7c: // LD A H [----]
                
                case 0x7d: // LD A L [----]
                
                case 0x7e: // LD A (HL) [----]
                
                case 0x7f: // LD A A [----]
                
                case 0x80: // ADD A B [Z0HC]
                
                case 0x81: // ADD A C [Z0HC]
                
                case 0x82: // ADD A D [Z0HC]
                
                case 0x83: // ADD A E [Z0HC]
                
                case 0x84: // ADD A H [Z0HC]
                
                case 0x85: // ADD A L [Z0HC]
                
                case 0x86: // ADD A (HL) [Z0HC]
                
                case 0x87: // ADD A A [Z0HC]
                
                case 0x88: // ADC A B [Z0HC]
                
                case 0x89: // ADC A C [Z0HC]
                
                case 0x8a: // ADC A D [Z0HC]
                
                case 0x8b: // ADC A E [Z0HC]
                
                case 0x8c: // ADC A H [Z0HC]
                
                case 0x8d: // ADC A L [Z0HC]
                
                case 0x8e: // ADC A (HL) [Z0HC]
                
                case 0x8f: // ADC A A [Z0HC]
                
                case 0x90: // SUB B  [Z1HC]
                
                case 0x91: // SUB C  [Z1HC]
                
                case 0x92: // SUB D  [Z1HC]
                
                case 0x93: // SUB E  [Z1HC]
                
                case 0x94: // SUB H  [Z1HC]
                
                case 0x95: // SUB L  [Z1HC]
                
                case 0x96: // SUB (HL)  [Z1HC]
                
                case 0x97: // SUB A  [Z1HC]
                
                case 0x98: // SBC A B [Z1HC]
                
                case 0x99: // SBC A C [Z1HC]
                
                case 0x9a: // SBC A D [Z1HC]
                
                case 0x9b: // SBC A E [Z1HC]
                
                case 0x9c: // SBC A H [Z1HC]
                
                case 0x9d: // SBC A L [Z1HC]
                
                case 0x9e: // SBC A (HL) [Z1HC]
                
                case 0x9f: // SBC A A [Z1HC]
                
                case 0xa0: // AND B  [Z010]
                
                case 0xa1: // AND C  [Z010]
                
                case 0xa2: // AND D  [Z010]
                
                case 0xa3: // AND E  [Z010]
                
                case 0xa4: // AND H  [Z010]
                
                case 0xa5: // AND L  [Z010]
                
                case 0xa6: // AND (HL)  [Z010]
                
                case 0xa7: // AND A  [Z010]
                
                case 0xa8: // XOR B  [Z000]
                
                case 0xa9: // XOR C  [Z000]
                
                case 0xaa: // XOR D  [Z000]
                
                case 0xab: // XOR E  [Z000]
                
                case 0xac: // XOR H  [Z000]
                
                case 0xad: // XOR L  [Z000]
                
                case 0xae: // XOR (HL)  [Z000]
                
                case 0xaf: // XOR A  [Z000]
                
                case 0xb0: // OR B  [Z000]
                
                case 0xb1: // OR C  [Z000]
                
                case 0xb2: // OR D  [Z000]
                
                case 0xb3: // OR E  [Z000]
                
                case 0xb4: // OR H  [Z000]
                
                case 0xb5: // OR L  [Z000]
                
                case 0xb6: // OR (HL)  [Z000]
                
                case 0xb7: // OR A  [Z000]
                
                case 0xb8: // CP B  [Z1HC]
                
                case 0xb9: // CP C  [Z1HC]
                
                case 0xba: // CP D  [Z1HC]
                
                case 0xbb: // CP E  [Z1HC]
                
                case 0xbc: // CP H  [Z1HC]
                
                case 0xbd: // CP L  [Z1HC]
                
                case 0xbe: // CP (HL)  [Z1HC]
                
                case 0xbf: // CP A  [Z1HC]
                
                case 0xc0: // RET NZ  [----]
                
                case 0xc1: // POP BC  [----]
                
                case 0xc2: // JP NZ a16 [----]
                
                case 0xc3: // JP a16  [----]
                
                case 0xc4: // CALL NZ a16 [----]
                
                case 0xc5: // PUSH BC  [----]
                
                case 0xc6: // ADD A d8 [Z0HC]
                
                case 0xc7: // RST 00H  [----]
                
                case 0xc8: // RET Z  [----]
                
                case 0xc9: // RET   [----]
                
                case 0xca: // JP Z a16 [----]
                
                case 0xcb: // PREFIX CB  [----]
                
                case 0xcc: // CALL Z a16 [----]
                
                case 0xcd: // CALL a16  [----]
                
                case 0xce: // ADC A d8 [Z0HC]
                
                case 0xcf: // RST 08H  [----]
                
                case 0xd0: // RET NC  [----]
                
                case 0xd1: // POP DE  [----]
                
                case 0xd2: // JP NC a16 [----]
                
                case 0xd4: // CALL NC a16 [----]
                
                case 0xd5: // PUSH DE  [----]
                
                case 0xd6: // SUB d8  [Z1HC]
                
                case 0xd7: // RST 10H  [----]
                
                case 0xd8: // RET C  [----]
                
                case 0xd9: // RETI   [----]
                
                case 0xda: // JP C a16 [----]
                
                case 0xdc: // CALL C a16 [----]
                
                case 0xde: // SBC A d8 [Z1HC]
                
                case 0xdf: // RST 18H  [----]
                
                case 0xe0: // LDH (a8) A [----]
                
                case 0xe1: // POP HL  [----]
                
                case 0xe2: // LD (C) A [----]
                
                case 0xe5: // PUSH HL  [----]
                
                case 0xe6: // AND d8  [Z010]
                
                case 0xe7: // RST 20H  [----]
                
                case 0xe8: // ADD SP r8 [00HC]
                
                case 0xe9: // JP (HL)  [----]
                
                case 0xea: // LD (a16) A [----]
                
                case 0xee: // XOR d8  [Z000]
                
                case 0xef: // RST 28H  [----]
                
                case 0xf0: // LDH A (a8) [----]
                
                case 0xf1: // POP AF  [ZNHC]
                
                case 0xf2: // LD A (C) [----]
                
                case 0xf3: // DI   [----]
                
                case 0xf5: // PUSH AF  [----]
                
                case 0xf6: // OR d8  [Z000]
                
                case 0xf7: // RST 30H  [----]
                
                case 0xf8: // LD HL SP+r8 [00HC]
                
                case 0xf9: // LD SP HL [----]
                
                case 0xfa: // LD A (a16) [----]
                
                case 0xfb: // EI   [----]
                
                case 0xfe: // CP d8  [Z1HC]
                
                case 0xff: // RST 38H  [----]
                
            
            
                case 0x0: // RLC B  [Z00C]
                
                case 0x1: // RLC C  [Z00C]
                
                case 0x2: // RLC D  [Z00C]
                
                case 0x3: // RLC E  [Z00C]
                
                case 0x4: // RLC H  [Z00C]
                
                case 0x5: // RLC L  [Z00C]
                
                case 0x6: // RLC (HL)  [Z00C]
                
                case 0x7: // RLC A  [Z00C]
                
                case 0x8: // RRC B  [Z00C]
                
                case 0x9: // RRC C  [Z00C]
                
                case 0xa: // RRC D  [Z00C]
                
                case 0xb: // RRC E  [Z00C]
                
                case 0xc: // RRC H  [Z00C]
                
                case 0xd: // RRC L  [Z00C]
                
                case 0xe: // RRC (HL)  [Z00C]
                
                case 0xf: // RRC A  [Z00C]
                
                case 0x10: // RL B  [Z00C]
                
                case 0x11: // RL C  [Z00C]
                
                case 0x12: // RL D  [Z00C]
                
                case 0x13: // RL E  [Z00C]
                
                case 0x14: // RL H  [Z00C]
                
                case 0x15: // RL L  [Z00C]
                
                case 0x16: // RL (HL)  [Z00C]
                
                case 0x17: // RL A  [Z00C]
                
                case 0x18: // RR B  [Z00C]
                
                case 0x19: // RR C  [Z00C]
                
                case 0x1a: // RR D  [Z00C]
                
                case 0x1b: // RR E  [Z00C]
                
                case 0x1c: // RR H  [Z00C]
                
                case 0x1d: // RR L  [Z00C]
                
                case 0x1e: // RR (HL)  [Z00C]
                
                case 0x1f: // RR A  [Z00C]
                
                case 0x20: // SLA B  [Z00C]
                
                case 0x21: // SLA C  [Z00C]
                
                case 0x22: // SLA D  [Z00C]
                
                case 0x23: // SLA E  [Z00C]
                
                case 0x24: // SLA H  [Z00C]
                
                case 0x25: // SLA L  [Z00C]
                
                case 0x26: // SLA (HL)  [Z00C]
                
                case 0x27: // SLA A  [Z00C]
                
                case 0x28: // SRA B  [Z000]
                
                case 0x29: // SRA C  [Z000]
                
                case 0x2a: // SRA D  [Z000]
                
                case 0x2b: // SRA E  [Z000]
                
                case 0x2c: // SRA H  [Z000]
                
                case 0x2d: // SRA L  [Z000]
                
                case 0x2e: // SRA (HL)  [Z000]
                
                case 0x2f: // SRA A  [Z000]
                
                case 0x30: // SWAP B  [Z000]
                
                case 0x31: // SWAP C  [Z000]
                
                case 0x32: // SWAP D  [Z000]
                
                case 0x33: // SWAP E  [Z000]
                
                case 0x34: // SWAP H  [Z000]
                
                case 0x35: // SWAP L  [Z000]
                
                case 0x36: // SWAP (HL)  [Z000]
                
                case 0x37: // SWAP A  [Z000]
                
                case 0x38: // SRL B  [Z00C]
                
                case 0x39: // SRL C  [Z00C]
                
                case 0x3a: // SRL D  [Z00C]
                
                case 0x3b: // SRL E  [Z00C]
                
                case 0x3c: // SRL H  [Z00C]
                
                case 0x3d: // SRL L  [Z00C]
                
                case 0x3e: // SRL (HL)  [Z00C]
                
                case 0x3f: // SRL A  [Z00C]
                
                case 0x40: // BIT 0 B [Z01-]
                
                case 0x41: // BIT 0 C [Z01-]
                
                case 0x42: // BIT 0 D [Z01-]
                
                case 0x43: // BIT 0 E [Z01-]
                
                case 0x44: // BIT 0 H [Z01-]
                
                case 0x45: // BIT 0 L [Z01-]
                
                case 0x46: // BIT 0 (HL) [Z01-]
                
                case 0x47: // BIT 0 A [Z01-]
                
                case 0x48: // BIT 1 B [Z01-]
                
                case 0x49: // BIT 1 C [Z01-]
                
                case 0x4a: // BIT 1 D [Z01-]
                
                case 0x4b: // BIT 1 E [Z01-]
                
                case 0x4c: // BIT 1 H [Z01-]
                
                case 0x4d: // BIT 1 L [Z01-]
                
                case 0x4e: // BIT 1 (HL) [Z01-]
                
                case 0x4f: // BIT 1 A [Z01-]
                
                case 0x50: // BIT 2 B [Z01-]
                
                case 0x51: // BIT 2 C [Z01-]
                
                case 0x52: // BIT 2 D [Z01-]
                
                case 0x53: // BIT 2 E [Z01-]
                
                case 0x54: // BIT 2 H [Z01-]
                
                case 0x55: // BIT 2 L [Z01-]
                
                case 0x56: // BIT 2 (HL) [Z01-]
                
                case 0x57: // BIT 2 A [Z01-]
                
                case 0x58: // BIT 3 B [Z01-]
                
                case 0x59: // BIT 3 C [Z01-]
                
                case 0x5a: // BIT 3 D [Z01-]
                
                case 0x5b: // BIT 3 E [Z01-]
                
                case 0x5c: // BIT 3 H [Z01-]
                
                case 0x5d: // BIT 3 L [Z01-]
                
                case 0x5e: // BIT 3 (HL) [Z01-]
                
                case 0x5f: // BIT 3 A [Z01-]
                
                case 0x60: // BIT 4 B [Z01-]
                
                case 0x61: // BIT 4 C [Z01-]
                
                case 0x62: // BIT 4 D [Z01-]
                
                case 0x63: // BIT 4 E [Z01-]
                
                case 0x64: // BIT 4 H [Z01-]
                
                case 0x65: // BIT 4 L [Z01-]
                
                case 0x66: // BIT 4 (HL) [Z01-]
                
                case 0x67: // BIT 4 A [Z01-]
                
                case 0x68: // BIT 5 B [Z01-]
                
                case 0x69: // BIT 5 C [Z01-]
                
                case 0x6a: // BIT 5 D [Z01-]
                
                case 0x6b: // BIT 5 E [Z01-]
                
                case 0x6c: // BIT 5 H [Z01-]
                
                case 0x6d: // BIT 5 L [Z01-]
                
                case 0x6e: // BIT 5 (HL) [Z01-]
                
                case 0x6f: // BIT 5 A [Z01-]
                
                case 0x70: // BIT 6 B [Z01-]
                
                case 0x71: // BIT 6 C [Z01-]
                
                case 0x72: // BIT 6 D [Z01-]
                
                case 0x73: // BIT 6 E [Z01-]
                
                case 0x74: // BIT 6 H [Z01-]
                
                case 0x75: // BIT 6 L [Z01-]
                
                case 0x76: // BIT 6 (HL) [Z01-]
                
                case 0x77: // BIT 6 A [Z01-]
                
                case 0x78: // BIT 7 B [Z01-]
                
                case 0x79: // BIT 7 C [Z01-]
                
                case 0x7a: // BIT 7 D [Z01-]
                
                case 0x7b: // BIT 7 E [Z01-]
                
                case 0x7c: // BIT 7 H [Z01-]
                
                case 0x7d: // BIT 7 L [Z01-]
                
                case 0x7e: // BIT 7 (HL) [Z01-]
                
                case 0x7f: // BIT 7 A [Z01-]
                
                case 0x80: // RES 0 B [----]
                
                case 0x81: // RES 0 C [----]
                
                case 0x82: // RES 0 D [----]
                
                case 0x83: // RES 0 E [----]
                
                case 0x84: // RES 0 H [----]
                
                case 0x85: // RES 0 L [----]
                
                case 0x86: // RES 0 (HL) [----]
                
                case 0x87: // RES 0 A [----]
                
                case 0x88: // RES 1 B [----]
                
                case 0x89: // RES 1 C [----]
                
                case 0x8a: // RES 1 D [----]
                
                case 0x8b: // RES 1 E [----]
                
                case 0x8c: // RES 1 H [----]
                
                case 0x8d: // RES 1 L [----]
                
                case 0x8e: // RES 1 (HL) [----]
                
                case 0x8f: // RES 1 A [----]
                
                case 0x90: // RES 2 B [----]
                
                case 0x91: // RES 2 C [----]
                
                case 0x92: // RES 2 D [----]
                
                case 0x93: // RES 2 E [----]
                
                case 0x94: // RES 2 H [----]
                
                case 0x95: // RES 2 L [----]
                
                case 0x96: // RES 2 (HL) [----]
                
                case 0x97: // RES 2 A [----]
                
                case 0x98: // RES 3 B [----]
                
                case 0x99: // RES 3 C [----]
                
                case 0x9a: // RES 3 D [----]
                
                case 0x9b: // RES 3 E [----]
                
                case 0x9c: // RES 3 H [----]
                
                case 0x9d: // RES 3 L [----]
                
                case 0x9e: // RES 3 (HL) [----]
                
                case 0x9f: // RES 3 A [----]
                
                case 0xa0: // RES 4 B [----]
                
                case 0xa1: // RES 4 C [----]
                
                case 0xa2: // RES 4 D [----]
                
                case 0xa3: // RES 4 E [----]
                
                case 0xa4: // RES 4 H [----]
                
                case 0xa5: // RES 4 L [----]
                
                case 0xa6: // RES 4 (HL) [----]
                
                case 0xa7: // RES 4 A [----]
                
                case 0xa8: // RES 5 B [----]
                
                case 0xa9: // RES 5 C [----]
                
                case 0xaa: // RES 5 D [----]
                
                case 0xab: // RES 5 E [----]
                
                case 0xac: // RES 5 H [----]
                
                case 0xad: // RES 5 L [----]
                
                case 0xae: // RES 5 (HL) [----]
                
                case 0xaf: // RES 5 A [----]
                
                case 0xb0: // RES 6 B [----]
                
                case 0xb1: // RES 6 C [----]
                
                case 0xb2: // RES 6 D [----]
                
                case 0xb3: // RES 6 E [----]
                
                case 0xb4: // RES 6 H [----]
                
                case 0xb5: // RES 6 L [----]
                
                case 0xb6: // RES 6 (HL) [----]
                
                case 0xb7: // RES 6 A [----]
                
                case 0xb8: // RES 7 B [----]
                
                case 0xb9: // RES 7 C [----]
                
                case 0xba: // RES 7 D [----]
                
                case 0xbb: // RES 7 E [----]
                
                case 0xbc: // RES 7 H [----]
                
                case 0xbd: // RES 7 L [----]
                
                case 0xbe: // RES 7 (HL) [----]
                
                case 0xbf: // RES 7 A [----]
                
                case 0xc0: // SET 0 B [----]
                
                case 0xc1: // SET 0 C [----]
                
                case 0xc2: // SET 0 D [----]
                
                case 0xc3: // SET 0 E [----]
                
                case 0xc4: // SET 0 H [----]
                
                case 0xc5: // SET 0 L [----]
                
                case 0xc6: // SET 0 (HL) [----]
                
                case 0xc7: // SET 0 A [----]
                
                case 0xc8: // SET 1 B [----]
                
                case 0xc9: // SET 1 C [----]
                
                case 0xca: // SET 1 D [----]
                
                case 0xcb: // SET 1 E [----]
                
                case 0xcc: // SET 1 H [----]
                
                case 0xcd: // SET 1 L [----]
                
                case 0xce: // SET 1 (HL) [----]
                
                case 0xcf: // SET 1 A [----]
                
                case 0xd0: // SET 2 B [----]
                
                case 0xd1: // SET 2 C [----]
                
                case 0xd2: // SET 2 D [----]
                
                case 0xd3: // SET 2 E [----]
                
                case 0xd4: // SET 2 H [----]
                
                case 0xd5: // SET 2 L [----]
                
                case 0xd6: // SET 2 (HL) [----]
                
                case 0xd7: // SET 2 A [----]
                
                case 0xd8: // SET 3 B [----]
                
                case 0xd9: // SET 3 C [----]
                
                case 0xda: // SET 3 D [----]
                
                case 0xdb: // SET 3 E [----]
                
                case 0xdc: // SET 3 H [----]
                
                case 0xdd: // SET 3 L [----]
                
                case 0xde: // SET 3 (HL) [----]
                
                case 0xdf: // SET 3 A [----]
                
                case 0xe0: // SET 4 B [----]
                
                case 0xe1: // SET 4 C [----]
                
                case 0xe2: // SET 4 D [----]
                
                case 0xe3: // SET 4 E [----]
                
                case 0xe4: // SET 4 H [----]
                
                case 0xe5: // SET 4 L [----]
                
                case 0xe6: // SET 4 (HL) [----]
                
                case 0xe7: // SET 4 A [----]
                
                case 0xe8: // SET 5 B [----]
                
                case 0xe9: // SET 5 C [----]
                
                case 0xea: // SET 5 D [----]
                
                case 0xeb: // SET 5 E [----]
                
                case 0xec: // SET 5 H [----]
                
                case 0xed: // SET 5 L [----]
                
                case 0xee: // SET 5 (HL) [----]
                
                case 0xef: // SET 5 A [----]
                
                case 0xf0: // SET 6 B [----]
                
                case 0xf1: // SET 6 C [----]
                
                case 0xf2: // SET 6 D [----]
                
                case 0xf3: // SET 6 E [----]
                
                case 0xf4: // SET 6 H [----]
                
                case 0xf5: // SET 6 L [----]
                
                case 0xf6: // SET 6 (HL) [----]
                
                case 0xf7: // SET 6 A [----]
                
                case 0xf8: // SET 7 B [----]
                
                case 0xf9: // SET 7 C [----]
                
                case 0xfa: // SET 7 D [----]
                
                case 0xfb: // SET 7 E [----]
                
                case 0xfc: // SET 7 H [----]
                
                case 0xfd: // SET 7 L [----]
                
                case 0xfe: // SET 7 (HL) [----]
                
                case 0xff: // SET 7 A [----]
                
                
            }
        } else {
            switch(instr) {

            }
        }
    }

    // Opcode helper instructions
    // ====

    // INC r8 [z0h-]
    inc_r8 = (reg8: R8) => {
        const val = this.getR8(reg8)
        const newVal = uint8(val + 1)
        this.setR8(reg8, newVal)

        this.F.z = newVal === 0
        this.F.n = false
        this.F.h = addHalfCarriesByte(val, 1)
        // c -
    }

    // DEC r8 [z1h-]
    dec_r8 = (reg8: R8) => {
        const val = this.getR8(reg8)
        const newVal = uint8(val - 1)
        this.setR8(reg8, newVal)

        this.F.z = newVal === 0
        this.F.n = true
        this.F.h = subHalfCarriesByte(val, 1)
        // c -
    }

    // LD r8 d8 [----]
    ld_r8_d8 = (reg8: R8) => {
        const d8 = this.mmu.rb(this.PC += 1)
        this.setR8(reg8, d8)
    }

    // RLCA [000c]
    // rotates A left one bit, with bit 7 moved to bit 0 and also stored in the carry.
    rlc_A = () => {
        const carry = (this.A & 0x80) != 0
        if (carry) {
            // shift left, discard top bit
            this.A = this.A << 1
            // set bit 0 to 1 (wrap bit 7 around)
            this.A |= 0x01
            // store bit in carry
            this.F.c = true
        } else {
            // top bit is 0, safe to shift left.
            this.A = this.A << 1
            // store bit in carry
            this.F.c = false
        }

        this.F.z = false
        this.F.n = false
        this.F.h = false
    }

    // RRCA [000c]
    // rotates A one bit right with bit 0 moved to bit 7 and also stored in the carry.
    rrc_A = () => {
        const carry = (this.A & 0x01) != 0
        if (carry) {
            // shift right (discard bottom bit)
            this.A = this.A >> 1
            // set top bit to 1 (wrap bit 0 around)
            this.A |= 0x80
            // store old bottom bit in carry
            this.F.c = true
        } else {
            // bottom bit is 0, safe to right shift
            this.A = this.A >> 1
            // store old bottom bit in carry
            this.F.c = false
        }
        
        this.F.z = false
        this.F.n = false
        this.F.h = false
    }

    // LD (a16) SP [----]
    ld_a16_SP = () => {
        const a16 = this.mmu.rw(this.PC += 1)
        this.PC += 1
        this.mmu.ww(a16, this.SP)
    }

    // LD SP d16 [----]
    ld_SP_d16 = () => {
        const d16 = this.mmu.rw(this.PC += 1)
        this.PC += 1
        this.SP = d16
    }

    // ADD HL r16 [-0hc]
    add_HL_r16 = (reg16: R16) => {
        const val = this.getR16(reg16)
        const hl = this.getHL()
        const newVal = uint16(hl+ val)
        this.setHL(newVal)

        // z -
        this.F.n = false
        this.F.h = addHalfCarriesWord(hl, val)
        this.F.c = addCarriesWord(hl, val)
    }

    // DEC r16 [----]
    dec_r16 = (reg16: R16) => {
        this.setR16(reg16, uint16(this.getR16(reg16) - 1))
    }

    // LD r16 d16 [----]
    ld_r16_d16 = (reg16: R16) => {
        const d16 = this.mmu.rw(this.PC += 1)
        this.PC += 1
        this.setR16(reg16, d16)
    }

    // LD (r16) A [----]
    // loads A into (r16)
    ld_valr16_A = (reg16: R16) => {
        this.mmu.wb(this.getR16(reg16), this.A)
    }

    // LD (HL+) A [----]
    // sets (HL) = A, then increments HL
    ld_valHLinc_A = () => {
        const hl = this.getHL()
        this.mmu.wb(hl, this.A)
        this.setHL(uint16(hl + 1))
    }

    // LD A (HL+) [---]
    // sets A = (HL), then increments HL
    ld_A_valHLinc = () => {
        const hl = this.getHL()
        this.A = this.mmu.rb(hl)
        this.setHL(uint16(hl + 1))
    }

    // LD (HL) d8 [----]
    ld_valHL_d8 = () => {
        const d8 = this.mmu.rb(this.PC += 1)
        this.mmu.wb(this.getHL(), d8)
    }

    // LD r8 (HL) [----]
    ld_r8_valHL = (reg8: R8) => {
        this.setR8(reg8, this.mmu.rb(this.getHL()))
    }

    // LD (HL) r8 [----]
    ld_valHL_r8 = (reg8: R8) => {
        this.mmu.wb(this.getHL(), this.getR8(reg8))
    }

    // INC (HL) [z0h-]
    // increments the byte at address HL
    inc_valHL = () => {
        const hl = this.getHL()
        const byte = this.mmu.rb(hl)
        const newByte = uint8(byte + 1)
        this.mmu.wb(hl, newByte)

        this.F.z = newByte === 0
        this.F.n = false
        this.F.h = addHalfCarriesByte(byte, 1)
        // c -
    }

    // DEC (HL) [z1h-]
    dec_valHL = () => {
        const hl = this.getHL()
        const byte = this.mmu.rb(hl)
        const newByte = uint8(byte - 1)
        this.mmu.wb(hl, newByte)

        this.F.z = newByte === 0
        this.F.n = true
        this.F.h = subHalfCarriesByte(byte, 1)
        // c -
    }

    // RLA [000c]
    // rotates A one bit left with the carry moved to bit 0
    // and bit 7 moved to the carry.
    rl_A = () => {
        const oldCarry = this.F.c ? 1 : 0
        // if top bit is 1, move bit to carry
        this.F.c = (this.A & 0x80) !== 0 ? true : false
        // shift A left one bit and set bit 0 to the value of the old carry
        this.A = this.A << 1
        this.A |= oldCarry

        this.F.z = false
        this.F.n = false
        this.F.h = false
    }

    // RRA [000c]
    // rotates A one bit right with the carry moved to bit 7
    // and bit 0 moved to the carry.
    rr_A = () => {
        const oldCarry = this.F.c ? 1 : 0
        // if top bit is 1, move bit to carry
        this.F.c = (this.A & 0x01) !== 0 ? true : false
        // shift A right one bit and set bit 7 to the value of the old carry
        this.A = this.A >> 1
        this.A |= oldCarry << 7

        this.F.z = false
        this.F.n = false
        this.F.h = false
    }

    // JR int8 [----]
    // relative jump to PC +/- int8
    jr_int8 = () => {
        const i8 = int8(this.mmu.rb(this.PC += 1))
        this.PC = uint16(this.PC + i8)
    }

    // ADD A {r8, d8} [z0hc]
    add_A = (byte: number) => {
        const oldA = this.A
        this.A = uint8(this.A + byte)

        this.F.z = this.A === 0
        this.F.n = false
        this.F.h = addHalfCarriesByte(oldA, byte)
        this.F.c = addCarriesByte(oldA, byte)
    }

    // ADC A {r8, d8} [z0hc]
    // sets A = A + {r8, d8} + carry
    adc_A = (val: number) => {
        const carry = this.F.c ? 1 : 0
        const oldA = this.A
        this.A = uint8(this.A + val + carry)

        this.F.z = this.A === 0
        this.F.n = false
        this.F.h = addHalfCarriesByte(oldA, val, carry)
        this.F.c = addCarriesByte(oldA, val + carry)
    }

    // SUB A {r8, d8} [z1hc]
    sub_A = (val: number) => {
        const oldA = this.A
        this.A = uint8(this.A - val)

        this.F.z = this.A === 0
        this.F.n = true
        this.F.n = subHalfCarriesByte(oldA, val)
        this.F.c = oldA - val < 0
    }

    sbc_A = (val: number) => {
        const carry = this.F.c ? 1 : 0
        const oldA = this.A
        this.A = uint8(this.A - val - carry)

        this.F.z = this.A === 0
        this.F.n = true
        this.F.h = subHalfCarriesByte(oldA, val, carry)
        this.F.c = oldA - val - carry < 0
    }

    // AND A r8 [z010]
    and_A = (val: number) => {
        this.A = this.A & val

        this.F.z = this.A === 0
        this.F.n = false
        this.F.h = true
        this.F.c = false
    }

    // XOR A r8 [z010]
    xor_A = (val: number) => {
        this.A = this.A | val

        this.F.z = this.A === 0
        this.F.n = false
        this.F.h = true
        this.F.c = false
    }

    // OR A r8 [z000]
    or_A = (val: number) => {
        this.A = this.A | val

        this.F.z = this.A === 0
        this.F.n = false
        this.F.h = false
        this.F.c = false
    }

    // DAA [z-0c]
    // A binary coded decimal instruction.
    // Intended to be called after an addition or subtraction of binary coded decimal values.
    // Adjusts the A register to contain the correct binary coded decimal result.
    // For a better explanation, see https://ehaskins.com/2018-01-30%20Z80%20DAA/
    daa = () => {
        let u = 0;
        // If last operation had half carry,
        // or if last op was add and lower nyb of A needs to be adjusted (ie, is greater than 9)
        if (this.F.h || (!this.F.n && (this.A & 0x0F) > 0x09)) {
            u = 0x06
        }
        // If last op had carry,
        // or if upper nyb of A needs to be adjusted (ie, is greater than 99)
        if (this.F.c || (!this.F.n && this.A > 0x99)) {
            u |= 0x60
            this.F.c = true
        }
        // Adjust A by subtracting u if last operation was a subtraction,
        // otherwise adjust A by adding u.
        if (this.F.n) {
            this.A = uint8(this.A - u)
        } else {
            this.A = uint8(this.A + u)
        }

        this.F.z = this.A === 0
        this.F.h = false
    }

    // CPL [-11-]
    // A ~= 0xFF (aka complement of A)
    cpl = () => {
        this.A ^= 0xFF
        
        // z -
        this.F.n = true
        this.F.h = true
        // c -
    }

    // SCF [-001]
    // sets carry flag and unsets N and H flags
    scf = () => {
        // z -
        this.F.n = false
        this.F.h = false
        this.F.c = true
    }

    // CCF [-00c]
    // complements (inverts) carry flag and resets N and H flags
    ccf = () => {
        this.F.n = false
        this.F.h = false
        this.F.c = !this.F.c
    }
    



}

