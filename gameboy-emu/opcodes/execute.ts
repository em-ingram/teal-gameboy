// generated from generate_execute.js at 12/25/2023 10:48:39 PM 
import { CPU, R8, R16, RSTVector } from "../cpu"
import { int8 } from "../utils"

export const execute = (cpu: CPU, instr: number, cbprefixed: boolean) => {
    if (!cbprefixed) {
        switch(instr) {
            
            case 0x0: // NOP   [----]
                
                break
            case 0x1: // LD BC d16 [----]
                cpu.setBC(cpu.nextWord())
                break
            case 0x2: // LD (BC) A [----]
                cpu.mmu.wb(cpu.getBC(), cpu.A)
                break
            case 0x3: // INC BC  [----]
                cpu.inc_reg16(R16.BC)
                break
            case 0x4: // INC B  [Z0H-]
                cpu.inc_reg8(R8.B)
                break
            case 0x5: // DEC B  [Z1H-]
                cpu.dec_reg8(R8.B)
                break
            case 0x6: // LD B d8 [----]
                cpu.B = cpu.nextByte()
                break
            case 0x7: // RLCA   [000C]
                cpu.rlca()
                break

            case 0x8: // LD (a16) SP [----]
                cpu.mmu.ww(cpu.nextWord(), cpu.SP)
                break
            case 0x9: // ADD HL BC [-0HC]
                cpu.add_HL_reg16(R16.BC)
                break
            case 0xa: // LD A (BC) [----]
                cpu.A = cpu.mmu.rb(cpu.getBC())
                break
            case 0xb: // DEC BC  [----]
                cpu.dec_reg16(R16.BC)
                break
            case 0xc: // INC C  [Z0H-]
                cpu.inc_reg8(R8.C)
                break
            case 0xd: // DEC C  [Z1H-]
                cpu.dec_reg8(R8.C)
                break
            case 0xe: // LD C d8 [----]
                cpu.C = cpu.nextByte()
                break
            case 0xf: // RRCA   [000C]
                cpu.rrca()
                break



            case 0x10: // STOP 0  [----]
                cpu.stop()
                break
            case 0x11: // LD DE d16 [----]
                cpu.setDE(cpu.nextWord())
                break
            case 0x12: // LD (DE) A [----]
                cpu.mmu.wb(cpu.getDE(), cpu.A)
                break
            case 0x13: // INC DE  [----]
                cpu.inc_reg16(R16.DE)
                break
            case 0x14: // INC D  [Z0H-]
                cpu.inc_reg8(R8.D)
                break
            case 0x15: // DEC D  [Z1H-]
                cpu.dec_reg8(R8.D)
                break
            case 0x16: // LD D d8 [----]
                cpu.D = cpu.nextByte()
                break
            case 0x17: // RLA   [000C]
                cpu.rla()
                break

            case 0x18: // JR r8  [----]
                cpu.jr(int8(cpu.nextByte()))
                break
            case 0x19: // ADD HL DE [-0HC]
                cpu.add_HL_reg16(R16.DE)
                break
            case 0x1a: // LD A (DE) [----]
                cpu.A = cpu.mmu.rb(cpu.getDE())
                break
            case 0x1b: // DEC DE  [----]
                cpu.dec_reg16(R16.DE)
                break
            case 0x1c: // INC E  [Z0H-]
                cpu.inc_reg8(R8.E)
                break
            case 0x1d: // DEC E  [Z1H-]
                cpu.dec_reg8(R8.E)
                break
            case 0x1e: // LD E d8 [----]
                cpu.E = cpu.nextByte()
                break
            case 0x1f: // RRA   [000C]
                cpu.rra()
                break



            case 0x20: // JR NZ r8 [----]
                if (!cpu.F.z) cpu.jr(int8(cpu.nextByte()))
                break
            case 0x21: // LD HL d16 [----]
                cpu.setHL(cpu.nextWord())
                break
            case 0x22: // LD (HL+) A [----]
                cpu.ld_valHLinc_A()
                break
            case 0x23: // INC HL  [----]
                cpu.inc_reg16(R16.HL)
                break
            case 0x24: // INC H  [Z0H-]
                cpu.inc_reg8(R8.H)
                break
            case 0x25: // DEC H  [Z1H-]
                cpu.dec_reg8(R8.H)
                break
            case 0x26: // LD H d8 [----]
                cpu.H = cpu.nextByte()
                break
            case 0x27: // DAA   [Z-0C]
                cpu.daa()
                break

            case 0x28: // JR Z r8 [----]
                if (cpu.F.z) cpu.jr(int8(cpu.nextByte()))
                break
            case 0x29: // ADD HL HL [-0HC]
                cpu.add_HL_reg16(R16.HL)
                break
            case 0x2a: // LD A (HL+) [----]
                cpu.ld_A_valHLinc()
                break
            case 0x2b: // DEC HL  [----]
                cpu.dec_reg16(R16.HL)
                break
            case 0x2c: // INC L  [Z0H-]
                cpu.inc_reg8(R8.L)
                break
            case 0x2d: // DEC L  [Z1H-]
                cpu.dec_reg8(R8.L)
                break
            case 0x2e: // LD L d8 [----]
                cpu.L = cpu.nextByte()
                break
            case 0x2f: // CPL   [-11-]
                cpu.cpl()
                break



            case 0x30: // JR NC r8 [----]
                if (!cpu.F.c) cpu.jr(int8(cpu.nextByte()))
                break
            case 0x31: // LD SP d16 [----]
                cpu.setSP(cpu.nextWord())
                break
            case 0x32: // LD (HL-) A [----]
                cpu.ld_valHLdec_A()
                break
            case 0x33: // INC SP  [----]
                cpu.inc_reg16(R16.SP)
                break
            case 0x34: // INC (HL)  [Z0H-]
                cpu.inc_valHL()
                break
            case 0x35: // DEC (HL)  [Z1H-]
                cpu.dec_valHL()
                break
            case 0x36: // LD (HL) d8 [----]
                cpu.mmu.wb(cpu.getHL(), cpu.nextByte())
                break
            case 0x37: // SCF   [-001]
                cpu.scf()
                break

            case 0x38: // JR C r8 [----]
                if (cpu.F.c) cpu.jr(int8(cpu.nextByte()))
                break
            case 0x39: // ADD HL SP [-0HC]
                cpu.add_HL_reg16(R16.SP)
                break
            case 0x3a: // LD A (HL-) [----]
                cpu.ld_A_valHLdec()
                break
            case 0x3b: // DEC SP  [----]
                cpu.dec_reg16(R16.SP)
                break
            case 0x3c: // INC A  [Z0H-]
                cpu.inc_reg8(R8.A)
                break
            case 0x3d: // DEC A  [Z1H-]
                cpu.dec_reg8(R8.A)
                break
            case 0x3e: // LD A d8 [----]
                cpu.A = cpu.nextByte()
                break
            case 0x3f: // CCF   [-00C]
                cpu.ccf()
                break



            case 0x40: // LD B B [----]
                cpu.B = cpu.B
                break
            case 0x41: // LD B C [----]
                cpu.B = cpu.C
                break
            case 0x42: // LD B D [----]
                cpu.B = cpu.D
                break
            case 0x43: // LD B E [----]
                cpu.B = cpu.E
                break
            case 0x44: // LD B H [----]
                cpu.B = cpu.H
                break
            case 0x45: // LD B L [----]
                cpu.B = cpu.L
                break
            case 0x46: // LD B (HL) [----]
                cpu.B = cpu.mmu.rb(cpu.getHL())
                break
            case 0x47: // LD B A [----]
                cpu.B = cpu.A
                break

            case 0x48: // LD C B [----]
                cpu.C = cpu.B
                break
            case 0x49: // LD C C [----]
                cpu.C = cpu.C
                break
            case 0x4a: // LD C D [----]
                cpu.C = cpu.D
                break
            case 0x4b: // LD C E [----]
                cpu.C = cpu.E
                break
            case 0x4c: // LD C H [----]
                cpu.C = cpu.H
                break
            case 0x4d: // LD C L [----]
                cpu.C = cpu.L
                break
            case 0x4e: // LD C (HL) [----]
                cpu.C = cpu.mmu.rb(cpu.getHL())
                break
            case 0x4f: // LD C A [----]
                cpu.C = cpu.A
                break



            case 0x50: // LD D B [----]
                cpu.D = cpu.B
                break
            case 0x51: // LD D C [----]
                cpu.D = cpu.C
                break
            case 0x52: // LD D D [----]
                cpu.D = cpu.D
                break
            case 0x53: // LD D E [----]
                cpu.D = cpu.E
                break
            case 0x54: // LD D H [----]
                cpu.D = cpu.H
                break
            case 0x55: // LD D L [----]
                cpu.D = cpu.L
                break
            case 0x56: // LD D (HL) [----]
                cpu.D = cpu.mmu.rb(cpu.getHL())
                break
            case 0x57: // LD D A [----]
                cpu.D = cpu.A
                break

            case 0x58: // LD E B [----]
                cpu.E = cpu.B
                break
            case 0x59: // LD E C [----]
                cpu.E = cpu.C
                break
            case 0x5a: // LD E D [----]
                cpu.E = cpu.D
                break
            case 0x5b: // LD E E [----]
                cpu.E = cpu.E
                break
            case 0x5c: // LD E H [----]
                cpu.E = cpu.H
                break
            case 0x5d: // LD E L [----]
                cpu.E = cpu.L
                break
            case 0x5e: // LD E (HL) [----]
                cpu.E = cpu.mmu.rb(cpu.getHL())
                break
            case 0x5f: // LD E A [----]
                cpu.E = cpu.A
                break



            case 0x60: // LD H B [----]
                cpu.H = cpu.B
                break
            case 0x61: // LD H C [----]
                cpu.H = cpu.C
                break
            case 0x62: // LD H D [----]
                cpu.H = cpu.D
                break
            case 0x63: // LD H E [----]
                cpu.H = cpu.E
                break
            case 0x64: // LD H H [----]
                cpu.H = cpu.H
                break
            case 0x65: // LD H L [----]
                cpu.H = cpu.L
                break
            case 0x66: // LD H (HL) [----]
                cpu.H = cpu.mmu.rb(cpu.getHL())
                break
            case 0x67: // LD H A [----]
                cpu.H = cpu.A
                break

            case 0x68: // LD L B [----]
                cpu.L = cpu.B
                break
            case 0x69: // LD L C [----]
                cpu.L = cpu.C
                break
            case 0x6a: // LD L D [----]
                cpu.L = cpu.D
                break
            case 0x6b: // LD L E [----]
                cpu.L = cpu.E
                break
            case 0x6c: // LD L H [----]
                cpu.L = cpu.H
                break
            case 0x6d: // LD L L [----]
                cpu.L = cpu.L
                break
            case 0x6e: // LD L (HL) [----]
                cpu.L = cpu.mmu.rb(cpu.getHL())
                break
            case 0x6f: // LD L A [----]
                cpu.L = cpu.A
                break



            case 0x70: // LD (HL) B [----]
                cpu.mmu.wb(cpu.getHL(), cpu.B)
                break
            case 0x71: // LD (HL) C [----]
                cpu.mmu.wb(cpu.getHL(), cpu.C)
                break
            case 0x72: // LD (HL) D [----]
                cpu.mmu.wb(cpu.getHL(), cpu.D)
                break
            case 0x73: // LD (HL) E [----]
                cpu.mmu.wb(cpu.getHL(), cpu.E)
                break
            case 0x74: // LD (HL) H [----]
                cpu.mmu.wb(cpu.getHL(), cpu.H)
                break
            case 0x75: // LD (HL) L [----]
                cpu.mmu.wb(cpu.getHL(), cpu.L)
                break
            case 0x76: // HALT   [----]
                cpu.halt()
                break
            case 0x77: // LD (HL) A [----]
                cpu.mmu.wb(cpu.getHL(), cpu.A)
                break

            case 0x78: // LD A B [----]
                cpu.A = cpu.B
                break
            case 0x79: // LD A C [----]
                cpu.A = cpu.C
                break
            case 0x7a: // LD A D [----]
                cpu.A = cpu.D
                break
            case 0x7b: // LD A E [----]
                cpu.A = cpu.E
                break
            case 0x7c: // LD A H [----]
                cpu.A = cpu.H
                break
            case 0x7d: // LD A L [----]
                cpu.A = cpu.L
                break
            case 0x7e: // LD A (HL) [----]
                cpu.A = cpu.mmu.rb(cpu.getHL())
                break
            case 0x7f: // LD A A [----]
                cpu.A = cpu.A
                break



            case 0x80: // ADD A B [Z0HC]
                cpu.add(cpu.B)
                break
            case 0x81: // ADD A C [Z0HC]
                cpu.add(cpu.C)
                break
            case 0x82: // ADD A D [Z0HC]
                cpu.add(cpu.D)
                break
            case 0x83: // ADD A E [Z0HC]
                cpu.add(cpu.E)
                break
            case 0x84: // ADD A H [Z0HC]
                cpu.add(cpu.H)
                break
            case 0x85: // ADD A L [Z0HC]
                cpu.add(cpu.L)
                break
            case 0x86: // ADD A (HL) [Z0HC]
                cpu.add(cpu.mmu.rb(cpu.getHL()))
                break
            case 0x87: // ADD A A [Z0HC]
                cpu.add(cpu.A)
                break

            case 0x88: // ADC A B [Z0HC]
                cpu.adc(cpu.B)
                break
            case 0x89: // ADC A C [Z0HC]
                cpu.adc(cpu.C)
                break
            case 0x8a: // ADC A D [Z0HC]
                cpu.adc(cpu.D)
                break
            case 0x8b: // ADC A E [Z0HC]
                cpu.adc(cpu.E)
                break
            case 0x8c: // ADC A H [Z0HC]
                cpu.adc(cpu.H)
                break
            case 0x8d: // ADC A L [Z0HC]
                cpu.adc(cpu.L)
                break
            case 0x8e: // ADC A (HL) [Z0HC]
                cpu.adc(cpu.mmu.rb(cpu.getHL()))
                break
            case 0x8f: // ADC A A [Z0HC]
                cpu.adc(cpu.A)
                break



            case 0x90: // SUB B  [Z1HC]
                cpu.sub(cpu.B)
                break
            case 0x91: // SUB C  [Z1HC]
                cpu.sub(cpu.C)
                break
            case 0x92: // SUB D  [Z1HC]
                cpu.sub(cpu.D)
                break
            case 0x93: // SUB E  [Z1HC]
                cpu.sub(cpu.E)
                break
            case 0x94: // SUB H  [Z1HC]
                cpu.sub(cpu.H)
                break
            case 0x95: // SUB L  [Z1HC]
                cpu.sub(cpu.L)
                break
            case 0x96: // SUB (HL)  [Z1HC]
                cpu.sub(cpu.mmu.rb(cpu.getHL()))
                break
            case 0x97: // SUB A  [Z1HC]
                cpu.sub(cpu.A)
                break

            case 0x98: // SBC A B [Z1HC]
                cpu.sbc(cpu.B)
                break
            case 0x99: // SBC A C [Z1HC]
                cpu.sbc(cpu.C)
                break
            case 0x9a: // SBC A D [Z1HC]
                cpu.sbc(cpu.D)
                break
            case 0x9b: // SBC A E [Z1HC]
                cpu.sbc(cpu.E)
                break
            case 0x9c: // SBC A H [Z1HC]
                cpu.sbc(cpu.H)
                break
            case 0x9d: // SBC A L [Z1HC]
                cpu.sbc(cpu.L)
                break
            case 0x9e: // SBC A (HL) [Z1HC]
                cpu.sbc(cpu.mmu.rb(cpu.getHL()))
                break
            case 0x9f: // SBC A A [Z1HC]
                cpu.sbc(cpu.A)
                break



            case 0xa0: // AND B  [Z010]
                cpu.and(cpu.B)
                break
            case 0xa1: // AND C  [Z010]
                cpu.and(cpu.C)
                break
            case 0xa2: // AND D  [Z010]
                cpu.and(cpu.D)
                break
            case 0xa3: // AND E  [Z010]
                cpu.and(cpu.E)
                break
            case 0xa4: // AND H  [Z010]
                cpu.and(cpu.H)
                break
            case 0xa5: // AND L  [Z010]
                cpu.and(cpu.L)
                break
            case 0xa6: // AND (HL)  [Z010]
                cpu.and(cpu.mmu.rb(cpu.getHL()))
                break
            case 0xa7: // AND A  [Z010]
                cpu.and(cpu.A)
                break

            case 0xa8: // XOR B  [Z000]
                cpu.xor(cpu.B)
                break
            case 0xa9: // XOR C  [Z000]
                cpu.xor(cpu.C)
                break
            case 0xaa: // XOR D  [Z000]
                cpu.xor(cpu.D)
                break
            case 0xab: // XOR E  [Z000]
                cpu.xor(cpu.E)
                break
            case 0xac: // XOR H  [Z000]
                cpu.xor(cpu.H)
                break
            case 0xad: // XOR L  [Z000]
                cpu.xor(cpu.L)
                break
            case 0xae: // XOR (HL)  [Z000]
                cpu.xor(cpu.mmu.rb(cpu.getHL()))
                break
            case 0xaf: // XOR A  [Z000]
                cpu.xor(cpu.A)
                break



            case 0xb0: // OR B  [Z000]
                cpu.or(cpu.B)
                break
            case 0xb1: // OR C  [Z000]
                cpu.or(cpu.C)
                break
            case 0xb2: // OR D  [Z000]
                cpu.or(cpu.D)
                break
            case 0xb3: // OR E  [Z000]
                cpu.or(cpu.E)
                break
            case 0xb4: // OR H  [Z000]
                cpu.or(cpu.H)
                break
            case 0xb5: // OR L  [Z000]
                cpu.or(cpu.L)
                break
            case 0xb6: // OR (HL)  [Z000]
                cpu.or(cpu.mmu.rb(cpu.getHL()))
                break
            case 0xb7: // OR A  [Z000]
                cpu.or(cpu.A)
                break

            case 0xb8: // CP B  [Z1HC]
                cpu.cp(cpu.B)
                break
            case 0xb9: // CP C  [Z1HC]
                cpu.cp(cpu.C)
                break
            case 0xba: // CP D  [Z1HC]
                cpu.cp(cpu.D)
                break
            case 0xbb: // CP E  [Z1HC]
                cpu.cp(cpu.E)
                break
            case 0xbc: // CP H  [Z1HC]
                cpu.cp(cpu.H)
                break
            case 0xbd: // CP L  [Z1HC]
                cpu.cp(cpu.L)
                break
            case 0xbe: // CP (HL)  [Z1HC]
                cpu.cp(cpu.mmu.rb(cpu.getHL()))
                break
            case 0xbf: // CP A  [Z1HC]
                cpu.cp(cpu.A)
                break



            case 0xc0: // RET NZ  [----]
                if (!cpu.F.z) cpu.ret()
                break
            case 0xc1: // POP BC  [----]
                cpu.pop(R16.BC)
                break
            case 0xc2: // JP NZ a16 [----]
                if (!cpu.F.z) cpu.jp(cpu.nextWord())
                break
            case 0xc3: // JP a16  [----]
                cpu.jp(cpu.nextWord())
                break
            case 0xc4: // CALL NZ a16 [----]
                if (!cpu.F.z) cpu.call(cpu.nextWord())
                break
            case 0xc5: // PUSH BC  [----]
                cpu.push(R16.BC)
                break
            case 0xc6: // ADD A d8 [Z0HC]
                cpu.add(cpu.nextByte())
                break
            case 0xc7: // RST 00H  [----]
                cpu.rst(RSTVector.$00)
                break

            case 0xc8: // RET Z  [----]
                if (cpu.F.z) cpu.ret()
                break
            case 0xc9: // RET   [----]
                cpu.ret()
                break
            case 0xca: // JP Z a16 [----]
                if (cpu.F.z) cpu.jp(cpu.nextWord())
                break
            case 0xcb: // PREFIX CB  [----]
                
                break
            case 0xcc: // CALL Z a16 [----]
                if (cpu.F.z) cpu.call(cpu.nextWord())
                break
            case 0xcd: // CALL a16  [----]
                cpu.call(cpu.nextWord())
                break
            case 0xce: // ADC A d8 [Z0HC]
                cpu.adc(cpu.nextByte())
                break
            case 0xcf: // RST 08H  [----]
                cpu.rst(RSTVector.$08)
                break



            case 0xd0: // RET NC  [----]
                if (!cpu.F.c) cpu.ret()
                break
            case 0xd1: // POP DE  [----]
                cpu.pop(R16.DE)
                break
            case 0xd2: // JP NC a16 [----]
                if (!cpu.F.c) cpu.jp(cpu.nextWord())
                break
            case 0xd4: // CALL NC a16 [----]
                if (!cpu.F.c) cpu.call(cpu.nextWord())
                break
            case 0xd5: // PUSH DE  [----]
                cpu.push(R16.DE)
                break
            case 0xd6: // SUB d8  [Z1HC]
                cpu.sub(cpu.nextByte())
                break
            case 0xd7: // RST 10H  [----]
                cpu.rst(RSTVector.$10)
                break

            case 0xd8: // RET C  [----]
                if (cpu.F.c) cpu.ret()
                break
            case 0xd9: // RETI   [----]
                cpu.reti()
                break
            case 0xda: // JP C a16 [----]
                if (cpu.F.c) cpu.jp(cpu.nextWord())
                break
            case 0xdc: // CALL C a16 [----]
                if (cpu.F.c) cpu.call(cpu.nextWord())
                break
            case 0xde: // SBC A d8 [Z1HC]
                cpu.sbc(cpu.nextByte())
                break
            case 0xdf: // RST 18H  [----]
                cpu.rst(RSTVector.$18)
                break



            case 0xe0: // LDH (a8) A [----]
                
                break
            case 0xe1: // POP HL  [----]
                cpu.pop(R16.HL)
                break
            case 0xe2: // LD (C) A [----]
                
                break
            case 0xe5: // PUSH HL  [----]
                cpu.push(R16.HL)
                break
            case 0xe6: // AND d8  [Z010]
                cpu.and(cpu.nextByte())
                break
            case 0xe7: // RST 20H  [----]
                cpu.rst(RSTVector.$20)
                break

            case 0xe8: // ADD SP r8 [00HC]
                cpu.add_SP_r8()
                break
            case 0xe9: // JP (HL)  [----]
                
                break
            case 0xea: // LD (a16) A [----]
                
                break
            case 0xee: // XOR d8  [Z000]
                cpu.xor(cpu.nextByte())
                break
            case 0xef: // RST 28H  [----]
                cpu.rst(RSTVector.$28)
                break



            case 0xf0: // LDH A (a8) [----]
                
                break
            case 0xf1: // POP AF  [ZNHC]
                cpu.pop_AF()
                break
            case 0xf2: // LD A (C) [----]
                
                break
            case 0xf3: // DI   [----]
                
                break
            case 0xf5: // PUSH AF  [----]
                cpu.push_AF()
                break
            case 0xf6: // OR d8  [Z000]
                cpu.or(cpu.nextByte())
                break
            case 0xf7: // RST 30H  [----]
                cpu.rst(RSTVector.$30)
                break

            case 0xf8: // LD HL SP+r8 [00HC]
                cpu.ld_HL_SPplusInt8()
                break
            case 0xf9: // LD SP HL [----]
                cpu.SP = cpu.HL
                break
            case 0xfa: // LD A (a16) [----]
                
                break
            case 0xfb: // EI   [----]
                
                break
            case 0xfe: // CP d8  [Z1HC]
                cpu.cp(cpu.nextByte())
                break
            case 0xff: // RST 38H  [----]
                cpu.rst(RSTVector.$38)
                break



        }

    } else {
        switch(instr) {
            
            case 0x0: // RLC B  [Z00C]
                cpu.rlc_r8(R8.B)
                break
            case 0x1: // RLC C  [Z00C]
                cpu.rlc_r8(R8.C)
                break
            case 0x2: // RLC D  [Z00C]
                cpu.rlc_r8(R8.D)
                break
            case 0x3: // RLC E  [Z00C]
                cpu.rlc_r8(R8.E)
                break
            case 0x4: // RLC H  [Z00C]
                cpu.rlc_r8(R8.H)
                break
            case 0x5: // RLC L  [Z00C]
                cpu.rlc_r8(R8.L)
                break
            case 0x6: // RLC (HL)  [Z00C]
                cpu.rlc_valHL()
                break
            case 0x7: // RLC A  [Z00C]
                cpu.rlc_r8(R8.A)
                break

            case 0x8: // RRC B  [Z00C]
                cpu.rrc_r8(R8.B)
                break
            case 0x9: // RRC C  [Z00C]
                cpu.rrc_r8(R8.C)
                break
            case 0xa: // RRC D  [Z00C]
                cpu.rrc_r8(R8.D)
                break
            case 0xb: // RRC E  [Z00C]
                cpu.rrc_r8(R8.E)
                break
            case 0xc: // RRC H  [Z00C]
                cpu.rrc_r8(R8.H)
                break
            case 0xd: // RRC L  [Z00C]
                cpu.rrc_r8(R8.L)
                break
            case 0xe: // RRC (HL)  [Z00C]
                cpu.rrc_valHL()
                break
            case 0xf: // RRC A  [Z00C]
                cpu.rrc_r8(R8.A)
                break



            case 0x10: // RL B  [Z00C]
                cpu.rl_r8(R8.B)
                break
            case 0x11: // RL C  [Z00C]
                cpu.rl_r8(R8.C)
                break
            case 0x12: // RL D  [Z00C]
                cpu.rl_r8(R8.D)
                break
            case 0x13: // RL E  [Z00C]
                cpu.rl_r8(R8.E)
                break
            case 0x14: // RL H  [Z00C]
                cpu.rl_r8(R8.H)
                break
            case 0x15: // RL L  [Z00C]
                cpu.rl_r8(R8.L)
                break
            case 0x16: // RL (HL)  [Z00C]
                cpu.rl_valHL()
                break
            case 0x17: // RL A  [Z00C]
                cpu.rl_r8(R8.A)
                break

            case 0x18: // RR B  [Z00C]
                cpu.rr_r8(R8.B)
                break
            case 0x19: // RR C  [Z00C]
                cpu.rr_r8(R8.C)
                break
            case 0x1a: // RR D  [Z00C]
                cpu.rr_r8(R8.D)
                break
            case 0x1b: // RR E  [Z00C]
                cpu.rr_r8(R8.E)
                break
            case 0x1c: // RR H  [Z00C]
                cpu.rr_r8(R8.H)
                break
            case 0x1d: // RR L  [Z00C]
                cpu.rr_r8(R8.L)
                break
            case 0x1e: // RR (HL)  [Z00C]
                cpu.rr_valHL()
                break
            case 0x1f: // RR A  [Z00C]
                cpu.rr_r8(R8.A)
                break



            case 0x20: // SLA B  [Z00C]
                cpu.sla_r8(R8.B)
                break
            case 0x21: // SLA C  [Z00C]
                cpu.sla_r8(R8.C)
                break
            case 0x22: // SLA D  [Z00C]
                cpu.sla_r8(R8.D)
                break
            case 0x23: // SLA E  [Z00C]
                cpu.sla_r8(R8.E)
                break
            case 0x24: // SLA H  [Z00C]
                cpu.sla_r8(R8.H)
                break
            case 0x25: // SLA L  [Z00C]
                cpu.sla_r8(R8.L)
                break
            case 0x26: // SLA (HL)  [Z00C]
                cpu.sla_valHL()
                break
            case 0x27: // SLA A  [Z00C]
                cpu.sla_r8(R8.A)
                break

            case 0x28: // SRA B  [Z000]
                cpu.sra_r8(R8.B)
                break
            case 0x29: // SRA C  [Z000]
                cpu.sra_r8(R8.C)
                break
            case 0x2a: // SRA D  [Z000]
                cpu.sra_r8(R8.D)
                break
            case 0x2b: // SRA E  [Z000]
                cpu.sra_r8(R8.E)
                break
            case 0x2c: // SRA H  [Z000]
                cpu.sra_r8(R8.H)
                break
            case 0x2d: // SRA L  [Z000]
                cpu.sra_r8(R8.L)
                break
            case 0x2e: // SRA (HL)  [Z000]
                cpu.sra_valHL()
                break
            case 0x2f: // SRA A  [Z000]
                cpu.sra_r8(R8.A)
                break



            case 0x30: // SWAP B  [Z000]
                cpu.swap_r8(R8.B)
                break
            case 0x31: // SWAP C  [Z000]
                cpu.swap_r8(R8.C)
                break
            case 0x32: // SWAP D  [Z000]
                cpu.swap_r8(R8.D)
                break
            case 0x33: // SWAP E  [Z000]
                cpu.swap_r8(R8.E)
                break
            case 0x34: // SWAP H  [Z000]
                cpu.swap_r8(R8.H)
                break
            case 0x35: // SWAP L  [Z000]
                cpu.swap_r8(R8.L)
                break
            case 0x36: // SWAP (HL)  [Z000]
                cpu.swap_valHL()
                break
            case 0x37: // SWAP A  [Z000]
                cpu.swap_r8(R8.A)
                break

            case 0x38: // SRL B  [Z00C]
                cpu.srl_r8(R8.B)
                break
            case 0x39: // SRL C  [Z00C]
                cpu.srl_r8(R8.C)
                break
            case 0x3a: // SRL D  [Z00C]
                cpu.srl_r8(R8.D)
                break
            case 0x3b: // SRL E  [Z00C]
                cpu.srl_r8(R8.E)
                break
            case 0x3c: // SRL H  [Z00C]
                cpu.srl_r8(R8.H)
                break
            case 0x3d: // SRL L  [Z00C]
                cpu.srl_r8(R8.L)
                break
            case 0x3e: // SRL (HL)  [Z00C]
                cpu.srl_valHL()
                break
            case 0x3f: // SRL A  [Z00C]
                cpu.srl_r8(R8.A)
                break



            case 0x40: // BIT 0 B [Z01-]
                cpu.bit_n_r8(0, R8.B)
                break
            case 0x41: // BIT 0 C [Z01-]
                cpu.bit_n_r8(0, R8.C)
                break
            case 0x42: // BIT 0 D [Z01-]
                cpu.bit_n_r8(0, R8.D)
                break
            case 0x43: // BIT 0 E [Z01-]
                cpu.bit_n_r8(0, R8.E)
                break
            case 0x44: // BIT 0 H [Z01-]
                cpu.bit_n_r8(0, R8.H)
                break
            case 0x45: // BIT 0 L [Z01-]
                cpu.bit_n_r8(0, R8.L)
                break
            case 0x46: // BIT 0 (HL) [Z01-]
                cpu.bit_n_valHL(0)
                break
            case 0x47: // BIT 0 A [Z01-]
                cpu.bit_n_r8(0, R8.A)
                break

            case 0x48: // BIT 1 B [Z01-]
                cpu.bit_n_r8(1, R8.B)
                break
            case 0x49: // BIT 1 C [Z01-]
                cpu.bit_n_r8(1, R8.C)
                break
            case 0x4a: // BIT 1 D [Z01-]
                cpu.bit_n_r8(1, R8.D)
                break
            case 0x4b: // BIT 1 E [Z01-]
                cpu.bit_n_r8(1, R8.E)
                break
            case 0x4c: // BIT 1 H [Z01-]
                cpu.bit_n_r8(1, R8.H)
                break
            case 0x4d: // BIT 1 L [Z01-]
                cpu.bit_n_r8(1, R8.L)
                break
            case 0x4e: // BIT 1 (HL) [Z01-]
                cpu.bit_n_valHL(1)
                break
            case 0x4f: // BIT 1 A [Z01-]
                cpu.bit_n_r8(1, R8.A)
                break



            case 0x50: // BIT 2 B [Z01-]
                cpu.bit_n_r8(2, R8.B)
                break
            case 0x51: // BIT 2 C [Z01-]
                cpu.bit_n_r8(2, R8.C)
                break
            case 0x52: // BIT 2 D [Z01-]
                cpu.bit_n_r8(2, R8.D)
                break
            case 0x53: // BIT 2 E [Z01-]
                cpu.bit_n_r8(2, R8.E)
                break
            case 0x54: // BIT 2 H [Z01-]
                cpu.bit_n_r8(2, R8.H)
                break
            case 0x55: // BIT 2 L [Z01-]
                cpu.bit_n_r8(2, R8.L)
                break
            case 0x56: // BIT 2 (HL) [Z01-]
                cpu.bit_n_valHL(2)
                break
            case 0x57: // BIT 2 A [Z01-]
                cpu.bit_n_r8(2, R8.A)
                break

            case 0x58: // BIT 3 B [Z01-]
                cpu.bit_n_r8(3, R8.B)
                break
            case 0x59: // BIT 3 C [Z01-]
                cpu.bit_n_r8(3, R8.C)
                break
            case 0x5a: // BIT 3 D [Z01-]
                cpu.bit_n_r8(3, R8.D)
                break
            case 0x5b: // BIT 3 E [Z01-]
                cpu.bit_n_r8(3, R8.E)
                break
            case 0x5c: // BIT 3 H [Z01-]
                cpu.bit_n_r8(3, R8.H)
                break
            case 0x5d: // BIT 3 L [Z01-]
                cpu.bit_n_r8(3, R8.L)
                break
            case 0x5e: // BIT 3 (HL) [Z01-]
                cpu.bit_n_valHL(3)
                break
            case 0x5f: // BIT 3 A [Z01-]
                cpu.bit_n_r8(3, R8.A)
                break



            case 0x60: // BIT 4 B [Z01-]
                cpu.bit_n_r8(4, R8.B)
                break
            case 0x61: // BIT 4 C [Z01-]
                cpu.bit_n_r8(4, R8.C)
                break
            case 0x62: // BIT 4 D [Z01-]
                cpu.bit_n_r8(4, R8.D)
                break
            case 0x63: // BIT 4 E [Z01-]
                cpu.bit_n_r8(4, R8.E)
                break
            case 0x64: // BIT 4 H [Z01-]
                cpu.bit_n_r8(4, R8.H)
                break
            case 0x65: // BIT 4 L [Z01-]
                cpu.bit_n_r8(4, R8.L)
                break
            case 0x66: // BIT 4 (HL) [Z01-]
                cpu.bit_n_valHL(4)
                break
            case 0x67: // BIT 4 A [Z01-]
                cpu.bit_n_r8(4, R8.A)
                break

            case 0x68: // BIT 5 B [Z01-]
                cpu.bit_n_r8(5, R8.B)
                break
            case 0x69: // BIT 5 C [Z01-]
                cpu.bit_n_r8(5, R8.C)
                break
            case 0x6a: // BIT 5 D [Z01-]
                cpu.bit_n_r8(5, R8.D)
                break
            case 0x6b: // BIT 5 E [Z01-]
                cpu.bit_n_r8(5, R8.E)
                break
            case 0x6c: // BIT 5 H [Z01-]
                cpu.bit_n_r8(5, R8.H)
                break
            case 0x6d: // BIT 5 L [Z01-]
                cpu.bit_n_r8(5, R8.L)
                break
            case 0x6e: // BIT 5 (HL) [Z01-]
                cpu.bit_n_valHL(5)
                break
            case 0x6f: // BIT 5 A [Z01-]
                cpu.bit_n_r8(5, R8.A)
                break



            case 0x70: // BIT 6 B [Z01-]
                cpu.bit_n_r8(6, R8.B)
                break
            case 0x71: // BIT 6 C [Z01-]
                cpu.bit_n_r8(6, R8.C)
                break
            case 0x72: // BIT 6 D [Z01-]
                cpu.bit_n_r8(6, R8.D)
                break
            case 0x73: // BIT 6 E [Z01-]
                cpu.bit_n_r8(6, R8.E)
                break
            case 0x74: // BIT 6 H [Z01-]
                cpu.bit_n_r8(6, R8.H)
                break
            case 0x75: // BIT 6 L [Z01-]
                cpu.bit_n_r8(6, R8.L)
                break
            case 0x76: // BIT 6 (HL) [Z01-]
                cpu.bit_n_valHL(6)
                break
            case 0x77: // BIT 6 A [Z01-]
                cpu.bit_n_r8(6, R8.A)
                break

            case 0x78: // BIT 7 B [Z01-]
                cpu.bit_n_r8(7, R8.B)
                break
            case 0x79: // BIT 7 C [Z01-]
                cpu.bit_n_r8(7, R8.C)
                break
            case 0x7a: // BIT 7 D [Z01-]
                cpu.bit_n_r8(7, R8.D)
                break
            case 0x7b: // BIT 7 E [Z01-]
                cpu.bit_n_r8(7, R8.E)
                break
            case 0x7c: // BIT 7 H [Z01-]
                cpu.bit_n_r8(7, R8.H)
                break
            case 0x7d: // BIT 7 L [Z01-]
                cpu.bit_n_r8(7, R8.L)
                break
            case 0x7e: // BIT 7 (HL) [Z01-]
                cpu.bit_n_valHL(7)
                break
            case 0x7f: // BIT 7 A [Z01-]
                cpu.bit_n_r8(7, R8.A)
                break



            case 0x80: // RES 0 B [----]
                cpu.res_n_r8(0, R8.B)
                break
            case 0x81: // RES 0 C [----]
                cpu.res_n_r8(0, R8.C)
                break
            case 0x82: // RES 0 D [----]
                cpu.res_n_r8(0, R8.D)
                break
            case 0x83: // RES 0 E [----]
                cpu.res_n_r8(0, R8.E)
                break
            case 0x84: // RES 0 H [----]
                cpu.res_n_r8(0, R8.H)
                break
            case 0x85: // RES 0 L [----]
                cpu.res_n_r8(0, R8.L)
                break
            case 0x86: // RES 0 (HL) [----]
                cpu.res_n_valHL(0)
                break
            case 0x87: // RES 0 A [----]
                cpu.res_n_r8(0, R8.A)
                break

            case 0x88: // RES 1 B [----]
                cpu.res_n_r8(1, R8.B)
                break
            case 0x89: // RES 1 C [----]
                cpu.res_n_r8(1, R8.C)
                break
            case 0x8a: // RES 1 D [----]
                cpu.res_n_r8(1, R8.D)
                break
            case 0x8b: // RES 1 E [----]
                cpu.res_n_r8(1, R8.E)
                break
            case 0x8c: // RES 1 H [----]
                cpu.res_n_r8(1, R8.H)
                break
            case 0x8d: // RES 1 L [----]
                cpu.res_n_r8(1, R8.L)
                break
            case 0x8e: // RES 1 (HL) [----]
                cpu.res_n_valHL(1)
                break
            case 0x8f: // RES 1 A [----]
                cpu.res_n_r8(1, R8.A)
                break



            case 0x90: // RES 2 B [----]
                cpu.res_n_r8(2, R8.B)
                break
            case 0x91: // RES 2 C [----]
                cpu.res_n_r8(2, R8.C)
                break
            case 0x92: // RES 2 D [----]
                cpu.res_n_r8(2, R8.D)
                break
            case 0x93: // RES 2 E [----]
                cpu.res_n_r8(2, R8.E)
                break
            case 0x94: // RES 2 H [----]
                cpu.res_n_r8(2, R8.H)
                break
            case 0x95: // RES 2 L [----]
                cpu.res_n_r8(2, R8.L)
                break
            case 0x96: // RES 2 (HL) [----]
                cpu.res_n_valHL(2)
                break
            case 0x97: // RES 2 A [----]
                cpu.res_n_r8(2, R8.A)
                break

            case 0x98: // RES 3 B [----]
                cpu.res_n_r8(3, R8.B)
                break
            case 0x99: // RES 3 C [----]
                cpu.res_n_r8(3, R8.C)
                break
            case 0x9a: // RES 3 D [----]
                cpu.res_n_r8(3, R8.D)
                break
            case 0x9b: // RES 3 E [----]
                cpu.res_n_r8(3, R8.E)
                break
            case 0x9c: // RES 3 H [----]
                cpu.res_n_r8(3, R8.H)
                break
            case 0x9d: // RES 3 L [----]
                cpu.res_n_r8(3, R8.L)
                break
            case 0x9e: // RES 3 (HL) [----]
                cpu.res_n_valHL(3)
                break
            case 0x9f: // RES 3 A [----]
                cpu.res_n_r8(3, R8.A)
                break



            case 0xa0: // RES 4 B [----]
                cpu.res_n_r8(4, R8.B)
                break
            case 0xa1: // RES 4 C [----]
                cpu.res_n_r8(4, R8.C)
                break
            case 0xa2: // RES 4 D [----]
                cpu.res_n_r8(4, R8.D)
                break
            case 0xa3: // RES 4 E [----]
                cpu.res_n_r8(4, R8.E)
                break
            case 0xa4: // RES 4 H [----]
                cpu.res_n_r8(4, R8.H)
                break
            case 0xa5: // RES 4 L [----]
                cpu.res_n_r8(4, R8.L)
                break
            case 0xa6: // RES 4 (HL) [----]
                cpu.res_n_valHL(4)
                break
            case 0xa7: // RES 4 A [----]
                cpu.res_n_r8(4, R8.A)
                break

            case 0xa8: // RES 5 B [----]
                cpu.res_n_r8(5, R8.B)
                break
            case 0xa9: // RES 5 C [----]
                cpu.res_n_r8(5, R8.C)
                break
            case 0xaa: // RES 5 D [----]
                cpu.res_n_r8(5, R8.D)
                break
            case 0xab: // RES 5 E [----]
                cpu.res_n_r8(5, R8.E)
                break
            case 0xac: // RES 5 H [----]
                cpu.res_n_r8(5, R8.H)
                break
            case 0xad: // RES 5 L [----]
                cpu.res_n_r8(5, R8.L)
                break
            case 0xae: // RES 5 (HL) [----]
                cpu.res_n_valHL(5)
                break
            case 0xaf: // RES 5 A [----]
                cpu.res_n_r8(5, R8.A)
                break



            case 0xb0: // RES 6 B [----]
                cpu.res_n_r8(6, R8.B)
                break
            case 0xb1: // RES 6 C [----]
                cpu.res_n_r8(6, R8.C)
                break
            case 0xb2: // RES 6 D [----]
                cpu.res_n_r8(6, R8.D)
                break
            case 0xb3: // RES 6 E [----]
                cpu.res_n_r8(6, R8.E)
                break
            case 0xb4: // RES 6 H [----]
                cpu.res_n_r8(6, R8.H)
                break
            case 0xb5: // RES 6 L [----]
                cpu.res_n_r8(6, R8.L)
                break
            case 0xb6: // RES 6 (HL) [----]
                cpu.res_n_valHL(6)
                break
            case 0xb7: // RES 6 A [----]
                cpu.res_n_r8(6, R8.A)
                break

            case 0xb8: // RES 7 B [----]
                cpu.res_n_r8(7, R8.B)
                break
            case 0xb9: // RES 7 C [----]
                cpu.res_n_r8(7, R8.C)
                break
            case 0xba: // RES 7 D [----]
                cpu.res_n_r8(7, R8.D)
                break
            case 0xbb: // RES 7 E [----]
                cpu.res_n_r8(7, R8.E)
                break
            case 0xbc: // RES 7 H [----]
                cpu.res_n_r8(7, R8.H)
                break
            case 0xbd: // RES 7 L [----]
                cpu.res_n_r8(7, R8.L)
                break
            case 0xbe: // RES 7 (HL) [----]
                cpu.res_n_valHL(7)
                break
            case 0xbf: // RES 7 A [----]
                cpu.res_n_r8(7, R8.A)
                break



            case 0xc0: // SET 0 B [----]
                cpu.set_n_r8(0, R8.B)
                break
            case 0xc1: // SET 0 C [----]
                cpu.set_n_r8(0, R8.C)
                break
            case 0xc2: // SET 0 D [----]
                cpu.set_n_r8(0, R8.D)
                break
            case 0xc3: // SET 0 E [----]
                cpu.set_n_r8(0, R8.E)
                break
            case 0xc4: // SET 0 H [----]
                cpu.set_n_r8(0, R8.H)
                break
            case 0xc5: // SET 0 L [----]
                cpu.set_n_r8(0, R8.L)
                break
            case 0xc6: // SET 0 (HL) [----]
                cpu.set_n_valHL(0)
                break
            case 0xc7: // SET 0 A [----]
                cpu.set_n_r8(0, R8.A)
                break

            case 0xc8: // SET 1 B [----]
                cpu.set_n_r8(1, R8.B)
                break
            case 0xc9: // SET 1 C [----]
                cpu.set_n_r8(1, R8.C)
                break
            case 0xca: // SET 1 D [----]
                cpu.set_n_r8(1, R8.D)
                break
            case 0xcb: // SET 1 E [----]
                cpu.set_n_r8(1, R8.E)
                break
            case 0xcc: // SET 1 H [----]
                cpu.set_n_r8(1, R8.H)
                break
            case 0xcd: // SET 1 L [----]
                cpu.set_n_r8(1, R8.L)
                break
            case 0xce: // SET 1 (HL) [----]
                cpu.set_n_valHL(1)
                break
            case 0xcf: // SET 1 A [----]
                cpu.set_n_r8(1, R8.A)
                break



            case 0xd0: // SET 2 B [----]
                cpu.set_n_r8(2, R8.B)
                break
            case 0xd1: // SET 2 C [----]
                cpu.set_n_r8(2, R8.C)
                break
            case 0xd2: // SET 2 D [----]
                cpu.set_n_r8(2, R8.D)
                break
            case 0xd3: // SET 2 E [----]
                cpu.set_n_r8(2, R8.E)
                break
            case 0xd4: // SET 2 H [----]
                cpu.set_n_r8(2, R8.H)
                break
            case 0xd5: // SET 2 L [----]
                cpu.set_n_r8(2, R8.L)
                break
            case 0xd6: // SET 2 (HL) [----]
                cpu.set_n_valHL(2)
                break
            case 0xd7: // SET 2 A [----]
                cpu.set_n_r8(2, R8.A)
                break

            case 0xd8: // SET 3 B [----]
                cpu.set_n_r8(3, R8.B)
                break
            case 0xd9: // SET 3 C [----]
                cpu.set_n_r8(3, R8.C)
                break
            case 0xda: // SET 3 D [----]
                cpu.set_n_r8(3, R8.D)
                break
            case 0xdb: // SET 3 E [----]
                cpu.set_n_r8(3, R8.E)
                break
            case 0xdc: // SET 3 H [----]
                cpu.set_n_r8(3, R8.H)
                break
            case 0xdd: // SET 3 L [----]
                cpu.set_n_r8(3, R8.L)
                break
            case 0xde: // SET 3 (HL) [----]
                cpu.set_n_valHL(3)
                break
            case 0xdf: // SET 3 A [----]
                cpu.set_n_r8(3, R8.A)
                break



            case 0xe0: // SET 4 B [----]
                cpu.set_n_r8(4, R8.B)
                break
            case 0xe1: // SET 4 C [----]
                cpu.set_n_r8(4, R8.C)
                break
            case 0xe2: // SET 4 D [----]
                cpu.set_n_r8(4, R8.D)
                break
            case 0xe3: // SET 4 E [----]
                cpu.set_n_r8(4, R8.E)
                break
            case 0xe4: // SET 4 H [----]
                cpu.set_n_r8(4, R8.H)
                break
            case 0xe5: // SET 4 L [----]
                cpu.set_n_r8(4, R8.L)
                break
            case 0xe6: // SET 4 (HL) [----]
                cpu.set_n_valHL(4)
                break
            case 0xe7: // SET 4 A [----]
                cpu.set_n_r8(4, R8.A)
                break

            case 0xe8: // SET 5 B [----]
                cpu.set_n_r8(5, R8.B)
                break
            case 0xe9: // SET 5 C [----]
                cpu.set_n_r8(5, R8.C)
                break
            case 0xea: // SET 5 D [----]
                cpu.set_n_r8(5, R8.D)
                break
            case 0xeb: // SET 5 E [----]
                cpu.set_n_r8(5, R8.E)
                break
            case 0xec: // SET 5 H [----]
                cpu.set_n_r8(5, R8.H)
                break
            case 0xed: // SET 5 L [----]
                cpu.set_n_r8(5, R8.L)
                break
            case 0xee: // SET 5 (HL) [----]
                cpu.set_n_valHL(5)
                break
            case 0xef: // SET 5 A [----]
                cpu.set_n_r8(5, R8.A)
                break



            case 0xf0: // SET 6 B [----]
                cpu.set_n_r8(6, R8.B)
                break
            case 0xf1: // SET 6 C [----]
                cpu.set_n_r8(6, R8.C)
                break
            case 0xf2: // SET 6 D [----]
                cpu.set_n_r8(6, R8.D)
                break
            case 0xf3: // SET 6 E [----]
                cpu.set_n_r8(6, R8.E)
                break
            case 0xf4: // SET 6 H [----]
                cpu.set_n_r8(6, R8.H)
                break
            case 0xf5: // SET 6 L [----]
                cpu.set_n_r8(6, R8.L)
                break
            case 0xf6: // SET 6 (HL) [----]
                cpu.set_n_valHL(6)
                break
            case 0xf7: // SET 6 A [----]
                cpu.set_n_r8(6, R8.A)
                break

            case 0xf8: // SET 7 B [----]
                cpu.set_n_r8(7, R8.B)
                break
            case 0xf9: // SET 7 C [----]
                cpu.set_n_r8(7, R8.C)
                break
            case 0xfa: // SET 7 D [----]
                cpu.set_n_r8(7, R8.D)
                break
            case 0xfb: // SET 7 E [----]
                cpu.set_n_r8(7, R8.E)
                break
            case 0xfc: // SET 7 H [----]
                cpu.set_n_r8(7, R8.H)
                break
            case 0xfd: // SET 7 L [----]
                cpu.set_n_r8(7, R8.L)
                break
            case 0xfe: // SET 7 (HL) [----]
                cpu.set_n_valHL(7)
                break
            case 0xff: // SET 7 A [----]
                cpu.set_n_r8(7, R8.A)
                break



        }
    }
}
