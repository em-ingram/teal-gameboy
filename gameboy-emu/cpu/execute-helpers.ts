import { CPU, Reg16, Reg8, RSTVector} from './cpu';
import {
    uint8,
    uint16,
    int8,
    addCarriesByte,
    subCarriesByte,
    addHalfCarriesByte,
    subHalfCarriesByte,
    addCarriesWord,
    addHalfCarriesWord
} from '../utils'


// INC r16 [----]
export const inc_reg16 = (cpu: CPU, reg16: Reg16) => {
    cpu.setR16(reg16, uint16(cpu.getR16(reg16) + 1))
}

// INC reg8 [z0h-]
export const inc_reg8 = (cpu: CPU, reg8: Reg8) => {
    const val = cpu.getR8(reg8)
    const newVal = uint8(val + 1)
    cpu.setR8(reg8, newVal)

    cpu.F.z = newVal === 0
    cpu.F.n = false
    cpu.F.h = addHalfCarriesByte(val, 1)
    // c -
}

// DEC reg8 [z1h-]
export const dec_reg8 = (cpu: CPU, reg8: Reg8) => {
    const val = cpu.getR8(reg8)
    const newVal = uint8(val - 1)
    cpu.setR8(reg8, newVal)

    cpu.F.z = newVal === 0
    cpu.F.n = true
    cpu.F.h = subHalfCarriesByte(val, 1)
    // c -
}

// RLCA [000c]
// rotates A left one bit, with bit 7 moved to bit 0 and also stored in the carry.
export const rlca = (cpu: CPU) => {
    const carry = (cpu.A & 0x80) != 0
    if (carry) {
        // shift left, discard top bit
        cpu.A = cpu.A << 1
        // set bit 0 to 1 (wrap bit 7 around)
        cpu.A |= 0x01
        // store bit in carry
        cpu.F.c = true
    } else {
        // top bit is 0, safe to shift left.
        cpu.A = cpu.A << 1
        // store bit in carry
        cpu.F.c = false
    }

    cpu.F.z = false
    cpu.F.n = false
    cpu.F.h = false
}

// RRCA [000c]
// rotates A one bit right with bit 0 moved to bit 7 and also stored in the carry.
export const rrca = (cpu: CPU) => {
    const carry = (cpu.A & 0x01) != 0
    if (carry) {
        // shift right (discard bottom bit)
        cpu.A = cpu.A >> 1
        // set top bit to 1 (wrap bit 0 around)
        cpu.A |= 0x80
        // store old bottom bit in carry
        cpu.F.c = true
    } else {
        // bottom bit is 0, safe to right shift
        cpu.A = cpu.A >> 1
        // store old bottom bit in carry
        cpu.F.c = false
    }
    
    cpu.F.z = false
    cpu.F.n = false
    cpu.F.h = false
}

// ADD HL reg16 [-0hc]
export const add_HL_reg16 = (cpu: CPU, reg16: Reg16) => {
    const val = cpu.getR16(reg16)
    const hl = cpu.getHL()
    const newVal = uint16(hl+ val)
    cpu.setHL(newVal)

    // z -
    cpu.F.n = false
    cpu.F.h = addHalfCarriesWord(hl, val)
    cpu.F.c = addCarriesWord(hl, val)
}

// DEC r16 [----]
export const dec_reg16 = (cpu: CPU, reg16: Reg16) => {
    cpu.setR16(reg16, uint16(cpu.getR16(reg16) - 1))
}

// LD (HL+) A [----]
// sets (HL) = A, then increments HL
export const ld_valHLplus_A = (cpu: CPU) => {
    const hl = cpu.getHL()
    cpu.mmu.wb(hl, cpu.A)
    cpu.setHL(uint16(hl + 1))
}

// LD A (HL-) [---]
// sets A = (HL), then decrements HL
export const ld_A_valHLminus = (cpu: CPU) => {
    const hl = cpu.getHL()
    cpu.A = cpu.mmu.rb(hl)
    cpu.setHL(uint16(hl - 1))
}

// LD (HL-) A [----]
// sets (HL) = A, then decrements HL
export const ld_valHLminus_A = (cpu: CPU) => {
    const hl = cpu.getHL()
    cpu.mmu.wb(hl, cpu.A)
    cpu.setHL(uint16(hl - 1))
}

// LD A (HL+) [---]
// sets A = (HL), then increments HL
export const ld_A_valHLplus = (cpu: CPU) => {
    const hl = cpu.getHL()
    cpu.A = cpu.mmu.rb(hl)
    cpu.setHL(uint16(hl + 1))
}

// LD HL SP+r8 [00hc]
export const ld_HL_SPplusr8 = (cpu: CPU) => {
    const r8 = int8(cpu.advanceNextByte())
    const sp = cpu.getSP()
    cpu.setHL(uint16(sp + r8))

    cpu.F.z = false
    cpu.F.n = false
    cpu.F.h = r8 > 0 ? addHalfCarriesByte(sp, r8) : subHalfCarriesByte(sp, r8)
    cpu.F.c = r8 > 0 ? addCarriesByte(sp, r8) : subCarriesByte(sp, r8)
}

// LDH (a8) A
export const ldh_vala8_A = (cpu: CPU) => {
    cpu.mmu.wb(0xFF00 + cpu.advanceNextByte(), cpu.A)
}

// LDH A (a8)
export const ldh_A_vala8 = (cpu: CPU) => {
    cpu.A = cpu.mmu.rb(0xFF00 + cpu.advanceNextByte())
} 

// LD (C) A
export const ld_valC_A = (cpu: CPU) => {
    cpu.mmu.wb(0xFF00 + cpu.C, cpu.A)
}

// LD A (C)
export const ld_A_valC = (cpu: CPU) => {
    cpu.A = cpu.mmu.rb(0xFF00 + cpu.C)
}

// LD (a16) A
export const ld_vala16_A = (cpu: CPU) => {
    cpu.mmu.ww(cpu.advanceNextWord(), cpu.A)
} 

// LD A (a16)
export const ld_A_vala16 = (cpu: CPU) => {
    cpu.A = cpu.mmu.rw(cpu.advanceNextWord())
}

// INC (HL) [z0h-]
// increments the byte at address HL
export const inc_valHL = (cpu: CPU) => {
    const hl = cpu.getHL()
    const byte = cpu.mmu.rb(hl)
    const newByte = uint8(byte + 1)
    cpu.mmu.wb(hl, newByte)

    cpu.F.z = newByte === 0
    cpu.F.n = false
    cpu.F.h = addHalfCarriesByte(byte, 1)
    // c -
}

// DEC (HL) [z1h-]
export const dec_valHL = (cpu: CPU) => {
    const hl = cpu.getHL()
    const byte = cpu.mmu.rb(hl)
    const newByte = uint8(byte - 1)
    cpu.mmu.wb(hl, newByte)

    cpu.F.z = newByte === 0
    cpu.F.n = true
    cpu.F.h = subHalfCarriesByte(byte, 1)
    // c -
}

// RLA [000c]
// rotates A one bit left with the carry moved to bit 0
// and bit 7 moved to the carry.
export const rla = (cpu: CPU) => {
    const oldCarry = cpu.F.c ? 1 : 0
    // if top bit is 1, move bit to carry
    cpu.F.c = (cpu.A & 0x80) !== 0 ? true : false
    // shift A left one bit and set bit 0 to the value of the old carry
    cpu.A = cpu.A << 1
    cpu.A |= oldCarry

    cpu.F.z = false
    cpu.F.n = false
    cpu.F.h = false
}

// RRA [000c]
// rotates A one bit right with the carry moved to bit 7
// and bit 0 moved to the carry.
export const rra = (cpu: CPU) => {
    const oldCarry = cpu.F.c ? 1 : 0
    // if top bit is 1, move bit to carry
    cpu.F.c = (cpu.A & 0x01) !== 0 ? true : false
    // shift A right one bit and set bit 7 to the value of the old carry
    cpu.A = cpu.A >> 1
    cpu.A |= oldCarry << 7

    cpu.F.z = false
    cpu.F.n = false
    cpu.F.h = false
}

export const _jr = (cpu: CPU, r8: number) => {
    cpu.PC = uint16(cpu.PC + r8)
}

// JR r8 [----]
// relative jump to PC +/- r8 (int8)
export const jr = (cpu: CPU) => {
    const r8 = int8(cpu.advanceNextByte())
    _jr(cpu, r8)
}
// JR NZ r8 [----]
// JR Z r8 [----]
// JR NC r8 [----]
// JR C r8 [----]
// conditional relative jump to PC +/- r8 if Z/C flag is/isn't set
export const jr_nz = (cpu: CPU) => {
    const r8 = int8(cpu.advanceNextByte())
    if (!cpu.F.z) _jr(cpu, r8) 
}
export const jr_z = (cpu: CPU) => {
    const r8 = int8(cpu.advanceNextByte())
    if (cpu.F.z) _jr(cpu, r8) 
}
export const jr_nc = (cpu: CPU) => {
    const r8 = int8(cpu.advanceNextByte())
    if (!cpu.F.c) _jr(cpu, r8) 
}
export const jr_c = (cpu: CPU) => {
    const r8 = int8(cpu.advanceNextByte())
    if (cpu.F.c) _jr(cpu, r8) 
}

// JP a16
export const jp = (cpu: CPU) => {
    const a16 = cpu.advanceNextWord()
    cpu.PC = a16
}
export const jp_nz = (cpu: CPU) => {
    const a16 = cpu.advanceNextWord()
    if (!cpu.F.z) cpu.PC = a16
}
export const jp_z = (cpu: CPU) => {
    const a16 = cpu.advanceNextWord()
    if (cpu.F.z) cpu.PC = a16
}
export const jp_nc = (cpu: CPU) => {
    const a16 = cpu.advanceNextWord()
    if (!cpu.F.c) cpu.PC = a16
}
export const jp_c = (cpu: CPU) => {
    const a16 = cpu.advanceNextWord()
    if (cpu.F.c) cpu.PC = a16
}

// JP (HL)
export const jp_valHL = (cpu: CPU) => {
    cpu.PC = cpu.mmu.rw(cpu.getHL())
}

// CALL a16
// CALL NZ, Z, NC, C
export const call = (cpu: CPU) => {
    const a16 = cpu.mmu.rw(cpu.PC)
    const sp = cpu.SP -= 2
    cpu.mmu.ww(sp, cpu.PC)
    cpu.PC = a16
}
export const call_nz = (cpu: CPU) => {
    if (!cpu.F.z) {
        call(cpu)
    } else {
        cpu.PC += 2
    }
}
export const call_z = (cpu: CPU) => {
    if (cpu.F.z) {
        call(cpu)
    } else {
        cpu.PC += 2
    }
}
export const call_nc = (cpu: CPU) => {
    if (!cpu.F.c) {
        call(cpu)
    } else {
        cpu.PC += 2
    }
}
export const call_c = (cpu: CPU) => {
    if (cpu.F.c) {
        call(cpu)
    } else {
        cpu.PC += 2
    }
}

// RET
// RET NZ, Z, NC, C
export const ret = (cpu: CPU) => {
    // pop value off stack
    const addr = cpu.mmu.rw(cpu.SP)
    cpu.SP += 2
    // set PC to popped value
    cpu.PC = addr
}
export const ret_nz = (cpu: CPU) => {
    if (!cpu.F.z) ret(cpu)
}
export const ret_z = (cpu: CPU) => {
    if (cpu.F.z) ret(cpu)
}
export const ret_nc = (cpu: CPU) => {
    if (!cpu.F.c) ret(cpu)
}
export const ret_c = (cpu: CPU) => {
    if (cpu.F.c) ret(cpu)
}

// RETI
// return and enable interrupts (immediately, unlike EI)
export const reti = (cpu: CPU) => {
    cpu.ime = true
    ret(cpu)
}

// RST
export const rst = (cpu: CPU, addr: RSTVector) => {
    // push PC onto stack
    const sp = cpu.SP -= 2
    cpu.mmu.ww(sp, cpu.PC)
    // Jump to reset vector
    cpu.PC = addr
}

// IME
// Enable interrupts after next machine cycle
export const ei = (cpu: CPU, ) => {
    cpu.scheduledIME = true
}

// DI
// Disable interrupts
export const di = (cpu: CPU, ) => {
    cpu.scheduledIME = false
}

// HALT [----]
// TODO there's apparently a HALT bug that I need to implement lol
export const halt = (cpu: CPU) => {
    cpu.halted = true
}

// STOP [----]
export const stop = (cpu: CPU) => {
    // read and discard next byte
    cpu.advanceNextByte()
    cpu.stopped = true
}

// ADD A {r8, d8} [z0hc]
export const add = (cpu: CPU, byte: number) => {
    const oldA = cpu.A
    cpu.A = uint8(cpu.A + byte)

    cpu.F.z = cpu.A === 0
    cpu.F.n = false
    cpu.F.h = addHalfCarriesByte(oldA, byte)
    cpu.F.c = addCarriesByte(oldA, byte)
}

// ADC A {r8, d8} [z0hc]
// sets A = A + {r8, d8} + carry
export const adc = (cpu: CPU, val: number) => {
    const carry = cpu.F.c ? 1 : 0
    const oldA = cpu.A
    cpu.A = uint8(cpu.A + val + carry)

    cpu.F.z = cpu.A === 0
    cpu.F.n = false
    cpu.F.h = addHalfCarriesByte(oldA, val, carry)
    cpu.F.c = addCarriesByte(oldA, val + carry)
}

// SUB A {r8, d8} [z1hc]
export const sub = (cpu: CPU, val: number) => {
    const oldA = cpu.A
    cpu.A = uint8(cpu.A - val)

    cpu.F.z = cpu.A === 0
    cpu.F.n = true
    cpu.F.h = subHalfCarriesByte(oldA, val)
    cpu.F.c = oldA - val < 0
}

export const sbc = (cpu: CPU, val: number) => {
    const carry = cpu.F.c ? 1 : 0
    const oldA = cpu.A
    cpu.A = uint8(cpu.A - val - carry)

    cpu.F.z = cpu.A === 0
    cpu.F.n = true
    cpu.F.h = subHalfCarriesByte(oldA, val, carry)
    cpu.F.c = oldA - val - carry < 0
}

// AND A r8 [z010]
export const and = (cpu: CPU, val: number) => {
    cpu.A = cpu.A & val

    cpu.F.z = cpu.A === 0
    cpu.F.n = false
    cpu.F.h = true
    cpu.F.c = false
}

// XOR A r8 [z000]
export const xor = (cpu: CPU, val: number) => {
    cpu.A = cpu.A ^ val

    cpu.F.z = cpu.A === 0
    cpu.F.n = false
    cpu.F.h = false
    cpu.F.c = false
}

// OR A r8 [z000]
export const or = (cpu: CPU, val: number) => {
    cpu.A = cpu.A | val

    cpu.F.z = cpu.A === 0
    cpu.F.n = false
    cpu.F.h = false
    cpu.F.c = false
}

// CP A r8 [z1hc]
// Compare A with value. Essentially a subtraction where you throw
// away the results.
export const cp = (cpu: CPU, val: number) => {
    const result = cpu.A - val

    cpu.F.z = result === 0
    cpu.F.n = true
    cpu.F.h = subHalfCarriesByte(cpu.A, val)
    cpu.F.c = cpu.A - val < 0
}

// DAA [z-0c]
// Ajusts A so that it's the correct result of Binary Coded Decimal math
// of the previous operation.
// BCD is just each nybble representing one digit from 0-9. 
// Example:
//   0x09 in BCD is 0b0000_1001 (0_9)
//   0x0F in BCD is 0b0001_0101 (1_5)
//   0x63 in BCD is 0b0110 0011 (9_9)
// If we're adding BCD numbers together, we need to adjust the result to make the
// result also in BCD.
// E.g. 0x09 + 0x01 gives us 0x0a (not a valid BCD number), but if we adjust the number
// by adding 6 to it (making up for the extra unused 6 numbers a,b,c,d,e,f) then we get
// 0x10, the correct BCD result.
// What about 0x90 + 0x11? We need to adjust the number by 60. And what about subtraction?
// It gets complicated -- read these detailed explanations.
// https://blog.ollien.com/posts/gb-daa/
// https://ehaskins.com/2018-01-30%20Z80%20DAA/
export const daa = (cpu: CPU) => {
    let correction = 0;

    if (cpu.F.h || (!cpu.F.n && (cpu.A & 0xf) > 9)) {
        correction |= 0x6;
    }

    if (cpu.F.c || (!cpu.F.n && cpu.A > 0x99)) {
        correction |= 0x60;
        cpu.F.c = true
    }

    cpu.A = uint8(cpu.A + (cpu.F.n ? -correction : correction));

    cpu.F.z = cpu.A === 0
    cpu.F.h = false
}

// CPL [-11-]
// A ~= 0xFF (aka complement of A)
export const cpl = (cpu: CPU) => {
    cpu.A ^= 0xFF
    
    // z -
    cpu.F.n = true
    cpu.F.h = true
    // c -
}

// SCF [-001]
// sets carry flag and unsets N and H flags
export const scf = (cpu: CPU) => {
    // z -
    cpu.F.n = false
    cpu.F.h = false
    cpu.F.c = true
}

// CCF [-00c]
// complements (inverts) carry flag and resets N and H flags
export const ccf = (cpu: CPU) => {
    cpu.F.n = false
    cpu.F.h = false
    cpu.F.c = !cpu.F.c
}

// ADD SP r8 [00hc]
export const add_SP_r8 = (cpu: CPU) => {
    const r8 = int8(cpu.advanceNextByte())
    const sp = cpu.SP
    cpu.SP = uint16(sp + r8)
    
    cpu.F.z = false
    cpu.F.n = false
    // use lower byte for carry / half-carry. https://stackoverflow.com/questions/57958631/game-boy-half-carry-flag-and-16-bit-instructions-especially-opcode-0xe8
    cpu.F.h = r8 > 0 ? addHalfCarriesByte(sp, r8) : subHalfCarriesByte(sp, r8)
    cpu.F.c = r8 > 0 ? addCarriesByte(sp, r8) : uint8(sp) - r8 < 0
}

// PUSH Reg16
export const push = (cpu: CPU, reg16: Reg16) => {
    cpu.mmu.ww(cpu.SP-=2, cpu.getR16(reg16))
}

// POP Reg16
export const pop = (cpu: CPU, reg16: Reg16) => {
    cpu.setR16(reg16, cpu.mmu.rw(cpu.SP))
    cpu.SP += 2
}

// POP AF [znhc]
export const pop_AF = (cpu: CPU) => pop(cpu, Reg16.AF)

// PUSH AF 
export const push_AF = (cpu: CPU) => push(cpu, Reg16.AF)

export const rlc_r8 = (cpu: CPU, reg8: Reg8) => {
    throw new Error('Function not implemented.');
}

export const rlc_valHL = (cpu: CPU) => {
    throw new Error('Function not implemented.');
}

export const rrc_r8 = (cpu: CPU, reg8: Reg8) => {
    throw new Error('Function not implemented.');
}

export const rrc_valHL = (cpu: CPU) => {
    throw new Error('Function not implemented.');
}

export const rl_r8 = (cpu: CPU, reg8: Reg8) => {
    throw new Error('Function not implemented.');
}

export const rl_valHL = (cpu: CPU) => {
    throw new Error('Function not implemented.');
}

export const rr_r8 = (cpu: CPU, reg8: Reg8) => {
    throw new Error('Function not implemented.');
}

export const rr_valHL = (cpu: CPU) => {
    throw new Error('Function not implemented.');
}

export const sla_r8 = (cpu: CPU, reg8: Reg8) => {
    throw new Error('Function not implemented.');
}

export const sla_valHL = (cpu: CPU) => {
    throw new Error('Function not implemented.');
}

export const sra_r8 = (cpu: CPU, reg8: Reg8) => {
    throw new Error('Function not implemented.');
}

export const sra_valHL = (cpu: CPU) => {
    throw new Error('Function not implemented.');
}

export const swap_r8 = (cpu: CPU, reg8: Reg8) => {
    throw new Error('Function not implemented.');
}

export const swap_valHL = (cpu: CPU) => {
    throw new Error('Function not implemented.');
}

export const srl_r8 = (cpu: CPU, reg8: Reg8) => {
    throw new Error('Function not implemented.');
}

export const srl_valHL = (cpu: CPU) => {
    throw new Error('Function not implemented.');
}

export const bit_n_r8 = (cpu: CPU, n: number, reg8: Reg8) => {
    throw new Error('Function not implemented.');
}

export const bit_n_valHL = (cpu: CPU, n: number) => {
    throw new Error('Function not implemented.');
}

export const res_n_r8 = (cpu: CPU, n: number, reg8: Reg8) => {
    throw new Error('Function not implemented.');
}

export const res_n_valHL = (cpu: CPU, n: number) => {
    throw new Error('Function not implemented.');
}

export const set_n_r8 = (cpu: CPU, n: number, reg8: Reg8) => {
    throw new Error('Function not implemented.');
}

export const set_n_valHL = (cpu: CPU, n: number) => {
    throw new Error('Function not implemented.');
}