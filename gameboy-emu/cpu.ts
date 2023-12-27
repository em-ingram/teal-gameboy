import { MMU } from './mmu'
import { execute } from './opcodes/execute'
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
} from './utils'
import { prefixedOpcodeTable, unprefixedOpcodeTable } from './opcodes/opcodeData'

// 8 bit registers
export enum R8 {
    A, B, C, D, E, H, L
}

// 16 bit registers
export enum R16 {
    AF, BC, DE, HL, SP
}

// Memory addresses that can be jumped to by the RST instruction
export enum RSTVector {
    $00 = 0x00, 
    $08 = 0x80,
    $10 = 0x10, 
    $18 = 0x18,
    $20 = 0x20, 
    $28 = 0x28, 
    $30 = 0x30,
    $38 = 0x38
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
        setFromUint8: (uint8: number) => void
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
        },
        setFromUint8: function(byte) {
            this.z = byte & 0x80 ? true : false
            this.n = byte & 0x40 ? true : false
            this.h = byte & 0x20 ? true : false
            this.c = byte & 0x10 ? true : false
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
            case R16.AF: this.setAF(val); return
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
    getSP(): number {
        return this.SP
    }

    setAF(word: number) {
        this.A = (word & 0xFF00) >> 8
        this.F.setFromUint8(word)
    }
    setBC(word: number) {
        this.B = (word & 0xFF00) >> 8 
        this.C = word & 0x00FF
    }
    setDE(word: number) {
        this.D = (word & 0xFF00) >> 8 
        this.E = word & 0x00FF
    }
    setHL(word: number) {
        this.H = (word & 0xFF00) >> 8 
        this.L = word & 0x00FF
    }
    setSP(word: number) {
        this.SP = word
    }

    // reads next byte at PC and advances PC
    nextByte() {
        return this.mmu.rb(this.PC += 1)
    }

    // reads next word at PC and advances PC
    nextWord() {
        const word = this.mmu.rw(this.PC += 1)
        this.PC += 1
        return word
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

        execute(this, opcode, prefixed)

        if (this.jumped) {
            return opcData.cycles
        }
        if (opcData.cyclesIfNoJump) {
            return opcData.cyclesIfNoJump
        }
        return opcData.cycles
    }

    // Opcode helper instructions
    // ====

    // INC r16 [----]
    inc_reg16 = (reg16: R16) => {
        this.setR16(reg16, uint16(this.getR16(reg16) + 1))
    }

    // INC reg8 [z0h-]
    inc_reg8 = (reg8: R8) => {
        const val = this.getR8(reg8)
        const newVal = uint8(val + 1)
        this.setR8(reg8, newVal)

        this.F.z = newVal === 0
        this.F.n = false
        this.F.h = addHalfCarriesByte(val, 1)
        // c -
    }

    // DEC reg8 [z1h-]
    dec_reg8 = (reg8: R8) => {
        const val = this.getR8(reg8)
        const newVal = uint8(val - 1)
        this.setR8(reg8, newVal)

        this.F.z = newVal === 0
        this.F.n = true
        this.F.h = subHalfCarriesByte(val, 1)
        // c -
    }

    // RLCA [000c]
    // rotates A left one bit, with bit 7 moved to bit 0 and also stored in the carry.
    rlca = () => {
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
    rrca = () => {
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

    stop = () => {
        this.stopped = true
    }

    // ADD HL reg16 [-0hc]
    add_HL_reg16 = (reg16: R16) => {
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
    dec_reg16 = (reg16: R16) => {
        this.setR16(reg16, uint16(this.getR16(reg16) - 1))
    }

    // LD (HL+) A [----]
    // sets (HL) = A, then increments HL
    ld_valHLplus_A = () => {
        const hl = this.getHL()
        this.mmu.wb(hl, this.A)
        this.setHL(uint16(hl + 1))
    }

    // LD A (HL-) [---]
    // sets A = (HL), then decrements HL
    ld_A_valHLminus = () => {
        const hl = this.getHL()
        this.A = this.mmu.rb(hl)
        this.setHL(uint16(hl - 1))
    }

    // LD (HL-) A [----]
    // sets (HL) = A, then decrements HL
    ld_valHLminus_A = () => {
        const hl = this.getHL()
        this.mmu.wb(hl, this.A)
        this.setHL(uint16(hl - 1))
    }

    // LD A (HL+) [---]
    // sets A = (HL), then increments HL
    ld_A_valHLplus = () => {
        const hl = this.getHL()
        this.A = this.mmu.rb(hl)
        this.setHL(uint16(hl + 1))
    }

    // LD HL SP+r8 [00hc]
    ld_HL_SPplusr8 = () => {
        console.debug('DEBUG', this.PC.toString(16))
        const r8 = int8(this.nextByte())
        console.debug('DEBUG', this.PC.toString(16))
        const sp = this.getSP()
        this.setHL(uint16(sp + r8))

        this.F.z = false
        this.F.n = false
        this.F.h = r8 > 0 ? addHalfCarriesByte(sp, r8) : subHalfCarriesByte(sp, r8)
        this.F.c = r8 > 0 ? addCarriesByte(sp, r8) : subCarriesByte(sp, r8)
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
    rla = () => {
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
    rra = () => {
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

    // JR r8 [----]
    // relative jump to PC +/- r8 (int8)
    jr = (r8: number) => {
        this.PC = uint16(this.PC + r8)
    }

    // HALT [----]
    // TODO there's apparently a HALT bug that I need to implement lol
    halt = () => {
        this.halted = true
    }

    // ADD A {r8, d8} [z0hc]
    add = (byte: number) => {
        const oldA = this.A
        this.A = uint8(this.A + byte)

        this.F.z = this.A === 0
        this.F.n = false
        this.F.h = addHalfCarriesByte(oldA, byte)
        this.F.c = addCarriesByte(oldA, byte)
    }

    // ADC A {r8, d8} [z0hc]
    // sets A = A + {r8, d8} + carry
    adc = (val: number) => {
        const carry = this.F.c ? 1 : 0
        const oldA = this.A
        this.A = uint8(this.A + val + carry)

        this.F.z = this.A === 0
        this.F.n = false
        this.F.h = addHalfCarriesByte(oldA, val, carry)
        this.F.c = addCarriesByte(oldA, val + carry)
    }

    // SUB A {r8, d8} [z1hc]
    sub = (val: number) => {
        const oldA = this.A
        this.A = uint8(this.A - val)

        this.F.z = this.A === 0
        this.F.n = true
        this.F.h = subHalfCarriesByte(oldA, val)
        this.F.c = oldA - val < 0
    }

    sbc = (val: number) => {
        const carry = this.F.c ? 1 : 0
        const oldA = this.A
        this.A = uint8(this.A - val - carry)

        this.F.z = this.A === 0
        this.F.n = true
        this.F.h = subHalfCarriesByte(oldA, val, carry)
        this.F.c = oldA - val - carry < 0
    }

    // AND A r8 [z010]
    and = (val: number) => {
        this.A = this.A & val

        this.F.z = this.A === 0
        this.F.n = false
        this.F.h = true
        this.F.c = false
    }

    // XOR A r8 [z000]
    xor = (val: number) => {
        this.A = this.A ^ val

        this.F.z = this.A === 0
        this.F.n = false
        this.F.h = false
        this.F.c = false
    }

    // OR A r8 [z000]
    or = (val: number) => {
        this.A = this.A | val

        this.F.z = this.A === 0
        this.F.n = false
        this.F.h = false
        this.F.c = false
    }

    // CP A r8 [z1hc]
    // Compare A with value. Essentially a subtraction where you throw
    // away the results.
    cp = (val: number) => {
        const result = this.A - val

        this.F.z = result === 0
        this.F.n = true
        this.F.h = subHalfCarriesByte(this.A, val)
        this.F.c = this.A - val < 0
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
    daa = () => {
        let correction = 0;

        if (this.F.h || (!this.F.n && (this.A & 0xf) > 9)) {
            correction |= 0x6;
        }

        if (this.F.c || (!this.F.n && this.A > 0x99)) {
            correction |= 0x60;
            this.F.c = true
        }

        this.A = uint8(this.A + (this.F.n ? -correction : correction));

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
    
    // ADD SP r8 [00hc]
    add_SP_r8 = () => {
        const r8 = int8(this.nextByte())
        const sp = this.SP
        this.SP = uint16(sp + r8)
        
        this.F.z = false
        this.F.n = false
        // use lower byte for carry / half-carry. https://stackoverflow.com/questions/57958631/game-boy-half-carry-flag-and-16-bit-instructions-especially-opcode-0xe8
        this.F.h = r8 > 0 ? addHalfCarriesByte(sp, r8) : subHalfCarriesByte(sp, r8)
        this.F.c = r8 > 0 ? addCarriesByte(sp, r8) : uint8(sp) - r8 < 0
    }

    // PUSH Reg16
    push = (reg16: R16) => {
        this.mmu.ww(this.SP-=2, this.getR16(reg16))
    }

    // POP Reg16
    pop = (reg16: R16) => {
        this.setR16(reg16, this.mmu.rw(this.SP))
        this.SP += 2
    }

    // POP AF [znhc]
    pop_AF = () => this.pop(R16.AF)

    // PUSH AF 
    push_AF = () => this.push(R16.AF)

}

