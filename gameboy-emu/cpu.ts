import { MMU } from './mmu'
import { execute } from './opcodes/execute'
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
    $08 = 0x08,
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

    SP: number = 0xFFFE
    PC: number = 0

    halted: boolean = false
    stopped: boolean = false

    jumped: boolean = false // records whether prev instruction was a jump
    
    ime: boolean = false; // Interrupt Master Enable
    scheduledIME: boolean | undefined = undefined; // Value of IME on next instruction; used by EI/DI command

    mmu: MMU
    cpu: any

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

    toString() {
        return `
        A: ${this.A.toString(16)}\tF: ${this.F.z ? 1 : 0}${this.F.n ? 1 : 0}${this.F.h ? 1 : 0}${this.F.c ? 1 : 0}
        B: ${this.B.toString(16)}\tC: ${this.C.toString(16)}
        D: ${this.D.toString(16)}\tE: ${this.D.toString(16)}
        H: ${this.H.toString(16)}\tL: ${this.L.toString(16)}

        PC: ${this.PC.toString(16)}\tSP: ${this.SP.toString(16)}`

    }

    // reads next byte at PC and advances PC
    nextByte() {
        const byte = this.mmu.rb(this.PC)
        this.PC += 1
        return byte
    }

    // reads next word at PC and advances PC
    nextWord() {
        const word = this.mmu.rw(this.PC)
        this.PC += 2
        return word
    }

    // executes the next instruction and returns the number of cycles consumed.
    step(): number {
        this.jumped = false

        if (this.scheduledIME !== undefined) {
            this.ime = this.scheduledIME
            this.scheduledIME = undefined
        }

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
}

