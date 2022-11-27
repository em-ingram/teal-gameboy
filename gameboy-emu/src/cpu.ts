import { MMU } from './mmu'
import opcodes from '../opcodes.json'
import { number } from 'prop-types'

const BYTE_MASK = 0xFF
const UINT16_MASK = 0xFFFF

interface Opcode {
    mnemonic: string
    length: number
    cycles: number 
    // cyclesIfNoJump is the number of cycles this instruction takes if a jump does not occur. 
    // Only some conditional branching instructions have this
    cyclesIfNoJump?: number 
    flags: [
        'Z'|'0'|'1'|'-', 
        'N'|'0'|'1'|'-', 
        'H'|'0'|'1'|'-', 
        'C'|'0'|'1'|'-'
    ]
    addr: string
    operand1?: string
    operand2?: string
}

interface OpcodeJSON {
    unprefixed: {
        [opc: string]: Opcode
    }
    cbprefixed: {
        [opc: string]: Opcode
    }
}

const unprefixedOpcodeTable: Opcode[] = []
const prefixedOpcodeTable: Opcode[] = []
Object.entries(opcodes.unprefixed).forEach( ([opc, data]) => {
    const num = Number(opc)
    unprefixedOpcodeTable[num] = {
        ...data,
        cycles: data.cycles[0],
        cyclesIfNoJump: data.cycles.length > 1 ? data.cycles[1] : undefined,
        flags: data.flags as any,
    }
})
Object.entries(opcodes.cbprefixed).forEach( ([opc, data]) => {
    const num = Number(opc)
    prefixedOpcodeTable[num] = {
        ...data,
        cycles: data.cycles[0],
        cyclesIfNoJump: data.cycles.length > 1 ? data.cycles[1] : undefined,
        flags: data.flags as any,
    }
})

export class CPU {
    A: number = 0
    B: number = 0
    C: number = 0
    D: number = 0
    E: number = 0
    H: number = 0
    L: number = 0

    fZ: boolean = false
    fN: boolean = false
    fH: boolean = false
    fC: boolean = false

    SP: number = 0
    PC: number = 0

    halted: boolean = false
    stopped: boolean = false
    
    ime: boolean = false; // Interrupt Master Enable
    setIMENext: boolean = false; // Set IME on next instruction; used by Ei() command

    mmu: MMU

    constructor(mmu: MMU) {
        this.mmu = mmu
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

    step(): number {
        const origPC = this.PC

        let opcode = this.mmu.rb(this.PC)
        let prefixed = false
        if (opcode === 0xCB) {
            this.PC += 1
            opcode = this.mmu.rb(this.PC)
            prefixed = true
        }

        const opcData = prefixed ? prefixedOpcodeTable[opcode] : unprefixedOpcodeTable[opcode]

        this.execute(opcode, prefixed)

        const jumped = this.PC != origPC
        if (jumped) {
            // no need to modify PC; the execute step will have handled that.
            return opcData.cycles
        }

        this.PC += opcData.length
        if (opcData.cyclesIfNoJump) {
            return opcData.cyclesIfNoJump
        }
        return opcData.cycles
    }

    execute(instr: number, prefixed: boolean): void {
        if (!prefixed) {
            switch(instr) {
                case 0x0: // NOP
                    // nothin
                case 0x1: // LD BC d16
                    this.setBC(this.mmu.rw(this.PC + 1))
                case 0x2: // LD (BC) A
                    this.A = this.mmu.rw(this.getBC())
                case 0x3: // INC BC
                    this.setBC(this.getBC() + 1)
                case 0x4: // INC B [Z0H-]
                    const B = (this.B + 1) & BYTE_MASK 
                    this.fZ = B == 0
                    this.fN = false
                    this.fH = halfCarry(this.B, B)
                
                case 0xc6: // Add A d8
                    add_R8_d8(this, Reg8.A, this.mmu.rb(this.PC + 1))
            }
        } else {
            switch(instr) {

            }
        }
    }
}

// Add r8 d8 [Z0HC]
export const add_R8_d8 = (cpu: CPU, r8: Reg8, d8: number): void => {
    const A = (cpu.A + d8) & BYTE_MASK
    cpu.fZ = A === 0
    cpu.fN = false 
    cpu.fH = halfCarry(cpu.A, d8)
    cpu.fC = A < cpu.A

    cpu.A = A
}

export enum Reg8 {
    A, B, C, D, E, H, L
}

export enum Reg16 {
    AF, BC, DE, HL
}

// halfCarry returns whether a half carry occurs when moving from
// b1 to b2.
const halfCarry = (b1: number, b2: number): boolean => {
    return (((b2 & 0x0F) + (b1 & 0x0F)) & 0x10) > 0
}
