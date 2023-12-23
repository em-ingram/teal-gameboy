import opcodes from './opcodes.json'

export interface Opcode {
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

export const unprefixedOpcodeTable: Opcode[] = []
Object.entries(opcodes.unprefixed).forEach( ([opc, data]) => {
    const num = Number(opc)
    unprefixedOpcodeTable[num] = {
        ...data,
        cycles: data.cycles[0],
        cyclesIfNoJump: data.cycles.length > 1 ? data.cycles[1] : undefined,
        flags: data.flags as any,
    }
})
export const prefixedOpcodeTable: Opcode[] = []
Object.entries(opcodes.cbprefixed).forEach( ([opc, data]) => {
    const num = Number(opc)
    prefixedOpcodeTable[num] = {
        ...data,
        cycles: data.cycles[0],
        cyclesIfNoJump: data.cycles.length > 1 ? data.cycles[1] : undefined,
        flags: data.flags as any,
    }
})