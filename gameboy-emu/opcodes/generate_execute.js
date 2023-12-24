// run code generator with `node generate_opcodes.ts`
const opcodes = require("./opcodes.json")
const fs = require("fs")
const path = require("path")

const OUTPUT_EMPTY_ROWS = true

const format = (opc, code = '') => {
    const output = `
            case ${opc.addr}: // ${opc.mnemonic} ${opc["operand1"] || ''} ${opc["operand2"] || ''} [${opc.flags[0]}${opc.flags[1]}${opc.flags[2]}${opc.flags[3]}]
                ${code}
                break`
    return output
}

let unprefixed = ""
Object.values(opcodes.unprefixed).forEach( opc => {
    let code = ''
    const addr = parseInt(opc.addr)
    let fn = `cpu.${opc.mnemonic.toLowerCase()}`

    // stop
    if (addr === 0x10) {
        code += `cpu.stop()`
    }

    // 16 bit loads
    const ld16_instrs = [
        0x01, // LD BC d16
        0x11, // LD DE d16
        0x21, // LD HL d16
        0x31, // LD SP d16
        0x08, // LD (a16) SP
        0xF8, // LD HL SP+r8 [00HC]
        0xF9, // LD SP HL 
    ]
    if (ld16_instrs.includes(addr)) {
        if (addr === 0x08) { // LD (a16) SP
            code += `cpu.mmu.ww(cpu.nextWord(), cpu.SP)`
        } else if (addr === 0xf8) { // LD HL SP+r8
            code += `${fn}_HL_SPplusInt8()`
        } else if (addr === 0xf9) { // LD SP HL
            code += `cpu.SP = cpu.HL`
        } else { // LD reg16 d16
            code += `cpu.set${opc.operand1}(cpu.nextWord())`
        }
    }

    // 16 bit arith
    const arith16_instrs = [
        0x03, // INC BC
        0x13, // INC DE
        0x23, // INC HL
        0x33, // INC SP

        0x0b, // DEC BC
        0x1b, // DEC DE
        0x2b, // DEC HL
        0x3b, // DEC SP

        0x09, // ADD HL BC
        0x19, // ADD HL DE 
        0x29, // ADD HL HL
        0x39, // ADD HL SP
    ]
    if (arith16_instrs.includes(addr)) {
        if (opc.mnemonic === 'ADD') {
           code += `cpu.add_HL_reg16(R16.${opc.operand2})`
        } else { // INC, DEC
            code += `${fn}_reg16(R16.${opc.operand1})`
        }
    }

    // 8 bit INC / DEC
    const inc8_instrs = [
        0x04, // INC B
        0x0c, // INC C
        0x14, // INC D
        0x1c, // INC E
        0x24, // INC H
        0x2c, // INC L
        0x34, // INC (HL)
        0x3c, // INC A

        0x05, // DEC B
        0x0d, // DEC C
        0x15, // DEC D
        0x1d, // DEC E
        0x25, // DEC H
        0x2d, // DEC L
        0x35, // DEC (HL)
        0x3d, // DEC A
    ]
    if (inc8_instrs.includes(addr)) {
        if (opc.operand1 === "(HL)") {
            code += `${fn}_valHL()`
        } else {
            code += `${fn}_reg8(R8.${opc.operand1})`
        }
    }

    // misc 8 bit loads
    const misc_8bit_loads_map = {
        0x02: `cpu.mmu.wb(cpu.getBC(), cpu.A)`, // LD (BC) A
        0x12: `cpu.mmu.wb(cpu.getDE(), cpu.A)`, // LD (DE) A
        0x0a: `cpu.A = cpu.mmu.rb(cpu.getBC())`, // LD A (BC)
        0x1a: `cpu.A = cpu.mmu.rb(cpu.getDE())`, // LD A (DE)

        0x22: `cpu.ld_valHLinc_A()`, // LD (HL+) A
        0x32: `cpu.ld_valHLdec_A()`, // LD (HL-) A
        0x2a: `cpu.ld_A_valHLinc()`, // LD A (HL+)
        0x3a: `cpu.ld_A_valHLdec()`, // LD A (HL-)

        0x06: `cpu.B = cpu.nextByte()`, // LD B d8
        0x0e: `cpu.C = cpu.nextByte()`, // LD C d8
        0x16: `cpu.D = cpu.nextByte()`, // LD D d8
        0x1e: `cpu.E = cpu.nextByte()`, // LD E d8
        0x26: `cpu.H = cpu.nextByte()`, // LD H d8
        0x2e: `cpu.L = cpu.nextByte()`, // LD L d8
        0x36: `cpu.mmu.wb(cpu.getHL(), cpu.nextByte())`, // LD (HL) d8
        0x3e: `cpu.A = cpu.nextByte()` // LD A d8
    }
    if (misc_8bit_loads_map[addr]) {
        code += misc_8bit_loads_map[addr]
    }

    // Simple 8 bit loads (LD A B etc)
    // and HALT 0x76
    if (addr >= 0x40 && addr <= 0x7f) { // LD A B etc
        if (addr === 0x76) { // HALT
            code = `cpu.halt()`
        } else if (opc.operand2 === "(HL)") { // LD B (HL) etc
            code = `cpu.${opc.operand1} = cpu.mmu.rb(cpu.getHL())`
        } else if (opc.operand1 === "(HL)") { // LD (HL) B etc
            code = `cpu.mmu.wb(cpu.getHL(), cpu.${opc.operand2})`
        } else {
            code = `cpu.${opc.operand1} = cpu.${opc.operand2}`
        }
    }

    // ADD, ADC, SUB, SBC, AND, XOR, OR, CP
    const d8_arith_instrs = [
        0xc6, // ADD A d8
        0xce, // ADC A d8
        0xd6, // SUB d8
        0xde, // SBC A d8
        0xe6, // AND d8
        0xee, // XOR d8
        0xf6, // OR d8
        0xfe, // CP d8
    ]
    if ((addr >= 0x80 && addr < 0xc0) || d8_arith_instrs.includes(addr)) { 
        const fn = `cpu.${opc.mnemonic.toLowerCase()}`
        const operand = ["ADD", "ADC", "SBC"].includes(opc.mnemonic)
            ? opc.operand2 // ADC, ADC, SBC all have 2 operands and the first is always A
            : opc.operand1 // SUB, AND, XOR, OR, CP all have 1 operand, A is implied
        if (operand === "(HL)") {
            code = `${fn}(cpu.mmu.rb(cpu.getHL()))`
        } else if (operand === "d8") {
            code = `${fn}(cpu.nextByte())`
        } else { // operand is 8 bit register, like A,B,C etc
            code = `${fn}(cpu.${operand})`
        }
    }

    const misc_8bitarith_instrs = [
        0x07, // RLCA
        0x17, // RLA
        0x0F, // RRCA
        0x1F, // RRA

        0x27, // DAA
        0x2F, // CPL
        0x37, // SCF
        0x3F, // CCF
    ]
    if (misc_8bitarith_instrs.includes(addr)) {
        code += `${fn}()`
    }

    // RET
    const return_instrs = [
        0xc0, // RET_NZ
        0xd0, // RET_NC
        0xc8, // RET_Z
        0xd8, // RET_N
        0xc9, // RET
        0xd9, // RETI
    ]
    if (return_instrs.includes(addr)) { 
        const fn = `cpu.${opc.mnemonic.toLowerCase()}`
        switch(opc.operand1) {
            case 'NZ':
                code += `if (!cpu.F.z) `
                break
            case 'NC':
                code += `if (!cpu.F.c) `
                break
            case 'Z':
                code += `if (cpu.F.z) `
                break
            case 'C':
                code += `if (cpu.F.c) `
                break
        } 
        code += `${fn}()`
    }

    // POP, PUSH 
    const pop_push_instrs = [
        0xc1, // POP BC
        0xd1, // POP DE
        0xe1, // POP HL
        0xf1, // POP AF [znhc]

        0xc5, // PUSH BC
        0xd5, // PUSH DE
        0xe5, // PUSH HL
        0xf5, // PUSH AF
    ]
    if (pop_push_instrs.includes(addr)) {
        const fn = `cpu.${opc.mnemonic.toLowerCase()}`
        if (addr === 0xf1 || addr === 0xf5) {
            code += `${fn}_AF()`
        } else {
            code += `${fn}(R16.${opc.operand1})`
        }
    }

    // JP, CALL a16
    const jump_instrs = [
        0x20, // JR NZ r8
        0x30, // JR NC r8
        0x18, // JR r8
        0x28, // JR Z r8
        0x38, // JR C r8

        0xc2, // JP_NZ
        0xd2, // JP_NC
        0xc3, // JP
        0xca, // JP_Z
        0xda, // JP_C

        0xc4, // CALL_NZ
        0xd4, // CALL_NC
        0xcc, // CALL_Z
        0xdc, // CALL_C
        0xcd, // CALL
    ]
    if (jump_instrs.includes(addr)) {
        switch(opc.operand1) {
            case 'NZ':
                code += `if (!cpu.F.z) `
                break
            case 'NC':
                code += `if (!cpu.F.c) `
                break
            case 'Z':
                code += `if (cpu.F.z) `
                break
            case 'C':
                code += `if (cpu.F.c) `
                break
        }
        if (opc.mnemonic === "JR") {
            code += `${fn}(int8(cpu.nextByte()))`
        } else {
            code += `${fn}(cpu.nextWord())`
        }
    }

    // RST
    const rst_instrs = [
        0xc7, // RST 00H
        0xcf, // RST 08H
        0xd7, // RST 10H
        0xdf, // RST 18H
        0xe7, // RST 20H
        0xef, // RST 28H
        0xf7, // RST 30H
        0xff, // RST 38H
    ]
    if (rst_instrs.includes(addr)) {
        const fn = `cpu.${opc.mnemonic.toLowerCase()}`
        code += `${fn}(RSTVector.$${opc.operand1.slice(0, -1)})`
    }

    if (code || OUTPUT_EMPTY_ROWS) {
        unprefixed += format(opc, code)
        if ((addr & 0xF) === 0x7)
            unprefixed += "\n"
        if ((addr & 0xF) === 0xF) {
            unprefixed += "\n\n\n"
        }
    }
})

let prefixed = ""
Object.values(opcodes.cbprefixed).forEach( opc => {
    let code = ''
    const addr = parseInt(opc.addr)

    if (addr < 0x40) { // RLC, RRC, RL, RR, SLA, SRA, SWAP, SRL
        const fn = `cpu.${opc.mnemonic.toLowerCase()}`
        if (opc.operand1 === "(HL)") {
            code = `${fn}_valHL()`
        } else {
            code = `${fn}_r8(R8.${opc.operand1})`
        }
    } else if (addr <= 0xFF) { // BIT, RES, SET
        const fn = `cpu.${opc.mnemonic.toLowerCase()}`
        if (opc.operand2 === "(HL)") {
            code = `${fn}_n_valHL(${opc.operand1})`
        } else {
            code = `${fn}_n_r8(${opc.operand1}, R8.${opc.operand2})`
        }
    }

    if (code || OUTPUT_EMPTY_ROWS) {
        prefixed += format(opc, code)
        if ((addr & 0xF) === 0x7)
            prefixed += "\n"
        if ((addr & 0xF) === 0xf) {
            prefixed += "\n\n\n"
        }
    }
})

const content = `// generated by \`node ${path.basename(__filename)}\ at ${new Date().toLocaleDateString()} ${new Date().toLocaleTimeString()} 
import { CPU, R8, R16, RSTVector } from "../cpu"
import { int8 } from "../utils"

export const execute = (cpu: CPU, instr: number, cbprefixed: boolean) => {
    if (!cbprefixed) {
        switch(instr) {
            ${unprefixed}
        }

    } else {
        switch(instr) {
            ${prefixed}
        }
    }
}
`
fs.writeFile("./execute.ts", content, err => console.error(err))
