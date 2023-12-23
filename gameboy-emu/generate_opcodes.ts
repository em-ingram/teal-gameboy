// run code generator with `node generate_opcodes.ts`
const opcodes = require("./opcodes.json")
const fs = require("fs")

const OUTPUT_EMPTY_ROWS = false

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

    if (addr >= 0x40 && addr <= 0x7f) { // 8 bit loads (LD A B)
        if (addr === 0x76) {
            code = `this.halted = true`
        } else if (opc.operand2 === "(HL)") {
            code = `this.ld_r8_valHL(R8.${opc.operand1})`
        } else if (opc.operand1 === "(HL)") {
            code = `this.ld_valHL_r8(R8.${opc.operand2})`
        } else {
            code = `this.${opc.operand1} = this.${opc.operand2}`
        }
    }

    if (addr >= 0x80 && addr < 0x90) { // ADD, ADC
        const fn = `this.${opc.mnemonic.toLowerCase()}`
        if (opc.operand2 === "(HL)") {
            code = `${fn}(this.mmu.rb(this.getHL()))`
        } else {
            code = `${fn}(this.${opc.operand2})`
        }
    }

    if (addr >= 0x90 && addr < 0xc0) { // SUB, SBC, AND, XOR, OR, CP
        const fn = `this.${opc.mnemonic.toLowerCase()}`
        if (opc.operand1 === "(HL)") {
            code = `${fn}(this.mmu.rb(this.getHL()))`
        } else {
            code = `${fn}(this.${opc.operand1})`
        }
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
        const fn = `this.${opc.mnemonic.toLowerCase()}`
        if (opc.operand1 === "(HL)") {
            code = `${fn}_valHL()`
        } else {
            code = `${fn}_r8(R8.${opc.operand1})`
        }
    } else if (addr <= 0xFF) { // BIT, RES, SET
        const fn = `this.${opc.mnemonic.toLowerCase()}`
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

const content = `
// generated by \`node generate_opcodes.ts\` 
if (!prefixed) {
    switch(instr) {
${unprefixed}
    }

} else {
    switch(instr) {
${prefixed}
    }
}
`
fs.writeFile("./opcodes_generated.txt", content, err => console.error(err))
