// run code generator with `node generate_opcodes.ts`
const opcodes = require("./opcodes.json")
const fs = require("fs")

const format = (opc) => {
    const output = `
    case ${opc.addr}: // ${opc.mnemonic} ${opc["operand1"] || ''} ${opc["operand2"] || ''} [${opc.flags[0]}${opc.flags[1]}${opc.flags[2]}${opc.flags[3]}]
    `
    return output
}
let unprefixed = ""
Object.values(opcodes.unprefixed).forEach( opc => {
    unprefixed += format(opc)
})

let prefixed = ""
Object.values(opcodes.cbprefixed).forEach( opc => {
    prefixed += format(opc)
})

const content = `
${unprefixed}

${prefixed}
`
fs.writeFile("./opcodes_generated.txt", content, err => console.error(err))
