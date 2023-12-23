import { assert } from "console";
import { CPU, Reg8, add_R8_d8 } from "./cpu";
import { MMU } from "./mmu";

interface SetupCPUOptions {
    code: number[]
    regs: Registers
    flagsIn: number[]
}
const setupCPU = (opt: SetupCPUOptions) => {
    const mmu = new MMU(new Uint8Array(), new Uint8Array())
    const cpu = new CPU(mmu)
    cpu.fZ = !!opt.flagsIn[0]
    cpu.fN = !!opt.flagsIn[1]
    cpu.fH = !!opt.flagsIn[2]
    cpu.fC = !!opt.flagsIn[3]

    const r = opt.regs
    if (r[Reg8.A] !== undefined) {
        cpu.A = r[Reg8.A] as number
    }
    if (r[Reg8.B] !== undefined) {
        cpu.B = r[Reg8.B] as number
    }
    if (r[Reg8.C] !== undefined) {
        cpu.C = r[Reg8.C] as number
    }
    if (r[Reg8.D] !== undefined) {
        cpu.D = r[Reg8.D] as number
    }
    if (r[Reg8.E] !== undefined) {
        cpu.E = r[Reg8.E] as number
    }
    if (r[Reg8.H] !== undefined) {
        cpu.H = r[Reg8.H] as number
    }
    if (r[Reg8.L] !== undefined) {
        cpu.L = r[Reg8.L] as number
    }
    if (r.PC !== undefined) {
        cpu.PC = r.PC 
    }
    if (r.SP !== undefined) {
        cpu.SP = r.SP
    }

    return cpu
}

const flags = (cpu: CPU): number[] => {
    return [
        cpu.fZ ? 1 : 0,
        cpu.fN ? 1 : 0,
        cpu.fH ? 1 : 0,
        cpu.fC ? 1 : 0,
    ]
}

interface Registers {
    [Reg8.A]?: number
    [Reg8.B]?: number
    [Reg8.C]?: number
    [Reg8.D]?: number
    [Reg8.E]?: number
    [Reg8.H]?: number
    [Reg8.L]?: number
    PC?: number
    SP?: number
}

describe("Add r8", () => {
    test.each([
        // opc | mnem | Registers | d8 | expected | flagsIn[Z,H,N,C] | flagsOut[Z,H,N,C] 
        { 
            opc: 0xc6, 
            mn: 'ADD A d8', 
            regs: {[Reg8.A]: 0x01}, 
            d8: 0x01, 
            exp: 0x02, 
            flagsIn: [0,1,1,1], 
            flagsOut: [0,0,0,0] 
        },
        { 
            opc: 0xc6, 
            mn: 'ADD A d8', 
            regs: {[Reg8.A]: 0x01}, 
            d8: 0xFF, 
            exp: 0x00, 
            flagsIn: [0,1,1,1], 
            flagsOut: [1,0,1,1] 
        }
    ])('$mn $regs d8=$d8', ({opc, regs, d8, exp, flagsIn, flagsOut} ) => {
        const cpu = setupCPU({
            code: [opc, d8], 
            regs,
            flagsIn
        })
        add_R8_d8(cpu, Reg8.A, d8)
        expect(cpu.A).toEqual(exp)
        expect(flags(cpu)).toEqual(flagsOut)
    })

})