import { describe, test, expect } from 'vitest';

import { setupCPU, setupMMU, dumpState, CPUState, MMUState } from './__test__/setup';
import { Opcode } from './opcodes';
import { execute } from './execute';

// TODO make this and put it in opcodes.ts
enum Opcode {
    ADD_A_B = 0x0
}

const testCPU = (opc: Opcode, input: CPUState, expected: CPUState, cbPrefixed: boolean = false) => {
    const cpu = setupCPU(input)
    execute(cpu, opc, cbPrefixed)
    const state = dumpState(cpu)
    // keep only the keys in actual that are specified in input
    const actual = {}
    Object.keys(input).forEach( key => {
        actual[key] = state[key]
    })
    expect(actual).toMatchObject(expected)
}    

describe("Add r8", () => {
    test.each([
        [ 
            Opcode.ADD_A_B, // opcode
            {A: 0x0, B: 0x1, F: [1, 0, 1, 0]}, // cpu in
            {A: 0x1, B: 0x1, F: [1, 0, 1, 0]}, // cpu out
            // mmu in
            // mmu out
        ],
    ]
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