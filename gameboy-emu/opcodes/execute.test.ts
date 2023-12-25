import { describe, test, expect } from 'vitest';

import { setupCPU, setupMMU, dumpState, CPUState, MMUState } from './__test__/setup';
import { Opcode } from './opcodes';
import { execute } from './execute';

interface TestHelperInput {
    opc: Opcode
    cbPrefixed?: boolean
    cpuIn: CPUState
    cpuExpected: CPUState
    mmuIn?: MMUState
    mmuExpected?: MMUState
}
const testHelper = ({opc, cbPrefixed = false, cpuIn, cpuExpected, mmuIn = {}, mmuExpected ={}}: TestHelperInput) => {
    const mmu = setupMMU(mmuIn)
    const cpu = setupCPU(cpuIn, mmu)
    execute(cpu, opc, cbPrefixed)

    // test CPU
    const state = dumpState(cpu)
    // keep only the keys in actual that are specified in input
    const cpuActual = {}
    Object.keys(cpuIn).forEach( key => {
        cpuActual[key] = state[key]
    })
    expect(cpuActual).toMatchObject(cpuExpected)

    // test MMU
    Object.entries(mmuExpected).forEach(([addr, byte]) => {
        expect(mmu.rb(parseInt(addr, 16))).toEqual(byte)
    })
}    

describe("8 bit arith", () => {
    // INC R8 [z0h-]
    test.each([
        [ Opcode.INC_A, 
            {A: 0x0, F: [1, 1, 1, 1]}, 
            {A: 0x1, F: [0, 0, 0, 1]}, 
        ],
        [ Opcode.INC_B, 
            {B: 0xFF, F: [0, 1, 1, 1]}, 
            {B: 0x00, F: [1, 0, 1, 1]}, 
        ],
        [ Opcode.INC_D, 
            {D: 0x0F, F: [1, 0, 0, 1]}, 
            {D: 0x10, F: [0, 0, 1, 1]}, 
        ],
        [ Opcode.INC_L, 
            {L: 0x01, F: [1, 1, 0, 0]}, 
            {L: 0x02, F: [0, 0, 0, 0]}, 
        ],
    ])('INC Reg8 test %#: %d %o', (opc, cpuIn, cpuExpected) => testHelper({opc, cpuIn, cpuExpected}))

    // INC (HL) [z0h-]
    test.each([
        [ Opcode.INC_valHL, 
            {HL: 0xd123, F: [1, 1, 0, 0]}, 
            {HL: 0xd123, F: [1, 0, 1, 0]}, 
            {"0xd123": 0xFF},
            {"0xd123": 0x00}
        ],
        [ Opcode.INC_valHL, 
            {HL: 0xd123, F: [1, 1, 1, 1]}, 
            {HL: 0xd123, F: [0, 0, 1, 1]}, 
            {"0xd123": 0xEF},
            {"0xd123": 0xF0}
        ],
    ])('INC (HL) test %#: %d %o', (opc, cpuIn, cpuExpected, mmuIn, mmuExpected) => testHelper({opc, cpuIn, cpuExpected, mmuIn, mmuExpected}))

    // DEC R8 [z1h-]
    test.each([
        [ Opcode.DEC_A, 
            {A: 0x1, F: [1, 1, 1, 1]}, 
            {A: 0x0, F: [1, 1, 0, 1]}, 
        ],
        [ Opcode.DEC_C, 
            {C: 0x00, F: [1, 1, 1, 1]}, 
            {C: 0xFF, F: [0, 1, 1, 1]}, 
        ],
        [ Opcode.DEC_D, 
            {D: 0x10, F: [1, 0, 0, 1]}, 
            {D: 0x0F, F: [0, 1, 1, 1]}, 
        ],
        [ Opcode.DEC_H, 
            {H: 0x02, F: [1, 1, 0, 0]}, 
            {H: 0x01, F: [0, 1, 0, 0]}, 
        ],
    ])('DEC Reg8 test %#: %d %o', (opc, cpuIn, cpuExpected) => testHelper({opc, cpuIn, cpuExpected}))

    // DEC (HL) [z1h-]
    test.each([
        [ Opcode.DEC_valHL, 
            {HL: 0xd123, F: [1, 1, 0, 0]}, 
            {HL: 0xd123, F: [1, 1, 0, 0]}, 
            {"0xd123": 0x01},
            {"0xd123": 0x00}
        ],
        [ Opcode.DEC_valHL, 
            {HL: 0xd123, F: [1, 1, 1, 0]}, 
            {HL: 0xd123, F: [0, 1, 1, 0]}, 
            {"0xd123": 0x00},
            {"0xd123": 0xFF}
        ],
        [ Opcode.DEC_valHL, 
            {HL: 0xd123, F: [1, 1, 1, 1]}, 
            {HL: 0xd123, F: [0, 1, 1, 1]}, 
            {"0xd123": 0x10},
            {"0xd123": 0x0F}
        ],
    ])('DEC (HL) test %#: %d %o', (opc, cpuIn, cpuExpected, mmuIn, mmuExpected) => testHelper({opc, cpuIn, cpuExpected, mmuIn, mmuExpected}))

    // DAA [z-0C]
    test.each([
        [ Opcode.DAA, 
            {A: 0x0a, F: [0, 0, 0, 0]}, 
            {A: 0x10, F: [0, 0, 0, 0]}, 
        ],
        [ Opcode.DAA, 
            {A: 0x10, F: [1, 0, 0, 0]}, 
            {A: 0x10, F: [0, 0, 0, 0]}, 
        ],
        // TODO could use some more test cases, I just don't understand DAA well enough to write them.
    ])('DAA test %#: %d %o', (opc, cpuIn, cpuExpected) => testHelper({opc, cpuIn, cpuExpected}))

    // CPL [-11-]
    test.each([
        [ Opcode.CPL, 
            {A: 0x01, F: [1, 0, 0, 1]}, 
            {A: 0xFE, F: [1, 1, 1, 1]}, 
        ],
        [ Opcode.CPL, 
            {A: 0x00, F: [0, 0, 0, 1]}, 
            {A: 0xFF, F: [0, 1, 1, 1]}, 
        ],
    ])('CPL test %#: %d %o', (opc, cpuIn, cpuExpected) => testHelper({opc, cpuIn, cpuExpected}))

    // SCF [-001]
    test.each([
        [ Opcode.SCF, 
            {A: 0x1, B: 0x1, F: [1, 1, 1, 1]}, 
            {A: 0x1, B: 0x1, F: [1, 0, 0, 1]}, 
        ],
        [ Opcode.SCF, 
            {H: 0x1, B: 0x1, F: [0, 1, 1, 0]}, 
            {H: 0x1, B: 0x1, F: [0, 0, 0, 1]}, 
        ],
    ])('SCF test %#: %d %o', (opc, cpuIn, cpuExpected) => testHelper({opc, cpuIn, cpuExpected}))

    // CCF [-00C]
    test.each([
        [ Opcode.CCF, 
            {A: 0x1, B: 0x1, F: [1, 1, 1, 1]}, 
            {A: 0x1, B: 0x1, F: [1, 0, 0, 0]}, 
        ],
        [ Opcode.CCF, 
            {H: 0x1, B: 0x1, F: [0, 1, 0, 0]}, 
            {H: 0x1, B: 0x1, F: [0, 0, 0, 1]}, 
        ],
    ])('CCF test %#: %d %o', (opc, cpuIn, cpuExpected) => testHelper({opc, cpuIn, cpuExpected}))

    // ADD A Reg8 [z0hc]
    test.each([
        [ Opcode.ADD_A_B, 
            {A: 0x0, B: 0x1, F: [1, 1, 1, 1]}, 
            {A: 0x1, B: 0x1, F: [0, 0, 0, 0]}, 
        ],
        [ Opcode.ADD_A_C, 
            {A: 0xFF, C: 0x1, F: [0, 1, 1, 1]}, 
            {A: 0x0, C: 0x1, F: [1, 0, 1, 1]}, 
        ],
        [ Opcode.ADD_A_D, 
            {A: 0x0F, D: 0x1, F: [1, 0, 0, 1]}, 
            {A: 0x10, D: 0x1, F: [0, 0, 1, 0]}, 
        ],
        [ Opcode.ADD_A_E, 
            {A: 0x01, E: 0xFF, F: [1, 1, 0, 0]}, 
            {A: 0x00, E: 0xFF, F: [1, 0, 1, 1]}, 
        ],
    ])('ADD A Reg8 test %#: %d %o', (opc, cpuIn, cpuExpected) => testHelper({opc, cpuIn, cpuExpected}))

    // ADD A (HL) [z0hc]
    test.each([
        [ Opcode.ADD_A_valHL, 
            {A: 0x01, HL: 0xc123, F: [1, 1, 0, 0]}, 
            {A: 0x20, HL: 0xc123, F: [0, 0, 1, 0]}, 
            {"0xc123": 0x1F},
        ],
    ])('ADD A Reg8 test %#: %d %o', (opc, cpuIn, cpuExpected, mmuIn) => testHelper({opc, cpuIn, cpuExpected, mmuIn}))

    // ADC [z0hc]
    test.each([
        [ Opcode.ADC_A_B, 
            {A: 0x0, B: 0x1, F: [1, 1, 1, 1]}, 
            {A: 0x2, B: 0x1, F: [0, 0, 0, 0]}, 
        ],
        [ Opcode.ADC_A_A, 
            {A: 0x2, F: [0, 1, 1, 1]}, 
            {A: 0x5, F: [0, 0, 0, 0]}, 
        ],
        [ Opcode.ADC_A_D, 
            {A: 0x0F, D: 0x1, F: [1, 0, 0, 1]}, 
            {A: 0x11, D: 0x1, F: [0, 0, 1, 0]}, 
        ],
        [ Opcode.ADC_A_E, 
            {A: 0x01, E: 0xFF, F: [1, 1, 0, 0]}, 
            {A: 0x00, E: 0xFF, F: [1, 0, 1, 1]}, 
        ],
    ])('ADC A Reg8 test %#: %d %o', (opc, cpuIn, cpuExpected) => testHelper({opc, cpuIn, cpuExpected}))

    // ADC A (HL) [z0hc]
    test.each([
        [ Opcode.ADC_A_valHL, 
            {A: 0x01, HL: 0xc123, F: [1, 1, 0, 1]}, 
            {A: 0x21, HL: 0xc123, F: [0, 0, 1, 0]}, 
            {"0xc123": 0x1F},
        ],
    ])('ADC A Reg8 test %#: %d %o', (opc, cpuIn, cpuExpected, mmuIn) => testHelper({opc, cpuIn, cpuExpected, mmuIn}))

    // SUB A Reg8 [z1hc]
    test.each([
        [ Opcode.SUB_A, 
            {A: 0x0, F: [1, 1, 1, 1]}, 
            {A: 0x0, F: [1, 1, 0, 0]}, 
        ],
        [ Opcode.SUB_A, 
            {A: 0x1, F: [1, 1, 1, 1]}, 
            {A: 0x0, F: [1, 1, 0, 0]}, 
        ],
        [ Opcode.SUB_C, 
            {A: 0x00, C: 0x1, F: [0, 1, 1, 1]}, 
            {A: 0xFF, C: 0x1, F: [0, 1, 1, 1]}, 
        ],
        [ Opcode.SUB_D, 
            {A: 0x10, D: 0x1, F: [1, 0, 0, 1]}, 
            {A: 0x0F, D: 0x1, F: [0, 1, 1, 0]}, 
        ],
        [ Opcode.SUB_E, 
            {A: 0x01, E: 0xFF, F: [1, 1, 0, 0]}, 
            {A: 0x02, E: 0xFF, F: [0, 1, 1, 1]}, 
        ],
        [ Opcode.SUB_L, 
            {A: 0x00, L: 0xFF, F: [1, 1, 0, 0]}, 
            {A: 0x01, L: 0xFF, F: [0, 1, 1, 1]}, 
        ],
    ])('SUB A Reg8 test %#: %d %o', (opc, cpuIn, cpuExpected) => testHelper({opc, cpuIn, cpuExpected}))

    // SUB A (HL) [z1hc]
    test.each([
        [ Opcode.SUB_valHL, 
            {A: 0x20, HL: 0xc123, F: [1, 1, 0, 0]}, 
            {A: 0x01, HL: 0xc123, F: [0, 1, 1, 0]}, 
            {"0xc123": 0x1F},
        ],
    ])('SUB A Reg8 test %#: %d %o', (opc, cpuIn, cpuExpected, mmuIn) => testHelper({opc, cpuIn, cpuExpected, mmuIn}))

    // SBC A Reg8 [z1hc]
    test.each([
        [ Opcode.SBC_A_A, 
            {A: 0x00, F: [1, 1, 1, 1]}, 
            {A: 0xFF, F: [0, 1, 1, 1]}, 
        ],
        [ Opcode.SBC_A_A, 
            {A: 0x01, F: [1, 1, 1, 1]}, 
            {A: 0xFF, F: [0, 1, 1, 1]}, 
        ],
        [ Opcode.SBC_A_C, 
            {A: 0x00, C: 0x1, F: [0, 1, 1, 1]}, 
            {A: 0xFE, C: 0x1, F: [0, 1, 1, 1]}, 
        ],
        [ Opcode.SBC_A_D, 
            {A: 0x10, D: 0x1, F: [1, 0, 0, 1]}, 
            {A: 0x0E, D: 0x1, F: [0, 1, 1, 0]}, 
        ],
        [ Opcode.SBC_A_E, 
            {A: 0x01, E: 0xFF, F: [1, 1, 0, 0]}, 
            {A: 0x02, E: 0xFF, F: [0, 1, 1, 1]}, 
        ],
        [ Opcode.SBC_A_L, 
            {A: 0x00, L: 0xFF, F: [1, 1, 0, 0]}, 
            {A: 0x01, L: 0xFF, F: [0, 1, 1, 1]}, 
        ],
    ])('SBC A Reg8 test %#: %d %o', (opc, cpuIn, cpuExpected) => testHelper({opc, cpuIn, cpuExpected}))

    // SBC A (HL) [z1hc]
    test.each([
        [ Opcode.SBC_A_valHL, 
            {A: 0x20, HL: 0xc123, F: [1, 1, 0, 0]}, 
            {A: 0x01, HL: 0xc123, F: [0, 1, 1, 0]}, 
            {"0xc123": 0x1F},
        ],
    ])('SBC A (HL) test %#: %d %o', (opc, cpuIn, cpuExpected, mmuIn) => testHelper({opc, cpuIn, cpuExpected, mmuIn}))

    // AND A Reg8 [z010]
    test.each([
        [ Opcode.AND_A,
            {A: 0x10, F: [1, 0, 1, 1]},
            {A: 0x10, F: [0, 0, 1, 0]}
        ],
        [ Opcode.AND_D,
            {A: 0x10, D: 0x00, F: [1, 0, 1, 1]},
            {A: 0x00, D: 0x00, F: [1, 0, 1, 0]}
        ],
        [ Opcode.AND_L,
            {A: 0x11, L: 0x10, F: [1, 1, 0, 1]},
            {A: 0x10, L: 0x10, F: [0, 0, 1, 0]}
        ],
    ])('AND A Reg8 test %#: %d %o', (opc, cpuIn, cpuExpected) => testHelper({opc, cpuIn, cpuExpected}))

    // AND A (HL) [z010]
    test.each([
        [ Opcode.AND_valHL, 
            {A: 0x11, HL: 0xc123, F: [1, 1, 0, 0]}, 
            {A: 0x10, HL: 0xc123, F: [0, 0, 1, 0]}, 
            {"0xc123": 0x10},
        ],
    ])('AND A (HL) test %#: %d %o', (opc, cpuIn, cpuExpected, mmuIn) => testHelper({opc, cpuIn, cpuExpected, mmuIn}))

    // XOR A Reg8 [z000]
    test.each([
        [ Opcode.XOR_A,
            {A: 0x10, F: [1, 0, 1, 1]},
            {A: 0x00, F: [1, 0, 0, 0]}
        ],
        [ Opcode.XOR_D,
            {A: 0x10, D: 0x00, F: [1, 0, 1, 1]},
            {A: 0x10, D: 0x00, F: [0, 0, 0, 0]}
        ],
        [ Opcode.XOR_L,
            {A: 0x11, L: 0x10, F: [1, 1, 0, 1]},
            {A: 0x01, L: 0x10, F: [0, 0, 0, 0]}
        ],
    ])('XOR A Reg8 test %#: %d %o', (opc, cpuIn, cpuExpected) => testHelper({opc, cpuIn, cpuExpected}))

    // XOR A (HL) [z000]
    test.each([
        [ Opcode.XOR_valHL, 
            {A: 0x11, HL: 0xc123, F: [1, 1, 0, 0]}, 
            {A: 0x01, HL: 0xc123, F: [0, 0, 0, 0]}, 
            {"0xc123": 0x10},
        ],
    ])('XOR A (HL) test %#: %d %o', (opc, cpuIn, cpuExpected, mmuIn) => testHelper({opc, cpuIn, cpuExpected, mmuIn}))

    // OR A Reg8 [z000]
    test.each([
        [ Opcode.OR_A,
            {A: 0x10, F: [1, 0, 1, 1]},
            {A: 0x10, F: [0, 0, 0, 0]}
        ],
        [ Opcode.OR_E,
            {A: 0x10, E: 0x00, F: [1, 0, 1, 1]},
            {A: 0x10, E: 0x00, F: [0, 0, 0, 0]}
        ],
        [ Opcode.OR_H,
            {A: 0x11, H: 0x10, F: [1, 1, 0, 1]},
            {A: 0x11, H: 0x10, F: [0, 0, 0, 0]}
        ],
        [ Opcode.OR_C,
            {A: 0x00, C: 0x00, F: [1, 1, 0, 1]},
            {A: 0x00, C: 0x00, F: [1, 0, 0, 0]}
        ],
    ])('OR A Reg8 test %#: %d %o', (opc, cpuIn, cpuExpected) => testHelper({opc, cpuIn, cpuExpected}))

    // OR A (HL) [z000]
    test.each([
        [ Opcode.OR_valHL, 
            {A: 0x01, HL: 0xc123, F: [1, 1, 0, 0]}, 
            {A: 0x11, HL: 0xc123, F: [0, 0, 0, 0]}, 
            {"0xc123": 0x10},
        ],
    ])('OR A (HL) test %#: %d %o', (opc, cpuIn, cpuExpected, mmuIn) => testHelper({opc, cpuIn, cpuExpected, mmuIn}))
    
    // CP A Reg8 [z1hc]
    test.each([
        [ Opcode.CP_A,
            {A: 0x10, F: [1, 0, 1, 1]},
            {A: 0x10, F: [1, 1, 0, 0]}
        ],
        [ Opcode.CP_E,
            {A: 0x10, E: 0x00, F: [1, 0, 1, 1]},
            {A: 0x10, E: 0x00, F: [0, 1, 0, 0]}
        ],
        [ Opcode.CP_H,
            {A: 0x11, H: 0x20, F: [1, 1, 0, 1]},
            {A: 0x11, H: 0x20, F: [0, 1, 0, 1]}
        ],
        [ Opcode.CP_C,
            {A: 0x21, C: 0x12, F: [1, 1, 0, 1]},
            {A: 0x21, C: 0x12, F: [0, 1, 1, 0]}
        ],
    ])('CP A Reg8 test %#: %d %o', (opc, cpuIn, cpuExpected) => testHelper({opc, cpuIn, cpuExpected}))

    // CP A (HL) [z1hc]
    test.each([
        [ Opcode.CP_valHL, 
            {A: 0x01, HL: 0xc123, F: [1, 1, 0, 0]}, 
            {A: 0x01, HL: 0xc123, F: [0, 1, 0, 1]}, 
            {"0xc123": 0x10},
        ],
    ])('CP A (HL) test %#: %d %o', (opc, cpuIn, cpuExpected, mmuIn) => testHelper({opc, cpuIn, cpuExpected, mmuIn}))

    // ADD A d8 [z0hc]
    // ADC A d8 [z0hc]

    // SUB A d8 [z1hc]
    // SBC A d8 [z1hc]
    // AND A d8 [z010]
    // XOR A d8 [z000]
    test.each([
        [ Opcode.XOR_d8, 
            {A: 0x11, PC: 0xc122, F: [1, 1, 0, 0]}, 
            {A: 0x01, PC: 0xc123, F: [0, 0, 0, 0]}, 
            {"0xc123": 0x10},
        ],
    ])('XOR d8 test %#: %d %o', (opc, cpuIn, cpuExpected, mmuIn) => testHelper({opc, cpuIn, cpuExpected, mmuIn}))

    // OR A d8 [z000]
    test.each([
        [ Opcode.OR_d8, 
            {A: 0x01, PC: 0xC000, F: [1, 1, 0, 0]}, 
            {A: 0x11, PC: 0xC001, F: [0, 0, 0, 0]}, 
            {"0xC001": 0x10},
        ],
    ])('OR A d8 test %#: %d %o', (opc, cpuIn, cpuExpected, mmuIn) => testHelper({opc, cpuIn, cpuExpected, mmuIn}))

    // CP A d8 [z1hc]
    test.each([
        [ Opcode.CP_d8, 
            {A: 0x01, PC: 0xd001, F: [1, 1, 0, 0]}, 
            {A: 0x01, PC: 0xd002, F: [0, 1, 0, 1]}, 
            {"0xd002": 0x10},
        ],
    ])('CP d8 test %#: %d %o', (opc, cpuIn, cpuExpected, mmuIn) => testHelper({opc, cpuIn, cpuExpected, mmuIn}))

})

// describe("16 bit arith", () => {
//     // INC Reg16 [----]
//     // DEC Reg16 [----]
//     // ADD HL Reg16 [-0hc]
//     // ADD SP r8 [00hc]
// })

// describe("16 bit loads / stack ops", () => {
//     // LD Reg16 d16o
//     // LD (a16) SP
//     // POP Reg16 
//     // POP AF [znhc]
//     // PUSH Reg16
//     // LD HL SP+r8 [00hc]
//     // LD SP HL
// })

// describe("jumps", () => {
//     // JR r8
//     // JP a16
//     // CALL a16
//     // RST
//     // RET
//     // RETI
// })

// describe("bit shifts", () => {
//     // RLCA [000c]
//     // RRCA [000c]
//     // RLA [000c]
//     // RRA [000c]

//     // CB RLC Reg8 [z00c]
//     // CB RLC (HL) [z00c]

//     // CB RRC Reg8 [z00c]
//     // CB RRC (HL) [z00c]

//     // CB RL Reg8 [z00c]
//     // CB RL (HL) [z00c]

//     // CB RR Reg8 [z00c]
//     // CB RR (HL) [z00c]

//     // CB SLA Reg8 [z00c]
//     // CB SLA (HL) [z00c]

//     // CB SRA Reg8 [z000]
//     // CB SRA (HL) [z000]

//     // CB SWAP Reg8 [z000]
//     // CB SWAP (HL) [z000]

//     // CB SRL Reg8 [z00c]
//     // CB SRL (HL) [z00c]

//     // BIT n Reg8 [z01-]
//     // BIT n (HL) [z01-]

//     // RES n Reg8 [----]
//     // RES n (HL) [----]

//     // SET n Reg8 [----]
//     // SET n (HL) [----]

// })