import { describe, test, expect } from 'vitest';

import { setupCPU, setupMMU, dumpState, CPUState, MMUState } from './__test__/setup';
import { Opcode } from './opcodes';
import { execute } from './execute';

/**
 * Test Format:
 * ====================
 * [ Opcode.INC_A,                                           <- Opcode to execute
 *   {A: 0x00, B: 0x01, ... HL: 0x00ff, SP: 0xFFF0, PC: 0x0100 },   <- CPU state in
 *   {A: 0x01, B: 0x01, ... HL: 0x00ff, SP: 0xFFF0, PC: 0x0101 },   <- CPU state out
 *   { 0x9f00: 0xff, 0x9f01: [0xfe, 0xfd] },                 <- MMU state in (optional)
 *   { 0x9f00: 0xff, 0x9f01: [0xfe, 0xfd] },                 <- MMU state out (optional)
 *   false,                                                  <- Whether the opcode is one of the CB-prefixed opcodes.
 * ]
 */ 

const testHelper = (opc: Opcode, cpuIn: CPUState, cpuExpected: CPUState, mmuIn: MMUState = {}, mmuExpected: MMUState = {}, cbPrefixed = false) => {
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
    Object.entries(mmuExpected).forEach(([addrString, bytes], i) => {
        const addr = parseInt(addrString) 
        if (Array.isArray(bytes)) {
            bytes.forEach((byte, i) => {
                expect(mmu.rb(addr + i)).toEqual(byte)
            })
        } else {
            const byte = bytes
            expect(mmu.rb(addr)).toEqual(byte)
        }
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
    ])('INC Reg8 test %#: %d %o', testHelper)

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
    ])('INC (HL) test %#: %d %o', testHelper)

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
    ])('DEC Reg8 test %#: %d %o', testHelper)

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
    ])('DEC (HL) test %#: %d %o', testHelper)

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
    ])('DAA test %#: %d %o', testHelper)

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
    ])('CPL test %#: %d %o', testHelper)

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
    ])('SCF test %#: %d %o', testHelper)

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
    ])('CCF test %#: %d %o', testHelper)

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
    ])('ADD A Reg8 test %#: %d %o', testHelper)

    // ADD A (HL) [z0hc]
    test.each([
        [ Opcode.ADD_A_valHL, 
            {A: 0x01, HL: 0xc123, F: [1, 1, 0, 0]}, 
            {A: 0x20, HL: 0xc123, F: [0, 0, 1, 0]}, 
            {"0xc123": 0x1F},
        ],
    ])('ADD A Reg8 test %#: %d %o', testHelper)

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
    ])('ADC A Reg8 test %#: %d %o', testHelper)

    // ADC A (HL) [z0hc]
    test.each([
        [ Opcode.ADC_A_valHL, 
            {A: 0x01, HL: 0xc123, F: [1, 1, 0, 1]}, 
            {A: 0x21, HL: 0xc123, F: [0, 0, 1, 0]}, 
            {"0xc123": 0x1F},
        ],
    ])('ADC A Reg8 test %#: %d %o', testHelper)

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
    ])('SUB A Reg8 test %#: %d %o', testHelper)

    // SUB A (HL) [z1hc]
    test.each([
        [ Opcode.SUB_valHL, 
            {A: 0x20, HL: 0xc123, F: [1, 1, 0, 0]}, 
            {A: 0x01, HL: 0xc123, F: [0, 1, 1, 0]}, 
            {"0xc123": 0x1F},
        ],
    ])('SUB A Reg8 test %#: %d %o', testHelper)

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
    ])('SBC A Reg8 test %#: %d %o', testHelper)

    // SBC A (HL) [z1hc]
    test.each([
        [ Opcode.SBC_A_valHL, 
            {A: 0x20, HL: 0xc123, F: [1, 1, 0, 0]}, 
            {A: 0x01, HL: 0xc123, F: [0, 1, 1, 0]}, 
            {"0xc123": 0x1F},
        ],
    ])('SBC A (HL) test %#: %d %o', testHelper)

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
    ])('AND A Reg8 test %#: %d %o', testHelper)

    // AND A (HL) [z010]
    test.each([
        [ Opcode.AND_valHL, 
            {A: 0x11, HL: 0xc123, F: [1, 1, 0, 0]}, 
            {A: 0x10, HL: 0xc123, F: [0, 0, 1, 0]}, 
            {"0xc123": 0x10},
        ],
    ])('AND A (HL) test %#: %d %o', testHelper)

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
    ])('XOR A Reg8 test %#: %d %o', testHelper)

    // XOR A (HL) [z000]
    test.each([
        [ Opcode.XOR_valHL, 
            {A: 0x11, HL: 0xc123, F: [1, 1, 0, 0]}, 
            {A: 0x01, HL: 0xc123, F: [0, 0, 0, 0]}, 
            {"0xc123": 0x10},
        ],
    ])('XOR A (HL) test %#: %d %o', testHelper)

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
    ])('OR A Reg8 test %#: %d %o', testHelper)

    // OR A (HL) [z000]
    test.each([
        [ Opcode.OR_valHL, 
            {A: 0x01, HL: 0xc123, F: [1, 1, 0, 0]}, 
            {A: 0x11, HL: 0xc123, F: [0, 0, 0, 0]}, 
            {"0xc123": 0x10},
        ],
    ])('OR A (HL) test %#: %d %o', testHelper)
    
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
    ])('CP A Reg8 test %#: %d %o', testHelper)

    // CP A (HL) [z1hc]
    test.each([
        [ Opcode.CP_valHL, 
            {A: 0x01, HL: 0xc123, F: [1, 1, 0, 0]}, 
            {A: 0x01, HL: 0xc123, F: [0, 1, 0, 1]}, 
            {"0xc123": 0x10},
            {"0xc123": 0x10}
        ],
    ])('CP A (HL) test %#: %d %o', testHelper)

    // ADD A d8 [z0hc]
    test.each([
        [ Opcode.ADD_A_d8, 
            {A: 0x11, PC: 0xc122, F: [1, 1, 0, 1]}, 
            {A: 0x21, PC: 0xc124, F: [0, 0, 0, 0]}, 
            {"0xc123": 0x10},
            {"0xc123": 0x10}
        ],
    ])('ADD A d8 test %#: %d %o', testHelper)

    // ADC A d8 [z0hc]
    test.each([
        [ Opcode.ADC_A_d8, 
            {A: 0x11, PC: 0xc122, F: [1, 1, 0, 1]}, 
            {A: 0x22, PC: 0xc124, F: [0, 0, 0, 0]}, 
            {"0xc123": 0x10},
            {"0xc123": 0x10}
        ],
    ])('ADC A d8 test %#: %d %o', testHelper)

    // SUB A d8 [z1hc]
    test.each([
        [ Opcode.SUB_d8, 
            {A: 0x11, PC: 0xc122, F: [1, 1, 0, 1]}, 
            {A: 0x01, PC: 0xc124, F: [0, 1, 0, 0]}, 
            {"0xc123": 0x10},
            {"0xc123": 0x10}
        ],
    ])('SUB d8 test %#: %d %o', testHelper)

    // SBC A d8 [z1hc]
    test.each([
        [ Opcode.SBC_A_d8, 
            {A: 0x11, PC: 0xc122, F: [1, 1, 0, 1]}, 
            {A: 0x00, PC: 0xc124, F: [1, 1, 0, 0]}, 
            {"0xc123": 0x10},
            {"0xc123": 0x10}
        ],
    ])('SBC d8 test %#: %d %o', testHelper)

    // AND A d8 [z010]
    test.each([
        [ Opcode.AND_d8, 
            {A: 0x11, PC: 0xc122, F: [1, 1, 0, 0]}, 
            {A: 0x10, PC: 0xc124, F: [0, 0, 1, 0]}, 
            {"0xc123": 0x10},
            {"0xc123": 0x10}
        ],
    ])('XOR d8 test %#: %d %o', testHelper)

    // XOR A d8 [z000]
    test.each([
        [ Opcode.XOR_d8, 
            {A: 0x11, PC: 0xc122, F: [1, 1, 0, 0]}, 
            {A: 0x01, PC: 0xc124, F: [0, 0, 0, 0]}, 
            {"0xc123": 0x10},
            {"0xc123": 0x10}
        ],
    ])('XOR d8 test %#: %d %o', testHelper)

    // OR A d8 [z000]
    test.each([
        [ Opcode.OR_d8, 
            {A: 0x01, PC: 0xC000, F: [1, 1, 0, 0]}, 
            {A: 0x11, PC: 0xC002, F: [0, 0, 0, 0]}, 
            {"0xC001": 0x10},
        ],
    ])('OR A d8 test %#: %d %o', testHelper)

    // CP A d8 [z1hc]
    test.each([
        [ Opcode.CP_d8, 
            {A: 0x01, PC: 0xd001, F: [1, 1, 0, 0]}, 
            {A: 0x01, PC: 0xd003, F: [0, 1, 0, 1]}, 
            {"0xd002": 0x10},
        ],
    ])('CP d8 test %#: %d %o', testHelper)
})

describe("16 bit arith", () => {
    // INC Reg16 [----]
    test.each([
        [Opcode.INC_BC,
            {BC: 0x00FF, F: [0,1,1,1]},
            {BC: 0x0100, F: [0,1,1,1]}
        ],
        [Opcode.INC_DE,
            {DE: 0x0000, F: [0,1,1,1]},
            {DE: 0x0001, F: [0,1,1,1]}
        ],
        [Opcode.INC_HL,
            {HL: 0xFFFF, F: [0,1,1,1]},
            {HL: 0x0000, F: [0,1,1,1]}
        ],
        [Opcode.INC_SP,
            {SP: 0xFFFE, F: [0,1,1,1]},
            {SP: 0xFFFF, F: [0,1,1,1]}
        ],
    ])('INC Reg16 test %#: %d %o', testHelper)

    // DEC Reg16 [----]
    test.each([
        [Opcode.DEC_BC,
            {BC: 0x0100, F: [0,1,1,1]},
            {BC: 0x00FF, F: [0,1,1,1]}
        ],
        [Opcode.DEC_DE,
            {DE: 0x0001, F: [0,1,1,1]},
            {DE: 0x0000, F: [0,1,1,1]}
        ],
        [Opcode.DEC_HL,
            {HL: 0x0000, F: [0,1,1,1]},
            {HL: 0xFFFF, F: [0,1,1,1]}
        ],
        [Opcode.DEC_SP,
            {SP: 0xFFFF, F: [0,1,1,1]},
            {SP: 0xFFFE, F: [0,1,1,1]}
        ],
    ])('DEC Reg16 test %#: %d %o', testHelper)

    // ADD HL Reg16 [-0hc]
    test.each([
        [Opcode.ADD_HL_BC,
            {HL: 0x1000, BC: 0xF000, F: [1,1,1,1]},
            {HL: 0x0000, BC: 0xF000, F: [1,0,0,1]}
        ],
        [Opcode.ADD_HL_DE,
            {HL: 0x0001, DE: 0xFFFF, F: [1,1,1,1]},
            {HL: 0x0000, DE: 0xFFFF, F: [1,0,0,0]}
        ],
        [Opcode.ADD_HL_HL,
            {HL: 0x0800, F: [1,1,1,1]},
            {HL: 0x1000, F: [1,0,1,0]}
        ],
        [Opcode.ADD_HL_SP,
            {HL: 0x0800, SP: 0x0900, F: [1,1,1,1]},
            {HL: 0x1100, SP: 0x0900, F: [1,0,1,0]}
        ],
    ])('ADD HL Reg16 test %#: %d %o', testHelper)

    // ADD SP r8 [00hc]
    test.each([
        [Opcode.ADD_SP_r8,
            {SP: 0x10FF, PC: 0xc000}, // H and C flags are complicated for this -- eg 0xFFFF + -0x01 = 0xFFFE sets H and C. (subtraction is done with 2's complement addition)
            {SP: 0x1100, PC: 0xc002},
            {"0xc001": 0x01} // 1
        ],
        [Opcode.ADD_SP_r8,
            {SP: 0x1000, PC: 0xd000},
            {SP: 0x0FFF, PC: 0xd002},
            {"0xd001": 0x81} // -1
        ],
    ])('ADD SP r8 test %#: %d %o', testHelper)
})

describe("16 bit loads / stack ops", () => {
    // LD Reg16 d16
    test.each([
        [ Opcode.LD_BC_d16,
            {BC: 0x0000, PC: 0xc000, F: [0,1,1,0]},
            {BC: 0x0001, PC: 0xc003, F: [0,1,1,0]},
            {0xc001: [0x00, 0x01]}
        ],
        [ Opcode.LD_SP_d16,
            {SP: 0x0000, PC: 0xc000, F: [0,1,1,0]},
            {SP: 0x1234, PC: 0xc003, F: [0,1,1,0]},
            {0xc001: [0x12, 0x34]}
        ]
    ])('LD Reg16 d16 test %#: %d %o', testHelper)

    // LD (a16) SP
    test.each([
        [ Opcode.LD_vala16_SP,
            {SP: 0x1234, PC: 0xc000, F: [0,1,1,0]},
            {SP: 0x1234, PC: 0xc003, F: [0,1,1,0]},
            {"0xc001": [0xd0, 0x01], "0xd001": [0xFF, 0xFF]},
            {"0xc001": [0xd0, 0x01], "0xd001": [0x12, 0x34]},
        ],
    ])('LD (a16) SP test %#: %d %o', testHelper)

    // POP Reg16 
    test.each([
        [ Opcode.POP_BC,
            {BC: 0x1234, SP: 0xFFFC, F: [0,1,1,0]},
            {BC: 0xabcd, SP: 0xFFFE, F: [0,1,1,0]},
            {"0xFFFC": [0xab, 0xcd]}
        ],
        [ Opcode.POP_DE,
            {DE: 0x1234, SP: 0xFFFA, F: [0,1,1,0]},
            {DE: 0xabcd, SP: 0xFFFC, F: [0,1,1,0]},
            {"0xFFFA": [0xab, 0xcd]}
        ],
        [ Opcode.POP_AF,
            {AF: 0x1280, SP: 0xFFFA, F: [1,0,0,0]},
            {AF: 0x2210, SP: 0xFFFC, F: [0,0,0,1]}, // note that bottom nyb of F is dropped, and flags are set to top nyb
            {"0xFFFA": [0x22, 0x11]}
        ],
    ])('POP Reg16 test %#: %d %o', testHelper)

    // PUSH Reg16
    test.each([
        [ Opcode.PUSH_BC,
            {BC: 0x1234, SP: 0xFFFE, F: [0,1,1,0]},
            {BC: 0x1234, SP: 0xFFFC, F: [0,1,1,0]},
            {"0xFFFC": [0x00, 0x00]},
            {"0xFFFC": [0x12, 0x34]}
        ],
        [ Opcode.PUSH_DE,
            {DE: 0x1234, SP: 0xFFFC, F: [0,1,1,0]},
            {DE: 0x1234, SP: 0xFFFA, F: [0,1,1,0]},
            {"0xFFFA": [0x00, 0x00]},
            {"0xFFFA": [0x12, 0x34]},
        ],
        [ Opcode.PUSH_AF,
            {AF: 0x1280, SP: 0xFFFC, F: [1,0,0,0]},
            {AF: 0x1280, SP: 0xFFFA, F: [1,0,0,0]}, 
            {"0xFFFA": [0x00, 0x00]},
            {"0xFFFA": [0x12, 0x80]},
        ],
    ])('PUSH Reg16 test %#: %d %o', testHelper)

    // LD HL SP+r8 [00hc]
    test.each([
        // TODO still not sure I understand the HC flags for subtraction.
        [ Opcode.LD_HL_SPplusr8,
            {HL: 0x1F1F, SP: 0x10FF, PC: 0xc000, F: [0,1,1,0]},
            {HL: 0x1100, SP: 0x10FF, PC: 0xc002, F: [0,0,1,1]},
            {"0xc001": 0x01} // +1
        ],
        [ Opcode.LD_HL_SPplusr8,
            {HL: 0x1F1F, SP: 0x00FF, PC: 0xc000, F: [0,1,1,0]},
            {HL: 0x00FE, SP: 0x00FF, PC: 0xc002, F: [0,0,0,0]},
            {"0xc001": 0x81} // -1
        ]
    ])('LD HL SP+r8 test %#: %d %o', testHelper)

    // LD SP HL
    test.each([
        [ Opcode.LD_SP_HL,
            {SP: 0x1234, HL: 0x2222, F: [0,1,1,0]},
            {SP: 0x2222, HL: 0x2222, F: [0,1,1,0]},
        ],
    ])('LD SP HL test %#: %d %o', testHelper)
})

describe("jumps", () => {
    // JR r8
    test.each([
        [ Opcode.JR_r8,
            {PC: 0xc000, F: [0,1,1,0]},
            {PC: 0xc006, F: [0,1,1,0]},
            {0xc001: 0x04} // +4
        ],
        [ Opcode.JR_r8,
            {PC: 0xd000, F: [0,1,1,0]},
            {PC: 0xcff3, F: [0,1,1,0]}, // 0xd002 - 0xF
            {0xd001: 0x8F} // -15
        ],
        [ Opcode.JR_NC_r8, // c = 0, yes jump
            {PC: 0xc000, F: [0,1,1,0]},
            {PC: 0xc006, F: [0,1,1,0]},
            {0xc001: 0x04} // +4
        ],
        [ Opcode.JR_NC_r8, // c = 1, no jump
            {PC: 0xc000, F: [0,1,1,1]},
            {PC: 0xc002, F: [0,1,1,1]},
            {0xc001: 0x04} // +4
        ],
        [ Opcode.JR_C_r8, // c = 0, no jump
            {PC: 0xc000, F: [0,1,1,0]},
            {PC: 0xc002, F: [0,1,1,0]},
            {0xc001: 0x04} // +4
        ],
        [ Opcode.JR_C_r8, // c = 1, yes jump
            {PC: 0xc000, F: [0,1,1,1]},
            {PC: 0xc006, F: [0,1,1,1]},
            {0xc001: 0x04} // +4
        ],
        [ Opcode.JR_NZ_r8, // z = 0, yes jump
            {PC: 0xc000, F: [0,1,1,0]},
            {PC: 0xc006, F: [0,1,1,0]},
            {0xc001: 0x04} // +4
        ],
        [ Opcode.JR_NZ_r8, // z = 1, no jump
            {PC: 0xc000, F: [1,1,1,1]},
            {PC: 0xc002, F: [1,1,1,1]},
            {0xc001: 0x04} // +4
        ],
        [ Opcode.JR_Z_r8, // z = 0, no jump
            {PC: 0xc000, F: [0,1,1,0]},
            {PC: 0xc002, F: [0,1,1,0]},
            {0xc001: 0x04} // +4
        ],
        [ Opcode.JR_Z_r8, // z = 1, yes jump
            {PC: 0xc000, F: [1,1,1,1]},
            {PC: 0xc006, F: [1,1,1,1]},
            {0xc001: 0x04} // +4
        ],
    ])('JR r8 test %#: %d %o', testHelper)

    // JP a16
    test.each([
        [ Opcode.JP_a16,
            {PC: 0xc000, F: [0,1,1,0]},
            {PC: 0x1234, F: [0,1,1,0]},
            {0xc001: [0x12, 0x34]} 
        ],
        [ Opcode.JP_NC_a16, // c = 0, yes jump
            {PC: 0xc000, F: [0,1,1,0]},
            {PC: 0x5678, F: [0,1,1,0]},
            {0xc001: [0x56, 0x78]} 
        ],
        [ Opcode.JP_NC_a16, // c = 1, no jump
            {PC: 0xc000, F: [0,1,1,1]},
            {PC: 0xc003, F: [0,1,1,1]},
            {0xc001: [0xba, 0xbe]} 
        ],
        [ Opcode.JP_C_a16, // c = 0, no jump
            {PC: 0xc000, F: [0,1,1,0]},
            {PC: 0xc003, F: [0,1,1,0]},
            {0xc001: [0xb0, 0x0b]} 
        ],
        [ Opcode.JP_C_a16, // c = 1, yes jump
            {PC: 0xc000, F: [0,1,1,1]},
            {PC: 0xb00b, F: [0,1,1,1]},
            {0xc001: [0xb0, 0x0b]} 
        ],
        [ Opcode.JP_NZ_a16, // z = 0, yes jump
            {PC: 0xc000, F: [0,1,1,0]},
            {PC: 0xb00b, F: [0,1,1,0]},
            {0xc001: [0xb0, 0x0b]}
        ],
        [ Opcode.JP_NZ_a16, // z = 1, no jump
            {PC: 0xc000, F: [1,1,1,1]},
            {PC: 0xc003, F: [1,1,1,1]},
            {0xc001: [0xb0, 0x0b]} 
        ],
        [ Opcode.JP_Z_a16, // z = 0, no jump
            {PC: 0xc000, F: [0,1,1,0]},
            {PC: 0xc003, F: [0,1,1,0]},
            {0xc001: [0xb0, 0x0b]} 
        ],
        [ Opcode.JP_Z_a16, // z = 1, yes jump
            {PC: 0xc000, F: [1,1,1,1]},
            {PC: 0xb00b, F: [1,1,1,1]},
            {0xc001: [0xb0, 0x0b]} 
        ],
    ])('JP a16 test %#: %d %o', testHelper)

    // JP (HL)
    test.each([
        [ Opcode.JP_valHL,
            {PC: 0xc000, HL: 0xd00f, F: [0,1,1,0]},
            {PC: 0xb00b, HL: 0xd00f, F: [0,1,1,0]},
            {0xd00f: [0xb0, 0x0b]}
        ],
    ])('JP (HL) test %#: %d %o', testHelper)

    // CALL a16
    test.each([
        [ Opcode.CALL_a16, // Push PC+1 onto stack and jump to a16.
            {PC: 0xc000, SP: 0xFFFE, F: [0,1,1,0]},
            {PC: 0x1234, SP: 0xFFFC, F: [0,1,1,0]},
            {0xc001: [0x12, 0x34], 0xFFFC: [0xFF, 0xFF]},
            {0xc001: [0x12, 0x34], 0xFFFC: [0xc0, 0x01]} 
        ],
        [ Opcode.CALL_NC_a16, // c = 0, yes jump
            {PC: 0xc000, SP: 0xFFFC, F: [0,1,1,0]},
            {PC: 0x1234, SP: 0xFFFA, F: [0,1,1,0]},
            {0xc001: [0x12, 0x34], 0xFFFA: [0xFF, 0xFF]},
            {0xc001: [0x12, 0x34], 0xFFFA: [0xc0, 0x01]}    
        ],
        [ Opcode.CALL_NC_a16, // c = 1, no jump
            {PC: 0xc000, SP: 0xFFFC, F: [0,1,1,1]},
            {PC: 0xc003, SP: 0xFFFC, F: [0,1,1,1]},
            {0xc001: [0x12, 0x34], 0xFFFA: [0xFF, 0xFF]},
            {0xc001: [0x12, 0x34], 0xFFFA: [0xFF, 0xFF]} 
        ],
        [ Opcode.CALL_C_a16, // c = 1, yes jump
            {PC: 0xc000, SP: 0xFFFC, F: [0,1,1,1]},
            {PC: 0x1234, SP: 0xFFFA, F: [0,1,1,1]},
            {0xc001: [0x12, 0x34], 0xFFFA: [0xFF, 0xFF]},
            {0xc001: [0x12, 0x34], 0xFFFA: [0xc0, 0x01]}    
        ],
        [ Opcode.CALL_C_a16, // c = 0, no jump
            {PC: 0xc000, SP: 0xFFFC, F: [0,1,1,0]},
            {PC: 0xc003, SP: 0xFFFC, F: [0,1,1,0]},
            {0xc001: [0x12, 0x34], 0xFFFA: [0xFF, 0xFF]},
            {0xc001: [0x12, 0x34], 0xFFFA: [0xFF, 0xFF]} 
        ],
        [ Opcode.CALL_NZ_a16, // z = 0, yes jump
            {PC: 0xc000, SP: 0xFFFC, F: [0,1,1,0]},
            {PC: 0x1234, SP: 0xFFFA, F: [0,1,1,0]},
            {0xc001: [0x12, 0x34], 0xFFFA: [0xFF, 0xFF]},
            {0xc001: [0x12, 0x34], 0xFFFA: [0xc0, 0x01]}    
        ],
        [ Opcode.CALL_NZ_a16, // z = 1, no jump
            {PC: 0xc000, SP: 0xFFFC, F: [1,1,1,1]},
            {PC: 0xc003, SP: 0xFFFC, F: [1,1,1,1]},
            {0xc001: [0x12, 0x34], 0xFFFA: [0xFF, 0xFF]},
            {0xc001: [0x12, 0x34], 0xFFFA: [0xFF, 0xFF]} 
        ],
        [ Opcode.CALL_Z_a16, // z = 1, yes jump
            {PC: 0xc000, SP: 0xFFFC, F: [1,1,1,1]},
            {PC: 0x1234, SP: 0xFFFA, F: [1,1,1,1]},
            {0xc001: [0x12, 0x34], 0xFFFA: [0xFF, 0xFF]},
            {0xc001: [0x12, 0x34], 0xFFFA: [0xc0, 0x01]}    
        ],
        [ Opcode.CALL_Z_a16, // z = 0, no jump
            {PC: 0xc000, SP: 0xFFFC, F: [0,1,1,0]},
            {PC: 0xc003, SP: 0xFFFC, F: [0,1,1,0]},
            {0xc001: [0x12, 0x34], 0xFFFA: [0xFF, 0xFF]},
            {0xc001: [0x12, 0x34], 0xFFFA: [0xFF, 0xFF]} 
        ],
    ])('CALL a16 test %#: %d %o', testHelper)

    // RST
    test.each([
        [ Opcode.RST_00H,
            {PC: 0xc000, F: [0,1,1,0]},
            {PC: 0x0000, F: [0,1,1,0]},
        ],
        [ Opcode.RST_08H,
            {PC: 0xc000, F: [0,1,1,0]},
            {PC: 0x0008, F: [0,1,1,0]},
        ],
        [ Opcode.RST_10H,
            {PC: 0xc000, F: [0,1,1,0]},
            {PC: 0x0010, F: [0,1,1,0]},
        ],
        [ Opcode.RST_18H,
            {PC: 0xc000, F: [0,1,1,0]},
            {PC: 0x0018, F: [0,1,1,0]},
        ],
        [ Opcode.RST_20H,
            {PC: 0xc000, F: [0,1,1,0]},
            {PC: 0x0020, F: [0,1,1,0]},
        ],
        [ Opcode.RST_28H,
            {PC: 0xc000, F: [0,1,1,0]},
            {PC: 0x0028, F: [0,1,1,0]},
        ],
        [ Opcode.RST_30H,
            {PC: 0xc000, F: [0,1,1,0]},
            {PC: 0x0030, F: [0,1,1,0]},
        ],
        [ Opcode.RST_38H,
            {PC: 0xc000, F: [0,1,1,0]},
            {PC: 0x0038, F: [0,1,1,0]},
        ],
    ])('RST test %#: %d %o', testHelper)

    // RET
    test.each([
        [ Opcode.RET,
            {PC: 0xdddd, SP: 0xFFFC, F: [0,1,1,0]},
            {PC: 0xc000, SP: 0xFFFE, F: [0,1,1,0]},
            {0xFFFC: [0xc0, 0x00]},
        ],
        [ Opcode.RET_NC, // c = 0, yes ret
            {PC: 0xdddd, SP: 0xFFFA, F: [0,1,1,0]},
            {PC: 0xc0ff, SP: 0xFFFC, F: [0,1,1,0]},
            {0xc001: [0x12, 0x34], 0xFFFA: [0xc0, 0xFF]},
        ],
        [ Opcode.RET_NC, // c = 1, no ret
            {PC: 0xc000, SP: 0xFFFA, F: [0,1,1,1]},
            {PC: 0xc001, SP: 0xFFFA, F: [0,1,1,1]},
            {0xc001: [0x12, 0x34], 0xFFFA: [0xFF, 0xFF]},
        ],
        // [ Opcode.RET_C, // c = 1, yes ret
        //     {PC: 0xc000, SP: 0xFFFA, F: [0,1,1,1]},
        //     {PC: 0xc0ff, SP: 0xFFFC, F: [0,1,1,1]},
        //     {0xc001: [0x12, 0x34], 0xFFFA: [0xc0, 0xFF]},
        // ],
        // [ Opcode.RET_C, // c = 0, no ret
        //     {PC: 0xc000, SP: 0xFFFC, F: [0,1,1,0]},
        //     {PC: 0xc001, SP: 0xFFFC, F: [0,1,1,0]},
        //     {0xc001: [0x12, 0x34], 0xFFFA: [0xFF, 0xFF]},
        // ],
        // [ Opcode.RET_NZ, // z = 0, yes ret
        //     {PC: 0xc000, SP: 0xFFFA, F: [0,1,1,1]},
        //     {PC: 0xc0ff, SP: 0xFFFC, F: [0,1,1,1]},
        //     {0xc001: [0x12, 0x34], 0xFFFA: [0xc0, 0xFF]},
        // ],
        // [ Opcode.RET_NZ, // z = 1, no ret
        //     {PC: 0xc000, SP: 0xFFFC, F: [1,1,1,0]},
        //     {PC: 0xc001, SP: 0xFFFC, F: [1,1,1,0]},
        //     {0xc001: [0x12, 0x34], 0xFFFA: [0xFF, 0xFF]},
        // ],
        // [ Opcode.RET_Z, // z = 1, yes ret
        //     {PC: 0xc000, SP: 0xFFFA, F: [1,1,1,1]},
        //     {PC: 0xc0ff, SP: 0xFFFC, F: [1,1,1,1]},
        //     {0xc001: [0x12, 0x34], 0xFFFA: [0xc0, 0xFF]},
        // ],
        // [ Opcode.RET_Z, // z = 0, no ret
        //     {PC: 0xc000, SP: 0xFFFC, F: [0,1,1,0]},
        //     {PC: 0xc001, SP: 0xFFFC, F: [0,1,1,0]},
        //     {0xc001: [0x12, 0x34], 0xFFFA: [0xFF, 0xFF]},
        // ],
    ])('RET a16 test %#: %d %o', testHelper)

    // RETI
})

describe.skip("8 bit loads", () => {
    // LD (HL+) A 
    // LD A (HL-) 
    // LD (HL-) A 
    // LD A (HL+) 

    // LDH (a8) A
    // LDH A (a8)
    // LD (C) A
    // LD A (C)
    // LD (a16) A
    // LD A (a16)
})

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