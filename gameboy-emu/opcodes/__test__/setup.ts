import { CPU } from "../../cpu"
import { MMU } from "../../mmu"

// usage:
//   0xfa: 0x0a // sets memory[0xfa] to 0x0a
//   0xf0: [0x00, 0x00, 0x00] // sets memory[0xf0 .. 0xf2] to [0x00, 0x00, 0x00]
export interface MMUState {
    [addr: number]: number | [number]
}
export const setupMMU = (state: MMUState): MMU => {
    const mmu = new MMU()
    Object.entries(state).forEach( ([addr, value]) => {
        if (Array.isArray(value)) {
            value.forEach((byte, i) => {
                mmu.wb(parseInt(addr, 16) + i, byte) 
            })
        }
    })
    return mmu
}

export interface CPUState {
    A?: number, 
    B?: number, C?: number, D?: number
    E?: number, H?: number, L?: number
    F?: [0|1, 0|1, 0|1, 0|1],

    BC?: number, DE?: number, HL?: number
    SP?: number, PC?: number
}
export const setupCPU = (state: CPUState, mmu: MMU = new MMU()): CPU => {
    const cpu = new CPU(mmu)
    const {A, B, C, D, E, H, L, F, BC, DE, HL, SP, PC} = state
    if (A !== undefined) cpu.A = A
    if (B !== undefined) cpu.B = B
    if (C !== undefined) cpu.C = C
    if (D !== undefined) cpu.D = D
    if (E !== undefined) cpu.E = E
    if (H !== undefined) cpu.H = H
    if (L !== undefined) cpu.L = L

    if (F !== undefined) {
        cpu.F.z = !!F[0]
        cpu.F.n = !!F[1]
        cpu.F.h = !!F[2]
        cpu.F.c = !!F[3]
    }

    if (BC !== undefined) cpu.setBC(BC)
    if (DE !== undefined) cpu.setBC(DE)
    if (HL !== undefined) cpu.setBC(HL)

    if (SP !== undefined) cpu.SP = SP
    if (PC !== undefined) cpu.PC = PC

    return cpu
}

export const dumpState = (cpu: CPU): CPUState => {
    // compare cpu to expected CPUState
    const state: CPUState = {}
    const {A, B, C, D, E, H, L, F,  SP, PC} = cpu
    if (A !== undefined) state.A = cpu.A
    if (B !== undefined) state.B = cpu.B
    if (C !== undefined) state.C = cpu.C
    if (D !== undefined) state.D = cpu.D
    if (E !== undefined) state.E = cpu.E
    if (H !== undefined) state.H = cpu.H
    if (L !== undefined) state.L = cpu.L

    if (F !== undefined) {
        state.F = [
            cpu.F.z ? 0 : 0,
            cpu.F.n ? 0 : 0,
            cpu.F.h ? 0 : 0,
            cpu.F.c ? 0 : 0,
        ]
    }

    if (BC !== undefined) state.BC = cpu.getBC()
    if (DE !== undefined) state.DE = cpu.getDE()
    if (HL !== undefined) state.HL = cpu.getHL()

    if (SP !== undefined) state.SP = cpu.SP
    if (PC !== undefined) state.PC = cpu.PC

    return state
}