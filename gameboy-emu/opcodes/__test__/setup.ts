import { CPU } from "../../cpu"
import { MMU } from "../../mmu"

// usage:
//   0x00fa: 0x0a // sets memory[0xfa] to 0x0a
//   0x00f0: [0x00, 0x00, 0x00] // sets memory[0xf0 .. 0xf2] to [0x00, 0x00, 0x00]
export interface MMUState {
    [addr: number]: number | number[]
}
export const setupMMU = (state: MMUState): MMU => {
    const mmu = new MMU()
    mmu.bootROMEnabled = false
    Object.entries(state).forEach( ([addrString, value]) => {
        const addr = parseInt(addrString)
        if (Array.isArray(value)) {
            value.forEach((byte, i) => {
                mmu.wb(addr + i, byte) 
            })
        } else {
            mmu.wb(addr, value)
        }
    })
    return mmu
}

export interface CPUState {
    A?: number, 
    B?: number, C?: number, D?: number
    E?: number, H?: number, L?: number
    F?: number[],

    AF?: number,
    BC?: number, DE?: number, HL?: number
    SP?: number, PC?: number,
    IME?: boolean
    scheduledIME?: boolean | undefined
}
export const setupCPU = (state: CPUState, mmu: MMU = new MMU()): CPU => {
    const cpu = new CPU(mmu)
    const {A, B, C, D, E, H, L, F, AF, BC, DE, HL, SP, PC, IME, scheduledIME} = state
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

    if (AF !== undefined) cpu.setAF(AF)
    if (BC !== undefined) cpu.setBC(BC)
    if (DE !== undefined) cpu.setDE(DE)
    if (HL !== undefined) cpu.setHL(HL)

    if (SP !== undefined) cpu.SP = SP
    if (PC !== undefined) cpu.PC = PC

    if (IME !== undefined) cpu.ime = IME
    if (scheduledIME !== undefined) cpu.scheduledIME = scheduledIME

    return cpu
}

export const dumpState = (cpu: CPU): CPUState => {
    return {
        ...cpu,
        F: [
            cpu.F.z ? 1 : 0,
            cpu.F.n ? 1 : 0,
            cpu.F.h ? 1 : 0,
            cpu.F.c ? 1 : 0,
        ],
        AF: cpu.getAF(),
        BC: cpu.getBC(),
        DE: cpu.getDE(),
        HL: cpu.getHL(),
        SP: cpu.SP,
        PC: cpu.PC,
        IME: cpu.ime,
        scheduledIME: cpu.scheduledIME,
    }
}