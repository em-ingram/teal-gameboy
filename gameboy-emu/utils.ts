export const addCarriesByte = (a: number, b: number): boolean => {
    return (a + b) > 0xFF
}

export const addCarriesWord = (a: number, b: number): boolean => {
    return (a + b) > 0xFFFF
}

// Checks for a carry from bit 3 to bit 4
// Optional parameter c is used for adc instruction, representing value of carry flag (0 or 1)
export const addHalfCarriesByte = (a: number, b: number, c: number = 0): boolean => {
    return (((a & 0xF) + (b & 0xF) + (c ? 1 : 0)) & 0x10) === 0x10
}

// Checks for a carry from bit 3 to bit 4
// Optional parameter c is used for adc instruction, representing value of carry flag (0 or 1)
export const subHalfCarriesByte = (a: number, b: number, c: number = 0): boolean => {
    return (((a & 0xF) - (b & 0xF) - (c ? 1 : 0)) & 0x10) === 0x10
}

export const addHalfCarriesWord = (a: number, b: number): boolean => {
    return (((a & 0xFFF) + (b & 0xFFF)) & 0x1000) === 0x1000
}

const MAX_UINT8 = 0xFF
export const uint8 = (n: number) => {
    if (n < 0) {
        const mod = (n * -1) % MAX_UINT8
        if (mod === 0) return 0
        return MAX_UINT8 - mod
    }
    return n & MAX_UINT8
}

const MAX_UINT16 = 0xFFFF
export const uint16 = (n: number) => {
    if (n < 0) {
        const mod = (n * -1) % MAX_UINT16
        if (mod === 0) return 0
        return MAX_UINT16 - mod
    }
    return n & MAX_UINT16
}
    
export const int8 = (n: number) => {
    const byte = uint8(n)
    const sign = byte & 0x80
    return (byte & 0x7F) * sign
}


