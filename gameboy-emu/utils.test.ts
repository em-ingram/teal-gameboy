import { describe, test, assert, expect } from "vitest"
import { subHalfCarriesByte } from "./utils"

describe("subHalfCarriesByte", () => {
    test.each([
        [0x01, 0x00, false], // 0b0001 - 0b0000
        [0x00, 0x01, true], // 0b0000 - 0b0001
        [0x10, 0x01, true], // 0b0000 - 0b0001
        [0x11, 0x02, true], // 0b0001 - 0b0002
        [0x1f, 0x1f, false], // 0b1111 - 0b1111
        [0x1e, 0x1f, true], // 0b0001 - 1111
        [0x00, 0xFF, true], // 0b0000 - 1111
    ])('subHalfCarriesByte', (a, b, expected) => {
        expect(subHalfCarriesByte(a,b)).toBe(expected)
    })
})