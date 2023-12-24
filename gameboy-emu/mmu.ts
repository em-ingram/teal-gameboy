// MMU represents the Memory Management Unit
// of the Gameboy. 
export class MMU {

    VRAM_OFFSET = 0x4000;
    CART_RAM_OFFSET = 0xA000;
    WORK_RAM_OFFSET = 0xC000;
    UPPER_MEMORY_OFFSET = 0xE000;

    bootROM: Uint8Array
    bootROMEnabled: boolean

    // (Cartridge ROM) 0x0000 - 0x7FFF [0x4000 + (0x4000 * 0xFF) bytes]
    // ---------------------
    // (Bank 0) 0x0000 - 0x3FFF [0x4000 bytes]
    // (Banks 1-NN) 0x4000 - 0x7FFF [0x4000 * 0xFF bytes]
    gameROMBanks: Uint8Array 

    // (VRAM) 0x8000 - 0x9FFF [0x2000 bytes]
    // ------------------------------
    // (VRAM block 0) 0x8000 - 0x87FF
    // (VRAM block 1) 0x8800 - 0x8FFF 
    // (VRAM block 2) 0x9000 - 0x97FF 
    // (BG Tile Map 1) 0x9800 - 0x9BFF
    // (BG Tile Map 2) 0x9C00 - 0x9FFF
    VRAM: Uint8Array 

    // (Cart RAM) 0xA000 - 0xBFFF [0x2000 bytes]
    cartRAM: Uint8Array 

    // (Work RAM) 0xC000 - 0xDFFF  [0x2000 + (0x2000 * 0xFF) bytes]
    // --------------------------------
    // (Bank 0) 0xC000 - 0xCFFF (Work RAM Bank 0)
    // (Banks 1-NN) 0xD000 - 0xDFFF (Work RAM Bank 1-NN)
    workRAMBanks: Uint8Array 

    // 0xE000 - 0xFFFF [0x2000 bytes]
    // ------------------
    // (Echo RAM) 0xE000 - 0xFDFF -- mirrors 0xC000-0xDDFF
    // (oam RAM) 0xFE00 - 0xFE9F 
    // (unusable memory) 0xFEA0 - 0xFEFF
    // (io registers) 0xFF00 - 0xFF7F [0x80 bytes]
    // (high RAM) 0xFF80 - 0xFFFE [0x79 bytes]
    // (Interrupt register) 0xFFFF 
    upperMemory: Uint8Array

    constructor(bootROM: Uint8Array = new Uint8Array(), gameROM: Uint8Array = new Uint8Array()) {
        this.bootROM = new Uint8Array(0x100)
        this.bootROM.set(bootROM)
        this.bootROMEnabled = true

        this.gameROMBanks = new Uint8Array(0x2000 * 0x100)
        this.gameROMBanks.set(gameROM);

        this.VRAM = new Uint8Array(0x2000)
        this.cartRAM = new Uint8Array(0x2000)
        this.workRAMBanks = new Uint8Array(0x2000 * 0x100)
        this.upperMemory = new Uint8Array(0x2000)
    }

    rb(addr: number): number {
        if (addr >= 0xFFFF || addr < 0) {
            console.warn(`Attempted to read invalid address ${addr}`);
            return -1;
        }

        if (this.bootROMEnabled && addr < 0x100) {
            return this.bootROM[addr]
        }

        if (addr >= this.UPPER_MEMORY_OFFSET) {
            return this.upperMemory[addr - this.UPPER_MEMORY_OFFSET]
        } else if (addr >= this.WORK_RAM_OFFSET) {
            // TODO switchable banks
            return this.workRAMBanks[addr - this.WORK_RAM_OFFSET]
        } else if (addr >= this.CART_RAM_OFFSET) {
            return this.cartRAM[addr - this.CART_RAM_OFFSET]
        } else if (addr >= this.VRAM_OFFSET) {
            return this.VRAM[addr - this.VRAM_OFFSET]
        } else {
            return this.gameROMBanks[addr]
        }
    }

    wb(addr: number, val: number): void {
        if (addr >= 0xFFFF || addr < 0) {
            console.warn(`Attempted to write invalid address ${addr}`);
        }

        // Writing 1 to 0xFF50 unmaps the boot ROM from the address space.
        if (this.bootROMEnabled && addr === 0xFF50) {
            this.bootROMEnabled = false;
        }

        if (addr >= this.UPPER_MEMORY_OFFSET) {
            this.upperMemory[addr - this.UPPER_MEMORY_OFFSET] = val
        } else if (addr >= this.WORK_RAM_OFFSET) {
            // TODO switchable banks
            this.workRAMBanks[addr - this.WORK_RAM_OFFSET] = val
        } else if (addr >= this.CART_RAM_OFFSET) {
            this.cartRAM[addr - this.CART_RAM_OFFSET] = val
        } else if (addr >= this.VRAM_OFFSET) {
            this.VRAM[addr - this.VRAM_OFFSET] = val
        } else {
            // Can't write to ROM
            console.warn(`Attempted to write to read-only address ${addr}`);
        }
    }

    rw(addr: number): number {
        if (addr >= 0xFFFF || addr < 0) {
            console.warn(`Attempted to read invalid address ${addr}`);
            return -1;
        }

        if (this.bootROMEnabled && addr < 0x100) {
            const hi = this.bootROM[addr]
            const lo = this.bootROM[addr + 1]
            return (hi << 8) | lo
        }

        var hi: number;
        var lo: number
        if (addr >= this.UPPER_MEMORY_OFFSET) {
            hi = this.upperMemory[addr - this.UPPER_MEMORY_OFFSET]
            lo = this.upperMemory[addr - this.UPPER_MEMORY_OFFSET + 1]
        } else if (addr >= this.WORK_RAM_OFFSET) {
            // TODO switchable banks
            hi = this.workRAMBanks[addr - this.WORK_RAM_OFFSET]
            lo = this.workRAMBanks[addr - this.WORK_RAM_OFFSET + 1]
        } else if (addr >= this.CART_RAM_OFFSET) {
            hi = this.cartRAM[addr - this.CART_RAM_OFFSET]
            lo = this.cartRAM[addr - this.CART_RAM_OFFSET + 1]
        } else if (addr >= this.VRAM_OFFSET) {
            hi = this.VRAM[addr - this.VRAM_OFFSET]
            lo = this.VRAM[addr - this.VRAM_OFFSET + 1]
        } else {
            hi = this.gameROMBanks[addr]
            lo = this.gameROMBanks[addr + 1]
        }
        return (hi << 8) | lo;
    }

    ww(addr: number, val: number): void {
        if (addr >= 0xFFFF || addr < 0) {
            console.warn(`Attempted to read invalid address ${addr}`);
        }
        const hi = val >> 8
        const lo = val // rely on behavior of TypedArrays to convert this to byte.  (Writes to a Uint8Array will keep the first byte of the input)
        if (addr >= this.UPPER_MEMORY_OFFSET) {
            this.upperMemory[addr - this.UPPER_MEMORY_OFFSET] = hi
            this.upperMemory[addr - this.UPPER_MEMORY_OFFSET + 1] = lo
        } else if (addr >= this.WORK_RAM_OFFSET) {
            // TODO switchable banks
            this.workRAMBanks[addr - this.WORK_RAM_OFFSET] = hi
            this.workRAMBanks[addr - this.WORK_RAM_OFFSET + 1] = lo
        } else if (addr >= this.CART_RAM_OFFSET) {
            this.cartRAM[addr - this.CART_RAM_OFFSET] = hi
            this.cartRAM[addr - this.CART_RAM_OFFSET + 1] = lo
        } else if (addr >= this.VRAM_OFFSET) {
            this.VRAM[addr - this.VRAM_OFFSET] = hi
            this.VRAM[addr - this.VRAM_OFFSET + 1] = lo
        } else {
            // Can't write to ROM
            console.warn(`Attempted to write to read-only address ${addr}`);
        }
    }
}