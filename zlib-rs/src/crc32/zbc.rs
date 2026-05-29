//! crc32 implementation using the riscv64 zbc ISA extension. Derived from
//! zlib-ng's implementation, see
//! https://github.com/zlib-ng/zlib-ng/blob/da22434b657578c41af1bdf06b27304e4aceb00f/arch/riscv/crc32_zbc.c

use crate::crc32::zbc::asm::{clmul, clmulh};

use super::crc32_braid;

const CLMUL_MIN_LEN: usize = 16;
const CLMUL_CHUNK_LEN: usize = 16;

const CONSTANT_R3: u64 = 0x1751997D0;
const CONSTANT_R4: u64 = 0x0CCAA009E;
const CONSTANT_R5: u64 = 0x163CD6124;
const MASK32: u64 = 0xFFFFFFFF;
const CRCPOLY_TRUE_LE_FULL: u64 = 0x1DB710641;
const CONSTANT_RU: u64 = 0x1F7011641;

/// # Safety
///
/// This function must only be called on riscv64 with the zbc (carryless
/// multiplication) feature.
pub unsafe fn crc32_zbc_riscv64(mut crc: u32, buf: &[u8]) -> u32 {
    if buf.len() < CLMUL_MIN_LEN {
        return crc32_braid(crc, buf);
    }

    let unaligned_len = buf.len() % CLMUL_CHUNK_LEN;
    if unaligned_len > 0 {
        crc = crc32_braid(crc, &buf[..unaligned_len]);
    }

    !crc32_zbc_riscv64_impl(!crc, &buf[unaligned_len..])
}

fn crc32_zbc_riscv64_impl(crc: u32, buf: &[u8]) -> u32 {
    // This unwrap is legal because crc32_zbc_riscv64 guarantees the input is at
    // least 16 bytes.
    let mut low = u64::from_le_bytes(buf[..8].try_into().unwrap()) ^ crc as u64;
    let mut high = u64::from_le_bytes(buf[8..16].try_into().unwrap());

    buf.chunks_exact(16).skip(1).for_each(|chunk| {
        let t2 = clmul(CONSTANT_R4, high);
        let t3 = clmulh(CONSTANT_R4, high);
        let t0_new = clmul(CONSTANT_R3, low);
        let t1_new = clmulh(CONSTANT_R3, low);
        low = t0_new ^ t2;
        high = t1_new ^ t3;
        low ^= u64::from_le_bytes(chunk[..8].try_into().unwrap());
        high ^= u64::from_le_bytes(chunk[8..].try_into().unwrap());
    });

    // Fold the 128-bit result into 64 bits
    let fold_t3 = clmulh(low, CONSTANT_R4);
    let fold_t2 = clmul(low, CONSTANT_R4);
    low = high ^ fold_t2;
    high = fold_t3;

    // Combine the low and high parts and perform polynomial reduction
    let combined = (low >> 32) | ((high & MASK32) << 32);
    let reduced_low = { clmul(low & MASK32, CONSTANT_R5) } ^ combined;

    // Barrett reduction step
    let mut barrett = clmul(reduced_low & MASK32, CONSTANT_RU) & MASK32;
    barrett = clmul(barrett, CRCPOLY_TRUE_LE_FULL);
    let ret = barrett ^ reduced_low;

    (ret >> 32) as u32
}

/// Inline assembly for required instructions, since the intrinsics are nightly
/// compiler only.
mod asm {
    // Returns the lower half of carryless multiplication of rs1 and rs2.
    // See https://riscv.github.io/riscv-isa-manual/snapshot/spec/#insns-clmul
    #[inline(always)]
    pub fn clmul(rs1: u64, rs2: u64) -> u64 {
        let rd;
        unsafe {
            core::arch::asm!(
                "clmul {rd}, {rs1}, {rs2}",
                rs1 = in(reg) rs1,
                rs2 = in(reg) rs2,
                rd = out(reg) rd,
                options(pure, nomem, nostack)
            );
        }
        rd
    }

    // Returns the upper half of carryless multiplication of rs1 and rs2.
    // See https://riscv.github.io/riscv-isa-manual/snapshot/spec/#insns-clmulh
    #[inline(always)]
    pub fn clmulh(rs1: u64, rs2: u64) -> u64 {
        let rd;
        unsafe {
            core::arch::asm!(
                "clmulh {rd}, {rs1}, {rs2}",
                rs1 = in(reg) rs1,
                rs2 = in(reg) rs2,
                rd = out(reg) rd,
                options(pure, nomem, nostack)
            );
        }
        rd
    }
}
