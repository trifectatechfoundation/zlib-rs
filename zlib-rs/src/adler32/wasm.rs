/// Adapted from https://github.com/mcountryman/simd-adler32/blob/main/src/imp/wasm.rs

const MOD: u32 = 65521;
const NMAX: usize = 5552;
const BLOCK_SIZE: usize = 32;
const CHUNK_SIZE: usize = NMAX / BLOCK_SIZE * BLOCK_SIZE;

#[cfg(target_arch = "wasm32")]
use core::arch::wasm32::*;
#[cfg(target_arch = "wasm64")]
use core::arch::wasm64::*;

pub fn adler32_wasm(checksum: u32, data: &[u8]) -> u32 {
    let a = checksum as u16;
    let b = (checksum >> 16) as u16;

    let (a, b) = update(a, b, data);

    (u32::from(b) << 16) | u32::from(a)
}

pub fn update(a: u16, b: u16, data: &[u8]) -> (u16, u16) {
    update_imp(a, b, data)
}

#[inline]
#[target_feature(enable = "simd128")]
fn update_imp(a: u16, b: u16, data: &[u8]) -> (u16, u16) {
    let mut a = a as u32;
    let mut b = b as u32;

    let chunks = data.chunks_exact(CHUNK_SIZE);
    let remainder = chunks.remainder();
    for chunk in chunks {
        update_chunk_block(&mut a, &mut b, chunk);
    }

    update_block(&mut a, &mut b, remainder);

    (a as u16, b as u16)
}

fn update_chunk_block(a: &mut u32, b: &mut u32, chunk: &[u8]) {
    debug_assert_eq!(
        chunk.len(),
        CHUNK_SIZE,
        "Unexpected chunk size (expected {}, got {})",
        CHUNK_SIZE,
        chunk.len()
    );

    reduce_add_blocks(a, b, chunk);

    *a %= MOD;
    *b %= MOD;
}

fn update_block(a: &mut u32, b: &mut u32, chunk: &[u8]) {
    debug_assert!(
        chunk.len() <= CHUNK_SIZE,
        "Unexpected chunk size (expected <= {}, got {})",
        CHUNK_SIZE,
        chunk.len()
    );

    for byte in reduce_add_blocks(a, b, chunk) {
        *a += *byte as u32;
        *b += *a;
    }

    *a %= MOD;
    *b %= MOD;
}

#[inline(always)]
fn reduce_add_blocks<'a>(a: &mut u32, b: &mut u32, chunk: &'a [u8]) -> &'a [u8] {
    if chunk.len() < BLOCK_SIZE {
        return chunk;
    }

    let blocks = chunk.chunks_exact(BLOCK_SIZE);
    let blocks_remainder = blocks.remainder();

    let weight_hi_v = get_weight_hi();
    let weight_lo_v = get_weight_lo();

    let mut p_v = u32x4(*a * blocks.len() as u32, 0, 0, 0);
    let mut a_v = u32x4(0, 0, 0, 0);
    let mut b_v = u32x4(*b, 0, 0, 0);

    for block in blocks {
        let block_ptr = block.as_ptr() as *const v128;
        let v_lo = unsafe { block_ptr.read_unaligned() };
        let v_hi = unsafe { block_ptr.add(1).read_unaligned() };

        p_v = u32x4_add(p_v, a_v);

        a_v = u32x4_add(a_v, u32x4_extadd_quarters_u8x16(v_lo));
        let mad = i32x4_dot_i8x16(v_lo, weight_lo_v);
        b_v = u32x4_add(b_v, mad);

        a_v = u32x4_add(a_v, u32x4_extadd_quarters_u8x16(v_hi));
        let mad = i32x4_dot_i8x16(v_hi, weight_hi_v);
        b_v = u32x4_add(b_v, mad);
    }

    b_v = u32x4_add(b_v, u32x4_shl(p_v, 5));

    *a += reduce_add(a_v);
    *b = reduce_add(b_v);

    blocks_remainder
}

#[inline(always)]
fn i32x4_dot_i8x16(a: v128, b: v128) -> v128 {
    let a_lo = u16x8_extend_low_u8x16(a);
    let a_hi = u16x8_extend_high_u8x16(a);

    let b_lo = u16x8_extend_low_u8x16(b);
    let b_hi = u16x8_extend_high_u8x16(b);

    let lo = i32x4_dot_i16x8(a_lo, b_lo);
    let hi = i32x4_dot_i16x8(a_hi, b_hi);

    i32x4_add(lo, hi)
}

#[inline(always)]
fn u32x4_extadd_quarters_u8x16(a: v128) -> v128 {
    u32x4_extadd_pairwise_u16x8(u16x8_extadd_pairwise_u8x16(a))
}

#[inline(always)]
fn reduce_add(v: v128) -> u32 {
    let arr: [u32; 4] = unsafe { std::mem::transmute(v) };
    let mut sum = 0u32;
    for val in arr {
        sum = sum.wrapping_add(val);
    }
    sum
}

#[inline(always)]
fn get_weight_lo() -> v128 {
    u8x16(
        32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17,
    )
}

#[inline(always)]
fn get_weight_hi() -> v128 {
    u8x16(16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
}
