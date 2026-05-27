//! Folding polynomial constants for CRC32 (bit-reflected polynomial 0xEDB88320).
//! Derived from the Apple zlib implementation in `crc32lt_arm64.s`.

use std::arch::aarch64::{
    poly64x2_t, uint8x16_t, vcombine_p64, vcreate_p64, vdupq_n_u32, vdupq_n_u64, veorq_u8,
    vgetq_lane_p64, vgetq_lane_u32, vgetq_lane_u64, vmull_high_p64, vmull_p64,
    vreinterpretq_p64_u8, vreinterpretq_u32_u8, vreinterpretq_u64_u8, vreinterpretq_u8_p128,
    vreinterpretq_u8_u32, vreinterpretq_u8_u64, vsetq_lane_u32, vsetq_lane_u64,
};

/// pmull v4.1q, v0.1d, k.1d  — polynomial multiply using the low 64-bit lanes.
#[inline]
#[target_feature(enable = "neon")]
#[target_feature(enable = "aes")]
unsafe fn pmull_lo(a: uint8x16_t, k: poly64x2_t) -> uint8x16_t {
    unsafe {
        let a = vreinterpretq_p64_u8(a);
        vreinterpretq_u8_p128(vmull_p64(vgetq_lane_p64::<0>(a), vgetq_lane_p64::<0>(k)))
    }
}

/// Polynomial multiply using the high 64-bit lanes.
#[inline]
#[target_feature(enable = "neon")]
#[target_feature(enable = "aes")]
unsafe fn pmull_hi(a: uint8x16_t, k: poly64x2_t) -> uint8x16_t {
    unsafe { vreinterpretq_u8_p128(vmull_high_p64(vreinterpretq_p64_u8(a), k)) }
}

/// Fold v0 into `next` using constant pair `k`.
#[inline]
#[target_feature(enable = "neon")]
#[target_feature(enable = "aes")]
unsafe fn fold1(v0: uint8x16_t, next: uint8x16_t, k: poly64x2_t) -> uint8x16_t {
    unsafe {
        let t = veorq_u8(pmull_lo(v0, k), next);
        veorq_u8(pmull_hi(v0, k), t)
    }
}

/// CRC32 vectorized using PMULL.  
///
/// Implements the PCLMULQDQ-equivalent technique on AArch64 as described in
/// Intel's "Fast CRC Computation for Generic Polynomials Using PCLMULQDQ Instruction".
///
/// Derived from the Apple zlib implementation of crc32_little_aligned_vector.
#[target_feature(enable = "neon")]
#[target_feature(enable = "aes")]
unsafe fn crc32_little_aligned_vector(crc: u32, mut buf: &[uint8x16_t]) -> u32 {
    // 4-way 512-bit folding: K1 = x^(512+64) mod P(x), K2 = x^512 mod P(x)
    let k12 = vcombine_p64(vcreate_p64(0x154442bd4), vcreate_p64(0x1c6e41596));
    // 1-way 128-bit folding: K3 = x^(128+64) mod P(x), K4 = x^128 mod P(x)
    let k34 = vcombine_p64(vcreate_p64(0x1751997d0), vcreate_p64(0x0ccaa009e));
    // 128-bit -> 64-bit reduction: K5 = x^96 mod P(x), K6 = x^64 mod P(x)
    let k56 = vcombine_p64(vcreate_p64(0x0ccaa009e), vcreate_p64(0x163cd6124));
    // Barrett reduction: ux = floor(x^64 / P(x)), Px = P(x)
    let upx = vcombine_p64(vcreate_p64(0x1F7011641), vcreate_p64(0x1DB710641));

    let mut v0 = buf[0];
    buf = &buf[1..];

    // Insert crc into lane 0 and XOR with the first vector.
    let zero_with_crc = unsafe { vreinterpretq_u8_u32(vsetq_lane_u32::<0>(crc, vdupq_n_u32(0))) };
    v0 = unsafe { veorq_u8(zero_with_crc, v0) };

    if !buf.is_empty() {
        if let [v1, v2, v3, rest @ ..] = buf {
            buf = rest;

            let mut v1 = *v1;
            let mut v2 = *v2;
            let mut v3 = *v3;

            // Fold 4 independent streams using K12.
            while let [n0, n1, n2, n3, rest @ ..] = buf {
                buf = rest;

                let t0 = unsafe { veorq_u8(pmull_lo(v0, k12), *n0) };
                let t1 = unsafe { veorq_u8(pmull_lo(v1, k12), *n1) };
                v0 = unsafe { veorq_u8(pmull_hi(v0, k12), t0) };
                v1 = unsafe { veorq_u8(pmull_hi(v1, k12), t1) };

                let t2 = unsafe { veorq_u8(pmull_lo(v2, k12), *n2) };
                let t3 = unsafe { veorq_u8(pmull_lo(v3, k12), *n3) };
                v2 = unsafe { veorq_u8(pmull_hi(v2, k12), t2) };
                v3 = unsafe { veorq_u8(pmull_hi(v3, k12), t3) };
            }

            // Merge the 4 streams into v0 using K34.
            v0 = unsafe { fold1(v0, v1, k34) };
            v0 = unsafe { fold1(v0, v2, k34) };
            v0 = unsafe { fold1(v0, v3, k34) };
        }

        // Fold remaining single vectors using K34.
        for &next in buf {
            v0 = unsafe { fold1(v0, next, k34) };
        }
    }

    // Reduce 128-bit v0 down to 32-bit CRC.

    // Step 1: 128 -> 96 bits using K5 (i.e. x^96 mod P(x)).

    // Fold low 64 bits forward by 96.
    let v1 = unsafe { pmull_lo(v0, k56) };
    // Shift high 64 bits to low position and XOR.
    let v2 = unsafe {
        let hi = vgetq_lane_u64::<1>(vreinterpretq_u64_u8(v0));
        vreinterpretq_u8_u64(vsetq_lane_u64::<0>(hi, vdupq_n_u64(0)))
    };
    v0 = unsafe { veorq_u8(v2, v1) };

    // Extract the 96-bit result into the correct position for the K6 multiply.
    let v3 = unsafe {
        let u = vreinterpretq_u32_u8(v0);
        let s0 = vgetq_lane_u32::<0>(u);
        let s1 = vgetq_lane_u32::<1>(u);
        let s2 = vgetq_lane_u32::<2>(u);
        let v3 = vreinterpretq_u8_u32(vsetq_lane_u32::<2>(s0, vdupq_n_u32(0)));
        let u = vsetq_lane_u32::<0>(s1, u);
        let u = vsetq_lane_u32::<1>(s2, u);
        v0 = vreinterpretq_u8_u32(u);
        v3
    };

    // Step 2: 96 -> 64 bits using K6 (i.e. x^64 mod P(x)).
    v0 = unsafe { veorq_u8(pmull_hi(v3, k56), v0) };

    // Barrett reduction: 64 -> 32 bits.
    //
    //   T1 = floor(v0 / x^32) * ux
    //   T2 = floor(T1 / x^32) * Px
    //   CRC = (v0 XOR T2) mod x^32

    // Isolate low 32 bits of v0 for T1, clear the other lanes.
    let floor_v0 = unsafe {
        let u = vreinterpretq_u32_u8(v0);
        vreinterpretq_u8_u32(vsetq_lane_u32::<0>(vgetq_lane_u32::<0>(u), vdupq_n_u32(0)))
    };

    let t1 = unsafe { pmull_lo(floor_v0, upx) };

    // Place low 32 bits of T1 into high lane where pmull_hi expects it.
    let floor_t1 = unsafe {
        let u3 = vreinterpretq_u32_u8(v0);
        let t = vgetq_lane_u32::<0>(vreinterpretq_u32_u8(t1));
        vreinterpretq_u8_u32(vsetq_lane_u32::<2>(t, u3))
    };

    let t2 = unsafe { pmull_hi(floor_t1, upx) };

    // Apply correction and extract the output.
    let crc = unsafe { veorq_u8(v0, t2) };
    unsafe { (vgetq_lane_u64::<0>(vreinterpretq_u64_u8(crc)) >> 32) as u32 }
}

/// CRC32 using Apple's pmull-based vectorized algorithm.
///
/// Requires the AES hardware feature (for 64-bit polynomial multiply).
/// Falls back to the scalar table for unaligned prefix, short chunks,
/// and trailing bytes.
#[target_feature(enable = "aes")]
pub unsafe fn crc32_pmull_aarch64(crc: u32, buf: &[u8]) -> u32 {
    let table = super::braid::get_crc_table();
    let mut crc = !crc;
    let mut buf = buf;

    // SAFETY: [u8; 16] safely transmutes into uint8x16_t.
    let (before, mut middle, after) = unsafe { buf.align_to::<uint8x16_t>() };

    // Align to 16 bytes using the CRC table
    for &byte in before {
        crc = (crc >> 8) ^ table[((crc ^ byte as u32) & 0xff) as usize];
    }

    // Minimum 32 bytes needed; process in chunks up to 2^30 bytes.
    while let [_, _, ..] = middle {
        let k = Ord::min(middle.len(), 1 << 26);
        let (now, later) = middle.split_at(k);
        crc = unsafe { crc32_little_aligned_vector(crc, now) };
        middle = later;
    }

    // Trailing bytes (< 32) processed with the CRC table
    for &byte in &buf[buf.len() - (middle.len() * 16 + after.len())..] {
        crc = (crc >> 8) ^ table[((crc ^ byte as u32) & 0xff) as usize];
    }

    !crc
}

#[cfg(test)]
mod tests {
    use super::*;
    quickcheck::quickcheck! {
        fn crc32_pmull_aarch64_is_crc32fast(v: Vec<u8>, start: u32) -> bool {
            if !crate::cpu_features::is_enabled_aes() {
                return true;
            }

            let mut h = crc32fast::Hasher::new_with_initial(start);
            h.update(&v);

            let a = unsafe { crc32_pmull_aarch64(start, &v) };
            let b = h.finalize();

            a == b
        }
    }
}
