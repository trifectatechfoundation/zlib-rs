use std::mem::MaybeUninit;

#[cfg(target_arch = "x86_64")]
mod avx2;
mod generic;
#[cfg(target_arch = "aarch64")]
mod neon;

pub fn adler32_aligned(start_checksum: u32, data: &[u8]) -> u32 {
    avx2::adler32_aligned_avx2(start_checksum, data)
}

pub fn adler32(start_checksum: u32, data: &[u8]) -> u32 {
    #[cfg(target_arch = "x86_64")]
    if std::is_x86_feature_detected!("avx2") {
        return avx2::adler32_avx2(start_checksum, data);
    }

    #[cfg(target_arch = "aarch64")]
    if std::arch::is_aarch64_feature_detected!("neon") {
        return self::neon::adler32_neon(start_checksum, data);
    }

    generic::adler32_rust(start_checksum, data)
}

pub fn adler32_fold_copy(start_checksum: u32, dst: &mut [MaybeUninit<u8>], src: &[u8]) -> u32 {
    debug_assert!(dst.len() >= src.len(), "{} < {}", dst.len(), src.len());

    #[cfg(target_arch = "x86_64")]
    if std::is_x86_feature_detected!("avx2") {
        return avx2::adler32_fold_copy_avx2(start_checksum, dst, src);
    }

    let adler = adler32(start_checksum, src);
    dst[..src.len()].copy_from_slice(slice_to_uninit(src));
    adler
}

// when stable, use MaybeUninit::write_slice
fn slice_to_uninit(slice: &[u8]) -> &[MaybeUninit<u8>] {
    // safety: &[T] and &[MaybeUninit<T>] have the same layout
    unsafe { &*(slice as *const [u8] as *const [MaybeUninit<u8>]) }
}

// inefficient but correct, useful for testing
#[cfg(test)]
fn naive_adler32(start_checksum: u32, data: &[u8]) -> u32 {
    const MOD_ADLER: u32 = 65521; // Largest prime smaller than 2^16

    let mut a = start_checksum & 0xFFFF;
    let mut b = (start_checksum >> 16) & 0xFFFF;

    for &byte in data {
        a = (a + byte as u32) % MOD_ADLER;
        b = (b + a) % MOD_ADLER;
    }

    (b << 16) | a
}

const BASE: u32 = 65521; /* largest prime smaller than 65536 */
const NMAX: u32 = 5552;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn naive_is_fancy_small_inputs() {
        for i in 0..128 {
            let v = (0u8..i).collect::<Vec<_>>();
            assert_eq!(naive_adler32(1, &v), generic::adler32_rust(1, &v));
        }
    }
}
