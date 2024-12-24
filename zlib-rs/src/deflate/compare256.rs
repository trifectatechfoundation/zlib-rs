#[cfg(test)]
const MAX_COMPARE_SIZE: usize = 256;

#[inline(always)]
pub fn compare256_slice(src0: &[u8], src1: &[u8]) -> usize {
    let src0 = first_chunk::<_, 256>(src0).unwrap();
    let src1 = first_chunk::<_, 256>(src1).unwrap();

    compare256(src0, src1)
}

/// Call the most optimal compare256
///
/// We attempt to call a specific version if its target feature is enabled at compile time
/// (e.g. via `-Ctarget-cpu`). If the desired target feature is not found, we defer to
/// [`compare256_via_function_pointer`].
#[inline(always)]
fn compare256(src0: &[u8; 256], src1: &[u8; 256]) -> usize {
    #[cfg(target_feature = "avx2")]
    return unsafe { avx2::compare256(src0, src1) };

    #[cfg(target_feature = "neon")]
    return unsafe { neon::compare256(src0, src1) };

    #[cfg(target_feature = "simd128")]
    return unsafe { wasm32::compare256(src0, src1) };

    #[allow(unreachable_code)]
    compare256_via_function_pointer(src0, src1)
}

/// Choose the most optimal implementation at runtime
///
/// We store the function pointer to the most optimal implementation in an AtomicPtr; every call
/// loads this function pointer and then calls it.
///
/// The value is initially set to `initializer`, which on the first call will determine what the
/// most efficient implementation is, and overwrite the value in the atomic, so that on subsequent
/// calls the best implementation is called immediately.
#[inline(always)]
fn compare256_via_function_pointer(src0: &[u8; 256], src1: &[u8; 256]) -> usize {
    use core::sync::atomic::{AtomicPtr, Ordering};

    type F = unsafe fn(&[u8; 256], &[u8; 256]) -> usize;

    static PTR: AtomicPtr<()> = AtomicPtr::new(initializer as *mut ());

    fn initializer(src0: &[u8; 256], src1: &[u8; 256]) -> usize {
        let ptr = match () {
            #[cfg(target_arch = "x86_64")]
            _ if crate::cpu_features::is_enabled_avx2() => avx2::compare256 as F,
            #[cfg(target_arch = "aarch64")]
            _ if crate::cpu_features::is_enabled_neon() => neon::compare256 as F,
            #[cfg(target_arch = "wasm32")]
            _ if crate::cpu_features::is_enabled_simd128() => wasm32::compare256 as F,
            _ => rust::compare256 as F,
        };

        PTR.store(ptr as *mut (), Ordering::Relaxed);

        // Safety: we've validated the target feature requirements
        unsafe { ptr(src0, src1) }
    }

    let ptr = PTR.load(Ordering::Relaxed);

    // Safety: we trust this function pointer (PTR is local to the function)
    let dynamic_compare256 = unsafe { core::mem::transmute::<*mut (), F>(ptr) };

    // Safety: we've validated the target feature requirements
    unsafe { dynamic_compare256(src0, src1) }
}

pub fn compare256_rle_slice(byte: u8, src: &[u8]) -> usize {
    rust::compare256_rle(byte, src)
}

#[inline]
pub const fn first_chunk<T, const N: usize>(slice: &[T]) -> Option<&[T; N]> {
    if slice.len() < N {
        None
    } else {
        // SAFETY: We explicitly check for the correct number of elements,
        //   and do not let the reference outlive the slice.
        Some(unsafe { &*(slice.as_ptr() as *const [T; N]) })
    }
}

mod rust {

    pub fn compare256(src0: &[u8; 256], src1: &[u8; 256]) -> usize {
        // only unrolls 4 iterations; zlib-ng unrolls 8
        src0.iter().zip(src1).take_while(|(x, y)| x == y).count()
    }

    // run-length encoding
    pub fn compare256_rle(byte: u8, src: &[u8]) -> usize {
        assert!(src.len() >= 256, "too short {}", src.len());

        let mut sv = byte as u64;
        sv |= sv << 8;
        sv |= sv << 16;
        sv |= sv << 32;

        let mut len = 0;

        // this optimizes well because we statically limit the slice to 256 bytes.
        // the loop gets unrolled 4 times automatically.
        for chunk in src[..256].chunks_exact(8) {
            let mv = u64::from_le_bytes(chunk.try_into().unwrap());

            let diff = sv ^ mv;

            if diff > 0 {
                let match_byte = diff.trailing_zeros() / 8;
                return len + match_byte as usize;
            }

            len += 8
        }

        256
    }

    #[test]
    fn test_compare256() {
        let str1 = [b'a'; super::MAX_COMPARE_SIZE];
        let mut str2 = [b'a'; super::MAX_COMPARE_SIZE];

        for i in 0..str1.len() {
            str2[i] = 0;

            let match_len = compare256(&str1, &str2);
            assert_eq!(match_len, i);

            str2[i] = b'a';
        }
    }

    #[test]
    fn test_compare256_rle() {
        let mut string = [b'a'; super::MAX_COMPARE_SIZE];

        for i in 0..string.len() {
            string[i] = 0;

            let match_len = compare256_rle(b'a', &string);
            assert_eq!(match_len, i);

            string[i] = b'a';
        }
    }
}

#[cfg(target_arch = "aarch64")]
mod neon {
    use core::arch::aarch64::{
        uint8x16_t, veorq_u8, vgetq_lane_u64, vld1q_u8, vreinterpretq_u64_u8,
    };

    /// # Safety
    ///
    /// Behavior is undefined if the `neon` target feature is not enabled
    #[target_feature(enable = "neon")]
    pub unsafe fn compare256(src0: &[u8; 256], src1: &[u8; 256]) -> usize {
        let src0: &[[u8; 16]; 16] = unsafe { core::mem::transmute(src0) };
        let src1: &[[u8; 16]; 16] = unsafe { core::mem::transmute(src1) };

        let mut len = 0;

        for (a, b) in src0.iter().zip(src1) {
            unsafe {
                let a: uint8x16_t = vld1q_u8(a.as_ptr());
                let b: uint8x16_t = vld1q_u8(b.as_ptr());

                let cmp = veorq_u8(a, b);

                let lane = vgetq_lane_u64(vreinterpretq_u64_u8(cmp), 0);
                if lane != 0 {
                    let match_byte = lane.trailing_zeros() / 8;
                    return len + match_byte as usize;
                }

                len += 8;

                let lane = vgetq_lane_u64(vreinterpretq_u64_u8(cmp), 1);
                if lane != 0 {
                    let match_byte = lane.trailing_zeros() / 8;
                    return len + match_byte as usize;
                }

                len += 8;
            }
        }

        256
    }

    #[test]
    fn test_compare256() {
        if crate::cpu_features::is_enabled_neon() {
            let str1 = [b'a'; super::MAX_COMPARE_SIZE];
            let mut str2 = [b'a'; super::MAX_COMPARE_SIZE];

            for i in 0..str1.len() {
                str2[i] = 0;

                let match_len = unsafe { compare256(&str1, &str2) };
                assert_eq!(match_len, i);

                str2[i] = b'a';
            }
        }
    }
}

#[cfg(target_arch = "x86_64")]
mod avx2 {
    use core::arch::x86_64::{
        __m256i, _mm256_cmpeq_epi8, _mm256_loadu_si256, _mm256_movemask_epi8,
    };

    /// # Safety
    ///
    /// Behavior is undefined if the `avx` target feature is not enabled
    #[target_feature(enable = "avx2")]
    pub unsafe fn compare256(src0: &[u8; 256], src1: &[u8; 256]) -> usize {
        let src0: &[[u8; 32]; 8] = unsafe { core::mem::transmute(src0) };
        let src1: &[[u8; 32]; 8] = unsafe { core::mem::transmute(src1) };

        let mut len = 0;

        unsafe {
            for (chunk0, chunk1) in src0.iter().zip(src1) {
                let ymm_src0 = _mm256_loadu_si256(chunk0.as_ptr() as *const __m256i);
                let ymm_src1 = _mm256_loadu_si256(chunk1.as_ptr() as *const __m256i);

                let ymm_cmp = _mm256_cmpeq_epi8(ymm_src0, ymm_src1); /* non-identical bytes = 00, identical bytes = FF */
                let mask = _mm256_movemask_epi8(ymm_cmp) as u32;

                if mask != 0xFFFFFFFF {
                    let match_byte = (!mask).trailing_zeros(); /* Invert bits so identical = 0 */
                    return len + match_byte as usize;
                }

                len += 32;
            }
        }

        256
    }

    #[test]
    fn test_compare256() {
        if crate::cpu_features::is_enabled_avx2() {
            let str1 = [b'a'; super::MAX_COMPARE_SIZE];
            let mut str2 = [b'a'; super::MAX_COMPARE_SIZE];

            for i in 0..str1.len() {
                str2[i] = 0;

                let match_len = unsafe { compare256(&str1, &str2) };
                assert_eq!(match_len, i);

                str2[i] = b'a';
            }
        }
    }
}

#[cfg(target_arch = "wasm32")]
mod wasm32 {
    use core::arch::wasm32::{u8x16_bitmask, u8x16_eq, v128, v128_load};

    #[target_feature(enable = "simd128")]
    pub fn compare256(src0: &[u8; 256], src1: &[u8; 256]) -> usize {
        let src0: &[[u8; 16]; 16] = unsafe { core::mem::transmute(src0) };
        let src1: &[[u8; 16]; 16] = unsafe { core::mem::transmute(src1) };

        let mut len = 0;

        for (chunk0, chunk1) in src0.iter().zip(src1) {
            // SAFETY: these are valid pointers to slice data.
            let v128_src0 = unsafe { v128_load(chunk0.as_ptr() as *const v128) };
            let v128_src1 = unsafe { v128_load(chunk1.as_ptr() as *const v128) };

            let v128_cmp = u8x16_eq(v128_src0, v128_src1);
            let mask = u8x16_bitmask(v128_cmp);

            if mask != 0xFFFF {
                let match_byte = (!mask).trailing_zeros(); /* Invert bits so identical = 0 */
                return len + match_byte as usize;
            }

            len += 16;
        }

        256
    }

    #[test]
    fn test_compare256() {
        if crate::cpu_features::is_enabled_simd128() {
            let str1 = [b'a'; super::MAX_COMPARE_SIZE];
            let mut str2 = [b'a'; super::MAX_COMPARE_SIZE];

            for i in 0..str1.len() {
                str2[i] = 0;

                let match_len = unsafe { compare256(&str1, &str2) };
                assert_eq!(match_len, i);

                str2[i] = b'a';
            }
        }
    }
}
