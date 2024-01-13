pub fn compare256(src0: &[u8; 256], src1: &[u8; 256]) -> usize {
    #[cfg(target_arch = "x86_64")]
    if std::is_x86_feature_detected!("avx2") {
        debug_assert_eq!(avx2::compare256(src0, src1), rust::compare256(src0, src1));

        return avx2::compare256(src0, src1);
    }

    rust::compare256(src0, src1)
}

mod rust {
    pub fn compare256(src0: &[u8; 256], src1: &[u8; 256]) -> usize {
        // only unrolls 4 iterations; zlib-ng unrolls 8
        src0.iter().zip(src1).take_while(|(x, y)| x == y).count()
    }
}

#[cfg(target_arch = "x86_64")]
mod avx2 {
    use std::arch::x86_64::{__m256i, _mm256_cmpeq_epi8, _mm256_loadu_si256, _mm256_movemask_epi8};

    pub fn compare256(src0: &[u8; 256], src1: &[u8; 256]) -> usize {
        let src0: &[[u8; 32]; 8] = unsafe { std::mem::transmute(src0) };
        let src1: &[[u8; 32]; 8] = unsafe { std::mem::transmute(src1) };

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
}
