use core::arch::x86_64::{
    __m512i, _mm256_add_epi32, _mm256_castsi256_si128, _mm256_extracti128_si256, _mm512_add_epi32,
    _mm512_castsi512_si256, _mm512_extracti64x4_epi64, _mm512_madd_epi16, _mm512_maddubs_epi16,
    _mm512_permutexvar_epi32, _mm512_sad_epu8, _mm512_set1_epi16, _mm512_setr_epi32,
    _mm512_slli_epi32, _mm512_zextsi128_si512, _mm_add_epi32, _mm_cvtsi128_si32, _mm_cvtsi32_si128,
    _mm_shuffle_epi32, _mm_unpackhi_epi64,
};

use crate::adler32::{BASE, NMAX};

const fn __m512i_literal(bytes: [u8; 64]) -> __m512i {
    // SAFETY: any valid [u8; 64] represents a valid __m512i
    unsafe { core::mem::transmute(bytes) }
}

const DOT2V: __m512i = __m512i_literal([
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,
    27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
    51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
]);

const ZERO: __m512i = __m512i_literal([0u8; 64]);

#[target_feature(enable = "avx512f")]
#[target_feature(enable = "avx512bw")]
pub fn adler32_avx512(adler: u32, src: &[u8]) -> u32 {
    assert!(cfg!(target_feature = "avx512f"));
    assert!(cfg!(target_feature = "avx512bw"));
    // SAFETY: the assertion above ensures this code is not executed unless the CPU has avx512.
    unsafe { adler32_avx512_help(adler, src) }
}

#[target_feature(enable = "avx512f")]
#[target_feature(enable = "avx512bw")]
unsafe fn adler32_avx512_help(adler: u32, src: &[u8]) -> u32 {
    if src.is_empty() {
        return adler;
    }

    // SAFETY: [u8; 64] safely transmutes into __m512i.
    let (before, middle, after) = unsafe { src.align_to::<__m512i>() };

    dbg!(adler, &src);
    dbg!(before.len(), middle.len(), after.len());
    dbg!(&src[before.len()..][..src.len() - before.len() - after.len()]);

    let adler = if !before.is_empty() {
        super::avx2::adler32_avx2(adler, before)
    } else {
        adler
    };

    let mut adler1 = (adler >> 16) & 0xffff;
    let mut adler0 = adler & 0xffff;

    // use largest step possible (without causing overflow)
    for chunk in middle.chunks(NMAX as usize / 64) {
        dbg!(adler0, adler1);
        (adler0, adler1) = unsafe { helper_64_bytes(adler0, adler1, chunk) };
        dbg!(adler0, adler1);
    }

    if after.is_empty() {
        adler0 | (adler1 << 16)
    } else {
        super::avx2::adler32_avx2(adler0 | (adler1 << 16), after)
    }
}

unsafe fn helper_64_bytes(mut adler0: u32, mut adler1: u32, src: &[__m512i]) -> (u32, u32) {
    unsafe {
        let mut vs1 = _mm512_zextsi128_si512(_mm_cvtsi32_si128(adler0 as i32));
        let mut vs2 = _mm512_zextsi128_si512(_mm_cvtsi32_si128(adler1 as i32));

        let mut vs1_0 = vs1;
        let mut vs3 = ZERO;

        let dot3v = _mm512_set1_epi16(1);

        for vbuf in src.iter().copied() {
            let vs1_sad = _mm512_sad_epu8(vbuf, ZERO);
            let v_short_sum2 = _mm512_maddubs_epi16(vbuf, DOT2V);
            vs1 = _mm512_add_epi32(vs1_sad, vs1);
            vs3 = _mm512_add_epi32(vs3, vs1_0);
            let vsum2 = _mm512_madd_epi16(v_short_sum2, dot3v);
            vs2 = _mm512_add_epi32(vsum2, vs2);
            vs1_0 = vs1;
        }

        /* Defer the multiplication with 32 to outside of the loop */
        vs3 = _mm512_slli_epi32(vs3, 6);
        vs2 = _mm512_add_epi32(vs2, vs3);

        adler0 = partial_hsum(vs1) % BASE;
        adler1 = _mm512_reduce_add_epu32(vs2) % BASE;

        (adler0, adler1)
    }
}

#[inline(always)]
unsafe fn _mm512_reduce_add_epu32(x: __m512i) -> u32 {
    unsafe {
        let a = _mm512_extracti64x4_epi64(x, 1);
        let b = _mm512_extracti64x4_epi64(x, 0);

        let a_plus_b = _mm256_add_epi32(a, b);
        let c = _mm256_extracti128_si256(a_plus_b, 1);
        let d = _mm256_extracti128_si256(a_plus_b, 0);
        let c_plus_d = _mm_add_epi32(c, d);

        let sum1 = _mm_unpackhi_epi64(c_plus_d, c_plus_d);
        let sum2 = _mm_add_epi32(sum1, c_plus_d);
        let sum3 = _mm_shuffle_epi32(sum2, 0x01);
        let sum4 = _mm_add_epi32(sum2, sum3);

        _mm_cvtsi128_si32(sum4) as u32
    }
}

#[inline(always)]
unsafe fn partial_hsum(x: __m512i) -> u32 {
    unsafe {
        // We need a permutation vector to extract every other integer. The
        // rest are going to be zeros. Marking this const so the compiler stands
        // a better chance of keeping this resident in a register through entire
        // loop execution. We certainly have enough zmm registers (32) */
        let perm_vec: __m512i =
            _mm512_setr_epi32(0, 2, 4, 6, 8, 10, 12, 14, 1, 1, 1, 1, 1, 1, 1, 1);

        let non_zero = _mm512_permutexvar_epi32(perm_vec, x);

        /* From here, it's a simple 256 bit wide reduction sum */
        let non_zero_avx = _mm512_castsi512_si256(non_zero);

        /* See Agner Fog's vectorclass for a decent reference. Essentially, phadd is
         * pretty slow, much slower than the longer instruction sequence below */
        let sum1 = _mm_add_epi32(
            _mm256_extracti128_si256(non_zero_avx, 1),
            _mm256_castsi256_si128(non_zero_avx),
        );
        let sum2 = _mm_add_epi32(sum1, _mm_unpackhi_epi64(sum1, sum1));
        let sum3 = _mm_add_epi32(sum2, _mm_shuffle_epi32(sum2, 1));

        _mm_cvtsi128_si32(sum3) as u32
    }
}

#[cfg(test)]
#[cfg(target_feature = "avx512f")]
#[cfg(target_feature = "avx512bw")]
mod test {
    use super::*;
    use core::arch::x86_64::__m256i;

    #[test]
    fn empty_input() {
        let avx512 = unsafe { adler32_avx512(0, &[]) };
        let rust = crate::adler32::generic::adler32_rust(0, &[]);

        assert_eq!(rust, avx512);
    }

    #[test]
    fn zero_chunks() {
        let input = &[
            1u8, 39, 76, 148, 0, 58, 0, 14, 255, 59, 1, 229, 1, 83, 5, 84, 207, 152, 188,
        ];
        let avx512 = unsafe { adler32_avx512(0, input) };
        let rust = crate::adler32::generic::adler32_rust(0, input);

        assert_eq!(rust, avx512);
    }

    #[test]
    fn one_chunk() {
        let input: [u8; 85] = core::array::from_fn(|i| i as u8);
        let avx512 = unsafe { adler32_avx512(0, &input) };
        let rust = crate::adler32::generic::adler32_rust(0, &input);

        assert_eq!(rust, avx512);
    }

    #[test]
    fn foobar() {
        let input: [u8; 64] = [
            20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41,
            42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
            64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83,
        ];

        unsafe {
            let input_avx512: __m512i = core::mem::transmute(input);
            let input_avx2: [__m256i; 2] = core::mem::transmute(input);

            let adler0 = 190;
            let adler1 = 1330;

            let output_avx2 = crate::adler32::avx2::helper_32_bytes(adler0, adler1, &input_avx2);
            let output_avx512 = helper_64_bytes(adler0, adler1, &[input_avx512]);

            assert_eq!(output_avx2, output_avx512);
        }
    }

    quickcheck::quickcheck! {
        fn adler32_avx512_is_adler32_rust(v: Vec<u8>, start: u32) -> bool {
            let avx512 = unsafe { adler32_avx512(start, &v) };
            let rust = crate::adler32::generic::adler32_rust(start, &v);

            rust == avx512
        }
    }

    const INPUT: [u8; 128] = {
        let mut array = [0; 128];
        let mut i = 0;
        while i < array.len() {
            array[i] = i as u8;
            i += 1;
        }

        array
    };

    #[test]
    fn start_alignment() {
        // SIMD algorithm is sensitive to alignment;
        for i in 0..16 {
            for start in [crate::ADLER32_INITIAL_VALUE as u32, 42] {
                let avx512 = unsafe { adler32_avx512(start, &INPUT[i..]) };
                let rust = crate::adler32::generic::adler32_rust(start, &INPUT[i..]);

                assert_eq!(avx512, rust, "offset = {i}, start = {start}");
            }
        }
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn large_input() {
        const DEFAULT: &[u8] = include_bytes!("../deflate/test-data/paper-100k.pdf");

        let avx512 = unsafe { adler32_avx512(42, DEFAULT) };
        let rust = crate::adler32::generic::adler32_rust(42, DEFAULT);

        assert_eq!(avx512, rust);
    }
}
