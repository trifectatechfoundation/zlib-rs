use std::{
    arch::x86_64::{
        __m256i, _mm256_add_epi32, _mm256_castsi256_si128, _mm256_extracti128_si256,
        _mm256_loadu_si256, _mm256_madd_epi16, _mm256_maddubs_epi16, _mm256_permutevar8x32_epi32,
        _mm256_sad_epu8, _mm256_slli_epi32, _mm256_storeu_si256, _mm256_zextsi128_si256,
        _mm_add_epi32, _mm_cvtsi128_si32, _mm_cvtsi32_si128, _mm_shuffle_epi32, _mm_unpackhi_epi64,
    },
    mem::MaybeUninit,
};

use crate::adler32::{
    generic::{adler32_copy_len_16, adler32_len_16, adler32_len_64},
    BASE, NMAX,
};

const fn __m256i_literal(bytes: [u8; 32]) -> __m256i {
    unsafe { std::mem::transmute(bytes) }
}

const DOT2V: __m256i = __m256i_literal([
    32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9,
    8, 7, 6, 5, 4, 3, 2, 1,
]);

const DOT3V: __m256i = __m256i_literal([
    1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
]);

const ZERO: __m256i = __m256i_literal([0; 32]);

// 32 bit horizontal sum, adapted from Agner Fog's vector library.
fn hsum256(x: __m256i) -> u32 {
    unsafe {
        let sum1 = _mm_add_epi32(_mm256_extracti128_si256(x, 1), _mm256_castsi256_si128(x));
        let sum2 = _mm_add_epi32(sum1, _mm_unpackhi_epi64(sum1, sum1));
        let sum3 = _mm_add_epi32(sum2, _mm_shuffle_epi32(sum2, 1));
        _mm_cvtsi128_si32(sum3) as u32
    }
}

fn partial_hsum256(x: __m256i) -> u32 {
    const PERM_VEC: __m256i = __m256i_literal([
        0, 0, 0, 0, //
        2, 0, 0, 0, //
        4, 0, 0, 0, //
        6, 0, 0, 0, //
        1, 0, 0, 0, //
        1, 0, 0, 0, //
        1, 0, 0, 0, //
        1, 0, 0, 0, //
    ]);

    unsafe {
        let non_zero = _mm256_permutevar8x32_epi32(x, PERM_VEC);
        let non_zero_sse = _mm256_castsi256_si128(non_zero);
        let sum2 = _mm_add_epi32(non_zero_sse, _mm_unpackhi_epi64(non_zero_sse, non_zero_sse));
        let sum3 = _mm_add_epi32(sum2, _mm_shuffle_epi32(sum2, 1));
        _mm_cvtsi128_si32(sum3) as u32
    }
}

fn slice_as_chunks<T, const N: usize>(slice: &[T]) -> (&[[T; N]], &[T]) {
    assert!(N != 0, "chunk size must be non-zero");
    let len = slice.len() / N;
    let (multiple_of_n, remainder) = slice.split_at(len * N);
    // SAFETY: We already panicked for zero, and ensured by construction
    // that the length of the subslice is a multiple of N.
    let array_slice: &[[T; N]] =
        unsafe { std::slice::from_raw_parts(multiple_of_n.as_ptr().cast(), len) };
    (array_slice, remainder)
}

pub fn adler32_aligned_avx2(adler: u32, src: &[u8]) -> u32 {
    adler32_avx2_aligned_help::<false>(adler, &mut [], src)
}

pub fn adler32_avx2(adler: u32, src: &[u8]) -> u32 {
    adler32_avx2_help::<false>(adler, &mut [], src)
}

pub fn adler32_fold_copy_avx2(adler: u32, dst: &mut [MaybeUninit<u8>], src: &[u8]) -> u32 {
    adler32_avx2_help::<true>(adler, dst, src)
}

fn adler32_avx2_help<const COPY: bool>(
    adler: u32,
    mut dst: &mut [MaybeUninit<u8>],
    src: &[u8],
) -> u32 {
    if src.is_empty() {
        return adler;
    }

    let mut adler1 = (adler >> 16) & 0xffff;
    let mut adler0 = adler & 0xffff;

    if src.len() < 16 {
        // use COPY const generic for this branch
        if COPY {
            return adler32_copy_len_16(adler0, dst, src, adler1);
        } else {
            return adler32_len_16(adler0, src, adler1);
        }
    } else if src.len() < 32 {
        // use COPY const generic for this branch
        if COPY {
            return adler32_copy_len_16(adler0, dst, src, adler1);
        } else {
            return adler32_len_64(adler0, src, adler1);
        }
    }

    // use largest step possible (without causing overflow)
    const N: usize = (NMAX - (NMAX % 32)) as usize;
    let (chunks, remainder) = slice_as_chunks::<_, N>(src);
    for chunk in chunks {
        (adler0, adler1) = unsafe { helper_32_bytes::<COPY>(adler0, adler1, dst, chunk) };
        if COPY {
            dst = &mut dst[N..];
        }
    }

    // then take steps of 32 bytes
    let (chunks, remainder) = slice_as_chunks::<_, 32>(remainder);
    for chunk in chunks {
        (adler0, adler1) = unsafe { helper_32_bytes::<COPY>(adler0, adler1, dst, chunk) };
        if COPY {
            dst = &mut dst[32..];
        }
    }

    if !remainder.is_empty() {
        if remainder.len() < 16 {
            if COPY {
                return adler32_copy_len_16(adler0, dst, remainder, adler1);
            } else {
                return adler32_len_16(adler0, remainder, adler1);
            }
        } else if remainder.len() < 32 {
            if COPY {
                return adler32_copy_len_16(adler0, dst, remainder, adler1);
            } else {
                return adler32_len_64(adler0, remainder, adler1);
            }
        } else {
            unreachable!()
        }
    }

    adler0 | (adler1 << 16)
}

fn adler32_avx2_aligned_help<const COPY: bool>(
    adler: u32,
    mut dst: &mut [MaybeUninit<u8>],
    src: &[u8],
) -> u32 {
    if src.is_empty() {
        return adler;
    }

    let mut adler1 = (adler >> 16) & 0xffff;
    let mut adler0 = adler & 0xffff;

    let (before, middle, after) = unsafe { src.align_to::<__m256i>() };

    if before.len() < 16 {
        // use COPY const generic for this branch
        let adler = if COPY {
            adler32_copy_len_16(adler0, dst, before, adler1)
        } else {
            adler32_len_16(adler0, before, adler1)
        };

        adler1 = (adler >> 16) & 0xffff;
        adler0 = adler & 0xffff;
    } else if before.len() < 32 {
        // use COPY const generic for this branch
        let adler = if COPY {
            adler32_copy_len_16(adler0, dst, before, adler1)
        } else {
            adler32_len_64(adler0, before, adler1)
        };

        adler1 = (adler >> 16) & 0xffff;
        adler0 = adler & 0xffff;
    }

    // use largest step possible (without causing overflow)
    for chunk in middle.chunks(NMAX as usize / 32) {
        (adler0, adler1) = unsafe { helper_32_bytes_aligned::<COPY>(adler0, adler1, dst, chunk) };
        if COPY {
            dst = &mut dst[32 * chunk.len()..];
        }
    }

    if !after.is_empty() {
        if after.len() < 16 {
            if COPY {
                return adler32_copy_len_16(adler0, dst, after, adler1);
            } else {
                return adler32_len_16(adler0, after, adler1);
            }
        } else if after.len() < 32 {
            if COPY {
                return adler32_copy_len_16(adler0, dst, after, adler1);
            } else {
                return adler32_len_64(adler0, after, adler1);
            }
        } else {
            unreachable!()
        }
    }

    adler0 | (adler1 << 16)
}

#[inline(always)]
unsafe fn helper_32_bytes_aligned<const COPY: bool>(
    mut adler0: u32,
    mut adler1: u32,
    dst: &mut [MaybeUninit<u8>],
    src: &[__m256i],
) -> (u32, u32) {
    let mut vs1 = _mm256_zextsi128_si256(_mm_cvtsi32_si128(adler0 as i32));
    let mut vs2 = _mm256_zextsi128_si256(_mm_cvtsi32_si128(adler1 as i32));

    let mut vs1_0 = vs1;
    let mut vs3 = ZERO;

    let mut out_chunks = dst.chunks_exact_mut(32);

    for vbuf in src.iter().copied() {
        if COPY {
            let out_chunk = out_chunks.next().unwrap();
            _mm256_storeu_si256(out_chunk.as_mut_ptr() as *mut __m256i, vbuf);
        }

        let vs1_sad = _mm256_sad_epu8(vbuf, ZERO); // Sum of abs diff, resulting in 2 x int32's

        vs1 = _mm256_add_epi32(vs1, vs1_sad);
        vs3 = _mm256_add_epi32(vs3, vs1_0);
        let v_short_sum2 = _mm256_maddubs_epi16(vbuf, DOT2V); // sum 32 uint8s to 16 shorts
        let vsum2 = _mm256_madd_epi16(v_short_sum2, DOT3V); // sum 16 shorts to 8 uint32s
        vs2 = _mm256_add_epi32(vsum2, vs2);
        vs1_0 = vs1;
    }

    /* Defer the multiplication with 32 to outside of the loop */
    vs3 = _mm256_slli_epi32(vs3, 5);
    vs2 = _mm256_add_epi32(vs2, vs3);

    adler0 = partial_hsum256(vs1) % BASE;
    adler1 = hsum256(vs2) % BASE;

    (adler0, adler1)
}

#[inline(always)]
unsafe fn helper_32_bytes<const COPY: bool>(
    mut adler0: u32,
    mut adler1: u32,
    dst: &mut [MaybeUninit<u8>],
    src: &[u8],
) -> (u32, u32) {
    debug_assert_eq!(src.len() % 32, 0);

    let mut vs1 = _mm256_zextsi128_si256(_mm_cvtsi32_si128(adler0 as i32));
    let mut vs2 = _mm256_zextsi128_si256(_mm_cvtsi32_si128(adler1 as i32));

    let mut vs1_0 = vs1;
    let mut vs3 = ZERO;

    let mut out_chunks = dst.chunks_exact_mut(32);

    for in_chunk in src.chunks_exact(32) {
        let vbuf = _mm256_loadu_si256(in_chunk.as_ptr() as *const __m256i);

        if COPY {
            // println!("simd copy {:?}", in_chunk);
            let out_chunk = out_chunks.next().unwrap();
            _mm256_storeu_si256(out_chunk.as_mut_ptr() as *mut __m256i, vbuf);
            // out_chunk.copy_from_slice(slice_to_uninit(in_chunk))
        }

        let vs1_sad = _mm256_sad_epu8(vbuf, ZERO); // Sum of abs diff, resulting in 2 x int32's

        vs1 = _mm256_add_epi32(vs1, vs1_sad);
        vs3 = _mm256_add_epi32(vs3, vs1_0);
        let v_short_sum2 = _mm256_maddubs_epi16(vbuf, DOT2V); // sum 32 uint8s to 16 shorts
        let vsum2 = _mm256_madd_epi16(v_short_sum2, DOT3V); // sum 16 shorts to 8 uint32s
        vs2 = _mm256_add_epi32(vsum2, vs2);
        vs1_0 = vs1;
    }

    /* Defer the multiplication with 32 to outside of the loop */
    vs3 = _mm256_slli_epi32(vs3, 5);
    vs2 = _mm256_add_epi32(vs2, vs3);

    adler0 = partial_hsum256(vs1) % BASE;
    adler1 = hsum256(vs2) % BASE;

    (adler0, adler1)
}

#[cfg(test)]
mod test {
    use crate::adler32::naive_adler32;

    use super::*;

    #[test]
    fn equivalent_small_inputs() {
        let vec: Vec<_> = (0..128).map(|x| x as u8).collect();

        for (i, _) in vec.iter().enumerate() {
            assert_eq!(naive_adler32(1, &vec[..i]), adler32_avx2(1, &vec[..i]));
        }
    }

    #[test]
    fn equivalent_big_inputs() {
        // in particular the NMAX boundary is of interest
        const L: usize = NMAX as usize - 128;
        const H: usize = NMAX as usize + 128;
        let vec: Vec<_> = (L..H).map(|x| x as u8).collect();

        for (i, _) in vec.iter().enumerate() {
            assert_eq!(naive_adler32(1, &vec[..i]), adler32_avx2(1, &vec[..i]));
        }
    }

    #[cfg(test)]
    // TODO: This could use `MaybeUninit::slice_assume_init` when it is stable.
    unsafe fn slice_assume_init(slice: &[MaybeUninit<u8>]) -> &[u8] {
        &*(slice as *const [MaybeUninit<u8>] as *const [u8])
    }

    #[test]
    fn fold_copy_copies() {
        let src: Vec<_> = (0..128).map(|x| x as u8).collect();
        let mut dst = [MaybeUninit::new(0); 128];

        for (i, _) in src.iter().enumerate() {
            dst.fill(MaybeUninit::new(0));

            adler32_fold_copy_avx2(1, &mut dst[..i], &src[..i]);

            assert_eq!(&src[..i], unsafe { slice_assume_init(&dst[..i]) })
        }
    }

    #[test]
    fn equivalent_small_inputs_aligned() {
        let vec: Vec<_> = (0..128).map(|x| x as u8).collect();

        for (i, _) in vec.iter().enumerate() {
            assert_eq!(
                naive_adler32(1, &vec[..i]),
                adler32_aligned_avx2(1, &vec[..i])
            );
        }
    }

    #[test]
    fn equivalent_big_inputs_aligned() {
        // in particular the NMAX boundary is of interest
        const L: usize = NMAX as usize - 128;
        const H: usize = NMAX as usize + 128;
        let vec: Vec<_> = (L..H).map(|x| x as u8).collect();

        for (i, _) in vec.iter().enumerate() {
            assert_eq!(
                naive_adler32(1, &vec[..i]),
                adler32_aligned_avx2(1, &vec[..i])
            );
        }
    }
}
