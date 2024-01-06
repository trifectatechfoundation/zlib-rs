pub fn adler32(start_checksum: u32, data: &[u8]) -> u32 {
    #[cfg(target_arch = "x86_64")]
    if std::is_x86_feature_detected!("avx2") {
        return avx2::adler32_avx2(start_checksum, data);
    }

    adler32_rust(start_checksum, data)
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

const UNROLL_MORE: bool = true;

// macros for loop unrolling
macro_rules! do1 {
    ($sum1:expr, $sum2:expr, $chunk:expr, $i:expr) => {
        $sum1 += unsafe { *$chunk.get_unchecked($i) } as u32;
        $sum2 += $sum1;
    };
}

macro_rules! do2 {
    ($sum1:expr, $sum2:expr, $chunk:expr, $i:expr) => {
        do1!($sum1, $sum2, $chunk, $i);
        do1!($sum1, $sum2, $chunk, $i + 1);
    };
}

macro_rules! do4 {
    ($sum1:expr, $sum2:expr, $chunk:expr, $i:expr) => {
        do2!($sum1, $sum2, $chunk, $i);
        do2!($sum1, $sum2, $chunk, $i + 2);
    };
}

macro_rules! do8 {
    ($sum1:expr, $sum2:expr, $chunk:expr, $i:expr) => {
        do4!($sum1, $sum2, $chunk, $i);
        do4!($sum1, $sum2, $chunk, $i + 4);
    };
}

macro_rules! do16 {
    ($sum1:expr, $sum2:expr, $chunk:expr) => {
        do8!($sum1, $sum2, $chunk, 0);
        do8!($sum1, $sum2, $chunk, 8);
    };
}

fn adler32_rust(mut adler: u32, buf: &[u8]) -> u32 {
    /* split Adler-32 into component sums */
    let mut sum2 = (adler >> 16) & 0xffff;
    adler &= 0xffff;

    /* in case user likes doing a byte at a time, keep it fast */
    if buf.len() == 1 {
        return adler32_len_1(adler, buf, sum2);
    }

    /* initial Adler-32 value (deferred check for len == 1 speed) */
    if buf.is_empty() {
        return 1;
    }

    /* in case short lengths are provided, keep it somewhat fast */
    if buf.len() < 16 {
        return adler32_len_16(adler, buf, sum2);
    }

    let mut it = buf.chunks_exact(NMAX as usize);
    while let Some(big_chunk) = it.next() {
        const N: usize = if UNROLL_MORE { 16 } else { 8 } as usize;
        let it = big_chunk.chunks_exact(N);
        for chunk in it {
            if N == 16 {
                do16!(adler, sum2, chunk);
            } else {
                do8!(adler, sum2, chunk, 0);
            }
        }

        adler %= BASE;
        sum2 %= BASE;
    }

    /* do remaining bytes (less than NMAX, still just one modulo) */
    return adler32_len_64(adler, it.remainder(), sum2);
}

fn adler32_len_1(mut adler: u32, buf: &[u8], mut sum2: u32) -> u32 {
    adler += buf[0] as u32;
    adler %= BASE;
    sum2 += adler;
    sum2 %= BASE;
    adler | (sum2 << 16)
}

fn adler32_len_16(mut adler: u32, buf: &[u8], mut sum2: u32) -> u32 {
    for b in buf {
        adler += (*b) as u32;
        sum2 += adler;
    }

    adler %= BASE;
    sum2 %= BASE; /* only added so many BASE's */
    /* return recombined sums */
    adler | (sum2 << 16)
}

fn adler32_len_64(mut adler: u32, buf: &[u8], mut sum2: u32) -> u32 {
    const N: usize = if UNROLL_MORE { 16 } else { 8 };
    let mut it = buf.chunks_exact(N);
    while let Some(chunk) = it.next() {
        if N == 16 {
            do16!(adler, sum2, chunk);
        } else {
            do8!(adler, sum2, chunk, 0);
        }
    }

    /* Process tail (len < 16).  */
    adler32_len_16(adler, it.remainder(), sum2)
}

mod avx2 {
    use super::*;

    use std::arch::x86_64::{
        __m256i, _mm256_add_epi32, _mm256_castsi256_si128, _mm256_extracti128_si256,
        _mm256_loadu_si256, _mm256_madd_epi16, _mm256_maddubs_epi16, _mm256_permutevar8x32_epi32,
        _mm256_sad_epu8, _mm256_slli_epi32, _mm256_zextsi128_si256, _mm_add_epi32,
        _mm_cvtsi128_si32, _mm_cvtsi32_si128, _mm_shuffle_epi32, _mm_unpackhi_epi64,
    };

    const fn __m256i_literal(bytes: [u8; 32]) -> __m256i {
        unsafe { std::mem::transmute(bytes) }
    }

    const DOT2V: __m256i = __m256i_literal([
        32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10,
        9, 8, 7, 6, 5, 4, 3, 2, 1,
    ]);

    const DOT3V: __m256i = __m256i_literal([
        1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
        1, 0,
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

    pub fn adler32_avx2(adler: u32, buf: &[u8]) -> u32 {
        if buf.is_empty() {
            return adler;
        }

        let mut adler1 = (adler >> 16) & 0xffff;
        let mut adler0 = adler & 0xffff;

        if buf.len() < 16 {
            return adler32_len_16(adler0, buf, adler1);
        } else if buf.len() < 32 {
            return adler32_len_64(adler0, buf, adler1);
        }

        // use largest step possible (without causing overflow)
        const N: usize = (NMAX - (NMAX % 32)) as usize;
        let (chunks, remainder) = slice_as_chunks::<_, N>(buf);
        for chunk in chunks {
            (adler0, adler1) = unsafe { helper_32_bytes(adler0, adler1, chunk) };
        }

        // then take steps of 32 bytes
        let (chunks, remainder) = slice_as_chunks::<_, 32>(remainder);
        for chunk in chunks {
            (adler0, adler1) = unsafe { helper_32_bytes(adler0, adler1, chunk) };
        }

        if !remainder.is_empty() {
            if remainder.len() < 16 {
                return adler32_len_16(adler0, remainder, adler1);
            } else if remainder.len() < 32 {
                return adler32_len_64(adler0, remainder, adler1);
            } else {
                unreachable!()
            }
        }

        adler0 | (adler1 << 16)
    }

    #[inline(always)]
    unsafe fn helper_32_bytes(mut adler0: u32, mut adler1: u32, buf: &[u8]) -> (u32, u32) {
        debug_assert_eq!(buf.len() % 32, 0);

        let mut vs1 = _mm256_zextsi128_si256(_mm_cvtsi32_si128(adler0 as i32));
        let mut vs2 = _mm256_zextsi128_si256(_mm_cvtsi32_si128(adler1 as i32));

        let mut vs1_0 = vs1;
        let mut vs3 = ZERO;

        for chunk in buf.chunks_exact(32) {
            let vbuf = _mm256_loadu_si256(chunk.as_ptr() as *const __m256i);

            let vs1_sad = _mm256_sad_epu8(vbuf, ZERO); // Sum of abs diff, resulting in 2 x int32's

            // TODO copy?

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
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn naive_is_fancy_small_inputs() {
        for i in 0..128 {
            let v = (0u8..i).collect::<Vec<_>>();
            assert_eq!(naive_adler32(1, &v), adler32_rust(1, &v));
        }
    }
}
