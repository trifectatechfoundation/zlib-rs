use core::arch::x86_64::__m128i;
use std::arch::x86_64::{
    _mm_and_si128, _mm_castps_si128, _mm_castsi128_ps, _mm_clmulepi64_si128, _mm_cvtsi32_si128,
    _mm_extract_epi32, _mm_load_si128, _mm_loadu_si128, _mm_or_si128, _mm_set1_epi32,
    _mm_set_epi32, _mm_setzero_si128, _mm_shuffle_epi8, _mm_slli_si128, _mm_srli_si128,
    _mm_storeu_si128, _mm_xor_ps, _mm_xor_si128,
};

#[derive(Debug)]
#[repr(C, align(16))]
struct Align16<T>(T);

#[derive(Debug)]
#[repr(C, align(32))]
struct Align32<T>(T);

#[derive(Debug)]
pub struct Crc32Fold {
    fold: Accumulator,
    value: u32,
}

impl Crc32Fold {
    pub fn new() -> Self {
        let mut this = Self {
            #[cfg(target_arch = "x86_64")]
            fold: Accumulator::new(),
            value: Default::default(),
        };

        this
    }

    fn is_pclmulqdq() -> bool {
        is_x86_feature_detected!("pclmulqdq")
            && is_x86_feature_detected!("sse2")
            && is_x86_feature_detected!("sse4.1")
    }

    fn fold(&mut self, src: &[u8], start: u32) {
        if Self::is_pclmulqdq() {
            unsafe { crc32_fold_help::<false>(self, &mut [], src, start) }
        } else {
            // in this case the start value is ignored
            self.value = crc32_braid(src, self.value);
        }
    }

    fn fold_copy(&mut self, dst: &mut [u8], src: &[u8]) {
        if Self::is_pclmulqdq() {
            unsafe { crc32_fold_help::<true>(self, dst, src, 0) }
        } else {
            self.value = crc32_braid(src, self.value);
            dst[..src.len()].copy_from_slice(src);
        }
    }

    fn finish(self) -> u32 {
        if Self::is_pclmulqdq() {
            unsafe { self.fold.finish() }
        } else {
            self.value
        }
    }
}

#[derive(Debug)]
#[cfg(target_arch = "x86_64")]
struct Accumulator {
    fold: [__m128i; 4],
}

impl Accumulator {
    pub fn new() -> Self {
        let mut this = Self {
            #[cfg(target_arch = "x86_64")]
            fold: [unsafe { _mm_setzero_si128() }; 4],
        };

        #[cfg(target_arch = "x86_64")]
        this.reset();

        this
    }

    fn load(&self) -> [__m128i; 4] {
        self.fold
    }

    fn store(&mut self, fold: [__m128i; 4]) {
        self.fold = fold;
    }

    #[cfg(target_arch = "x86_64")]
    fn reset(&mut self) {
        unsafe {
            let xmm_crc0 = _mm_cvtsi32_si128(0x9db42487u32 as i32);
            let xmm_zero = _mm_setzero_si128();

            self.store([xmm_crc0, xmm_zero, xmm_zero, xmm_zero])
        }
    }

    #[target_feature(enable = "pclmulqdq", enable = "sse2", enable = "sse4.1")]
    pub unsafe fn finish(mut self) -> u32 {
        const CRC_MASK: __m128i = unsafe {
            core::mem::transmute([0xFFFFFFFFu32, 0xFFFFFFFFu32, 0x00000000u32, 0x00000000u32])
        };

        const CRC_MASK2: __m128i = unsafe {
            core::mem::transmute([0x00000000u32, 0xFFFFFFFFu32, 0xFFFFFFFFu32, 0xFFFFFFFFu32])
        };

        const CRC_K: [__m128i; 3] = {
            let bytes: [u32; 12] = [
                0xccaa009e, 0x00000000, /* rk1 */
                0x751997d0, 0x00000001, /* rk2 */
                0xccaa009e, 0x00000000, /* rk5 */
                0x63cd6124, 0x00000001, /* rk6 */
                0xf7011640, 0x00000001, /* rk7 */
                0xdb710640, 0x00000001, /* rk8 */
            ];

            unsafe { core::mem::transmute(bytes) }
        };

        let xmm_mask = CRC_MASK;
        let xmm_mask2 = CRC_MASK2;

        let [mut xmm_crc0, mut xmm_crc1, mut xmm_crc2, mut xmm_crc3] = self.fold;

        /*
         * k1
         */
        let mut crc_fold = CRC_K[0];

        let x_tmp0 = _mm_clmulepi64_si128(xmm_crc0, crc_fold, 0x10);
        xmm_crc0 = _mm_clmulepi64_si128(xmm_crc0, crc_fold, 0x01);
        xmm_crc1 = _mm_xor_si128(xmm_crc1, x_tmp0);
        xmm_crc1 = _mm_xor_si128(xmm_crc1, xmm_crc0);

        let x_tmp1 = _mm_clmulepi64_si128(xmm_crc1, crc_fold, 0x10);
        xmm_crc1 = _mm_clmulepi64_si128(xmm_crc1, crc_fold, 0x01);
        xmm_crc2 = _mm_xor_si128(xmm_crc2, x_tmp1);
        xmm_crc2 = _mm_xor_si128(xmm_crc2, xmm_crc1);

        let x_tmp2 = _mm_clmulepi64_si128(xmm_crc2, crc_fold, 0x10);
        xmm_crc2 = _mm_clmulepi64_si128(xmm_crc2, crc_fold, 0x01);
        xmm_crc3 = _mm_xor_si128(xmm_crc3, x_tmp2);
        xmm_crc3 = _mm_xor_si128(xmm_crc3, xmm_crc2);

        /*
         * k5
         */
        crc_fold = CRC_K[1];

        xmm_crc0 = xmm_crc3;
        xmm_crc3 = _mm_clmulepi64_si128(xmm_crc3, crc_fold, 0);
        xmm_crc0 = _mm_srli_si128(xmm_crc0, 8);
        xmm_crc3 = _mm_xor_si128(xmm_crc3, xmm_crc0);

        xmm_crc0 = xmm_crc3;
        xmm_crc3 = _mm_slli_si128(xmm_crc3, 4);
        xmm_crc3 = _mm_clmulepi64_si128(xmm_crc3, crc_fold, 0x10);
        xmm_crc3 = _mm_xor_si128(xmm_crc3, xmm_crc0);
        xmm_crc3 = _mm_and_si128(xmm_crc3, xmm_mask2);

        /*
         * k7
         */
        xmm_crc1 = xmm_crc3;
        xmm_crc2 = xmm_crc3;
        crc_fold = CRC_K[2];

        xmm_crc3 = _mm_clmulepi64_si128(xmm_crc3, crc_fold, 0);
        xmm_crc3 = _mm_xor_si128(xmm_crc3, xmm_crc2);
        xmm_crc3 = _mm_and_si128(xmm_crc3, xmm_mask);

        xmm_crc2 = xmm_crc3;
        xmm_crc3 = _mm_clmulepi64_si128(xmm_crc3, crc_fold, 0x10);
        xmm_crc3 = _mm_xor_si128(xmm_crc3, xmm_crc2);
        xmm_crc3 = _mm_xor_si128(xmm_crc3, xmm_crc1);

        !(_mm_extract_epi32(xmm_crc3, 2) as u32)
    }
}

pub fn crc32(buf: &[u8], start: u32) -> u32 {
    /* For lens < 64, crc32_braid method is faster. The CRC32 instruction for
     * these short lengths might also prove to be effective */
    if buf.len() < 64 {
        return crc32_braid(buf, start);
    }

    let mut crc_state = Crc32Fold::new();
    crc_state.fold(buf, start);
    return crc_state.finish();
}

fn crc32_braid(buf: &[u8], start: u32) -> u32 {
    crate::crc32::crc32_braid::<5>(buf, start)
}

pub fn crc32_fold(crc: &mut Crc32Fold, src: &[u8], start: u32) {
    unsafe { crc32_fold_help::<false>(crc, &mut [], src, start) }
}

#[target_feature(enable = "pclmulqdq", enable = "sse2", enable = "sse4.1")]
unsafe fn crc32_fold_help<const COPY: bool>(
    crc: &mut Crc32Fold,
    mut dst: &mut [u8],
    mut src: &[u8],
    init_crc: u32,
) {
    let mut xmm_crc_part = _mm_setzero_si128();

    let mut partial_buf = Align16([0u8; 16]);
    let xmm_initial = _mm_cvtsi32_si128(init_crc as i32);
    let mut first = init_crc != 0;

    macro_rules! once {
        ($tokens:tt) => {
            if first {
                first = false;
                let _ = first;
                $tokens
            }
        };
    }

    macro_rules! xor_initial128 {
        ($where:ident) => {
            once!({ $where = _mm_xor_si128($where, xmm_initial) });
        };
    }

    // Technically the CRC functions don't even call this for input < 64, but a bare minimum of 31
    // bytes of input is needed for the aligning load that occurs.  If there's an initial CRC, to
    // carry it forward through the folded CRC there must be 16 - src % 16 + 16 bytes available, which
    // by definition can be up to 15 bytes + one full vector load. */
    assert!(src.len() >= 31 || !first);

    let [mut xmm_crc0, mut xmm_crc1, mut xmm_crc2, mut xmm_crc3] = crc.fold.load();

    if src.len() < 16 {
        if COPY {
            if src.len() == 0 {
                return;
            }

            partial_buf.0[..src.len()].copy_from_slice(src);
            xmm_crc_part = _mm_load_si128(partial_buf.0.as_mut_ptr() as *mut __m128i);
            dst[..src.len()].copy_from_slice(&partial_buf.0[..src.len()]);
        }
    } else {
        let align_diff = (16 - (src.as_ptr() as usize & 0xF)) & 0xF;
        if align_diff != 0 {
            xmm_crc_part = _mm_loadu_si128(src.as_ptr() as *const __m128i);
            if COPY {
                _mm_storeu_si128(dst.as_mut_ptr() as *mut __m128i, xmm_crc_part);
                dst = &mut dst[align_diff..];
            } else {
                xor_initial128!(xmm_crc_part);

                if align_diff < 4 && init_crc != 0 {
                    let xmm_t0 = xmm_crc_part;
                    xmm_crc_part = _mm_loadu_si128((src.as_ptr() as *const __m128i).add(1));

                    [xmm_crc0, xmm_crc1, xmm_crc2, xmm_crc3] =
                        fold::<1>([xmm_crc0, xmm_crc1, xmm_crc2, xmm_crc3]);

                    xmm_crc3 = _mm_xor_si128(xmm_crc3, xmm_t0);
                    src = &src[16..];
                }
            }

            partial_fold(
                align_diff,
                &mut xmm_crc0,
                &mut xmm_crc1,
                &mut xmm_crc2,
                &mut xmm_crc3,
                xmm_crc_part,
            );

            src = &src[align_diff..];
        }

        // #ifdef X86_VPCLMULQDQ
        //     if (len >= 256) {
        // #ifdef COPY
        //         size_t n = fold_16_vpclmulqdq_copy(&xmm_crc0, &xmm_crc1, &xmm_crc2, &xmm_crc3, dst, src, len);
        //         dst += n;
        // #else
        //         size_t n = fold_16_vpclmulqdq(&xmm_crc0, &xmm_crc1, &xmm_crc2, &xmm_crc3, src, len,
        //             xmm_initial, first);
        //         first = 0;
        // #endif
        //         len -= n;
        //         src += n;
        //     }
        // #endif

        while src.len() >= 64 {
            [xmm_crc0, xmm_crc1, xmm_crc2, xmm_crc3] = progress::<1, COPY>(
                &mut &mut dst[..],
                &mut src,
                [xmm_crc0, xmm_crc1, xmm_crc2, xmm_crc3],
                &mut first,
                xmm_initial,
            );
        }

        // len = num bytes left - 64
        if src.len() >= 3 * 16 {
            [xmm_crc0, xmm_crc1, xmm_crc2, xmm_crc3] = progress::<3, COPY>(
                &mut &mut dst[..],
                &mut src,
                [xmm_crc0, xmm_crc1, xmm_crc2, xmm_crc3],
                &mut first,
                xmm_initial,
            );
        } else if src.len() >= 2 * 32 {
            [xmm_crc0, xmm_crc1, xmm_crc2, xmm_crc3] = progress::<2, COPY>(
                &mut &mut dst[..],
                &mut src,
                [xmm_crc0, xmm_crc1, xmm_crc2, xmm_crc3],
                &mut first,
                xmm_initial,
            );
        } else if src.len() >= 1 * 16 {
            [xmm_crc0, xmm_crc1, xmm_crc2, xmm_crc3] = progress::<1, COPY>(
                &mut &mut dst[..],
                &mut src,
                [xmm_crc0, xmm_crc1, xmm_crc2, xmm_crc3],
                &mut first,
                xmm_initial,
            );
        }
    }

    if !src.is_empty() {
        std::ptr::copy_nonoverlapping(
            src.as_ptr(),
            &mut xmm_crc_part as *mut _ as *mut u8,
            src.len(),
        );
        if COPY {
            _mm_storeu_si128(partial_buf.0.as_mut_ptr() as *mut __m128i, xmm_crc_part);
            std::ptr::copy_nonoverlapping(partial_buf.0.as_ptr(), dst.as_mut_ptr(), src.len());
        }
        partial_fold(
            src.len(),
            &mut xmm_crc0,
            &mut xmm_crc1,
            &mut xmm_crc2,
            &mut xmm_crc3,
            xmm_crc_part,
        );
    }

    crc.fold.store([xmm_crc0, xmm_crc1, xmm_crc2, xmm_crc3])
}

const XMM_FOLD4: __m128i = {
    // _mm_set_epi32(0x00000001u32, 0x54442bd4u32, 0x00000001u32, 0xc6e41596u32);
    // for a transmute the words need to be reversed
    let bytes = [0xc6e41596u32, 0x00000001u32, 0x54442bd4u32, 0x00000001u32];
    unsafe { core::mem::transmute(bytes) }
};

#[inline(always)]
unsafe fn step(input: __m128i) -> __m128i {
    let tmp1 = _mm_clmulepi64_si128(input, XMM_FOLD4, 0x01);
    let tmp2 = _mm_clmulepi64_si128(input, XMM_FOLD4, 0x10);
    let ps_crc0 = _mm_castsi128_ps(tmp1);
    let ps_crc3 = _mm_castsi128_ps(tmp2);
    let ps_res = _mm_xor_ps(ps_crc0, ps_crc3);

    _mm_castps_si128(ps_res)
}

fn fold<const N: usize>(input: [__m128i; 4]) -> [__m128i; 4] {
    std::array::from_fn(|i| match input.get(i + N) {
        Some(v) => *v,
        None => unsafe { step(input[(i + N) - 4]) },
    })
}

fn progress<'inner, 'outer: 'inner, const N: usize, const COPY: bool>(
    dst: &'outer mut &'inner mut [u8],
    src: &'_ mut &[u8],
    state: [__m128i; 4],
    first: &mut bool,
    xmm_initial: __m128i,
) -> [__m128i; 4] {
    let mut input = [unsafe { _mm_setzero_si128() }; 4];

    let src_ptr = src.as_ptr() as *const __m128i;
    for i in 0..N {
        input[i] = unsafe { _mm_load_si128(src_ptr.add(i)) };
    }

    *src = &src[N * 16..];

    if COPY {
        let dst_ptr = dst.as_mut_ptr() as *mut __m128i;
        for i in 0..N {
            unsafe { _mm_storeu_si128(dst_ptr.add(0), input[i]) };
        }
        *dst = &mut (*dst)[N * 16..];
    } else {
        if *first {
            *first = false;
            input[0] = unsafe { _mm_xor_si128(input[0], xmm_initial) };
        }
    }

    let mut state = fold::<N>(state);

    for i in 0..N {
        state[i + (4 - N)] = unsafe { _mm_xor_si128(state[i + (4 - N)], input[i]) };
    }

    state
}

static PSHUFB_SHF_TABLE: Align32<[u32; 60]> = Align32([
    0x84838281, 0x88878685, 0x8c8b8a89, 0x008f8e8d, /* shl 15 (16 - 1)/shr1 */
    0x85848382, 0x89888786, 0x8d8c8b8a, 0x01008f8e, /* shl 14 (16 - 3)/shr2 */
    0x86858483, 0x8a898887, 0x8e8d8c8b, 0x0201008f, /* shl 13 (16 - 4)/shr3 */
    0x87868584, 0x8b8a8988, 0x8f8e8d8c, 0x03020100, /* shl 12 (16 - 4)/shr4 */
    0x88878685, 0x8c8b8a89, 0x008f8e8d, 0x04030201, /* shl 11 (16 - 5)/shr5 */
    0x89888786, 0x8d8c8b8a, 0x01008f8e, 0x05040302, /* shl 10 (16 - 6)/shr6 */
    0x8a898887, 0x8e8d8c8b, 0x0201008f, 0x06050403, /* shl  9 (16 - 7)/shr7 */
    0x8b8a8988, 0x8f8e8d8c, 0x03020100, 0x07060504, /* shl  8 (16 - 8)/shr8 */
    0x8c8b8a89, 0x008f8e8d, 0x04030201, 0x08070605, /* shl  7 (16 - 9)/shr9 */
    0x8d8c8b8a, 0x01008f8e, 0x05040302, 0x09080706, /* shl  6 (16 -10)/shr10*/
    0x8e8d8c8b, 0x0201008f, 0x06050403, 0x0a090807, /* shl  5 (16 -11)/shr11*/
    0x8f8e8d8c, 0x03020100, 0x07060504, 0x0b0a0908, /* shl  4 (16 -12)/shr12*/
    0x008f8e8d, 0x04030201, 0x08070605, 0x0c0b0a09, /* shl  3 (16 -13)/shr13*/
    0x01008f8e, 0x05040302, 0x09080706, 0x0d0c0b0a, /* shl  2 (16 -14)/shr14*/
    0x0201008f, 0x06050403, 0x0a090807, 0x0e0d0c0b, /* shl  1 (16 -15)/shr15*/
]);

unsafe fn partial_fold(
    len: usize,

    xmm_crc0: &mut __m128i,
    xmm_crc1: &mut __m128i,
    xmm_crc2: &mut __m128i,
    xmm_crc3: &mut __m128i,

    xmm_crc_part: __m128i,
) {
    let xmm_fold4 = _mm_set_epi32(0x00000001, 0x54442bd4, 0x00000001, 0xc6e41596u32 as i32);
    let xmm_mask3 = _mm_set1_epi32(0x80808080u32 as i32);

    let xmm_shl = _mm_load_si128(PSHUFB_SHF_TABLE.0[(4 * (len - 1))..].as_ptr() as *const __m128i);
    let mut xmm_shr = xmm_shl;
    xmm_shr = _mm_xor_si128(xmm_shr, xmm_mask3);

    let xmm_a0_0 = _mm_shuffle_epi8(*xmm_crc0, xmm_shl);

    *xmm_crc0 = _mm_shuffle_epi8(*xmm_crc0, xmm_shr);
    let xmm_tmp1 = _mm_shuffle_epi8(*xmm_crc1, xmm_shl);
    *xmm_crc0 = _mm_or_si128(*xmm_crc0, xmm_tmp1);

    *xmm_crc1 = _mm_shuffle_epi8(*xmm_crc1, xmm_shr);
    let xmm_tmp2 = _mm_shuffle_epi8(*xmm_crc2, xmm_shl);
    *xmm_crc1 = _mm_or_si128(*xmm_crc1, xmm_tmp2);

    *xmm_crc2 = _mm_shuffle_epi8(*xmm_crc2, xmm_shr);
    let xmm_tmp3 = _mm_shuffle_epi8(*xmm_crc3, xmm_shl);
    *xmm_crc2 = _mm_or_si128(*xmm_crc2, xmm_tmp3);

    *xmm_crc3 = _mm_shuffle_epi8(*xmm_crc3, xmm_shr);
    let xmm_crc_part = _mm_shuffle_epi8(xmm_crc_part, xmm_shl);
    *xmm_crc3 = _mm_or_si128(*xmm_crc3, xmm_crc_part);

    let xmm_a0_1 = _mm_clmulepi64_si128(xmm_a0_0, xmm_fold4, 0x10);
    let xmm_a0_0 = _mm_clmulepi64_si128(xmm_a0_0, xmm_fold4, 0x01);

    let ps_crc3 = _mm_castsi128_ps(*xmm_crc3);
    let psa0_0 = _mm_castsi128_ps(xmm_a0_0);
    let psa0_1 = _mm_castsi128_ps(xmm_a0_1);

    let ps_res = _mm_xor_ps(ps_crc3, psa0_0);
    let ps_res = _mm_xor_ps(ps_res, psa0_1);

    *xmm_crc3 = _mm_castps_si128(ps_res);
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn long_enough() {
        let v = std::iter::repeat(0..255)
            .take(4)
            .flatten()
            .take(128)
            .collect::<Vec<_>>();
        let start = 0;

        let mut h = crc32fast::Hasher::new_with_initial(start);
        h.update(&v[..]);
        let a = unsafe { load_dynamic_libz_ng::crc32(0, v.as_ptr(), v.len()) };
        assert_eq!(crc32(&v[..], start), h.finalize());
        let b = crc32(&v[..], start);
        assert_eq!(a, b)
    }

    quickcheck::quickcheck! {
        fn crc_sse_is_crc32fast(v: Vec<u8>, start: u32) -> bool {
            let mut h = crc32fast::Hasher::new_with_initial(start);
            let mut v = v.clone();
            let n = v.len();
            v.extend([0; 512]);

            h.update(&v[..n]);

            let a = crc32(&v[..n], start) ;
            let b = h.finalize();

            if a != b {
                dbg!(a,b);
            }

            a == b
        }

    }
}
