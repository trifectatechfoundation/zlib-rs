use crate::CRC32_INITIAL_VALUE;

mod braid;
#[cfg(target_arch = "x86_64")]
mod pclmulqdq;

pub fn crc32(buf: &[u8], start: u32) -> u32 {
    /* For lens < 64, crc32_braid method is faster. The CRC32 instruction for
     * these short lengths might also prove to be effective */
    if buf.len() < 64 {
        return braid::crc32_braid::<5>(buf, start);
    }

    let mut crc_state = Crc32Fold::new_with_initial(start);
    crc_state.fold(buf, start);
    crc_state.finish()
}

#[allow(unused)]
pub fn crc32_copy(dst: &mut [u8], buf: &[u8]) -> u32 {
    /* For lens < 64, crc32_braid method is faster. The CRC32 instruction for
     * these short lengths might also prove to be effective */
    if buf.len() < 64 {
        dst.copy_from_slice(buf);
        return braid::crc32_braid::<5>(buf, CRC32_INITIAL_VALUE);
    }

    let mut crc_state = Crc32Fold::new();
    crc_state.fold_copy(dst, buf);
    crc_state.finish()
}

#[derive(Debug)]
pub struct Crc32Fold {
    #[cfg(target_arch = "x86_64")]
    fold: pclmulqdq::Accumulator,
    value: u32,
}

impl Default for Crc32Fold {
    fn default() -> Self {
        Self::new()
    }
}

impl Crc32Fold {
    pub const fn new() -> Self {
        Self::new_with_initial(CRC32_INITIAL_VALUE)
    }

    pub const fn new_with_initial(initial: u32) -> Self {
        Self {
            #[cfg(target_arch = "x86_64")]
            fold: pclmulqdq::Accumulator::new(),
            value: initial,
        }
    }

    #[cfg(target_arch = "x86_64")]
    fn is_pclmulqdq() -> bool {
        is_x86_feature_detected!("pclmulqdq")
            && is_x86_feature_detected!("sse2")
            && is_x86_feature_detected!("sse4.1")
    }

    pub fn fold(&mut self, src: &[u8], _start: u32) {
        #[cfg(target_arch = "x86_64")]
        if Self::is_pclmulqdq() {
            return self.fold.fold(src, _start);
        }

        // in this case the start value is ignored
        self.value = braid::crc32_braid::<5>(src, self.value);
    }

    pub fn fold_copy(&mut self, dst: &mut [u8], src: &[u8]) {
        #[cfg(target_arch = "x86_64")]
        if Self::is_pclmulqdq() {
            return self.fold.fold_copy(dst, src);
        }

        self.value = braid::crc32_braid::<5>(src, self.value);
        dst[..src.len()].copy_from_slice(src);
    }

    pub fn finish(self) -> u32 {
        #[cfg(target_arch = "x86_64")]
        if Self::is_pclmulqdq() {
            return unsafe { self.fold.finish() };
        }

        self.value
    }
}

#[cfg(any(target_arch = "aarch64", target_arch = "arm"))]
pub mod arm {
    /// CRC32 single round checksum for bytes (8 bits).
    ///
    /// [Arm's documentation](https://developer.arm.com/architectures/instruction-sets/intrinsics/__crc32b)
    #[target_feature(enable = "crc")]
    #[cfg_attr(target_arch = "arm", target_feature(enable = "v8"))]
    pub unsafe fn __crc32b(h: u32, val: u8) -> u32 {
        let mut out;
        unsafe {
            std::arch::asm!("crc32b {wd:w}, {wn:w}, {wm:w}",
            wn = in(reg) h,
            wm = in(reg) val,
            wd = out(reg) out,
            )
        }
        out
    }

    /// CRC32 single round checksum for half words (16 bits).
    ///
    /// [Arm's documentation](https://developer.arm.com/architectures/instruction-sets/intrinsics/__crc32h)
    #[target_feature(enable = "crc")]
    #[cfg_attr(target_arch = "arm", target_feature(enable = "v8"))]
    pub unsafe fn __crc32h(h: u32, val: u16) -> u32 {
        let mut out;
        unsafe {
            std::arch::asm!("crc32h {wd:w}, {wn:w}, {wm:w}",
            wn = in(reg) h,
            wm = in(reg) val,
            wd = out(reg) out,
            )
        }
        out
    }

    /// CRC32 single round checksum for words (32 bits).
    ///
    /// [Arm's documentation](https://developer.arm.com/architectures/instruction-sets/intrinsics/__crc32w)
    #[target_feature(enable = "crc")]
    #[cfg_attr(target_arch = "arm", target_feature(enable = "v8"))]
    pub unsafe fn __crc32w(h: u32, val: u32) -> u32 {
        let mut out;
        unsafe {
            std::arch::asm!("crc32w {wd:w}, {wn:w}, {wm:w}",
            wn = in(reg) h,
            wm = in(reg) val,
            wd = out(reg) out,
            )
        }
        out
    }

    /// CRC32-C single round checksum for words (32 bits).
    ///
    /// [Arm's documentation](https://developer.arm.com/architectures/instruction-sets/intrinsics/__crc32cw)
    #[target_feature(enable = "crc")]
    #[cfg_attr(target_arch = "arm", target_feature(enable = "v8"))]
    pub unsafe fn __crc32cw(crc: u32, data: u32) -> u32 {
        let mut out;
        unsafe {
            std::arch::asm!("crc32cw {wd:w}, {wn:w}, {wm:w}",
            wn = in(reg) crc,
            wm = in(reg) data,
            wd = out(reg) out,
            )
        }
        out
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: [u8; 1024] = {
        let mut array = [0; 1024];
        let mut i = 0;
        while i < array.len() {
            array[i] = i as u8;
            i += 1;
        }

        array
    };

    #[test]
    fn test_crc32_fold() {
        // input large enough to trigger the SIMD
        let mut h = crc32fast::Hasher::new_with_initial(CRC32_INITIAL_VALUE);
        h.update(&INPUT);
        assert_eq!(crc32(&INPUT, CRC32_INITIAL_VALUE), h.finalize());
    }

    #[test]
    fn test_crc32_fold_align() {
        // SIMD algorithm is sensitive to alignment;
        for i in 0..16 {
            for start in [CRC32_INITIAL_VALUE, 42] {
                let mut h = crc32fast::Hasher::new_with_initial(start);
                h.update(&INPUT[i..]);
                assert_eq!(
                    crc32(&INPUT[i..], start),
                    h.finalize(),
                    "offset = {i}, start = {start}"
                );
            }
        }
    }

    #[test]
    fn test_crc32_fold_copy() {
        // input large enough to trigger the SIMD
        let mut h = crc32fast::Hasher::new_with_initial(CRC32_INITIAL_VALUE);
        h.update(&INPUT);
        let mut dst = [0; INPUT.len()];

        assert_eq!(crc32_copy(&mut dst, &INPUT), h.finalize());

        assert_eq!(INPUT, dst);
    }

    quickcheck::quickcheck! {
        fn crc_fold_is_crc32fast(v: Vec<u8>, start: u32) -> bool {
            let mut h = crc32fast::Hasher::new_with_initial(start);
            h.update(&v);

            let a = crc32(&v, start) ;
            let b = h.finalize();

            a == b
        }

        fn crc_fold_copy_is_crc32fast(v: Vec<u8>) -> bool {
            let mut h = crc32fast::Hasher::new_with_initial(CRC32_INITIAL_VALUE);
            h.update(&v);

            let mut dst = vec![0; v.len()];

            let a = crc32_copy(&mut dst, &v) ;
            let b = h.finalize();

            assert_eq!(a,b);

            v == dst
        }
    }

    #[cfg(target_arch = "aarch64")]
    mod arm_crc32_asm {
        use super::arm::*;

        #[test]
        fn test_crc32b() {
            unsafe {
                assert_eq!(__crc32b(0, 0), 0);
                assert_eq!(__crc32b(0, 255), 755167117);
            }
        }

        #[test]
        fn test_crc32h() {
            unsafe {
                assert_eq!(__crc32h(0, 0), 0);
                assert_eq!(__crc32h(0, 16384), 1994146192);
            }
        }

        #[test]
        fn test_crc32w() {
            unsafe {
                assert_eq!(__crc32w(0, 0), 0);
                assert_eq!(__crc32w(0, 4294967295), 3736805603);
            }
        }

        #[test]
        fn test_crc32cw() {
            unsafe {
                assert_eq!(__crc32cw(0, 0), 0);
                assert_eq!(__crc32cw(0, 4294967295), 3080238136);
            }
        }
    }
}
