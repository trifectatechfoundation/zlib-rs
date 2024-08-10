#[cfg_attr(not(target_arch = "aarch64"), allow(unused))]
pub fn crc32_acle_aarch64(crc: u32, buf: &[u8]) -> u32 {
    let mut c = !crc;

    let (before, middle, after) = unsafe { buf.align_to::<u64>() };

    c = remainder(c, before);

    if middle.is_empty() && after.is_empty() {
        return !c;
    }

    for d in middle {
        c = unsafe { __crc32d(c, *d) };
    }

    c = remainder(c, after);

    !c
}

#[cfg_attr(not(target_arch = "arm"), allow(unused))]
pub fn crc32_acle_arm(crc: u32, buf: &[u8]) -> u32 {
    let mut c = !crc;

    let (before, middle, after) = unsafe { buf.align_to::<u32>() };

    c = remainder(c, before);

    if middle.is_empty() && after.is_empty() {
        return !c;
    }

    for w in middle {
        c = unsafe { __crc32w(c, *w) };
    }

    c = remainder(c, after);

    !c
}

fn remainder(mut c: u32, mut buf: &[u8]) -> u32 {
    if let [b0, b1, b2, b3, rest @ ..] = buf {
        c = unsafe { __crc32w(c, u32::from_le_bytes([*b0, *b1, *b2, *b3])) };
        buf = rest;
    }

    if let [b0, b1, rest @ ..] = buf {
        c = unsafe { __crc32h(c, u16::from_le_bytes([*b0, *b1])) };
        buf = rest;
    }

    if let [b0, rest @ ..] = buf {
        c = unsafe { __crc32b(c, *b0) };
        buf = rest;
    }

    debug_assert!(buf.is_empty());

    c
}

/// CRC32 single round checksum for bytes (8 bits).
///
/// [Arm's documentation](https://developer.arm.com/architectures/instruction-sets/intrinsics/__crc32b)
#[target_feature(enable = "crc")]
#[cfg_attr(target_arch = "arm", target_feature(enable = "v8"))]
unsafe fn __crc32b(mut crc: u32, data: u8) -> u32 {
    core::arch::asm!("crc32b {crc:w}, {crc:w}, {data:w}", crc = inout(reg) crc, data = in(reg) data);
    crc
}

/// CRC32 single round checksum for half words (16 bits).
///
/// [Arm's documentation](https://developer.arm.com/architectures/instruction-sets/intrinsics/__crc32h)
#[target_feature(enable = "crc")]
#[cfg_attr(target_arch = "arm", target_feature(enable = "v8"))]
unsafe fn __crc32h(mut crc: u32, data: u16) -> u32 {
    core::arch::asm!("crc32h {crc:w}, {crc:w}, {data:w}", crc = inout(reg) crc, data = in(reg) data);
    crc
}

/// CRC32 single round checksum for words (32 bits).
///
/// [Arm's documentation](https://developer.arm.com/architectures/instruction-sets/intrinsics/__crc32w)
#[target_feature(enable = "crc")]
#[cfg_attr(target_arch = "arm", target_feature(enable = "v8"))]
unsafe fn __crc32w(mut crc: u32, data: u32) -> u32 {
    core::arch::asm!("crc32w {crc:w}, {crc:w}, {data:w}", crc = inout(reg) crc, data = in(reg) data);
    crc
}

/// CRC32 single round checksum for double words (64 bits).
///
/// [Arm's documentation](https://developer.arm.com/architectures/instruction-sets/intrinsics/__crc32d)
#[cfg(target_arch = "aarch64")]
#[target_feature(enable = "crc")]
unsafe fn __crc32d(mut crc: u32, data: u64) -> u32 {
    core::arch::asm!("crc32x {crc:w}, {crc:w}, {data:x}", crc = inout(reg) crc, data = in(reg) data);
    crc
}

/// CRC32-C single round checksum for words (32 bits).
///
/// [Arm's documentation](https://developer.arm.com/architectures/instruction-sets/intrinsics/__crc32cw)
#[target_feature(enable = "crc")]
#[cfg_attr(target_arch = "arm", target_feature(enable = "v8"))]
pub unsafe fn __crc32cw(mut crc: u32, data: u32) -> u32 {
    core::arch::asm!("crc32cw {crc:w}, {crc:w}, {data:w}", crc = inout(reg) crc, data = in(reg) data);
    crc
}

#[cfg(test)]
mod tests {
    use super::*;

    quickcheck::quickcheck! {
        #[cfg(target_arch = "aarch64")]
        fn crc32_acle_aarch64_is_crc32fast(v: Vec<u8>, start: u32) -> bool {
            let mut h = crc32fast::Hasher::new_with_initial(start);
            h.update(&v);

            let a = crc32_acle_aarch64(start, &v) ;
            let b = h.finalize();

            a == b
        }

        fn crc32_acle_arm_is_crc32fast(v: Vec<u8>, start: u32) -> bool {
            let mut h = crc32fast::Hasher::new_with_initial(start);
            h.update(&v);

            let a = crc32_acle_arm(start, &v) ;
            let b = h.finalize();

            a == b
        }
    }

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
    #[cfg(target_arch = "aarch64")]
    fn test_crc32d() {
        unsafe {
            assert_eq!(__crc32d(0, 0), 0);
            assert_eq!(__crc32d(0, 18446744073709551615), 1147535477);
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
