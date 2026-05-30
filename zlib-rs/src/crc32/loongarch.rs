use super::combine::{multmodp, x2nmodp};

const Z_BATCH: usize = 3990;
const Z_BATCH_ZEROS: u32 = x2nmodp(Z_BATCH as u64, 6);
const _: () = assert!(Z_BATCH_ZEROS == 0xa10d3d0c);
const Z_BATCH_MIN: usize = 800;

pub fn crc32_loongarch64(crc: u32, buf: &[u8]) -> u32 {
    let mut crc = (!crc).cast_signed();

    // SAFETY: [u8; 8] safely transmutes into u64.
    let (before, middle, after) = unsafe { buf.align_to::<i64>() };

    // SAFETY: requires the "crc" feature.
    crc = unsafe { remainder(crc, before) };

    let mut words = middle;

    while words.len() >= 3 * Z_BATCH {
        let mut crc1: i32 = 0;
        let mut crc2: i32 = 0;
        for i in 0..Z_BATCH {
            // SAFETY: requires the "crc" feature.
            crc = crc_w_d_w(words[i], crc);
            crc1 = crc_w_d_w(words[i + Z_BATCH], crc1);
            crc2 = crc_w_d_w(words[i + 2 * Z_BATCH], crc2);
        }
        words = &words[3 * Z_BATCH..];
        crc = multmodp(Z_BATCH_ZEROS, crc.cast_unsigned()).cast_signed() ^ crc1;
        crc = multmodp(Z_BATCH_ZEROS, crc.cast_unsigned()).cast_signed() ^ crc2;
    }

    let last = words.len() / 3;
    if last >= Z_BATCH_MIN {
        let mut crc1: i32 = 0;
        let mut crc2: i32 = 0;
        for i in 0..last {
            // SAFETY: requires the "crc" feature.
            unsafe {
                crc = crc_w_d_w(words[i], crc);
                crc1 = crc_w_d_w(words[i + last], crc1);
                crc2 = crc_w_d_w(words[i + 2 * last], crc2);
            }
        }
        words = &words[3 * last..];
        let val = x2nmodp(last as u64, 6);
        crc = multmodp(val, crc.cast_unsigned()).cast_signed() ^ crc1;
        crc = multmodp(val, crc.cast_unsigned()).cast_signed() ^ crc2;
    }

    for &w in words {
        // SAFETY: requires the "crc" feature.
        crc = crc_w_d_w(w, crc);
    }

    // SAFETY: requires the "crc" feature.
    crc = unsafe { remainder(crc, after) };

    (!crc).cast_unsigned()
}

#[inline]
fn remainder(mut c: i32, mut buf: &[u8]) -> i32 {
    if let [b0, b1, b2, b3, rest @ ..] = buf {
        c = crc_w_w_w(i32::from_le_bytes([*b0, *b1, *b2, *b3]), c);
        buf = rest;
    }

    if let [b0, b1, rest @ ..] = buf {
        c = crc_w_h_w(i16::from_le_bytes([*b0, *b1]), c);
        buf = rest;
    }

    if let [b0, rest @ ..] = buf {
        c = crc_w_b_w(*b0 as i8, c);
        buf = rest;
    }

    debug_assert!(buf.is_empty());

    c
}

crate::cfg_select! {
    miri => {
        use core::arch::loongarch64::{crc_w_b_w, crc_w_h_w, crc_w_w_w, crc_w_d_w};
    }
    _ => {
        use asm::{crc_w_b_w, crc_w_h_w, crc_w_w_w, crc_w_d_w};
    }
}

// FIXME: there are intrinsics for these in the standard library, but currently
// unstable behind the stdarch_loongarch feature
//
// CRC32 instructions are part of the basic integer operations and therefore
// always available.
mod asm {
    /// CRC32 single round checksum for bytes (8 bits).
    ///
    /// [Loongson's documentation](https://loongson.github.io/LoongArch-Documentation/LoongArch-Vol1-EN.html#crc-check-instructions)
    pub fn crc_w_b_w(data: i8, mut crc: i32) -> i32 {
        unsafe {
            core::arch::asm!("crc.w.b.w {crc}, {data}, {crc}", crc = inout(reg) crc, data = in(reg) data);
            crc
        }
    }

    /// CRC32 single round checksum for half words (16 bits).
    ///
    /// [Loongson's documentation](https://loongson.github.io/LoongArch-Documentation/LoongArch-Vol1-EN.html#crc-check-instructions)
    pub fn crc_w_h_w(data: i16, mut crc: i32) -> i32 {
        unsafe {
            core::arch::asm!("crc.w.h.w {crc}, {data}, {crc}", crc = inout(reg) crc, data = in(reg) data);
            crc
        }
    }

    /// CRC32 single round checksum for words (32 bits).
    ///
    /// [Loongson's documentation](https://loongson.github.io/LoongArch-Documentation/LoongArch-Vol1-EN.html#crc-check-instructions)
    pub fn crc_w_w_w(data: i32, mut crc: i32) -> i32 {
        unsafe {
            core::arch::asm!("crc.w.w.w {crc}, {data}, {crc}", crc = inout(reg) crc, data = in(reg) data);
            crc
        }
    }

    /// CRC32 single round checksum for double words (64 bits).
    ///
    /// [Loongson's documentation](https://loongson.github.io/LoongArch-Documentation/LoongArch-Vol1-EN.html#crc-check-instructions)
    pub fn crc_w_d_w(data: i64, mut crc: i32) -> i32 {
        unsafe {
            core::arch::asm!("crc.w.d.w {crc}, {data}, {crc}", crc = inout(reg) crc, data = in(reg) data);
            crc
        }
    }
}
