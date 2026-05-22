pub fn crc32_loongarch64(crc: u32, buf: &[u8]) -> u32 {
    let mut c = !crc as i32;

    // SAFETY: [u8; 8] safely transmutes into i64.
    let (before, middle, after) = unsafe { buf.align_to::<i64>() };

    c = remainder(c, before);

    if middle.is_empty() && after.is_empty() {
        return !c as u32;
    }

    for d in middle {
        c = crc_w_d_w(*d, c);
    }

    c = remainder(c, after);

    !c as u32
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
