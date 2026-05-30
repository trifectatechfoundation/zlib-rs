use super::{BASE, NMAX};

const UNROLL_MORE: bool = true;

// macros for loop unrolling
macro_rules! do1 {
    ($sum1:expr, $sum2:expr, $chunk:expr, $i:expr) => {
        // SAFETY: $i is bounded by either [0, 8] or [0, 16], and the caller ensures the chunk is
        // long enough, so we can omit bound checking.
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

pub fn adler32_rust(mut adler: u32, buf: &[u8]) -> u32 {
    /* split Adler-32 into component sums */
    let mut sum2 = (adler >> 16) & 0xffff;
    adler &= 0xffff;

    /* in case user likes doing a byte at a time, keep it fast */
    if buf.len() == 1 {
        return adler32_len_1(adler, buf, sum2);
    }

    /* initial Adler-32 value (deferred check for len == 1 speed) */
    if buf.is_empty() {
        return adler | (sum2 << 16);
    }

    /* in case short lengths are provided, keep it somewhat fast */
    if buf.len() < 16 {
        return adler32_len_16(adler, buf, sum2);
    }

    const N: usize = if UNROLL_MORE { 16 } else { 8 };
    let mut buf = buf;
    while buf.len() >= NMAX as usize {
        // SAFETY: buf.len() >= NMAX
        let big_chunk = unsafe { buf.get_unchecked(..NMAX as usize) };
        buf = unsafe { buf.get_unchecked(NMAX as usize..) };

        /* Advance a raw pointer through big_chunk, matching C's `buf += 16`.
         * This avoids the per-iteration base-pointer recomputation that
         * the offset-based `get_unchecked(pos..pos+N)` requires. */
        let mut ptr = big_chunk.as_ptr();
        // SAFETY: big_chunk.len() == NMAX, so ptr.add(NMAX) is one-past-end and valid to form
        let end = unsafe { ptr.add(NMAX as usize) };
        loop {
            // SAFETY: ptr < end before this point, so [ptr, ptr+N) lies within big_chunk
            let chunk = unsafe { core::slice::from_raw_parts(ptr, N) };
            if N == 16 {
                do16!(adler, sum2, chunk);
            } else {
                do8!(adler, sum2, chunk, 0);
            }
            ptr = unsafe { ptr.add(N) };
            if ptr == end {
                break;
            }
        }

        adler %= BASE;
        sum2 %= BASE;
    }

    /* do remaining bytes (less than NMAX, still just one modulo) */
    if buf.is_empty() {
        /* adler and sum2 already reduced from the outer loop */
        return adler | (sum2 << 16);
    }
    adler32_len_64(adler, buf, sum2)
}

pub(crate) fn adler32_len_1(mut adler: u32, buf: &[u8], mut sum2: u32) -> u32 {
    adler += buf[0] as u32;
    if adler >= BASE {
        adler -= BASE;
    }
    sum2 += adler;
    if sum2 >= BASE {
        sum2 -= BASE;
    }
    adler | (sum2 << 16)
}

pub(crate) fn adler32_len_16(mut adler: u32, buf: &[u8], mut sum2: u32) -> u32 {
    for b in buf {
        adler += (*b) as u32;
        sum2 += adler;
    }

    // callers guarantee adler < BASE on entry, so after at most 15 additions of ≤ 255
    // adler < BASE + 15 * 255 < 2 * BASE — one conditional subtract suffices
    if adler >= BASE {
        adler -= BASE;
    }
    sum2 %= BASE; /* only added so many BASE's */
    /* return recombined sums */
    adler | (sum2 << 16)
}

pub(crate) fn adler32_len_64(mut adler: u32, buf: &[u8], mut sum2: u32) -> u32 {
    const N: usize = if UNROLL_MORE { 16 } else { 8 };
    let n_chunks = buf.len() / N;
    let tail_start = n_chunks * N;

    if n_chunks > 0 {
        /* Advance a raw pointer through buf, matching C's `buf += 16`. */
        let mut ptr = buf.as_ptr();
        // SAFETY: tail_start <= buf.len(), so ptr.add(tail_start) is valid to form
        let end = unsafe { ptr.add(tail_start) };
        loop {
            // SAFETY: ptr < end before this point, so [ptr, ptr+N) lies within buf
            let chunk = unsafe { core::slice::from_raw_parts(ptr, N) };
            if N == 16 {
                do16!(adler, sum2, chunk);
            } else {
                do8!(adler, sum2, chunk, 0);
            }
            ptr = unsafe { ptr.add(N) };
            if ptr == end {
                break;
            }
        }
    }

    /* Process tail (len < N). */
    adler %= BASE;
    // SAFETY: tail_start = (buf.len() / N) * N <= buf.len()
    adler32_len_16(adler, unsafe { buf.get_unchecked(tail_start..) }, sum2)
}
