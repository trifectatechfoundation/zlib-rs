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

    let (big_chunks, rest) = slice_as_chunks::<_, { NMAX as usize }>(buf);
    for big_chunk in big_chunks {
        const N: usize = if UNROLL_MORE { 16 } else { 8 } as usize;
        for chunk in slice_as_chunks::<_, N>(big_chunk).0 {
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
    adler32_len_64(adler, rest, sum2)
}

pub(crate) fn adler32_len_1(mut adler: u32, buf: &[u8], mut sum2: u32) -> u32 {
    adler += buf[0] as u32;
    adler %= BASE;
    sum2 += adler;
    sum2 %= BASE;
    adler | (sum2 << 16)
}

pub(crate) fn adler32_len_16(mut adler: u32, buf: &[u8], mut sum2: u32) -> u32 {
    for b in buf {
        adler += (*b) as u32;
        sum2 += adler;
    }

    adler %= BASE;
    sum2 %= BASE; /* only added so many BASE's */
    /* return recombined sums */
    adler | (sum2 << 16)
}

pub(crate) fn adler32_len_64(mut adler: u32, buf: &[u8], mut sum2: u32) -> u32 {
    const N: usize = if UNROLL_MORE { 16 } else { 8 };
    let mut it = buf.chunks_exact(N);
    for chunk in it.by_ref() {
        if N == 16 {
            do16!(adler, sum2, chunk);
        } else {
            do8!(adler, sum2, chunk, 0);
        }
    }

    /* Process tail (len < 16).  */
    adler32_len_16(adler, it.remainder(), sum2)
}

// FIXME use the method on slices once available.
fn slice_as_chunks<T, const N: usize>(slice: &[T]) -> (&[[T; N]], &[T]) {
    assert!(N != 0, "chunk size must be non-zero");
    let len_rounded_down = slice.len() / N * N;
    // SAFETY: The rounded-down value is always the same or smaller than the
    // original length, and thus must be in-bounds of the slice.
    let (multiple_of_n, remainder) = unsafe { slice_split_at_unchecked(slice, len_rounded_down) };
    // SAFETY: We already panicked for zero, and ensured by construction
    // that the length of the subslice is a multiple of N.
    let array_slice = unsafe { slice_as_chunks_unchecked(multiple_of_n) };
    (array_slice, remainder)
}

unsafe fn slice_as_chunks_unchecked<T, const N: usize>(slice: &[T]) -> &[[T; N]] {
    // SAFETY: Caller must guarantee that `N` is nonzero and exactly divides the slice length
    let new_len = slice.len() / N;
    // SAFETY: We cast a slice of `new_len * N` elements into
    // a slice of `new_len` many `N` elements chunks.
    unsafe { core::slice::from_raw_parts(slice.as_ptr().cast(), new_len) }
}

pub unsafe fn slice_split_at_unchecked<T>(slice: &[T], mid: usize) -> (&[T], &[T]) {
    let len = slice.len();
    let ptr = slice.as_ptr();

    // SAFETY: Caller has to check that `0 <= mid <= self.len()`
    unsafe {
        (
            core::slice::from_raw_parts(ptr, mid),
            core::slice::from_raw_parts(ptr.add(mid), len - mid),
        )
    }
}
