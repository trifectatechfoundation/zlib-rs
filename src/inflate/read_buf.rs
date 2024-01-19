#![allow(unused)]
// taken from https://docs.rs/tokio/latest/src/tokio/io/read_buf.rs.html#23-27
// based on https://rust-lang.github.io/rfcs/2930-read-buf.html
use std::fmt;
use std::mem::MaybeUninit;

/// A wrapper around a byte buffer that is incrementally filled and initialized.
///
/// This type is a sort of "double cursor". It tracks three regions in the
/// buffer: a region at the beginning of the buffer that has been logically
/// filled with data, a region that has been initialized at some point but not
/// yet logically filled, and a region at the end that may be uninitialized.
/// The filled region is guaranteed to be a subset of the initialized region.
///
/// In summary, the contents of the buffer can be visualized as:
///
/// ```not_rust
/// [             capacity              ]
/// [ filled |         unfilled         ]
/// [    initialized    | uninitialized ]
/// ```
///
/// It is undefined behavior to de-initialize any bytes from the uninitialized
/// region, since it is merely unknown whether this region is uninitialized or
/// not, and if part of it turns out to be initialized, it must stay initialized.
pub struct ReadBuf<'a> {
    buf: &'a mut [MaybeUninit<u8>],
    filled: usize,
    initialized: usize,
}

impl<'a> ReadBuf<'a> {
    /// Creates a new `ReadBuf` from a fully initialized buffer.
    #[inline]
    pub fn new(buf: &'a mut [u8]) -> ReadBuf<'a> {
        let initialized = buf.len();
        let buf = unsafe { slice_to_uninit_mut(buf) };
        ReadBuf {
            buf,
            filled: 0,
            initialized,
        }
    }

    pub unsafe fn from_raw_parts(ptr: *mut u8, len: usize) -> Self {
        let buf = std::slice::from_raw_parts_mut(ptr as _, len);

        Self {
            buf,
            filled: 0,
            initialized: 0,
        }
    }

    #[inline]
    pub fn as_mut_ptr(&mut self) -> *mut MaybeUninit<u8> {
        self.buf[self.filled..].as_mut_ptr()
    }

    /// Creates a new `ReadBuf` from a fully uninitialized buffer.
    ///
    /// Use `assume_init` if part of the buffer is known to be already initialized.
    #[inline]
    pub fn uninit(buf: &'a mut [MaybeUninit<u8>]) -> ReadBuf<'a> {
        ReadBuf {
            buf,
            filled: 0,
            initialized: 0,
        }
    }

    /// Returns the total capacity of the buffer.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.buf.len()
    }

    /// Returns a shared reference to the filled portion of the buffer.
    #[inline]
    pub fn filled(&self) -> &[u8] {
        let slice = &self.buf[..self.filled];
        // safety: filled describes how far into the buffer that the
        // user has filled with bytes, so it's been initialized.
        unsafe { slice_assume_init(slice) }
    }

    /// Returns a mutable reference to the filled portion of the buffer.
    #[inline]
    pub fn filled_mut(&mut self) -> &mut [u8] {
        let slice = &mut self.buf[..self.filled];
        // safety: filled describes how far into the buffer that the
        // user has filled with bytes, so it's been initialized.
        unsafe { slice_assume_init_mut(slice) }
    }

    /// Returns a new `ReadBuf` comprised of the unfilled section up to `n`.
    #[inline]
    pub fn take(&mut self, n: usize) -> ReadBuf<'_> {
        let max = std::cmp::min(self.remaining(), n);
        // Safety: We don't set any of the `unfilled_mut` with `MaybeUninit::uninit`.
        unsafe { ReadBuf::uninit(&mut self.unfilled_mut()[..max]) }
    }

    /// Returns a shared reference to the initialized portion of the buffer.
    ///
    /// This includes the filled portion.
    #[inline]
    pub fn initialized(&self) -> &[u8] {
        let slice = &self.buf[..self.initialized];
        // safety: initialized describes how far into the buffer that the
        // user has at some point initialized with bytes.
        unsafe { slice_assume_init(slice) }
    }

    /// Returns a mutable reference to the initialized portion of the buffer.
    ///
    /// This includes the filled portion.
    #[inline]
    pub fn initialized_mut(&mut self) -> &mut [u8] {
        let slice = &mut self.buf[..self.initialized];
        // safety: initialized describes how far into the buffer that the
        // user has at some point initialized with bytes.
        unsafe { slice_assume_init_mut(slice) }
    }

    /// Returns a mutable reference to the entire buffer, without ensuring that it has been fully
    /// initialized.
    ///
    /// The elements between 0 and `self.filled().len()` are filled, and those between 0 and
    /// `self.initialized().len()` are initialized (and so can be converted to a `&mut [u8]`).
    ///
    /// The caller of this method must ensure that these invariants are upheld. For example, if the
    /// caller initializes some of the uninitialized section of the buffer, it must call
    /// [`assume_init`](Self::assume_init) with the number of bytes initialized.
    ///
    /// # Safety
    ///
    /// The caller must not de-initialize portions of the buffer that have already been initialized.
    /// This includes any bytes in the region marked as uninitialized by `ReadBuf`.
    #[inline]
    pub unsafe fn inner_mut(&mut self) -> &mut [MaybeUninit<u8>] {
        self.buf
    }

    /// Returns a mutable reference to the unfilled part of the buffer without ensuring that it has been fully
    /// initialized.
    ///
    /// # Safety
    ///
    /// The caller must not de-initialize portions of the buffer that have already been initialized.
    /// This includes any bytes in the region marked as uninitialized by `ReadBuf`.
    #[inline]
    pub unsafe fn unfilled_mut(&mut self) -> &mut [MaybeUninit<u8>] {
        &mut self.buf[self.filled..]
    }

    /// Returns a mutable reference to the unfilled part of the buffer, ensuring it is fully initialized.
    ///
    /// Since `ReadBuf` tracks the region of the buffer that has been initialized, this is effectively "free" after
    /// the first use.
    #[inline]
    pub fn initialize_unfilled(&mut self) -> &mut [u8] {
        self.initialize_unfilled_to(self.remaining())
    }

    /// Returns a mutable reference to the first `n` bytes of the unfilled part of the buffer, ensuring it is
    /// fully initialized.
    ///
    /// # Panics
    ///
    /// Panics if `self.remaining()` is less than `n`.
    #[inline]
    #[track_caller]
    pub fn initialize_unfilled_to(&mut self, n: usize) -> &mut [u8] {
        assert!(self.remaining() >= n, "n overflows remaining");

        // This can't overflow, otherwise the assert above would have failed.
        let end = self.filled + n;

        if self.initialized < end {
            unsafe {
                self.buf[self.initialized..end]
                    .as_mut_ptr()
                    .write_bytes(0, end - self.initialized);
            }
            self.initialized = end;
        }

        let slice = &mut self.buf[self.filled..end];
        // safety: just above, we checked that the end of the buf has
        // been initialized to some value.
        unsafe { slice_assume_init_mut(slice) }
    }

    /// Returns the number of bytes at the end of the slice that have not yet been filled.
    #[inline]
    pub fn remaining(&self) -> usize {
        self.capacity() - self.filled
    }

    /// Clears the buffer, resetting the filled region to empty.
    ///
    /// The number of initialized bytes is not changed, and the contents of the buffer are not modified.
    #[inline]
    pub fn clear(&mut self) {
        self.filled = 0;
    }

    /// Advances the size of the filled region of the buffer.
    ///
    /// The number of initialized bytes is not changed.
    ///
    /// # Panics
    ///
    /// Panics if the filled region of the buffer would become larger than the initialized region.
    #[inline]
    #[track_caller]
    pub fn advance(&mut self, n: usize) {
        let new = self.filled.checked_add(n).expect("filled overflow");
        self.set_filled(new);
    }

    /// Sets the size of the filled region of the buffer.
    ///
    /// The number of initialized bytes is not changed.
    ///
    /// Note that this can be used to *shrink* the filled region of the buffer in addition to growing it (for
    /// example, by a `AsyncRead` implementation that compresses data in-place).
    ///
    /// # Panics
    ///
    /// Panics if the filled region of the buffer would become larger than the initialized region.
    #[inline]
    #[track_caller]
    pub fn set_filled(&mut self, n: usize) {
        assert!(
            n <= self.initialized,
            "filled must not become larger than initialized"
        );
        self.filled = n;
    }

    /// Asserts that the first `n` unfilled bytes of the buffer are initialized.
    ///
    /// `ReadBuf` assumes that bytes are never de-initialized, so this method does nothing when called with fewer
    /// bytes than are already known to be initialized.
    ///
    /// # Safety
    ///
    /// The caller must ensure that `n` unfilled bytes of the buffer have already been initialized.
    #[inline]
    pub unsafe fn assume_init(&mut self, n: usize) {
        let new = self.filled + n;
        if new > self.initialized {
            self.initialized = new;
        }
    }

    #[track_caller]
    pub fn push(&mut self, byte: u8) {
        assert!(
            self.remaining() >= 1,
            "read_buf is full ({} bytes)",
            self.capacity()
        );

        self.buf[self.filled] = MaybeUninit::new(byte);

        self.initialized = Ord::max(self.initialized, self.filled + 1);
        self.filled += 1;
    }

    /// Appends data to the buffer, advancing the written position and possibly also the initialized position.
    ///
    /// # Panics
    ///
    /// Panics if `self.remaining()` is less than `buf.len()`.
    #[inline(always)]
    #[track_caller]
    pub fn extend(&mut self, buf: &[u8]) {
        assert!(
            self.remaining() >= buf.len(),
            "buf.len() must fit in remaining()"
        );

        let amt = buf.len();
        // Cannot overflow, asserted above
        let end = self.filled + amt;

        //        // Safety: the length is asserted above
        //        unsafe {
        //            self.buf[self.filled..end]
        //                .as_mut_ptr()
        //                .cast::<u8>()
        //                .copy_from_nonoverlapping(buf.as_ptr(), amt);
        //        }

        let (it1, remainder) = slice_as_chunks::<_, 32>(buf);
        let (it2, _) = slice_as_chunks_mut::<_, 32>(&mut self.buf[self.filled..]);

        for (d, s) in it2.iter_mut().zip(it1.iter()) {
            use std::arch::x86_64::{_mm256_loadu_si256, _mm256_storeu_si256};
            unsafe {
                let chunk = _mm256_loadu_si256(s.as_ptr().cast());
                _mm256_storeu_si256(d.as_mut_ptr().cast(), chunk);
            }
        }

        unsafe {
            self.buf[self.filled + buf.len() - remainder.len()..]
                .as_mut_ptr()
                .cast::<u8>()
                .copy_from_nonoverlapping(remainder.as_ptr(), remainder.len());
        }

        if self.initialized < end {
            self.initialized = end;
        }
        self.filled = end;
    }

    #[inline(never)]
    pub fn copy_match(&mut self, offset_from_end: usize, length: usize) {
        let current = self.filled;

        let start = current.checked_sub(offset_from_end).expect("in bounds");
        let end = start.checked_add(length).expect("in bounds");

        // Note also that the referenced string may overlap the current
        // position; for example, if the last 2 bytes decoded have values
        // X and Y, a string reference with <length = 5, distance = 2>
        // adds X,Y,X,Y,X to the output stream.

        if end > self.filled {
            if offset_from_end == 1 {
                use std::arch::x86_64::{_mm256_set1_epi8, _mm256_storeu_si256};

                // this will just repeat this value many times
                let element = self.buf[current - 1];

                let b = unsafe { element.assume_init() };
                let chunk = unsafe { std::arch::x86_64::_mm256_set1_epi8(b as i8) };

                for d in self.buf[current..][..length].chunks_mut(32) {
                    unsafe {
                        _mm256_storeu_si256(d.as_mut_ptr().cast(), chunk);
                    }
                }
            } else {
                //                for i in 0..length {
                //                    self.buf[current + i] = self.buf[start + i];
                //                }

                unsafe { copy_many_chunked(self.buf, start, end, current) }
            }
        } else {
            //            let (before, after) = self.buf.split_at_mut(current);
            //
            //            for (s, d) in before[start..end].chunks(32).zip(after.chunks_mut(32)) {
            //                use std::arch::x86_64::{_mm256_loadu_si256, _mm256_storeu_si256};
            //
            //                unsafe {
            //                    let chunk = _mm256_loadu_si256(s.as_ptr().cast());
            //                    _mm256_storeu_si256(d.as_mut_ptr().cast(), chunk);
            //                }
            //            }

            unsafe { copy_once_chunked(self.buf, start, end, current) }
        }

        // safety: we just copied length initialized bytes right beyond self.filled
        unsafe { self.assume_init(length) };

        self.advance(length);
    }
}

#[inline(always)]
unsafe fn copy_once_chunked(
    buf: &mut [MaybeUninit<u8>],
    start: usize,
    end: usize,
    write_index: usize,
) {
    let mut s = buf.as_ptr().wrapping_add(start);
    let end_ptr = buf.as_ptr().wrapping_add(end);
    let mut d = buf.as_mut_ptr().wrapping_add(write_index);

    while s < end_ptr {
        use std::arch::x86_64::{_mm256_loadu_si256, _mm256_storeu_si256};
        unsafe {
            let chunk = _mm256_loadu_si256(s.cast());
            _mm256_storeu_si256(d.cast(), chunk);
        }

        s = s.wrapping_add(32);
        d = d.wrapping_add(32);
    }
}

unsafe fn copy_many_chunked(
    buf: &mut [MaybeUninit<u8>],
    start: usize,
    end: usize,
    mut write_index: usize,
) {
    let step = write_index - start;
    let input_end = write_index;

    loop {
        unsafe { copy_once_chunked(buf, start, input_end, write_index) }

        if write_index >= end {
            break;
        }

        write_index += step;
    }
}

#[test]
fn foobar() {
    let mut buf = [MaybeUninit::new(0); 64];

    buf[0] = MaybeUninit::new(b'x');
    buf[1] = MaybeUninit::new(b'y');

    let write_index = 2;
    let start = 0;
    let end = 5;

    unsafe { copy_many_chunked(&mut buf, start, end, write_index) };

    let buf = unsafe { buf.map(|v| v.assume_init()) };

    assert_eq!(&buf[..5], b"xyxyx")
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

fn slice_as_chunks_mut<T, const N: usize>(slice: &mut [T]) -> (&mut [[T; N]], &mut [T]) {
    assert!(N != 0, "chunk size must be non-zero");
    let len = slice.len() / N;
    let (multiple_of_n, remainder) = slice.split_at_mut(len * N);
    // SAFETY: We already panicked for zero, and ensured by construction
    // that the length of the subslice is a multiple of N.
    let array_slice: &mut [[T; N]] =
        unsafe { std::slice::from_raw_parts_mut(multiple_of_n.as_mut_ptr().cast(), len) };
    (array_slice, remainder)
}

impl std::io::Write for ReadBuf<'_> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if self.remaining() < buf.len() {
            const MSG: &str = "failed to write whole buffer";
            return Err(std::io::Error::new(std::io::ErrorKind::WriteZero, MSG));
        }

        self.extend(buf);

        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        /* do nothing */
        Ok(())
    }
}

impl fmt::Debug for ReadBuf<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ReadBuf")
            .field("filled", &self.filled)
            .field("initialized", &self.initialized)
            .field("capacity", &self.capacity())
            .finish()
    }
}

unsafe fn slice_to_uninit_mut(slice: &mut [u8]) -> &mut [MaybeUninit<u8>] {
    &mut *(slice as *mut [u8] as *mut [MaybeUninit<u8>])
}

// TODO: This could use `MaybeUninit::slice_assume_init` when it is stable.
unsafe fn slice_assume_init(slice: &[MaybeUninit<u8>]) -> &[u8] {
    &*(slice as *const [MaybeUninit<u8>] as *const [u8])
}

// TODO: This could use `MaybeUninit::slice_assume_init_mut` when it is stable.
unsafe fn slice_assume_init_mut(slice: &mut [MaybeUninit<u8>]) -> &mut [u8] {
    &mut *(slice as *mut [MaybeUninit<u8>] as *mut [u8])
}

trait ChunkSet {
    const N: usize = core::mem::size_of::<Self::Chunk>();

    type Chunk;

    unsafe fn memset_2(from: *const u8, chunk: &mut Self::Chunk);
    unsafe fn memset_4(from: *const u8, chunk: &mut Self::Chunk);
    unsafe fn memset_8(from: *const u8, chunk: &mut Self::Chunk);

    unsafe fn loadchunk(from: *const u8, chunk: &mut Self::Chunk);
    unsafe fn storechunk(out: *mut u8, chunk: &Self::Chunk);

    #[inline(always)]
    unsafe fn get_chunk_mag(buf: *const u8, chunk_rem: &mut usize, dist: usize) -> Self::Chunk {
        let mut bytes_remaining = Self::N;

        let mut chunk = MaybeUninit::zeroed().assume_init();
        let mut cur_chunk = &mut chunk as *mut Self::Chunk as *mut u8;

        while bytes_remaining > 0 {
            let cpy_dist = Ord::min(dist, bytes_remaining);
            std::ptr::copy_nonoverlapping(buf, cur_chunk, cpy_dist);
            bytes_remaining -= cpy_dist;
            cur_chunk = cur_chunk.add(cpy_dist);

            // saves an expensive integer division (somewhere)
            *chunk_rem = cpy_dist;
        }

        chunk
    }

    unsafe fn chunkcopy(mut out: *mut u8, mut from: *const u8, mut len: usize) -> *mut u8 {
        assert!(len > 0, "chunkcopy should never have a length 0");

        let mut chunk = MaybeUninit::zeroed().assume_init();

        let align = ((len - 1) % Self::N) + 1;
        Self::loadchunk(from, &mut chunk);
        Self::storechunk(out, &chunk);

        out = out.add(align);
        from = from.add(align);
        len -= align;

        while len > 0 {
            Self::loadchunk(from, &mut chunk);
            Self::storechunk(out, &chunk);

            out = out.add(Self::N);
            from = from.add(Self::N);
            len -= Self::N;
        }

        out
    }

    unsafe fn chunkunroll(mut out: *mut u8, dist: &mut usize, len: &mut usize) -> *mut u8 {
        let from = out.sub(*dist);
        let mut chunk = MaybeUninit::zeroed().assume_init();

        while *dist < *len && *dist < Self::N {
            Self::loadchunk(from, &mut chunk);
            Self::storechunk(out, &chunk);

            out = out.add(*dist);
            *len -= *dist;
            *dist += *dist;
        }

        out
    }

    unsafe fn chunkmemset(mut out: *mut u8, dist: usize, mut len: usize) -> *mut u8 {
        assert!(dist > 0, "chunkmemset cannot have a distance 0");

        let from = out.sub(dist);

        let mut chunk_load = MaybeUninit::zeroed().assume_init();
        let mut chunk_mod = 0;

        match dist {
            1 => {
                std::ptr::write_bytes(out, *from, len);
                return out.add(len);
            }
            _ if dist > Self::N => {
                return Self::chunkcopy(out, out.sub(dist), len);
            }
            2 => {
                Self::memset_2(from, &mut chunk_load);
            }
            4 => {
                Self::memset_4(from, &mut chunk_load);
            }
            8 => {
                Self::memset_8(from, &mut chunk_load);
            }
            _ if dist == Self::N => {
                Self::loadchunk(from, &mut chunk_load);
            }
            _ => {
                chunk_load = Self::get_chunk_mag(from, &mut chunk_mod, dist);
            }
        }

        if chunk_mod == 0 {
            while len >= 2 * Self::N {
                Self::storechunk(out, &chunk_load);
                Self::storechunk(out.add(Self::N), &chunk_load);
                out = out.add(2 * Self::N);
                len -= 2 * Self::N;
            }
        }

        let adv_amount = Self::N - chunk_mod;
        assert!(adv_amount != 0, "{:?}", (Self::N, chunk_mod));
        while len >= Self::N {
            Self::storechunk(out, &chunk_load);
            len -= adv_amount;
            out = out.add(adv_amount);
        }

        if len != 0 {
            std::ptr::copy_nonoverlapping(&chunk_load as *const _ as *const u8, out, len);
            out = out.add(len);
        }

        out
    }

    unsafe fn chunkmemset_safe(
        mut out: *mut u8,
        dist: usize,
        mut len: usize,
        mut left: usize,
    ) -> *mut u8 {
        // TODO unaligned optimizations?
        const ALIGN_MASK: usize = 7;

        len = Ord::min(len, left);
        let mut from = out.sub(dist);

        while (out as usize & ALIGN_MASK) != 0 && len > 0 {
            *out = *from;
            out = out.add(1);
            from = from.add(1);

            len -= 1;
            left -= 1;
        }

        if left < (3 * Self::N) {
            while len > 0 {
                *out = *from;
                out = out.add(1);
                from = from.add(1);
                len -= 1;
            }
            return out;
        }

        if len != 0 {
            return Self::chunkmemset(out, dist, len);
        }

        out
    }
}

struct Standard;

impl ChunkSet for Standard {
    type Chunk = u64;

    unsafe fn memset_2(from: *const u8, chunk: &mut Self::Chunk) {
        let [a, b]: [u8; 2] = std::ptr::read(from.cast());
        *chunk = u64::from_ne_bytes([a, b, 0, 0, 0, 0, 0, 0]);
    }

    unsafe fn memset_4(from: *const u8, chunk: &mut Self::Chunk) {
        let [a, b, c, d]: [u8; 4] = std::ptr::read(from.cast());
        *chunk = u64::from_ne_bytes([a, b, c, d, 0, 0, 0, 0]);
    }

    unsafe fn memset_8(from: *const u8, chunk: &mut Self::Chunk) {
        let tmp: [u8; 8] = std::ptr::read(from.cast());
        *chunk = u64::from_ne_bytes(tmp);
    }

    unsafe fn loadchunk(from: *const u8, chunk: &mut Self::Chunk) {
        let tmp: [u8; 8] = std::ptr::read(from.cast());
        *chunk = u64::from_ne_bytes(tmp);
    }

    unsafe fn storechunk(out: *mut u8, chunk: &Self::Chunk) {
        std::ptr::write(out as *mut [u8; 8], chunk.to_ne_bytes());
    }
}

struct Avx2;

impl ChunkSet for Avx2 {
    type Chunk = std::arch::x86_64::__m256i;

    unsafe fn memset_2(from: *const u8, chunk: &mut Self::Chunk) {
        let tmp: i16 = std::ptr::read_unaligned(from.cast());
        *chunk = std::arch::x86_64::_mm256_set1_epi16(tmp);
    }

    unsafe fn memset_4(from: *const u8, chunk: &mut Self::Chunk) {
        let tmp: i32 = std::ptr::read_unaligned(from.cast());
        *chunk = std::arch::x86_64::_mm256_set1_epi32(tmp);
    }

    unsafe fn memset_8(from: *const u8, chunk: &mut Self::Chunk) {
        let tmp: i64 = std::ptr::read_unaligned(from.cast());
        *chunk = std::arch::x86_64::_mm256_set1_epi64x(tmp);
    }

    unsafe fn loadchunk(from: *const u8, chunk: &mut Self::Chunk) {
        *chunk = std::arch::x86_64::_mm256_loadu_si256(from.cast());
    }

    unsafe fn storechunk(out: *mut u8, chunk: &Self::Chunk) {
        std::arch::x86_64::_mm256_storeu_si256(out as *mut Self::Chunk, *chunk);
    }
}
