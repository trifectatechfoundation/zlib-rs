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

    /// Returns the length of the filled part of the buffer
    #[inline]
    pub fn len(&self) -> usize {
        self.filled
    }

    /// Returns true if there are no bytes in this ReadBuf
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.filled == 0
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
    /// The elements between 0 and `self.len()` are filled, and those between 0 and
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

    #[inline(always)]
    pub fn copy_match(&mut self, offset_from_end: usize, length: usize) {
        let current = self.filled;
        // println!("({current}, {offset_from_end}, {length}),");

        if false && current < (1 << 15) - 300 {
            let mut f = std::fs::File::options()
                .write(true)
                .create(true)
                .append(true)
                .open("/tmp/copy_match.dat")
                .unwrap();

            use std::io::Write;
            f.write_all(&current.to_ne_bytes());
            f.write_all(&offset_from_end.to_ne_bytes());
            f.write_all(&length.to_ne_bytes());
        }

        #[cfg(target_arch = "x86_64")]
        if std::is_x86_feature_detected!("avx2") {
            return self.copy_match_avx2(offset_from_end, length);
        }

        return self.copy_match_generic(offset_from_end, length);
    }

    pub fn copy_match_generic(&mut self, offset_from_end: usize, length: usize) {
        let current = self.filled;

        let start = current.checked_sub(offset_from_end).expect("in bounds");
        let end = start.checked_add(length).expect("in bounds");

        if end > current {
            if offset_from_end == 1 {
                // this will just repeat this value many times
                let element = self.buf[current - 1];
                self.buf[current..][..length].fill(element);
            } else {
                for i in 0..length {
                    self.buf[current + i] = self.buf[start + i];
                }
            }
        } else {
            self.buf.copy_within(start..end, current);
        }

        // safety: we just copied length initialized bytes right beyond self.filled
        unsafe { self.assume_init(length) };

        self.advance(length);
    }

    #[cfg(target_arch = "x86_64")]
    pub fn copy_match_sse(&mut self, offset_from_end: usize, length: usize) {
        let current = self.filled;

        let start = current.checked_sub(offset_from_end).expect("in bounds");
        let end = start.checked_add(length).expect("in bounds");

        let safe_to_chunk = (current + length).next_multiple_of(16) <= self.buf.len();

        if end > self.filled {
            if offset_from_end == 1 {
                use std::arch::x86_64::{_mm_set1_epi8, _mm_storeu_si128};

                // this will just repeat this value many times
                let element = self.buf[current - 1];
                let b = unsafe { element.assume_init() };

                if safe_to_chunk {
                    let chunk = unsafe { std::arch::x86_64::_mm_set1_epi8(b as i8) };
                    for d in self.buf[current..][..length].chunks_mut(16) {
                        unsafe {
                            _mm_storeu_si128(d.as_mut_ptr().cast(), chunk);
                        }
                    }
                } else {
                    self.buf[current..][..length].fill(element);
                }
            } else {
                for i in 0..length {
                    self.buf[current + i] = self.buf[start + i];
                }
            }
        } else {
            let (before, after) = self.buf.split_at_mut(current);

            if safe_to_chunk {
                for (s, d) in before[start..end].chunks(16).zip(after.chunks_mut(16)) {
                    use std::arch::x86_64::{_mm_loadu_si128, _mm_storeu_si128};

                    unsafe {
                        let chunk = _mm_loadu_si128(s.as_ptr().cast());
                        _mm_storeu_si128(d.as_mut_ptr().cast(), chunk);
                    }
                }
            } else {
                // a full simd copy does not fit in the output buffer
                self.buf.copy_within(start..end, current);
            }
        }

        // safety: we just copied length initialized bytes right beyond self.filled
        unsafe { self.assume_init(length) };

        self.advance(length);
    }

    #[cfg(target_arch = "x86_64")]
    pub fn copy_match_avx2(&mut self, offset_from_end: usize, length: usize) {
        let current = self.filled;

        let start = current.checked_sub(offset_from_end).expect("in bounds");
        let end = start.checked_add(length).expect("in bounds");

        let safe_to_chunk = (current + length).next_multiple_of(32) <= self.buf.len();

        if end > self.filled {
            if offset_from_end == 1 {
                // this will just repeat this value many times
                let element = self.buf[current - 1];
                let b = unsafe { element.assume_init() };

                self.buf[current..][..length].fill(element);
            } else {
                for i in 0..length {
                    self.buf[current + i] = self.buf[start + i];
                }
            }
        } else {
            Avx2::copy_chunk(self.buf, current, start, end)
        }

        // safety: we just copied length initialized bytes right beyond self.filled
        unsafe { self.assume_init(length) };

        self.advance(length);
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

trait ChunkCopy {
    const N: usize = core::mem::size_of::<Self::Chunk>();

    type Chunk;

    /// Safety: must be valid to read a `Self::Chunk` value from `from` with an unaligned read.
    unsafe fn load_chunk(from: *const MaybeUninit<u8>) -> Self::Chunk;

    /// Safety: must be valid to write a `Self::Chunk` value to `out` with an unaligned write.
    unsafe fn store_chunk(out: *mut MaybeUninit<u8>, chunk: Self::Chunk);

    #[inline(always)]
    fn copy_chunk(buf: &mut [MaybeUninit<u8>], current: usize, start: usize, end: usize) {
        let (before, after) = buf.split_at_mut(current);

        if (end - start).next_multiple_of(32) <= after.len() {
            let src = &before[start..end];
            let dst = after;
            unsafe { Self::copy_chunk_unchecked(src, dst) }
        } else {
            // a full simd copy does not fit in the output buffer
            buf.copy_within(start..end, current);
        }
    }

    /// # Safety
    ///
    /// - src.len().div_ceil(Self::N) >= dst.div_ceil(Self::N)
    #[inline(always)]
    unsafe fn copy_chunk_unchecked(src: &[MaybeUninit<u8>], dst: &mut [MaybeUninit<u8>]) {
        // if this condition is false, the final simd write will go out of bounds
        debug_assert!(src.len().div_ceil(Self::N) >= dst.len().div_ceil(Self::N));

        for (s, d) in src.chunks(Self::N).zip(dst.chunks_mut(Self::N)) {
            let chunk = Self::load_chunk(s.as_ptr());
            Self::store_chunk(d.as_mut_ptr(), chunk);
        }
    }
}

struct Generic;

impl ChunkCopy for Generic {
    type Chunk = u64;

    unsafe fn load_chunk(from: *const MaybeUninit<u8>) -> Self::Chunk {
        std::ptr::read_unaligned(from.cast())
    }

    unsafe fn store_chunk(out: *mut MaybeUninit<u8>, chunk: Self::Chunk) {
        std::ptr::copy_nonoverlapping(chunk.to_ne_bytes().as_ptr().cast(), out, Self::N)
    }
}

struct Avx2;

impl ChunkCopy for Avx2 {
    type Chunk = core::arch::x86_64::__m256i;

    #[inline(always)]
    unsafe fn load_chunk(from: *const MaybeUninit<u8>) -> Self::Chunk {
        core::arch::x86_64::_mm256_loadu_si256(from.cast())
    }

    #[inline(always)]
    unsafe fn store_chunk(out: *mut MaybeUninit<u8>, chunk: Self::Chunk) {
        core::arch::x86_64::_mm256_storeu_si256(out as *mut Self::Chunk, chunk);
    }
}
