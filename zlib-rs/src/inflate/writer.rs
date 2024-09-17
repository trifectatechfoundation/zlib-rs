use core::fmt;
use core::mem::MaybeUninit;

pub struct Writer<'a> {
    buf: &'a mut [MaybeUninit<u8>],
    filled: usize,
}

impl<'a> Writer<'a> {
    /// Creates a new `Writer` from a fully initialized buffer.
    #[inline]
    pub fn new(buf: &'a mut [u8]) -> Writer<'a> {
        Self::new_uninit(unsafe { slice_to_uninit_mut(buf) })
    }

    /// Creates a new `Writer` from an uninitialized buffer.
    #[inline]
    pub fn new_uninit(buf: &'a mut [MaybeUninit<u8>]) -> Writer<'a> {
        Writer { buf, filled: 0 }
    }

    /// Pointer to where the next byte will be written
    #[inline]
    pub fn next_out(&mut self) -> *mut MaybeUninit<u8> {
        self.buf.as_mut_ptr().wrapping_add(self.filled)
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

    /// Returns a shared reference to the filled portion of the buffer.
    #[inline]
    pub fn filled(&self) -> &[u8] {
        unsafe { core::slice::from_raw_parts(self.buf.as_ptr().cast(), self.filled) }
    }

    /// Returns the number of bytes at the end of the slice that have not yet been filled.
    #[inline]
    pub fn remaining(&self) -> usize {
        self.capacity() - self.filled
    }

    pub fn push(&mut self, byte: u8) {
        self.buf[self.filled] = MaybeUninit::new(byte);

        self.filled += 1;
    }

    /// Appends data to the buffer
    #[inline(always)]
    pub fn extend(&mut self, buf: &[u8]) {
        // using simd here (on x86_64) was not fruitful
        self.buf[self.filled..][..buf.len()].copy_from_slice(slice_to_uninit(buf));

        self.filled += buf.len();
    }

    #[inline(always)]
    pub fn copy_match(&mut self, offset_from_end: usize, length: usize) {
        #[cfg(target_arch = "x86_64")]
        if crate::cpu_features::is_enabled_avx512() {
            return self.copy_match_help::<core::arch::x86_64::__m512i>(offset_from_end, length);
        }

        #[cfg(target_arch = "x86_64")]
        if crate::cpu_features::is_enabled_avx2() {
            return self.copy_match_help::<core::arch::x86_64::__m256i>(offset_from_end, length);
        }

        #[cfg(target_arch = "x86_64")]
        if crate::cpu_features::is_enabled_sse() {
            return self.copy_match_help::<core::arch::x86_64::__m128i>(offset_from_end, length);
        }

        self.copy_match_help::<u64>(offset_from_end, length)
    }

    #[inline(always)]
    fn copy_match_help<C: Chunk>(&mut self, offset_from_end: usize, length: usize) {
        assert!(self.filled + length <= self.capacity());

        let current = self.filled;

        let start = current.checked_sub(offset_from_end).expect("in bounds");
        let end = start.checked_add(length).expect("in bounds");

        // Note also that the referenced string may overlap the current
        // position; for example, if the last 2 bytes decoded have values
        // X and Y, a string reference with <length = 5, distance = 2>
        // adds X,Y,X,Y,X to the output stream.

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
            Self::copy_chunked_within::<C>(self.buf, current, start, end)
        }

        self.filled += length
    }

    #[inline(always)]
    fn copy_chunked_within<C: Chunk>(
        buf: &mut [MaybeUninit<u8>],
        current: usize,
        start: usize,
        end: usize,
    ) {
        if (end - start).next_multiple_of(core::mem::size_of::<C>()) <= (buf.len() - current) {
            unsafe {
                Self::copy_chunk_unchecked::<C>(
                    buf.as_ptr().add(start),
                    buf.as_mut_ptr().add(current),
                    buf.as_ptr().add(end),
                )
            }
        } else {
            // a full simd copy does not fit in the output buffer
            buf.copy_within(start..end, current);
        }
    }

    /// # Safety
    ///
    /// `src` must be safe to perform unaligned reads in `core::mem::size_of::<C>()` chunks until
    /// `end` is reached. `dst` must be safe to (unalingned) write that number of chunks.
    #[inline(always)]
    unsafe fn copy_chunk_unchecked<C: Chunk>(
        mut src: *const MaybeUninit<u8>,
        mut dst: *mut MaybeUninit<u8>,
        end: *const MaybeUninit<u8>,
    ) {
        let chunk = C::load_chunk(src);
        C::store_chunk(dst, chunk);

        src = src.add(core::mem::size_of::<C>());
        dst = dst.add(core::mem::size_of::<C>());

        while src < end {
            let chunk = C::load_chunk(src);
            C::store_chunk(dst, chunk);

            src = src.add(core::mem::size_of::<C>());
            dst = dst.add(core::mem::size_of::<C>());
        }
    }
}

impl fmt::Debug for Writer<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Writer")
            .field("filled", &self.filled)
            .field("capacity", &self.capacity())
            .finish()
    }
}

fn slice_to_uninit(slice: &[u8]) -> &[MaybeUninit<u8>] {
    unsafe { &*(slice as *const [u8] as *const [MaybeUninit<u8>]) }
}

unsafe fn slice_to_uninit_mut(slice: &mut [u8]) -> &mut [MaybeUninit<u8>] {
    &mut *(slice as *mut [u8] as *mut [MaybeUninit<u8>])
}

trait Chunk {
    /// Safety: must be valid to read a `Self::Chunk` value from `from` with an unaligned read.
    unsafe fn load_chunk(from: *const MaybeUninit<u8>) -> Self;

    /// Safety: must be valid to write a `Self::Chunk` value to `out` with an unaligned write.
    unsafe fn store_chunk(out: *mut MaybeUninit<u8>, chunk: Self);
}

impl Chunk for u64 {
    unsafe fn load_chunk(from: *const MaybeUninit<u8>) -> Self {
        u64::to_le(core::ptr::read_unaligned(from.cast()))
    }

    unsafe fn store_chunk(out: *mut MaybeUninit<u8>, chunk: Self) {
        core::ptr::copy_nonoverlapping(
            chunk.to_le_bytes().as_ptr().cast(),
            out,
            core::mem::size_of::<Self>(),
        )
    }
}

#[cfg(target_arch = "x86_64")]
impl Chunk for core::arch::x86_64::__m128i {
    #[inline(always)]
    unsafe fn load_chunk(from: *const MaybeUninit<u8>) -> Self {
        core::arch::x86_64::_mm_loadu_si128(from.cast())
    }

    #[inline(always)]
    unsafe fn store_chunk(out: *mut MaybeUninit<u8>, chunk: Self) {
        core::arch::x86_64::_mm_storeu_si128(out as *mut Self, chunk);
    }
}

#[cfg(target_arch = "x86_64")]
impl Chunk for core::arch::x86_64::__m256i {
    #[inline(always)]
    unsafe fn load_chunk(from: *const MaybeUninit<u8>) -> Self {
        core::arch::x86_64::_mm256_loadu_si256(from.cast())
    }

    #[inline(always)]
    unsafe fn store_chunk(out: *mut MaybeUninit<u8>, chunk: Self) {
        core::arch::x86_64::_mm256_storeu_si256(out as *mut Self, chunk);
    }
}

#[cfg(target_arch = "x86_64")]
impl Chunk for core::arch::x86_64::__m512i {
    #[inline(always)]
    unsafe fn load_chunk(from: *const MaybeUninit<u8>) -> Self {
        // TODO AVX-512 is effectively unstable.
        // We cross our fingers that LLVM optimizes this into a vmovdqu32
        //
        // https://www.intel.com/content/www/us/en/docs/intrinsics-guide/index.html#text=_mm512_loadu_si512&expand=3420&ig_expand=4110
        core::ptr::read_unaligned(from.cast())
    }

    #[inline(always)]
    unsafe fn store_chunk(out: *mut MaybeUninit<u8>, chunk: Self) {
        // TODO AVX-512 is effectively unstable.
        // We cross our fingers that LLVM optimizes this into a vmovdqu32
        //
        // https://www.intel.com/content/www/us/en/docs/intrinsics-guide/index.html#text=_mm512_storeu_si512&expand=3420&ig_expand=4110,6550
        core::ptr::write_unaligned(out.cast(), chunk)
    }
}
