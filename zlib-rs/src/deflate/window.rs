use crate::{allocate::Allocator, weak_slice::WeakSliceMut};

#[derive(Debug)]
pub struct Window<'a> {
    // the full window allocation. This is longer than w_size so that operations don't need to
    // perform bounds checks.
    buf: WeakSliceMut<'a, u8>,

    // number of initialized bytes
    filled: usize,

    window_bits: usize,

    high_water: usize,
}

impl<'a> Window<'a> {
    pub fn new_in(alloc: &Allocator<'a>, window_bits: usize) -> Option<Self> {
        let len = 2 * ((1 << window_bits) + Self::padding());
        let ptr = alloc.allocate_zeroed(len)?;
        // SAFETY: freshly allocated buffer
        let buf = unsafe { WeakSliceMut::from_raw_parts_mut(ptr, len) };

        Some(Self {
            buf,
            filled: len,
            window_bits,
            high_water: len,
        })
    }

    pub fn clone_in(&self, alloc: &Allocator<'a>) -> Option<Self> {
        let mut clone = Self::new_in(alloc, self.window_bits)?;

        clone
            .buf
            .as_mut_slice()
            .copy_from_slice(self.buf.as_slice());
        clone.filled = self.filled;
        clone.high_water = self.high_water;

        Some(clone)
    }

    /// # Safety
    ///
    /// [`Self`] must not be used after calling this function.
    pub unsafe fn drop_in(&mut self, alloc: &Allocator) {
        if !self.buf.is_empty() {
            let mut buf = core::mem::replace(&mut self.buf, WeakSliceMut::empty());
            unsafe { alloc.deallocate(buf.as_mut_ptr(), buf.len()) };
        }
    }

    pub fn capacity(&self) -> usize {
        2 * (1 << self.window_bits)
    }

    /// Returns a shared reference to the filled portion of the buffer.
    #[inline]
    pub fn filled(&self) -> &[u8] {
        // SAFETY: `self.buf` has been initialized for at least `filled` elements
        unsafe { core::slice::from_raw_parts(self.buf.as_ptr().cast(), self.filled) }
    }

    /// Returns a mutable reference to the filled portion of the buffer.
    #[inline]
    pub fn filled_mut(&mut self) -> &mut [u8] {
        // SAFETY: `self.buf` has been initialized for at least `filled` elements
        unsafe { core::slice::from_raw_parts_mut(self.buf.as_mut_ptr().cast(), self.filled) }
    }

    /// # Safety
    ///
    /// `src` must point to `range.end - range.start` valid (initialized!) bytes
    pub unsafe fn copy_and_initialize(&mut self, range: core::ops::Range<usize>, src: *const u8) {
        let (start, end) = (range.start, range.end);

        let dst = self.buf.as_mut_slice()[range].as_mut_ptr() as *mut u8;
        unsafe { core::ptr::copy_nonoverlapping(src, dst, end - start) };

        if start >= self.filled {
            self.filled = Ord::max(self.filled, end);
        }

        self.high_water = Ord::max(self.high_water, self.filled);
    }

    // this library has many functions that operated in a chunked fashion on memory. For
    // performance, we want to minimize bounds checks. Therefore we reserve initialize some extra
    // memory at the end of the window so that chunked operations can use the whole buffer. If they
    // go slightly over `self.capacity` that's okay, we account for that here by making sure the
    // memory there is initialized!
    pub fn initialize_out_of_bounds(&mut self) {}

    pub fn initialize_at_least(&mut self, at_least: usize) {}

    // padding required so that SIMD operations going out-of-bounds are not a problem
    pub fn padding() -> usize {
        if crate::cpu_features::is_enabled_pclmulqdq() {
            8
        } else {
            0
        }
    }
}
