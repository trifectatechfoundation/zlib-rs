use std::mem::MaybeUninit;

#[derive(Debug)]
pub struct Window<'a> {
    // the full window allocation. This is longer than w_size so that operations don't need to
    // perform bounds checks.
    buf: &'a mut [MaybeUninit<u8>],

    // number of initialized bytes
    filled: usize,

    // same as 2 * (1 << window_bits)
    capacity: usize,

    high_water: usize,
}

impl<'a> Window<'a> {
    pub unsafe fn from_raw_parts(data: *mut MaybeUninit<u8>, len: usize, window_bits: i32) -> Self {
        let buf = std::slice::from_raw_parts_mut(data, len);

        Self {
            buf,
            filled: 0,
            capacity: 2 * (1 << window_bits),
            high_water: 0,
        }
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

    pub fn capacity(&self) -> usize {
        self.buf.len()
    }

    pub fn as_mut_ptr(&mut self) -> *mut u8 {
        self.buf.as_mut_ptr() as *mut u8
    }

    /// # Safety
    ///
    /// `src` must point to `range.end - range.start` valid (initialized!) bytes
    pub unsafe fn copy_and_initialize(&mut self, range: std::ops::Range<usize>, src: *const u8) {
        let (start, end) = (range.start, range.end);

        let dst = self.buf[range].as_mut_ptr() as *mut u8;
        std::ptr::copy_nonoverlapping(src, dst, end - start);

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
    pub fn initialize_out_of_bounds(&mut self) {
        const WIN_INIT: usize = crate::deflate::STD_MAX_MATCH;

        // If the WIN_INIT bytes after the end of the current data have never been
        // written, then zero those bytes in order to avoid memory check reports of
        // the use of uninitialized (or uninitialised as Julian writes) bytes by
        // the longest match routines.  Update the high water mark for the next
        // time through here.  WIN_INIT is set to STD_MAX_MATCH since the longest match
        // routines allow scanning to strstart + STD_MAX_MATCH, ignoring lookahead.
        if self.high_water < self.capacity {
            let curr = self.filled().len();

            if self.high_water < curr {
                // Previous high water mark below current data -- zero WIN_INIT
                // bytes or up to end of window, whichever is less.
                let init = Ord::min(self.capacity - curr, WIN_INIT);

                self.buf[curr..][..init].fill(MaybeUninit::new(0));

                self.high_water = curr + init;

                self.filled += init;
            } else if self.high_water < curr + WIN_INIT {
                // High water mark at or above current data, but below current data
                // plus WIN_INIT -- zero out to current data plus WIN_INIT, or up
                // to end of window, whichever is less.
                let init = Ord::min(
                    curr + WIN_INIT - self.high_water,
                    self.capacity - self.high_water,
                );

                self.buf[self.high_water..][..init].fill(MaybeUninit::new(0));

                self.high_water += init;
                self.filled += init;
            }
        }
    }

    pub fn initialize_at_least(&mut self, at_least: usize) {
        let end = at_least.clamp(self.high_water, self.buf.len());
        self.buf[self.high_water..end].fill(MaybeUninit::new(0));

        self.high_water = end;
        self.filled = end;
    }
}

// TODO: This could use `MaybeUninit::slice_assume_init` when it is stable.
unsafe fn slice_assume_init(slice: &[MaybeUninit<u8>]) -> &[u8] {
    &*(slice as *const [MaybeUninit<u8>] as *const [u8])
}

// TODO: This could use `MaybeUninit::slice_assume_init_mut` when it is stable.
unsafe fn slice_assume_init_mut(slice: &mut [MaybeUninit<u8>]) -> &mut [u8] {
    &mut *(slice as *mut [MaybeUninit<u8>] as *mut [u8])
}
