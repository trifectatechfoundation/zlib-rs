use std::marker::PhantomData;

pub struct Pending<'a> {
    buf: *mut u8,
    out: *mut u8,
    pub(crate) pending: usize,
    end: *mut u8,
    _marker: PhantomData<&'a mut [u8]>,
}

impl<'a> Pending<'a> {
    pub fn from_raw_parts(data: *mut u8, len: usize) -> Self {
        Self {
            buf: data,
            out: data,
            pending: 0,
            end: data.wrapping_add(len),
            _marker: PhantomData,
        }
    }

    pub fn as_mut_ptr(&mut self) -> *mut u8 {
        self.buf
    }

    pub fn reset_keep(&mut self) {
        // keep the buffer as it is
        self.pending = 0;
    }

    pub fn pending(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.out, self.pending) }
    }

    // in practice, pending uses more than lit_bufsize bytes and therefore runs into sym_buf
    // that is annoying, because we somehow need to make that safe ...
    //
    //    fn remaining(&self) -> usize {
    //        self.end as usize - self.out as usize
    //    }

    pub(crate) fn capacity(&self) -> usize {
        self.end as usize - self.buf as usize
    }

    #[inline(always)]
    #[track_caller]
    pub fn advance(&mut self, n: usize) {
        // assert!(n <= self.remaining(), "advancing past the end");
        debug_assert!(self.pending >= n);

        self.out = self.out.wrapping_add(n);
        self.pending -= n;

        if self.pending == 0 {
            self.out = self.buf;
        }
    }

    #[inline(always)]
    #[track_caller]
    pub fn rewind(&mut self, n: usize) {
        assert!(n <= self.pending, "rewinding past then start");

        self.pending -= n;
    }

    #[inline(always)]
    #[track_caller]
    pub fn extend(&mut self, buf: &[u8]) {
        // assert!( self.remaining() >= buf.len(), "buf.len() must fit in remaining()");

        unsafe {
            std::ptr::copy_nonoverlapping(buf.as_ptr(), self.out.add(self.pending), buf.len());
        }

        self.pending += buf.len();
    }
}
