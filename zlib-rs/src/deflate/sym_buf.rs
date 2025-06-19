// taken from https://docs.rs/tokio/latest/src/tokio/io/read_buf.rs.html#23-27
// based on https://rust-lang.github.io/rfcs/2930-read-buf.html

use core::marker::PhantomData;

use crate::allocate::Allocator;

pub(crate) struct SymBuf<'a> {
    ptr: *mut u8,
    sym_next: usize,
    sym_end: usize,
    _marker: PhantomData<&'a mut [u8]>,
}

impl<'a> SymBuf<'a> {
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (u16, u8)> + '_ {
        let slice = unsafe { core::slice::from_raw_parts(self.ptr, self.sym_next) };
        slice.chunks_exact(3).map(|chunk| match *chunk {
            [dist_low, dist_high, lc] => (u16::from_le_bytes([dist_low, dist_high]), lc),
            _ => unreachable!("chunks are exactly 3 elements wide"),
        })
    }

    #[inline]
    pub fn should_flush_block(&self) -> bool {
        self.sym_next == self.sym_end
    }

    /// Returns true if there are no bytes in this ReadBuf
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.sym_next == 0
    }

    /// Clears the buffer, resetting the filled region to empty.
    ///
    /// The number of initialized bytes is not changed, and the contents of the buffer are not modified.
    #[inline]
    pub fn clear(&mut self) {
        unsafe { core::ptr::write_bytes(self.ptr, 0, self.sym_next) };
        self.sym_next = 0;
    }

    #[inline(always)]
    pub fn push_lit(&mut self, byte: u8) {
        // NOTE: we rely on the buffer being zeroed here!
        unsafe { self.ptr.add(self.sym_next + 2).write(byte) };

        self.sym_next += 3;
    }

    #[inline(always)]
    pub fn push_dist(&mut self, dist: u16, len: u8) {
        let [dist1, dist2] = dist.to_le_bytes();

        unsafe {
            let ptr = self.ptr.add(self.sym_next);
            ptr.add(0).write(dist1);
            ptr.add(1).write(dist2);
            ptr.add(2).write(len);
        }

        self.sym_next += 3;
    }

    pub(crate) fn new_in(alloc: &Allocator<'a>, len: usize) -> Option<Self> {
        let ptr = alloc.allocate_zeroed_buffer(len * 3)?;

        Some(Self {
            ptr: ptr.as_ptr(),
            sym_end: len * 3 - 3,
            sym_next: 0,
            _marker: PhantomData,
        })
    }

    pub(crate) fn clone_in(&self, alloc: &Allocator<'a>) -> Option<Self> {
        let mut clone = Self::new_in(alloc, self.sym_end + 3)?;

        unsafe { core::ptr::copy_nonoverlapping(self.ptr, clone.ptr, self.sym_next) };

        clone.sym_next = self.sym_next;
        clone.sym_end = self.sym_end;

        Some(clone)
    }

    pub(crate) unsafe fn drop_in(&mut self, alloc: &Allocator<'a>) {
        if !self.ptr.is_null() {
            let buf = core::mem::replace(&mut self.ptr, core::ptr::null_mut());
            unsafe { alloc.deallocate(buf, self.sym_end + 3) }
        }
    }
}
