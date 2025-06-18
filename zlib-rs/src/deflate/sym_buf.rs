// taken from https://docs.rs/tokio/latest/src/tokio/io/read_buf.rs.html#23-27
// based on https://rust-lang.github.io/rfcs/2930-read-buf.html

use crate::allocate::Allocator;
use crate::weak_slice::WeakSliceMut;

pub(crate) struct SymBuf<'a> {
    buf: WeakSliceMut<'a, u8>,
    filled: usize,
}

impl<'a> SymBuf<'a> {
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (u16, u8)> + '_ {
        self.buf.as_slice()[..self.filled]
            .chunks_exact(3)
            .map(|chunk| match *chunk {
                [dist_low, dist_high, lc] => (u16::from_le_bytes([dist_low, dist_high]), lc),
                _ => unreachable!("chunks are exactly 3 elements wide"),
            })
    }

    #[inline]
    pub fn should_flush_block(&self) -> bool {
        self.filled == self.buf.len() - 3
    }

    /// Returns true if there are no bytes in this ReadBuf
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.filled == 0
    }

    /// Clears the buffer, resetting the filled region to empty.
    ///
    /// The number of initialized bytes is not changed, and the contents of the buffer are not modified.
    #[inline]
    pub fn clear(&mut self) {
        self.buf.as_mut_slice().fill(0);
        self.filled = 0;
    }

    #[inline(always)]
    pub fn push_lit(&mut self, byte: u8) {
        // NOTE: we rely on the buffer being zeroed here!
        self.buf.as_mut_slice()[self.filled + 2] = byte;

        self.filled += 3;
    }

    #[inline(always)]
    pub fn push_dist(&mut self, dist: u16, len: u8) {
        let buf = &mut self.buf.as_mut_slice()[self.filled..][..3];
        let [dist1, dist2] = dist.to_le_bytes();

        buf[0] = dist1;
        buf[1] = dist2;
        buf[2] = len;

        self.filled += 3;
    }

    pub(crate) fn new_in(alloc: &Allocator<'a>, len: usize) -> Option<Self> {
        let ptr = alloc.allocate_zeroed_buffer(len * 3)?;

        // safety: all elements are now initialized
        let buf = unsafe { WeakSliceMut::from_raw_parts_mut(ptr.as_ptr(), len * 3) };

        Some(Self { buf, filled: 0 })
    }

    pub(crate) fn clone_in(&self, alloc: &Allocator<'a>) -> Option<Self> {
        let mut clone = Self::new_in(alloc, self.buf.len() / 3)?;

        clone
            .buf
            .as_mut_slice()
            .copy_from_slice(self.buf.as_slice());
        clone.filled = self.filled;

        Some(clone)
    }

    pub(crate) unsafe fn drop_in(&mut self, alloc: &Allocator<'a>) {
        if !self.buf.is_empty() {
            let mut buf = core::mem::replace(&mut self.buf, WeakSliceMut::empty());
            unsafe { alloc.deallocate(buf.as_mut_ptr(), buf.len()) }
        }
    }
}
