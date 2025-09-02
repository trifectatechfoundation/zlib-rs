// taken from https://docs.rs/tokio/latest/src/tokio/io/read_buf.rs.html#23-27
// based on https://rust-lang.github.io/rfcs/2930-read-buf.html

use crate::allocate::Allocator;
use crate::weak_slice::WeakSliceMut;

#[derive(Clone, Copy, Default)]
#[repr(C)]
pub(crate) struct Symbol {
    pub len: u8,
    _padding: u8,
    pub dist: u16,
}

pub(crate) struct SymBuf<'a> {
    buf: WeakSliceMut<'a, Symbol>,
    filled: usize,
}

impl<'a> SymBuf<'a> {
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = Symbol> + '_ {
        self.buf.as_slice()[..self.filled].iter().copied()
    }

    #[inline]
    pub fn should_flush_block(&self) -> bool {
        self.filled == self.buf.len() - 1
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
        self.buf.as_mut_slice().fill(Symbol::default());
        self.filled = 0;
    }

    #[inline(always)]
    pub fn push_lit(&mut self, len: u8) {
        // NOTE: we rely on the value being zeroed here!
        self.buf.as_mut_slice()[self.filled] = Symbol {
            len,
            ..Symbol::default()
        };

        self.filled += 1;
    }

    #[inline(always)]
    pub fn push_dist(&mut self, dist: u16, len: u8) {
        self.buf.as_mut_slice()[self.filled] = Symbol {
            len,
            dist,
            ..Symbol::default()
        };

        self.filled += 1;
    }

    pub(crate) fn new_in(alloc: &Allocator<'a>, len: usize) -> Option<Self> {
        let ptr = alloc.allocate_zeroed_buffer(len * 4)?;

        // safety: all elements are now initialized
        let buf = unsafe { WeakSliceMut::from_raw_parts_mut(ptr.as_ptr().cast(), len) };

        Some(Self { buf, filled: 0 })
    }

    pub(crate) fn clone_in(&self, alloc: &Allocator<'a>) -> Option<Self> {
        let mut clone = Self::new_in(alloc, self.buf.len())?;

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
