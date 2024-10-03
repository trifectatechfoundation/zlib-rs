use core::{marker::PhantomData, mem::MaybeUninit};

use crate::allocate::Allocator;

pub struct Pending<'a> {
    /// start of the allocation
    buf: &'a mut [MaybeUninit<u8>],
    /// next pending byte to output to the stream
    out: usize,
    /// number of bytes in the pending buffer
    pub(crate) pending: usize,
    /// semantically we're storing a mutable slice of bytes
    _marker: PhantomData<&'a mut [u8]>,
}

impl<'a> Pending<'a> {
    pub fn reset_keep(&mut self) {
        // keep the buffer as it is
        self.pending = 0;
    }

    pub fn pending(&self) -> &[u8] {
        let slice = &self.buf[self.out..][..self.pending];
        unsafe { core::mem::transmute(slice) }
    }

    /// Number of bytes that can be added to the pending buffer until it is full
    pub(crate) fn remaining(&self) -> usize {
        self.buf.len() - (self.out + self.pending)
    }

    /// Total number of bytes that can be stored in the pending buffer
    pub(crate) fn capacity(&self) -> usize {
        self.buf.len()
    }

    #[inline(always)]
    #[track_caller]
    /// Mark a number of pending bytes as no longer pending
    pub fn advance(&mut self, number_of_bytes: usize) {
        debug_assert!(self.pending >= number_of_bytes);

        self.out = self.out.wrapping_add(number_of_bytes);
        self.pending -= number_of_bytes;

        if self.pending == 0 {
            self.out = 0;
        }
    }

    #[inline(always)]
    #[track_caller]
    pub fn rewind(&mut self, n: usize) {
        assert!(n <= self.pending, "rewinding past then start");

        self.pending -= n;

        if self.pending == 0 {
            self.out = 0;
        }
    }

    #[inline(always)]
    #[track_caller]
    pub fn extend(&mut self, buf: &[u8]) {
        assert!(
            self.remaining() >= buf.len(),
            "buf.len() must fit in remaining()"
        );

        let buf: &[MaybeUninit<u8>] = unsafe { core::mem::transmute(buf) };

        self.buf[self.out + self.pending..][..buf.len()].copy_from_slice(buf);

        self.pending += buf.len();
    }

    pub(crate) fn new_in(alloc: &Allocator<'a>, len: usize) -> Option<Self> {
        let slice = alloc.allocate_slice::<u8>(len)?;

        Some(Self {
            buf: slice,
            out: 0,
            pending: 0,
            _marker: PhantomData,
        })
    }

    pub(crate) fn clone_in(&self, alloc: &Allocator<'a>) -> Option<Self> {
        let mut clone = Self::new_in(alloc, self.buf.len())?;

        clone.buf.copy_from_slice(self.buf);
        clone.out = self.out;
        clone.pending = self.pending;

        Some(clone)
    }

    pub(crate) unsafe fn drop_in(&mut self, alloc: &Allocator) {
        alloc.deallocate(self.buf.as_mut_ptr(), self.buf.len());
    }
}
