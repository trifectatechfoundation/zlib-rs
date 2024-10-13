// taken from https://docs.rs/tokio/latest/src/tokio/io/read_buf.rs.html#23-27
// based on https://rust-lang.github.io/rfcs/2930-read-buf.html
use core::fmt;
use core::mem::MaybeUninit;

use crate::allocate::Allocator;

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

    /// Pointer to the start of the `ReadBuf`
    #[inline]
    pub fn as_mut_ptr(&mut self) -> *mut MaybeUninit<u8> {
        self.buf.as_mut_ptr()
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

    /// Returns the number of bytes at the end of the slice that have not yet been filled.
    #[inline]
    fn remaining(&self) -> usize {
        self.capacity() - self.filled
    }

    /// Clears the buffer, resetting the filled region to empty.
    ///
    /// The number of initialized bytes is not changed, and the contents of the buffer are not modified.
    #[inline]
    pub fn clear(&mut self) {
        self.filled = 0;
    }

    fn push_symbol(&mut self, a: u8, b: u8, c: u8) {
        assert!(
            self.remaining() >= 3,
            "read_buf is full ({} bytes)",
            self.capacity()
        );

        self.buf[self.filled] = MaybeUninit::new(a);
        self.buf[self.filled + 1] = MaybeUninit::new(b);
        self.buf[self.filled + 2] = MaybeUninit::new(c);

        self.initialized = Ord::max(self.initialized, self.filled + 3);
        self.filled += 3;
    }

    pub fn push_lit(&mut self, byte: u8) {
        self.push_symbol(0, 0, byte)
    }

    pub fn push_dist(&mut self, dist: u16, len: u8) {
        let [dist1, dist2] = dist.to_le_bytes();
        self.push_symbol(dist1, dist2, len)
    }

    pub(crate) fn new_in(alloc: &Allocator<'a>, len: usize) -> Option<Self> {
        let buf = alloc.allocate_slice::<u8>(len)?;

        Some(Self {
            buf,
            filled: 0,
            initialized: 0,
        })
    }

    pub(crate) fn clone_in(&self, alloc: &Allocator<'a>) -> Option<Self> {
        let mut clone = Self::new_in(alloc, self.buf.len())?;

        clone.buf.copy_from_slice(self.buf);
        clone.filled = self.filled;
        clone.initialized = self.initialized;

        Some(clone)
    }

    pub(crate) unsafe fn drop_in(&mut self, alloc: &Allocator<'a>) {
        if !self.buf.is_empty() {
            let buf = core::mem::take(&mut self.buf);
            alloc.deallocate(buf.as_mut_ptr(), buf.len());
        }
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
