use crate::{
    adler32::{adler32, adler32_fold_copy},
    allocate::Allocator,
    crc32::Crc32Fold,
};

// translation guide:
//
// wsize -> buf.capacity()
// wnext -> buf.ptr
// whave -> buf.filled.len()
#[derive(Debug)]
pub struct Window<'a> {
    buf: &'a mut [u8],

    have: usize, // number of bytes logically written to the window. this can be higher than
    // buf.len() if we run out of space in the window
    next: usize, // write head
}

impl<'a> Window<'a> {
    pub fn into_inner(self) -> &'a mut [u8] {
        self.buf
    }

    pub fn is_empty(&self) -> bool {
        self.size() == 0
    }

    pub fn size(&self) -> usize {
        if self.buf.is_empty() {
            // an empty `buf` is used when the window has not yet been allocated,
            // or when it has been deallocated.
            0
        } else {
            self.buf.len() - Self::padding()
        }
    }

    /// number of bytes in the window. Saturates at `Self::capacity`.
    pub fn have(&self) -> usize {
        self.have
    }

    /// Position where the next byte will be written
    pub fn next(&self) -> usize {
        self.next
    }

    pub fn empty() -> Self {
        Self {
            buf: &mut [],
            have: 0,
            next: 0,
        }
    }

    pub fn clear(&mut self) {
        self.have = 0;
        self.next = 0;
    }

    pub fn as_slice(&self) -> &[u8] {
        &self.buf[..self.have]
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.buf.as_ptr()
    }

    #[cfg(test)]
    fn extend_adler32(&mut self, slice: &[u8], checksum: &mut u32) {
        self.extend(slice, 0, true, checksum, &mut Crc32Fold::new());
    }

    pub fn extend(
        &mut self,
        slice: &[u8],
        flags: i32,
        update_checksum: bool,
        checksum: &mut u32,
        crc_fold: &mut Crc32Fold,
    ) {
        let len = slice.len();
        let wsize = self.size();

        if len >= wsize {
            // We have to split the checksum over non-copied and copied bytes
            let pos = len.saturating_sub(self.size());
            let (non_window_slice, window_slice) = slice.split_at(pos);

            if update_checksum {
                if flags != 0 {
                    crc_fold.fold(non_window_slice, 0);
                    crc_fold.fold_copy(&mut self.buf[..wsize], window_slice);
                } else {
                    *checksum = adler32(*checksum, non_window_slice);
                    *checksum = adler32_fold_copy(*checksum, self.buf, window_slice);
                }
            } else {
                self.buf[..wsize].copy_from_slice(window_slice);
            }

            self.next = 0;
            self.have = self.size();
        } else {
            let dist = Ord::min(wsize - self.next, slice.len());

            // the end part goes onto the end of the window. The start part wraps around and is
            // written to the start of the window.
            let (end_part, start_part) = slice.split_at(dist);

            if update_checksum {
                let dst = &mut self.buf[self.next..][..end_part.len()];
                if flags != 0 {
                    crc_fold.fold_copy(dst, end_part);
                } else {
                    *checksum = adler32_fold_copy(*checksum, dst, end_part);
                }
            } else {
                self.buf[self.next..][..end_part.len()].copy_from_slice(end_part);
            }

            if !start_part.is_empty() {
                let dst = &mut self.buf[..start_part.len()];

                if update_checksum {
                    if flags != 0 {
                        crc_fold.fold_copy(dst, start_part);
                    } else {
                        *checksum = adler32_fold_copy(*checksum, dst, start_part);
                    }
                } else {
                    dst.copy_from_slice(start_part);
                }

                self.next = start_part.len();
                self.have = self.size();
            } else {
                self.next += dist;
                if self.next == self.size() {
                    self.next = 0;
                }
                if self.have < self.size() {
                    self.have += dist;
                }
            }
        }
    }

    pub fn new_in(alloc: &Allocator<'a>, window_bits: usize) -> Option<Self> {
        let len = (1 << window_bits) + Self::padding();
        let ptr = alloc.allocate_zeroed(len);

        if ptr.is_null() {
            return None;
        }

        let buf = unsafe { core::slice::from_raw_parts_mut(ptr, len) };

        Some(Self {
            buf,
            have: 0,
            next: 0,
        })
    }

    pub fn clone_in(&self, alloc: &Allocator<'a>) -> Option<Self> {
        let len = self.buf.len();
        let ptr = alloc.allocate_zeroed(len);

        if ptr.is_null() {
            return None;
        }

        let buf = unsafe { core::slice::from_raw_parts_mut(ptr, len) };

        Some(Self {
            buf,
            have: self.have,
            next: self.next,
        })
    }

    // padding required so that SIMD operations going out-of-bounds are not a problem
    pub fn padding() -> usize {
        64 // very conservative
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::allocate::Allocator;

    fn init_window(window_bits_log2: usize) -> Window<'static> {
        let mut window = Window::new_in(&Allocator::RUST, window_bits_log2).unwrap();
        window.have = 0;
        window.next = 0;
        window
    }

    #[test]
    fn extend_in_bounds() {
        let mut checksum = 0;

        let mut window = init_window(4);

        window.extend_adler32(&[1; 5], &mut checksum);
        assert_eq!(window.have, 5);
        assert_eq!(window.next, 5);

        let slice = &window.buf[..window.size()];
        assert_eq!(&[1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], slice);

        window.extend_adler32(&[2; 7], &mut checksum);
        assert_eq!(window.have, 12);
        assert_eq!(window.next, 12);

        let slice = &window.buf[..window.size()];
        assert_eq!(&[1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0], slice);

        assert_eq!(checksum, 6946835);

        unsafe { Allocator::RUST.deallocate(window.buf.as_mut_ptr(), window.buf.len()) }
    }

    #[test]
    fn extend_crosses_bounds() {
        let mut checksum = 0;

        let mut window = init_window(2);

        window.extend_adler32(&[1; 3], &mut checksum);
        assert_eq!(window.have, 3);
        assert_eq!(window.next, 3);

        let slice = &window.buf[..window.size()];
        assert_eq!(&[1, 1, 1, 0], slice);

        window.extend_adler32(&[2; 3], &mut checksum);
        assert_eq!(window.have, 4);
        assert_eq!(window.next, 2);

        let slice = &window.buf[..window.size()];
        assert_eq!(&[2, 2, 1, 2], slice);

        assert_eq!(checksum, 1769481);

        unsafe { Allocator::RUST.deallocate(window.buf.as_mut_ptr(), window.buf.len()) }
    }

    #[test]
    fn extend_out_of_bounds() {
        let mut checksum = 0;

        let mut window = init_window(3);

        // adds 9 numbers, that won't fit into a window of size 8
        window.extend_adler32(&[1, 2, 3, 4, 5, 6, 7, 8, 9], &mut checksum);
        assert_eq!(window.have, 8);
        assert_eq!(window.next, 0);

        let slice = &window.buf[..window.size()];
        assert_eq!(&[2, 3, 4, 5, 6, 7, 8, 9], slice);

        assert_eq!(checksum, 10813485);

        unsafe { Allocator::RUST.deallocate(window.buf.as_mut_ptr(), window.buf.len()) }
    }
}
