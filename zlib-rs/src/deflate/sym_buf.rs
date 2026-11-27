// taken from https://docs.rs/tokio/latest/src/tokio/io/read_buf.rs.html#23-27
// based on https://rust-lang.github.io/rfcs/2930-read-buf.html

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

    /// Zeroes the buffer, initializing its storage and resetting the filled region to empty.
    #[inline]
    pub fn clear(&mut self) {
        // The first clear runs before the backing storage has been initialized.
        unsafe { self.buf.as_mut_ptr().write_bytes(0, self.buf.len()) };
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

        // Write both dist bytes as a single 2-byte store. This avoids the
        // `movb %ch, [mem]` instruction pattern (store from high-byte register
        // alias) that LLVM otherwise emits when dist arrives as a wide register.
        // That pattern triggers the Intel Raptor Lake CPU errata, causing silent
        // 2-byte stores that corrupt the adjacent `len` byte.
        // SAFETY: buf has at least 3 bytes (guaranteed above), so the unaligned
        // 2-byte write at offset 0 stays within bounds.
        unsafe {
            core::ptr::write_unaligned(buf.as_mut_ptr().cast::<[u8; 2]>(), dist.to_le_bytes())
        };
        buf[2] = len;

        self.filled += 3;
    }

    pub(crate) unsafe fn from_raw_parts(ptr: *mut u8, lit_bufsize: usize) -> Self {
        Self {
            buf: unsafe { WeakSliceMut::from_raw_parts_mut(ptr, lit_bufsize * 3) },
            filled: 0,
        }
    }

    pub(crate) unsafe fn clone_to(&self, ptr: *mut u8) -> Self {
        unsafe { ptr.copy_from_nonoverlapping(self.buf.as_ptr(), self.buf.len()) };
        Self {
            buf: unsafe { WeakSliceMut::from_raw_parts_mut(ptr, self.buf.len()) },
            filled: self.filled,
        }
    }
}

#[cfg(test)]
mod tests {
    use core::mem::MaybeUninit;

    use super::*;

    #[test]
    fn clear_initializes_uninitialized_symbol_storage() {
        for lit_bufsize in [4, 128, 32768] {
            let len = lit_bufsize * 3;
            let mut storage = vec![MaybeUninit::<u8>::uninit(); len + 2];
            storage[0].write(0xa5);
            storage[len + 1].write(0xa5);
            let mut symbols =
                unsafe { SymBuf::from_raw_parts(storage.as_mut_ptr().add(1).cast(), lit_bufsize) };

            symbols.clear();
            assert!(symbols.is_empty());
            assert!(symbols.buf.as_slice().iter().all(|&byte| byte == 0));
            symbols.push_dist(257, 7);
            symbols.push_lit(b'A');
            assert!(symbols.iter().eq([(257, 7), (0, b'A')]));

            symbols.clear();
            assert!(symbols.is_empty());
            assert!(symbols.iter().next().is_none());
            symbols.push_lit(b'B');
            assert!(symbols.iter().eq([(0, b'B')]));

            assert_eq!(unsafe { storage[0].assume_init() }, 0xa5);
            assert_eq!(unsafe { storage[len + 1].assume_init() }, 0xa5);
        }
    }
}
