use std::ffi::{c_int, c_ulong, c_void};

mod adler32;
pub mod allocate;
mod bitreader;
mod c_api;
#[cfg(test)]
mod dynamic;
mod inffixed_tbl;
pub mod inflate;
mod inftrees;
mod read_buf;
mod window;

pub use c_api::*;
pub const INFLATE_STATE_SIZE: usize = core::mem::size_of::<crate::inflate::State>();

/// Maximum size of the dynamic table.  The maximum number of code structures is
/// 1924, which is the sum of 1332 for literal/length codes and 592 for distance
/// codes.  These values were found by exhaustive searches using the program
/// examples/enough.c found in the zlib distributions.  The arguments to that
/// program are the number of symbols, the initial root table size, and the
/// maximum bit length of a code.  "enough 286 10 15" for literal/length codes
/// returns 1332, and "enough 30 9 15" for distance codes returns 592.
/// The initial root table size (10 or 9) is found in the fifth argument of the
/// inflate_table() calls in inflate.c and infback.c.  If the root table size is
/// changed, then these maximum sizes would be need to be recalculated and
/// updated.
pub(crate) const ENOUGH: usize = ENOUGH_LENS + ENOUGH_DISTS;
pub(crate) const ENOUGH_LENS: usize = 1332;
pub(crate) const ENOUGH_DISTS: usize = 592;

/// initial adler-32 hash value
pub(crate) const ADLER32_INITIAL_VALUE: usize = 1;
/// initial crc-32 hash value
pub(crate) const CRC32_INITIAL_VALUE: usize = 0;

pub(crate) const MIN_WBITS: i32 = 8; // 256b LZ77 window
pub(crate) const MAX_WBITS: i32 = 15; // 32kb LZ77 window
pub(crate) const DEF_WBITS: i32 = MAX_WBITS;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Flush {
    #[default]
    NoFlush = 0,
    PartialFlush = 1,
    SyncFlush = 2,
    FullFlush = 3,
    Finish = 4,
    Block = 5,
    Trees = 6,
}

impl TryFrom<i32> for Flush {
    type Error = ();

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Flush::NoFlush),
            1 => Ok(Flush::PartialFlush),
            2 => Ok(Flush::SyncFlush),
            3 => Ok(Flush::FullFlush),
            4 => Ok(Flush::Finish),
            5 => Ok(Flush::Block),
            6 => Ok(Flush::Trees),
            _ => Err(()),
        }
    }
}

impl z_stream {
    unsafe fn alloc_layout(&self, layout: std::alloc::Layout) -> *mut c_void {
        match self.zalloc {
            None => unreachable!("zalloc no initialized"),
            Some(f) => f(self.opaque, 1, layout.size() as u32),
        }
    }

    unsafe fn alloc<T>(&self) -> *mut T {
        self.alloc_layout(std::alloc::Layout::new::<T>()).cast()
    }

    pub(crate) unsafe fn alloc_value<T>(&self, value: T) -> *mut T {
        let ptr = self.alloc_layout(std::alloc::Layout::new::<T>()).cast();

        if ptr as usize != 0 {
            std::ptr::write(ptr, value);
        }

        ptr
    }

    unsafe fn dealloc<T>(&self, ptr: *mut T) {
        match self.zfree {
            None => unreachable!("zfree no initialized"),
            Some(f) => f(self.opaque, ptr.cast()),
        }
    }

    fn inflate_state_check(strm: *const z_stream) -> bool {
        if strm.is_null() {
            return true;
        }

        let strm = unsafe { &*strm };

        if strm.zalloc.is_none() || strm.zfree.is_none() {
            return true;
        }

        if strm.state as usize == 0 {
            return true;
        }

        // TODO check that the strm.state  == state.strm

        // TODO check state.mode?

        false
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Code {
    /// operation, extra bits, table bits
    pub op: u8,
    /// bits in this part of the code
    pub bits: u8,
    /// offset in table or code value
    pub val: u16,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(i32)]
pub enum ReturnCode {
    Ok = 0,
    StreamEnd = 1,
    NeedDict = 2,
    ErrNo = -1,
    StreamError = -2,
    DataError = -3,
    MemError = -4,
    BufError = -5,
    VersionError = -6,
}

impl From<i32> for ReturnCode {
    fn from(value: i32) -> Self {
        use ReturnCode::*;

        match value {
            0 => Ok,
            1 => StreamEnd,
            2 => NeedDict,
            -1 => ErrNo,
            -2 => StreamError,
            -3 => DataError,
            -4 => MemError,
            -5 => BufError,
            -6 => VersionError,
            _ => panic!("invalid return code {value}"),
        }
    }
}
