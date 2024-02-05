use std::ffi::c_void;

mod adler32;
pub mod allocate;
mod c_api;
pub mod deflate;
#[cfg(test)]
mod dynamic;
pub mod inflate;

pub use c_api::*;
pub const INFLATE_STATE_SIZE: usize = core::mem::size_of::<crate::inflate::State>();

#[macro_export]
macro_rules! trace {
    ($($arg:tt)*) => {
        // eprint!($($arg)*)
    };
}

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
#[allow(unused)]
pub(crate) const ENOUGH: usize = ENOUGH_LENS + ENOUGH_DISTS;
pub(crate) const ENOUGH_LENS: usize = 1332;
pub(crate) const ENOUGH_DISTS: usize = 592;

/// initial adler-32 hash value
pub(crate) const ADLER32_INITIAL_VALUE: usize = 1;
/// initial crc-32 hash value
#[allow(unused)]
pub(crate) const CRC32_INITIAL_VALUE: usize = 0;

pub const MIN_WBITS: i32 = 8; // 256b LZ77 window
pub const MAX_WBITS: i32 = 15; // 32kb LZ77 window
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

#[cfg(any(test, feature = "__internal-fuzz"))]
pub fn uncompress_help(input: &[u8]) -> Result<Vec<u8>, ReturnCode> {
    let mut dest_vec = vec![0u8; Ord::max(1 << 16, 2 * input.len())];

    let mut dest_len = dest_vec.len();
    let dest = dest_vec.as_mut_ptr();

    let source = input.as_ptr();
    let source_len = input.len();

    let err = unsafe { libz_ng_sys::uncompress(dest, &mut dest_len, source, source_len) };

    if err != 0 {
        Err(ReturnCode::from(err))
    } else {
        dest_vec.truncate(dest_len);

        Ok(dest_vec)
    }
}

#[cfg(any(debug_assertions, feature = "__internal-fuzz"))]
pub fn compress_rs(
    dest: &mut [u8],
    dest_len: &mut usize,
    source: &[u8],
    //
    level: i32,
) -> ReturnCode {
    const VERSION: *const libc::c_char = "2.3.0\0".as_ptr() as *const libc::c_char;
    const STREAM_SIZE: libc::c_int = std::mem::size_of::<z_stream>() as libc::c_int;

    let mut stream = z_stream {
        next_in: source.as_ptr() as *mut u8,
        avail_in: 0, // for special logic in the first  iteration
        total_in: 0,
        next_out: dest.as_mut_ptr(),
        avail_out: 0, // for special logic on the first iteration
        total_out: 0,
        msg: std::ptr::null_mut(),
        state: std::ptr::null_mut(),
        zalloc: Some(allocate::zcalloc),
        zfree: Some(allocate::zcfree),
        opaque: std::ptr::null_mut(),
        data_type: 0,
        adler: 0,
        reserved: 0,
    };

    let method = Z_DEFLATED;
    let window_bits = 15;
    let mem_level = 8;
    let strategy = Z_DEFAULT_STRATEGY;

    let err = {
        let strm: *mut z_stream = &mut stream;
        unsafe {
            deflateInit2_(
                strm,
                level,
                method,
                window_bits,
                mem_level,
                strategy,
                VERSION,
                STREAM_SIZE,
            )
        }
    };

    if ReturnCode::from(err) != ReturnCode::Ok as _ {
        return ReturnCode::from(err);
    }

    let max = libc::c_uint::MAX as usize;

    let mut left = dest.len();
    let mut source_len = source.len();

    loop {
        if stream.avail_out == 0 {
            stream.avail_out = Ord::min(left, max) as _;
            left -= stream.avail_out as usize;
        }

        if stream.avail_in == 0 {
            stream.avail_in = Ord::min(source_len, max) as _;
            source_len -= stream.avail_in as usize;
        }

        let flush = if source_len > 0 {
            Flush::NoFlush
        } else {
            Flush::Finish
        };

        let err = unsafe { deflate(&mut stream, flush as i32) };
        if ReturnCode::from(err) != ReturnCode::Ok {
            break;
        }
    }

    *dest_len = stream.total_out as _;

    unsafe { deflateEnd(&mut stream) };

    ReturnCode::Ok
}

#[cfg(any(test, feature = "__internal-fuzz"))]
pub fn compress_ng(
    dest: &mut [u8],
    dest_len: &mut usize,
    source: &[u8],
    //
    level: i32,
) -> ReturnCode {
    use libz_ng_sys::{deflate, deflateEnd, deflateInit2_, z_stream};

    const VERSION: *const libc::c_char = "2.3.0\0".as_ptr() as *const libc::c_char;
    const STREAM_SIZE: libc::c_int = std::mem::size_of::<z_stream>() as libc::c_int;

    let mut stream = z_stream {
        next_in: source.as_ptr() as *mut u8,
        avail_in: 0, // for special logic in the first  iteration
        total_in: 0,
        next_out: dest.as_mut_ptr(),
        avail_out: 0, // for special logic on the first iteration
        total_out: 0,
        msg: std::ptr::null_mut(),
        state: std::ptr::null_mut(),
        zalloc: allocate::zcalloc,
        zfree: allocate::zcfree,
        opaque: std::ptr::null_mut(),
        data_type: 0,
        adler: 0,
        reserved: 0,
    };

    let method = Z_DEFLATED;
    let window_bits = 15;
    let mem_level = 8;
    let strategy = Z_DEFAULT_STRATEGY;

    let err = {
        let strm: *mut z_stream = &mut stream;
        unsafe {
            deflateInit2_(
                strm,
                level,
                method,
                window_bits,
                mem_level,
                strategy,
                VERSION,
                STREAM_SIZE,
            )
        }
    };

    if ReturnCode::from(err) != ReturnCode::Ok as _ {
        return ReturnCode::from(err);
    }

    let max = libc::c_uint::MAX as usize;

    let mut left = dest.len();
    let mut source_len = source.len();

    loop {
        if stream.avail_out == 0 {
            stream.avail_out = Ord::min(left, max) as _;
            left -= stream.avail_out as usize;
        }

        if stream.avail_in == 0 {
            stream.avail_in = Ord::min(source_len, max) as _;
            source_len -= stream.avail_in as usize;
        }

        let flush = if source_len > 0 {
            Flush::NoFlush
        } else {
            Flush::Finish
        };

        let err = unsafe { deflate(&mut stream, flush as i32) };
        if ReturnCode::from(err) != ReturnCode::Ok {
            break;
        }
    }

    *dest_len = stream.total_out as _;

    unsafe { deflateEnd(&mut stream) };

    ReturnCode::Ok
}
