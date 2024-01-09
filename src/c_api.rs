#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(clippy::missing_safety_doc)] // obviously needs to be fixed long-term

use libc::{c_char, c_int, c_uchar, c_uint, c_ulong, c_void};

use crate::{deflate::DeflateStream, inflate::InflateStream, ReturnCode};

pub type alloc_func = unsafe extern "C" fn(voidpf, uInt, uInt) -> voidpf;
pub type Bytef = u8;
pub type free_func = unsafe extern "C" fn(voidpf, voidpf);
pub type in_func = unsafe extern "C" fn(*mut c_void, *mut *const c_uchar) -> c_uint;
pub type out_func = unsafe extern "C" fn(*mut c_void, *mut c_uchar, c_uint) -> c_int;
pub type uInt = c_uint;
pub type uLong = c_ulong;
pub type uLongf = c_ulong;
pub type voidp = *mut c_void;
pub type voidpc = *const c_void;
pub type voidpf = *mut c_void;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct z_stream {
    pub next_in: *mut Bytef,
    pub avail_in: uInt,
    pub total_in: z_size,
    pub next_out: *mut Bytef,
    pub avail_out: uInt,
    pub total_out: z_size,
    pub msg: *mut c_char,
    pub state: *mut internal_state,
    pub zalloc: Option<alloc_func>,
    pub zfree: Option<free_func>,
    pub opaque: voidpf,
    pub data_type: c_int,
    pub adler: z_checksum,
    pub reserved: uLong,
}
pub type z_streamp = *mut z_stream;

impl Default for z_stream {
    fn default() -> Self {
        Self {
            next_in: std::ptr::null_mut(),
            avail_in: 0,
            total_in: 0,
            next_out: std::ptr::null_mut(),
            avail_out: 0,
            total_out: 0,
            msg: std::ptr::null_mut(),
            state: std::ptr::null_mut(),
            zalloc: Some(crate::allocate::zcalloc),
            zfree: Some(crate::allocate::zcfree),
            opaque: std::ptr::null_mut(),
            data_type: 0,
            adler: 0,
            reserved: 0,
        }
    }
}

// // zlib stores Adler-32 and CRC-32 checksums in unsigned long; zlib-ng uses uint32_t.
pub(crate) type z_size = c_ulong;
pub(crate) type z_checksum = c_ulong;

// opaque to the user
pub enum internal_state {}

pub const Z_NO_FLUSH: c_int = 0;
pub const Z_PARTIAL_FLUSH: c_int = 1;
pub const Z_SYNC_FLUSH: c_int = 2;
pub const Z_FULL_FLUSH: c_int = 3;
pub const Z_FINISH: c_int = 4;
pub const Z_BLOCK: c_int = 5;
pub const Z_TREES: c_int = 6;

pub const Z_OK: c_int = 0;
pub const Z_STREAM_END: c_int = 1;
pub const Z_NEED_DICT: c_int = 2;
pub const Z_ERRNO: c_int = -1;
pub const Z_STREAM_ERROR: c_int = -2;
pub const Z_DATA_ERROR: c_int = -3;
pub const Z_MEM_ERROR: c_int = -4;
pub const Z_BUF_ERROR: c_int = -5;
pub const Z_VERSION_ERROR: c_int = -6;

/// Inflates `source` into `dest`, and writes the final inflated size into `destLen`.
///
/// # Safety
///
/// Behavior is undefined if any of the following conditions are violated:
///
/// - `source` must be [valid](https://doc.rust-lang.org/std/ptr/index.html#safety) for reads for
/// `sourceLen` bytes. The entity of `source` must be contained in one allocated object!
/// - `source` must point to `sourceLen` consecutive properly initialized values of type `u8`.
/// - `dest` must be [valid](https://doc.rust-lang.org/std/ptr/index.html#safety) for reads for
/// `*destLen` bytes. The entity of `source` must be contained in one allocated object!
/// - `dest` must point to `*destLen` consecutive properly initialized values of type `u8`.
/// - while this function runs, both read and write actions to the `source` and `dest` memory
/// ranges are forbidden
pub unsafe extern "C" fn uncompress(
    dest: *mut u8,
    destLen: *mut c_ulong,
    source: *const u8,
    sourceLen: c_ulong,
) -> c_int {
    crate::inflate::uncompress(dest, destLen, source, sourceLen)
}

pub unsafe extern "C" fn inflate(strm: *mut z_stream, flush: i32) -> i32 {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        let flush = crate::Flush::try_from(flush).unwrap_or_default();
        crate::inflate::inflate(stream, flush) as _
    } else {
        ReturnCode::StreamError as _
    }
}

pub unsafe extern "C" fn inflateEnd(strm: *mut z_stream) -> i32 {
    crate::inflate::end(strm)
}

pub unsafe extern "C" fn inflateBackInit_(
    _strm: z_streamp,
    _windowBits: c_int,
    _window: *mut c_uchar,
    _version: *const c_char,
    _stream_size: c_int,
) -> c_int {
    todo!("inflateBack is not implemented yet")
}

pub unsafe extern "C" fn inflateBack(
    _strm: z_streamp,
    _in: in_func,
    _in_desc: *mut c_void,
    _out: out_func,
    _out_desc: *mut c_void,
) -> c_int {
    todo!("inflateBack is not implemented yet")
}

pub unsafe extern "C" fn inflateBackEnd(_strm: z_streamp) -> c_int {
    todo!("inflateBack is not implemented yet")
}

pub unsafe extern "C" fn inflateCopy(dest: *mut z_stream, source: *const z_stream) -> i32 {
    if let Some(source) = InflateStream::from_stream_ref(source) {
        crate::inflate::copy(dest, source) as _
    } else {
        ReturnCode::StreamError as _
    }
}

pub unsafe extern "C" fn inflateMark(strm: *const z_stream) -> libc::c_long {
    if let Some(stream) = InflateStream::from_stream_ref(strm) {
        crate::inflate::mark(stream)
    } else {
        libc::c_long::MIN
    }
}

pub unsafe extern "C" fn inflateSync(strm: *mut z_stream) -> i32 {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        crate::inflate::sync(stream) as _
    } else {
        ReturnCode::StreamError as _
    }
}

// undocumented
pub unsafe extern "C" fn inflateSyncPoint(strm: *mut z_stream) -> i32 {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        crate::inflate::sync_point(stream) as i32
    } else {
        ReturnCode::StreamError as _
    }
}

pub unsafe extern "C" fn inflateInit_(
    strm: z_streamp,
    _version: *const c_char,
    _stream_size: c_int,
) -> c_int {
    crate::inflate::init(strm)
}

pub unsafe extern "C" fn inflateInit2_(
    strm: z_streamp,
    windowBits: c_int,
    _version: *const c_char,
    _stream_size: c_int,
) -> c_int {
    crate::inflate::init2(strm, windowBits)
}

pub unsafe extern "C" fn inflatePrime(strm: *mut z_stream, bits: i32, value: i32) -> i32 {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        crate::inflate::prime(stream, bits, value) as _
    } else {
        ReturnCode::StreamError as _
    }
}

pub unsafe extern "C" fn inflateReset(strm: *mut z_stream) -> i32 {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        crate::inflate::reset(stream) as _
    } else {
        ReturnCode::StreamError as _
    }
}

pub unsafe extern "C" fn inflateReset2(strm: *mut z_stream, windowBits: c_int) -> i32 {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        crate::inflate::reset2(stream, windowBits) as _
    } else {
        ReturnCode::StreamError as _
    }
}

pub unsafe extern "C" fn inflateSetDictionary(
    strm: *mut z_stream,
    dictionary: *const u8,
    dictLength: libc::c_uint,
) -> libc::c_int {
    let Some(stream) = InflateStream::from_stream_mut(strm) else {
        return ReturnCode::StreamError as _;
    };

    let dict = if dictLength == 0 || dictionary.is_null() {
        &[]
    } else {
        unsafe { std::slice::from_raw_parts(dictionary, dictLength as usize) }
    };

    crate::inflate::set_dictionary(stream, dict) as _
}

// pub unsafe extern "C" fn inflateGetHeader(strm: z_streamp, head: gz_headerp) -> c_int {
//     todo!("part of gzip support")
// }

// undocumented but exposed function
pub unsafe extern "C" fn inflateUndermine(strm: *mut z_stream, subvert: i32) -> libc::c_int {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        crate::inflate::undermine(stream, subvert) as i32
    } else {
        ReturnCode::StreamError as _
    }
}

// undocumented but exposed function
pub unsafe extern "C" fn inflateResetKeep(strm: *mut z_stream) -> i32 {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        crate::inflate::reset_keep(stream) as _
    } else {
        ReturnCode::StreamError as _
    }
}

// undocumented but exposed function
pub unsafe extern "C" fn inflateCodesUsed(_strm: *mut z_stream) -> c_ulong {
    todo!()
}

pub unsafe extern "C" fn deflate(strm: *mut z_stream, flush: i32) -> i32 {
    if let Some(stream) = DeflateStream::from_stream_mut(strm) {
        match crate::Flush::try_from(flush) {
            Ok(flush) => crate::deflate::deflate(stream, flush) as _,
            Err(()) => ReturnCode::StreamError as _,
        }
    } else {
        ReturnCode::StreamError as _
    }
}

pub unsafe extern "C" fn compress(
    dest: *mut Bytef,
    destLen: *mut c_ulong,
    source: *const Bytef,
    sourceLen: c_ulong,
) -> c_int {
    let data = dest;
    let len = std::ptr::read(destLen) as usize;
    let output = std::slice::from_raw_parts_mut(data, len);

    let data = source;
    let len = sourceLen as usize;
    let input = std::slice::from_raw_parts(data, len);

    let (output, err) = crate::deflate::compress(output, input);

    std::ptr::write(destLen, output.len() as _);

    err as c_int
}
