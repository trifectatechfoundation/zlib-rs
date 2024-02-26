#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(clippy::missing_safety_doc)] // obviously needs to be fixed long-term

#[cfg(test)]
mod tests;

use std::mem::MaybeUninit;

use std::ffi::{c_char, c_int, c_long, c_uchar, c_uint, c_ulong, c_void};

use zlib_rs::{
    deflate::{DeflateConfig, DeflateStream, Method, Strategy},
    inflate::{InflateConfig, InflateStream},
    Flush, ReturnCode,
};

pub use zlib_rs::c_api::*;

pub unsafe extern "C" fn crc32(crc: c_ulong, buf: *const Bytef, len: uInt) -> c_ulong {
    let buf = unsafe { std::slice::from_raw_parts(buf, len as usize) };
    zlib_rs::crc32(crc as u32, buf) as c_ulong
}

pub unsafe extern "C" fn crc32_combine(crc1: c_ulong, crc2: c_ulong, len2: z_off_t) -> c_ulong {
    zlib_rs::crc32_combine(crc1 as u32, crc2 as u32, len2 as u64) as c_ulong
}

pub unsafe extern "C" fn adler32(adler: c_ulong, buf: *const Bytef, len: uInt) -> c_ulong {
    let buf = unsafe { std::slice::from_raw_parts(buf, len as usize) };
    zlib_rs::adler32(adler as u32, buf) as c_ulong
}

pub unsafe extern "C" fn adler32_combine(
    adler1: c_ulong,
    adler2: c_ulong,
    len2: z_off_t,
) -> c_ulong {
    if let Ok(len2) = u64::try_from(len2) {
        zlib_rs::adler32_combine(adler1 as u32, adler2 as u32, len2) as c_ulong
    } else {
        // for negative len, return invalid adler32 as a clue for debugging
        0xffffffff
    }
}

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
    let data = dest;
    let len = std::ptr::read(destLen) as usize;
    let output = std::slice::from_raw_parts_mut(data as *mut MaybeUninit<u8>, len);

    let data = source;
    let len = sourceLen as usize;
    let input = std::slice::from_raw_parts(data, len);

    let (output, err) = zlib_rs::inflate::uncompress(output, input, InflateConfig::default());

    std::ptr::write(destLen, output.len() as _);

    err as c_int
}

pub unsafe extern "C" fn inflate(strm: *mut z_stream, flush: i32) -> i32 {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        let flush = crate::Flush::try_from(flush).unwrap_or_default();
        zlib_rs::inflate::inflate(stream, flush) as _
    } else {
        ReturnCode::StreamError as _
    }
}

pub unsafe extern "C" fn inflateEnd(strm: *mut z_stream) -> i32 {
    zlib_rs::inflate::end(strm)
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
        zlib_rs::inflate::copy(dest, source) as _
    } else {
        ReturnCode::StreamError as _
    }
}

pub unsafe extern "C" fn inflateMark(strm: *const z_stream) -> c_long {
    if let Some(stream) = InflateStream::from_stream_ref(strm) {
        zlib_rs::inflate::mark(stream)
    } else {
        c_long::MIN
    }
}

pub unsafe extern "C" fn inflateSync(strm: *mut z_stream) -> i32 {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        zlib_rs::inflate::sync(stream) as _
    } else {
        ReturnCode::StreamError as _
    }
}

// undocumented
pub unsafe extern "C" fn inflateSyncPoint(strm: *mut z_stream) -> i32 {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        zlib_rs::inflate::sync_point(stream) as i32
    } else {
        ReturnCode::StreamError as _
    }
}

pub unsafe extern "C" fn inflateInit_(
    strm: z_streamp,
    _version: *const c_char,
    _stream_size: c_int,
) -> c_int {
    if strm.is_null() {
        ReturnCode::StreamError as _
    } else {
        zlib_rs::inflate::init(&mut *strm, InflateConfig::default()) as _
    }
}

pub unsafe extern "C" fn inflateInit2_(
    strm: z_streamp,
    windowBits: c_int,
    _version: *const c_char,
    _stream_size: c_int,
) -> c_int {
    inflateInit2(strm, windowBits)
}

pub unsafe extern "C" fn inflateInit2(strm: z_streamp, windowBits: c_int) -> c_int {
    if strm.is_null() {
        ReturnCode::StreamError as _
    } else {
        let config = InflateConfig {
            window_bits: windowBits,
        };
        zlib_rs::inflate::init(&mut *strm, config) as _
    }
}

pub unsafe extern "C" fn inflatePrime(strm: *mut z_stream, bits: i32, value: i32) -> i32 {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        zlib_rs::inflate::prime(stream, bits, value) as _
    } else {
        ReturnCode::StreamError as _
    }
}

pub unsafe extern "C" fn inflateReset(strm: *mut z_stream) -> i32 {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        zlib_rs::inflate::reset(stream) as _
    } else {
        ReturnCode::StreamError as _
    }
}

pub unsafe extern "C" fn inflateReset2(strm: *mut z_stream, windowBits: c_int) -> i32 {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        let config = InflateConfig {
            window_bits: windowBits,
        };
        zlib_rs::inflate::reset_with_config(stream, config) as _
    } else {
        ReturnCode::StreamError as _
    }
}

pub unsafe extern "C" fn inflateSetDictionary(
    strm: *mut z_stream,
    dictionary: *const u8,
    dictLength: c_uint,
) -> c_int {
    let Some(stream) = InflateStream::from_stream_mut(strm) else {
        return ReturnCode::StreamError as _;
    };

    let dict = if dictLength == 0 || dictionary.is_null() {
        &[]
    } else {
        unsafe { std::slice::from_raw_parts(dictionary, dictLength as usize) }
    };

    zlib_rs::inflate::set_dictionary(stream, dict) as _
}

// part of gzip
pub unsafe extern "C" fn inflateGetHeader(strm: z_streamp, head: gz_headerp) -> c_int {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        let header = if head.is_null() {
            None
        } else {
            Some(unsafe { &mut *(head) })
        };

        zlib_rs::inflate::get_header(stream, header) as i32
    } else {
        ReturnCode::StreamError as _
    }
}

// undocumented but exposed function
pub unsafe extern "C" fn inflateUndermine(strm: *mut z_stream, subvert: i32) -> c_int {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        zlib_rs::inflate::undermine(stream, subvert) as i32
    } else {
        ReturnCode::StreamError as _
    }
}

// undocumented but exposed function
pub unsafe extern "C" fn inflateResetKeep(strm: *mut z_stream) -> i32 {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        zlib_rs::inflate::reset_keep(stream) as _
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
            Ok(flush) => zlib_rs::deflate::deflate(stream, flush) as _,
            Err(()) => ReturnCode::StreamError as _,
        }
    } else {
        ReturnCode::StreamError as _
    }
}

pub unsafe extern "C" fn deflateSetHeader(strm: *mut z_stream, head: gz_headerp) -> i32 {
    if let Some(stream) = DeflateStream::from_stream_mut(strm) {
        zlib_rs::deflate::set_header(
            stream,
            if head.is_null() {
                None
            } else {
                Some(&mut *head)
            },
        ) as _
    } else {
        ReturnCode::StreamError as _
    }
}

pub unsafe extern "C" fn deflateBound(strm: *mut z_stream, sourceLen: c_ulong) -> c_ulong {
    zlib_rs::deflate::bound(DeflateStream::from_stream_mut(strm), sourceLen as usize) as c_ulong
}

pub unsafe extern "C" fn compress(
    dest: *mut Bytef,
    destLen: *mut c_ulong,
    source: *const Bytef,
    sourceLen: c_ulong,
) -> c_int {
    compress2(
        dest,
        destLen,
        source,
        sourceLen,
        DeflateConfig::default().level,
    )
}

pub unsafe extern "C" fn compress2(
    dest: *mut Bytef,
    destLen: *mut c_ulong,
    source: *const Bytef,
    sourceLen: c_ulong,
    level: c_int,
) -> c_int {
    let data = dest;
    let len = std::ptr::read(destLen) as usize;
    let output = std::slice::from_raw_parts_mut(data as *mut MaybeUninit<u8>, len);

    let data = source;
    let len = sourceLen as usize;
    let input = std::slice::from_raw_parts(data, len);

    let config = DeflateConfig::new(level);
    let (output, err) = zlib_rs::deflate::compress(output, input, config);

    std::ptr::write(destLen, output.len() as _);

    err as c_int
}

pub extern "C" fn compressBound(sourceLen: c_ulong) -> c_ulong {
    zlib_rs::deflate::compress_bound(sourceLen as usize) as c_ulong
}

pub unsafe extern "C" fn deflateEnd(strm: *mut z_stream) -> i32 {
    match DeflateStream::from_stream_mut(strm) {
        Some(stream) => zlib_rs::deflate::end(stream) as _,
        None => ReturnCode::StreamError as _,
    }
}

pub unsafe extern "C" fn deflateReset(strm: *mut z_stream) -> i32 {
    match DeflateStream::from_stream_mut(strm) {
        Some(stream) => zlib_rs::deflate::reset(stream) as _,
        None => ReturnCode::StreamError as _,
    }
}

pub unsafe extern "C" fn deflateParams(strm: z_streamp, level: c_int, strategy: c_int) -> c_int {
    let Ok(strategy) = Strategy::try_from(strategy) else {
        return ReturnCode::StreamError as _;
    };

    match DeflateStream::from_stream_mut(strm) {
        Some(stream) => zlib_rs::deflate::params(stream, level, strategy) as _,
        None => ReturnCode::StreamError as _,
    }
}

pub unsafe extern "C" fn deflateSetDictionary(
    strm: z_streamp,
    dictionary: *const Bytef,
    dictLength: uInt,
) -> c_int {
    let dictionary = core::slice::from_raw_parts(dictionary, dictLength as usize);

    match DeflateStream::from_stream_mut(strm) {
        Some(stream) => zlib_rs::deflate::set_dictionary(stream, dictionary) as _,
        None => ReturnCode::StreamError as _,
    }
}

pub unsafe extern "C" fn deflatePrime(strm: z_streamp, bits: c_int, value: c_int) -> c_int {
    match DeflateStream::from_stream_mut(strm) {
        Some(stream) => zlib_rs::deflate::prime(stream, bits, value) as _,
        None => ReturnCode::StreamError as _,
    }
}

pub unsafe extern "C" fn deflateInit_(
    strm: z_streamp,
    level: c_int,
    _version: *const c_char,
    _stream_size: c_int,
) -> c_int {
    if strm.is_null() {
        ReturnCode::StreamError as _
    } else {
        zlib_rs::deflate::init(&mut *strm, DeflateConfig::new(level)) as _
    }
}

pub unsafe extern "C" fn deflateInit2_(
    strm: z_streamp,
    level: c_int,
    method: c_int,
    windowBits: c_int,
    memLevel: c_int,
    strategy: c_int,
    _version: *const c_char,
    _stream_size: c_int,
) -> c_int {
    if strm.is_null() {
        ReturnCode::StreamError as _
    } else {
        let Ok(method) = Method::try_from(method) else {
            return ReturnCode::StreamError as _;
        };

        let Ok(strategy) = Strategy::try_from(strategy) else {
            return ReturnCode::StreamError as _;
        };

        let config = DeflateConfig {
            level,
            method,
            window_bits: windowBits,
            mem_level: memLevel,
            strategy,
        };

        zlib_rs::deflate::init(&mut *strm, config) as _
    }
}

pub unsafe extern "C" fn deflateTune(
    strm: z_streamp,
    good_length: c_int,
    max_lazy: c_int,
    nice_length: c_int,
    max_chain: c_int,
) -> c_int {
    match DeflateStream::from_stream_mut(strm) {
        Some(stream) => zlib_rs::deflate::tune(
            stream,
            good_length as usize,
            max_lazy as usize,
            nice_length as usize,
            max_chain as usize,
        ) as _,
        None => ReturnCode::StreamError as _,
    }
}
