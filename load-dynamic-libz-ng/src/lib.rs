#![allow(unused)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(clippy::missing_safety_doc)]

use libz_ng_sys::Bytef;
use std::{
    ffi::{c_char, c_int, c_uint, c_ulong},
    mem::MaybeUninit,
};

const LIBZ_NG_SO: &str = "/home/folkertdev/rust/zlib-ng/libz-ng.so";

pub unsafe fn inflateInit2_(
    strm: *mut libz_ng_sys::z_stream,
    windowBits: c_int,
    version: *const c_char,
    stream_size: c_int,
) -> std::ffi::c_int {
    let lib = libloading::Library::new(LIBZ_NG_SO).unwrap();

    type Func = unsafe extern "C" fn(
        strm: *mut libz_ng_sys::z_stream,
        windowBits: c_int,
        version: *const c_char,
        stream_size: c_int,
    ) -> std::ffi::c_int;

    let f: libloading::Symbol<Func> = lib.get(b"zng_inflateInit2_").unwrap();

    f(strm, windowBits, version, stream_size)
}

pub unsafe fn inflate(strm: *mut libz_ng_sys::z_stream, flush: i32) -> std::ffi::c_int {
    let lib = libloading::Library::new(LIBZ_NG_SO).unwrap();

    type Func =
        unsafe extern "C" fn(strm: *mut libz_ng_sys::z_stream, flush: i32) -> std::ffi::c_int;

    let f: libloading::Symbol<Func> = lib.get(b"zng_inflate").unwrap();

    f(strm, flush)
}

pub unsafe fn inflateSetDictionary(
    strm: *mut libz_ng_sys::z_stream,
    dictionary: *const u8,
    dictLength: c_uint,
) -> std::ffi::c_int {
    let lib = libloading::Library::new(LIBZ_NG_SO).unwrap();

    type Func = unsafe extern "C" fn(
        strm: *mut libz_ng_sys::z_stream,
        dictionary: *const u8,
        dictLength: c_uint,
    ) -> std::ffi::c_int;

    let f: libloading::Symbol<Func> = lib.get(b"zng_inflateSetDictionary").unwrap();

    f(strm, dictionary, dictLength)
}

pub unsafe fn inflateEnd(strm: *mut libz_ng_sys::z_stream) -> std::ffi::c_int {
    let lib = libloading::Library::new(LIBZ_NG_SO).unwrap();

    type Func = unsafe extern "C" fn(strm: *mut libz_ng_sys::z_stream) -> std::ffi::c_int;

    let f: libloading::Symbol<Func> = lib.get(b"zng_inflateEnd").unwrap();

    f(strm)
}

pub unsafe fn compress(
    dest: *mut Bytef,
    destLen: *mut c_ulong,
    source: *const Bytef,
    sourceLen: c_ulong,
) -> std::ffi::c_int {
    let lib = libloading::Library::new(LIBZ_NG_SO).unwrap();

    type Func = unsafe extern "C" fn(
        dest: *mut Bytef,
        destLen: *mut c_ulong,
        source: *const Bytef,
        sourceLen: c_ulong,
    ) -> std::ffi::c_int;

    let f: libloading::Symbol<Func> = lib.get(b"zng_compress").unwrap();

    f(dest, destLen, source, sourceLen)
}

pub unsafe fn deflate(strm: *mut libz_ng_sys::z_stream, flush: i32) -> std::ffi::c_int {
    const LIBZ_NG_SO: &str = "/home/folkertdev/rust/zlib-ng/libz-ng.so";

    let lib = libloading::Library::new(LIBZ_NG_SO).unwrap();

    type Func = unsafe extern "C" fn(strm: *mut libz_ng_sys::z_stream, flush: i32) -> i32;

    let f: libloading::Symbol<Func> = lib.get(b"zng_deflate").unwrap();

    f(strm, flush)
}

#[allow(non_camel_case_types)]
#[allow(non_snake_case)]
unsafe fn deflateInit2(
    strm: *mut libz_ng_sys::z_stream,
    level: c_int,
    method: c_int,
    windowBits: c_int,
    memLevel: c_int,
    strategy: c_int,
) -> std::ffi::c_int {
    const LIBZ_NG_SO: &str = "/home/folkertdev/rust/zlib-ng/libz-ng.so";

    const VERSION: *const c_char = "2.1.4\0".as_ptr() as *const c_char;
    const STREAM_SIZE: c_int = std::mem::size_of::<libz_ng_sys::z_stream>() as c_int;

    let lib = libloading::Library::new(LIBZ_NG_SO).unwrap();

    type Func = unsafe extern "C" fn(
        strm: *mut libz_ng_sys::z_stream,
        level: c_int,
        method: c_int,
        windowBits: c_int,
        memLevel: c_int,
        strategy: c_int,
        version: *const c_char,
        stream_size: c_int,
    ) -> i32;

    let f: libloading::Symbol<Func> = lib.get(b"zng_deflateInit2_").unwrap();

    f(
        strm,
        level,
        method,
        windowBits,
        memLevel,
        strategy,
        VERSION,
        STREAM_SIZE,
    )
}

pub fn compress_slice<'a>(
    output: &'a mut [u8],
    input: &[u8],
    level: i32,
    method: i32,
    window_bits: i32,
    mem_level: i32,
    strategy: i32,
) -> (&'a mut [u8], i32) {
    let mut stream = MaybeUninit::zeroed();

    unsafe {
        let err = deflateInit2(
            stream.as_mut_ptr(),
            level,
            method,
            window_bits,
            mem_level,
            strategy,
        );

        if err != libz_ng_sys::Z_OK {
            return (&mut [], err);
        }
    };

    let stream = unsafe { stream.assume_init_mut() };

    let error = unsafe { deflate(stream, libz_ng_sys::Z_FINISH) };

    assert_eq!(libz_ng_sys::Z_STREAM_END, error);

    unsafe {
        let err = libz_ng_sys::deflateEnd(stream);
        assert_eq!(libz_ng_sys::Z_OK, err);
    }

    (&mut output[..stream.total_out], libz_ng_sys::Z_OK)
}

pub unsafe fn crc32(start: u32, buf: *const u8, len: usize) -> u32 {
    let lib = libloading::Library::new("/home/folkertdev/c/libcrc.so").unwrap();

    type Func = unsafe extern "C" fn(start: u32, buf: *const u8, len: usize) -> u32;

    let f: libloading::Symbol<Func> = lib.get(b"crc32_sse").unwrap();

    f(start, buf, len)
}
