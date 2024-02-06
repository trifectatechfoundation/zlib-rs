#![allow(unused)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use crate::{deflate::DeflateConfig, Flush, ReturnCode};

const LIBZ_NG_SO: &str = "/home/folkertdev/rust/zlib-ng/libz-ng.so";

pub fn empty_stream() -> libz_ng_sys::z_stream {
    libz_ng_sys::z_stream {
        next_in: std::ptr::null_mut(),
        avail_in: 0,
        total_in: 0,
        next_out: std::ptr::null_mut(),
        avail_out: 0,
        total_out: 0,
        msg: std::ptr::null_mut(),
        state: std::ptr::null_mut(),
        zalloc: crate::allocate::zcalloc,
        zfree: crate::allocate::zcfree,
        opaque: std::ptr::null_mut(),
        data_type: 0,
        adler: 0,
        reserved: 0,
    }
}

pub unsafe fn inflateInit2_(
    strm: *mut libz_ng_sys::z_stream,
    windowBits: libc::c_int,
    version: *const libc::c_char,
    stream_size: libc::c_int,
) -> std::ffi::c_int {
    let lib = libloading::Library::new(LIBZ_NG_SO).unwrap();

    type Func = unsafe extern "C" fn(
        strm: *mut libz_ng_sys::z_stream,
        windowBits: libc::c_int,
        version: *const libc::c_char,
        stream_size: libc::c_int,
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
    dictLength: libc::c_uint,
) -> std::ffi::c_int {
    let lib = libloading::Library::new(LIBZ_NG_SO).unwrap();

    type Func = unsafe extern "C" fn(
        strm: *mut libz_ng_sys::z_stream,
        dictionary: *const u8,
        dictLength: libc::c_uint,
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
    dest: *mut crate::c_api::Bytef,
    destLen: *mut libc::c_ulong,
    source: *const crate::c_api::Bytef,
    sourceLen: libc::c_ulong,
) -> std::ffi::c_int {
    let lib = libloading::Library::new(LIBZ_NG_SO).unwrap();

    type Func = unsafe extern "C" fn(
        dest: *mut crate::c_api::Bytef,
        destLen: *mut libc::c_ulong,
        source: *const crate::c_api::Bytef,
        sourceLen: libc::c_ulong,
    ) -> std::ffi::c_int;

    let f: libloading::Symbol<Func> = lib.get(b"zng_compress").unwrap();

    f(dest, destLen, source, sourceLen)
}

unsafe fn uncompress_dynamic(
    dest: *mut u8,
    dest_len: *mut std::ffi::c_ulong,
    source: *const u8,
    source_len: std::ffi::c_ulong,
) -> std::ffi::c_int {
    let lib = libloading::Library::new("/home/folkertdev/rust/zlib-ng/libz-ng.so").unwrap();

    type Func = unsafe extern "C" fn(
        dest: *mut u8,
        dest_len: *mut std::ffi::c_ulong,
        source: *const u8,
        source_len: std::ffi::c_ulong,
    ) -> std::ffi::c_int;

    let f: libloading::Symbol<Func> = lib.get(b"zng_uncompress").unwrap();

    f(dest, dest_len, source, source_len)
}

unsafe fn deflate_dynamic(strm: *mut libz_ng_sys::z_stream, flush: i32) -> std::ffi::c_int {
    const LIBZ_NG_SO: &str = "/home/folkertdev/rust/zlib-ng/libz-ng.so";

    let lib = libloading::Library::new(LIBZ_NG_SO).unwrap();

    type Func = unsafe extern "C" fn(strm: *mut libz_ng_sys::z_stream, flush: i32) -> i32;

    let f: libloading::Symbol<Func> = lib.get(b"zng_deflate").unwrap();

    f(strm, flush)
}

#[allow(non_camel_case_types)]
#[allow(non_snake_case)]
unsafe fn deflateInit2_dynamic(
    strm: *mut libz_ng_sys::z_stream,
    level: libc::c_int,
    method: libc::c_int,
    windowBits: libc::c_int,
    memLevel: libc::c_int,
    strategy: libc::c_int,
) -> std::ffi::c_int {
    const LIBZ_NG_SO: &str = "/home/folkertdev/rust/zlib-ng/libz-ng.so";

    const VERSION: *const libc::c_char = "2.1.4\0".as_ptr() as *const libc::c_char;
    const STREAM_SIZE: libc::c_int = std::mem::size_of::<libz_ng_sys::z_stream>() as libc::c_int;

    let lib = libloading::Library::new(LIBZ_NG_SO).unwrap();

    type Func = unsafe extern "C" fn(
        strm: *mut libz_ng_sys::z_stream,
        level: libc::c_int,
        method: libc::c_int,
        windowBits: libc::c_int,
        memLevel: libc::c_int,
        strategy: libc::c_int,
        version: *const libc::c_char,
        stream_size: libc::c_int,
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

fn compress_slice_dynamic<'a>(
    output: &'a mut [u8],
    input: &[u8],
    config: DeflateConfig,
) -> (&'a mut [u8], ReturnCode) {
    let DeflateConfig {
        level,
        method,
        window_bits,
        mem_level,
        strategy,
    } = config;

    let mut stream = libz_ng_sys::z_stream {
        next_in: input.as_ptr() as *mut u8,
        avail_in: input.len() as _,
        total_in: 0,
        next_out: output.as_mut_ptr(),
        avail_out: output.len() as _,
        total_out: 0,
        msg: std::ptr::null_mut(),
        state: std::ptr::null_mut(),
        zalloc: crate::allocate::zcalloc,
        zfree: crate::allocate::zcfree,
        opaque: std::ptr::null_mut(),
        data_type: 0,
        adler: 0,
        reserved: 0,
    };

    unsafe {
        let err = deflateInit2_dynamic(
            &mut stream,
            level,
            method as i32,
            window_bits,
            mem_level,
            strategy as i32,
            // b"1.3.0\0".as_ptr() as *const i8,
            // std::mem::size_of::<libz_ng_sys::z_stream>() as i32,
        );
        let return_code = ReturnCode::from(err);

        if return_code != ReturnCode::Ok {
            return (&mut [], return_code);
        }
    };

    // let error = unsafe { libz_ng_sys::deflate(&mut stream, Flush::Finish as _) };
    let error = unsafe { deflate_dynamic(&mut stream, Flush::Finish as _) };

    let error: ReturnCode = ReturnCode::from(error as i32);
    assert_eq!(ReturnCode::StreamEnd, error);

    unsafe {
        let err = libz_ng_sys::deflateEnd(&mut stream);
        let return_code: ReturnCode = ReturnCode::from(err);
        assert_eq!(ReturnCode::Ok, return_code);
    }

    (&mut output[..stream.total_out as usize], ReturnCode::Ok)
}
