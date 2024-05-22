#![allow(unused)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(clippy::missing_safety_doc)]

// we use the libz_sys but configure zlib-ng in zlib compat mode
use libz_sys as libz_ng_sys;

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
        "2.1.4".as_ptr().cast(),
        104,
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
    compress_slice_with_flush(
        output,
        input,
        level,
        method,
        window_bits,
        mem_level,
        strategy,
        libz_ng_sys::Z_FINISH,
    )
}

pub fn compress_slice_with_flush<'a>(
    output: &'a mut [u8],
    input: &[u8],
    level: i32,
    method: i32,
    window_bits: i32,
    mem_level: i32,
    strategy: i32,
    final_flush: i32,
) -> (&'a mut [u8], i32) {
    let output_uninit = unsafe {
        core::slice::from_raw_parts_mut(output.as_mut_ptr() as *mut MaybeUninit<u8>, output.len())
    };

    compress_with_flush(
        output_uninit,
        input,
        level,
        method,
        window_bits,
        mem_level,
        strategy,
        final_flush,
    )
}

pub fn compress_with_flush<'a>(
    output: &'a mut [MaybeUninit<u8>],
    input: &[u8],
    level: i32,
    method: i32,
    window_bits: i32,
    mem_level: i32,
    strategy: i32,
    final_flush: i32,
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

    stream.next_in = input.as_ptr() as *mut u8;
    stream.next_out = output.as_mut_ptr() as *mut u8;

    let max = core::ffi::c_uint::MAX as usize;

    let mut left = output.len();
    let mut source_len = input.len();

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
            libz_ng_sys::Z_NO_FLUSH
        } else {
            final_flush
        };

        let err = unsafe { deflate(stream, flush) };

        if err != libz_ng_sys::Z_OK {
            break;
        }
    }

    // SAFETY: we have now initialized these bytes
    let output_slice = unsafe {
        core::slice::from_raw_parts_mut(output.as_mut_ptr() as *mut u8, stream.total_out as usize)
    };

    unsafe {
        let err = libz_ng_sys::deflateEnd(stream);
        assert_eq!(libz_ng_sys::Z_OK, err);
    }

    (output_slice, libz_ng_sys::Z_OK)
}

pub unsafe fn crc32(start: u32, buf: *const u8, len: usize) -> u32 {
    // source code: https://gist.github.com/folkertdev/daa2caff0d0b91a19e81138cb4a780bb
    let path = std::path::Path::new("libcrc32.so").canonicalize().unwrap();

    let lib = libloading::Library::new(path).unwrap();

    type Func = unsafe extern "C" fn(start: u32, buf: *const u8, len: usize) -> u32;

    let f: libloading::Symbol<Func> = lib.get(b"crc32_sse").unwrap();

    f(start, buf, len)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn try_crc32() {
        if std::path::Path::new("libcrc32.so").exists() {
            if is_x86_feature_detected!("pclmulqdq")
                && is_x86_feature_detected!("sse2")
                && is_x86_feature_detected!("sse4.1")
            {
                let buf = (0..255u8).collect::<Vec<_>>();

                let x = unsafe { crc32(0, buf.as_ptr(), buf.len()) };

                assert_eq!(x, 3543112608);
            } else {
                eprintln!("pclmulqdq not supported");
            }
        } else {
            eprintln!("libcrc32.so not found");
        }
    }
}
