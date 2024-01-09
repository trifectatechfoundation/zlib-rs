#![allow(unused)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

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
