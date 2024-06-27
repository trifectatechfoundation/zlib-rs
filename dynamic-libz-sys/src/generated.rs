#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(clippy::too_many_arguments)]
use std::os::raw::{c_char, c_int, c_long, c_uchar, c_uint, c_ulong, c_void};
#[cfg(not(zng))]
macro_rules! if_zng {
    ($ _zng : tt , $ not_zng : tt) => {
        $not_zng
    };
}
#[cfg(zng)]
macro_rules! if_zng {
    ($ zng : tt , $ _not_zng : tt) => {
        $zng
    };
}
type z_size = if_zng!(usize, c_ulong);
type z_checksum = if_zng!(u32, c_ulong);
pub type alloc_func = unsafe extern "C" fn(voidpf, uInt, uInt) -> voidpf;
pub type Bytef = u8;
pub type free_func = unsafe extern "C" fn(voidpf, voidpf);
#[cfg(any(zng, feature = "libc"))]
pub type gzFile = *mut gzFile_s;
pub type in_func = unsafe extern "C" fn(*mut c_void, *mut *const c_uchar) -> c_uint;
pub type out_func = unsafe extern "C" fn(*mut c_void, *mut c_uchar, c_uint) -> c_int;
pub type uInt = c_uint;
pub type uLong = c_ulong;
pub type uLongf = c_ulong;
pub type voidp = *mut c_void;
pub type voidpc = *const c_void;
pub type voidpf = *mut c_void;
#[cfg(any(zng, feature = "libc"))]
pub enum gzFile_s {}
pub enum internal_state {}
#[cfg(all(
    not(zng),
    feature = "libc",
    not(all(target_family = "wasm", target_os = "unknown"))
))]
pub type z_off_t = libc::off_t;
#[cfg(all(
    not(zng),
    feature = "libc",
    all(target_family = "wasm", target_os = "unknown")
))]
pub type z_off_t = c_long;
#[cfg(all(zng, windows, not(target_env = "gnu")))]
pub type z_off_t = i64;
#[cfg(all(zng, not(all(windows, not(target_env = "gnu")))))]
pub type z_off_t = libc::off_t;
#[repr(C)]
#[derive(Copy, Clone)]
pub struct gz_header {
    pub text: c_int,
    pub time: uLong,
    pub xflags: c_int,
    pub os: c_int,
    pub extra: *mut Bytef,
    pub extra_len: uInt,
    pub extra_max: uInt,
    pub name: *mut Bytef,
    pub name_max: uInt,
    pub comment: *mut Bytef,
    pub comm_max: uInt,
    pub hcrc: c_int,
    pub done: c_int,
}
pub type gz_headerp = *mut gz_header;
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
    pub zalloc: alloc_func,
    pub zfree: free_func,
    pub opaque: voidpf,
    pub data_type: c_int,
    pub adler: z_checksum,
    pub reserved: uLong,
}
pub type z_streamp = *mut z_stream;
#[cfg(not(zng))]
macro_rules! zng_prefix {
    ($ name : expr) => {
        stringify!($name)
    };
}
#[cfg(zng)]
macro_rules! zng_prefix {
    ($ name : expr) => {
        concat!("zng_", stringify!($name))
    };
}
pub unsafe fn adler32(adler: z_checksum, buf: *const Bytef, len: uInt) -> z_checksum {
    type Func = unsafe extern "C" fn(adler: z_checksum, buf: *const Bytef, len: uInt) -> z_checksum;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "adler32") }, "adler32").as_bytes())
        .unwrap();
    f(adler, buf, len)
}
pub unsafe fn crc32(crc: z_checksum, buf: *const Bytef, len: uInt) -> z_checksum {
    type Func = unsafe extern "C" fn(crc: z_checksum, buf: *const Bytef, len: uInt) -> z_checksum;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "crc32") }, "crc32").as_bytes())
        .unwrap();
    f(crc, buf, len)
}
pub unsafe fn deflate(strm: z_streamp, flush: c_int) -> c_int {
    type Func = unsafe extern "C" fn(strm: z_streamp, flush: c_int) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "deflate") }, "deflate").as_bytes())
        .unwrap();
    f(strm, flush)
}
pub unsafe fn deflateBound(strm: z_streamp, sourceLen: uLong) -> uLong {
    type Func = unsafe extern "C" fn(strm: z_streamp, sourceLen: uLong) -> uLong;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "deflateBound") }, "deflateBound").as_bytes())
        .unwrap();
    f(strm, sourceLen)
}
pub unsafe fn deflateCopy(dest: z_streamp, source: z_streamp) -> c_int {
    type Func = unsafe extern "C" fn(dest: z_streamp, source: z_streamp) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "deflateCopy") }, "deflateCopy").as_bytes())
        .unwrap();
    f(dest, source)
}
pub unsafe fn deflateEnd(strm: z_streamp) -> c_int {
    type Func = unsafe extern "C" fn(strm: z_streamp) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "deflateEnd") }, "deflateEnd").as_bytes())
        .unwrap();
    f(strm)
}
pub unsafe fn deflateParams(strm: z_streamp, level: c_int, strategy: c_int) -> c_int {
    type Func = unsafe extern "C" fn(strm: z_streamp, level: c_int, strategy: c_int) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "deflateParams") }, "deflateParams").as_bytes())
        .unwrap();
    f(strm, level, strategy)
}
pub unsafe fn deflatePrime(strm: z_streamp, bits: c_int, value: c_int) -> c_int {
    type Func = unsafe extern "C" fn(strm: z_streamp, bits: c_int, value: c_int) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "deflatePrime") }, "deflatePrime").as_bytes())
        .unwrap();
    f(strm, bits, value)
}
pub unsafe fn deflateReset(strm: z_streamp) -> c_int {
    type Func = unsafe extern "C" fn(strm: z_streamp) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "deflateReset") }, "deflateReset").as_bytes())
        .unwrap();
    f(strm)
}
pub unsafe fn deflateSetDictionary(
    strm: z_streamp,
    dictionary: *const Bytef,
    dictLength: uInt,
) -> c_int {
    type Func =
        unsafe extern "C" fn(strm: z_streamp, dictionary: *const Bytef, dictLength: uInt) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(
            if_zng!(
                { concat!("zng_", "deflateSetDictionary") },
                "deflateSetDictionary"
            )
            .as_bytes(),
        )
        .unwrap();
    f(strm, dictionary, dictLength)
}
pub unsafe fn deflateSetHeader(strm: z_streamp, head: gz_headerp) -> c_int {
    type Func = unsafe extern "C" fn(strm: z_streamp, head: gz_headerp) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "deflateSetHeader") }, "deflateSetHeader").as_bytes())
        .unwrap();
    f(strm, head)
}
pub unsafe fn deflateTune(
    strm: z_streamp,
    good_length: c_int,
    max_lazy: c_int,
    nice_length: c_int,
    max_chain: c_int,
) -> c_int {
    type Func = unsafe extern "C" fn(
        strm: z_streamp,
        good_length: c_int,
        max_lazy: c_int,
        nice_length: c_int,
        max_chain: c_int,
    ) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "deflateTune") }, "deflateTune").as_bytes())
        .unwrap();
    f(strm, good_length, max_lazy, nice_length, max_chain)
}
pub unsafe fn inflate(strm: z_streamp, flush: c_int) -> c_int {
    type Func = unsafe extern "C" fn(strm: z_streamp, flush: c_int) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "inflate") }, "inflate").as_bytes())
        .unwrap();
    f(strm, flush)
}
pub unsafe fn inflateBack(
    strm: z_streamp,
    _in: in_func,
    in_desc: *mut c_void,
    out: out_func,
    out_desc: *mut c_void,
) -> c_int {
    type Func = unsafe extern "C" fn(
        strm: z_streamp,
        _in: in_func,
        in_desc: *mut c_void,
        out: out_func,
        out_desc: *mut c_void,
    ) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "inflateBack") }, "inflateBack").as_bytes())
        .unwrap();
    f(strm, _in, in_desc, out, out_desc)
}
pub unsafe fn inflateBackEnd(strm: z_streamp) -> c_int {
    type Func = unsafe extern "C" fn(strm: z_streamp) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "inflateBackEnd") }, "inflateBackEnd").as_bytes())
        .unwrap();
    f(strm)
}
pub unsafe fn inflateCopy(dest: z_streamp, source: z_streamp) -> c_int {
    type Func = unsafe extern "C" fn(dest: z_streamp, source: z_streamp) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "inflateCopy") }, "inflateCopy").as_bytes())
        .unwrap();
    f(dest, source)
}
pub unsafe fn inflateEnd(strm: z_streamp) -> c_int {
    type Func = unsafe extern "C" fn(strm: z_streamp) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "inflateEnd") }, "inflateEnd").as_bytes())
        .unwrap();
    f(strm)
}
pub unsafe fn inflateGetHeader(strm: z_streamp, head: gz_headerp) -> c_int {
    type Func = unsafe extern "C" fn(strm: z_streamp, head: gz_headerp) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "inflateGetHeader") }, "inflateGetHeader").as_bytes())
        .unwrap();
    f(strm, head)
}
pub unsafe fn inflateMark(strm: z_streamp) -> c_long {
    type Func = unsafe extern "C" fn(strm: z_streamp) -> c_long;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "inflateMark") }, "inflateMark").as_bytes())
        .unwrap();
    f(strm)
}
pub unsafe fn inflatePrime(strm: z_streamp, bits: c_int, value: c_int) -> c_int {
    type Func = unsafe extern "C" fn(strm: z_streamp, bits: c_int, value: c_int) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "inflatePrime") }, "inflatePrime").as_bytes())
        .unwrap();
    f(strm, bits, value)
}
pub unsafe fn inflateReset(strm: z_streamp) -> c_int {
    type Func = unsafe extern "C" fn(strm: z_streamp) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "inflateReset") }, "inflateReset").as_bytes())
        .unwrap();
    f(strm)
}
pub unsafe fn inflateReset2(strm: z_streamp, windowBits: c_int) -> c_int {
    type Func = unsafe extern "C" fn(strm: z_streamp, windowBits: c_int) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "inflateReset2") }, "inflateReset2").as_bytes())
        .unwrap();
    f(strm, windowBits)
}
pub unsafe fn inflateSetDictionary(
    strm: z_streamp,
    dictionary: *const Bytef,
    dictLength: uInt,
) -> c_int {
    type Func =
        unsafe extern "C" fn(strm: z_streamp, dictionary: *const Bytef, dictLength: uInt) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(
            if_zng!(
                { concat!("zng_", "inflateSetDictionary") },
                "inflateSetDictionary"
            )
            .as_bytes(),
        )
        .unwrap();
    f(strm, dictionary, dictLength)
}
pub unsafe fn inflateSync(strm: z_streamp) -> c_int {
    type Func = unsafe extern "C" fn(strm: z_streamp) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "inflateSync") }, "inflateSync").as_bytes())
        .unwrap();
    f(strm)
}
pub unsafe fn zlibCompileFlags() -> uLong {
    type Func = unsafe extern "C" fn() -> uLong;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "zlibCompileFlags") }, "zlibCompileFlags").as_bytes())
        .unwrap();
    f()
}
pub unsafe fn zlibVersion() -> *const c_char {
    type Func = unsafe extern "C" fn() -> *const c_char;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "zlibVersion") }, "zlibVersion").as_bytes())
        .unwrap();
    f()
}
#[cfg(not(zng))]
pub unsafe fn deflateInit_(
    strm: z_streamp,
    level: c_int,
    version: *const c_char,
    stream_size: c_int,
) -> c_int {
    type Func = unsafe extern "C" fn(
        strm: z_streamp,
        level: c_int,
        version: *const c_char,
        stream_size: c_int,
    ) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "deflateInit_") }, "deflateInit_").as_bytes())
        .unwrap();
    f(strm, level, version, stream_size)
}
#[cfg(not(zng))]
pub unsafe fn deflateInit2_(
    strm: z_streamp,
    level: c_int,
    method: c_int,
    windowBits: c_int,
    memLevel: c_int,
    strategy: c_int,
    version: *const c_char,
    stream_size: c_int,
) -> c_int {
    type Func = unsafe extern "C" fn(
        strm: z_streamp,
        level: c_int,
        method: c_int,
        windowBits: c_int,
        memLevel: c_int,
        strategy: c_int,
        version: *const c_char,
        stream_size: c_int,
    ) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "deflateInit2_") }, "deflateInit2_").as_bytes())
        .unwrap();
    f(
        strm,
        level,
        method,
        windowBits,
        memLevel,
        strategy,
        version,
        stream_size,
    )
}
#[cfg(not(zng))]
pub unsafe fn inflateBackInit_(
    strm: z_streamp,
    windowBits: c_int,
    window: *mut c_uchar,
    version: *const c_char,
    stream_size: c_int,
) -> c_int {
    type Func = unsafe extern "C" fn(
        strm: z_streamp,
        windowBits: c_int,
        window: *mut c_uchar,
        version: *const c_char,
        stream_size: c_int,
    ) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "inflateBackInit_") }, "inflateBackInit_").as_bytes())
        .unwrap();
    f(strm, windowBits, window, version, stream_size)
}
#[cfg(not(zng))]
pub unsafe fn inflateInit_(strm: z_streamp, version: *const c_char, stream_size: c_int) -> c_int {
    type Func =
        unsafe extern "C" fn(strm: z_streamp, version: *const c_char, stream_size: c_int) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "inflateInit_") }, "inflateInit_").as_bytes())
        .unwrap();
    f(strm, version, stream_size)
}
#[cfg(not(zng))]
pub unsafe fn inflateInit2_(
    strm: z_streamp,
    windowBits: c_int,
    version: *const c_char,
    stream_size: c_int,
) -> c_int {
    type Func = unsafe extern "C" fn(
        strm: z_streamp,
        windowBits: c_int,
        version: *const c_char,
        stream_size: c_int,
    ) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "inflateInit2_") }, "inflateInit2_").as_bytes())
        .unwrap();
    f(strm, windowBits, version, stream_size)
}
#[cfg(zng)]
pub unsafe fn zng_deflateInit(strm: z_streamp, level: c_int) -> c_int {
    type Func = unsafe extern "C" fn(strm: z_streamp, level: c_int) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "zng_deflateInit") }, "zng_deflateInit").as_bytes())
        .unwrap();
    f(strm, level)
}
#[cfg(zng)]
pub unsafe fn zng_deflateInit2(
    strm: z_streamp,
    level: c_int,
    method: c_int,
    windowBits: c_int,
    memLevel: c_int,
    strategy: c_int,
) -> c_int {
    type Func = unsafe extern "C" fn(
        strm: z_streamp,
        level: c_int,
        method: c_int,
        windowBits: c_int,
        memLevel: c_int,
        strategy: c_int,
    ) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "zng_deflateInit2") }, "zng_deflateInit2").as_bytes())
        .unwrap();
    f(strm, level, method, windowBits, memLevel, strategy)
}
#[cfg(zng)]
pub unsafe fn zng_inflateBackInit(
    strm: z_streamp,
    windowBits: c_int,
    window: *mut c_uchar,
) -> c_int {
    type Func =
        unsafe extern "C" fn(strm: z_streamp, windowBits: c_int, window: *mut c_uchar) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(
            if_zng!(
                { concat!("zng_", "zng_inflateBackInit") },
                "zng_inflateBackInit"
            )
            .as_bytes(),
        )
        .unwrap();
    f(strm, windowBits, window)
}
#[cfg(zng)]
pub unsafe fn zng_inflateInit(strm: z_streamp) -> c_int {
    type Func = unsafe extern "C" fn(strm: z_streamp) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "zng_inflateInit") }, "zng_inflateInit").as_bytes())
        .unwrap();
    f(strm)
}
#[cfg(zng)]
pub unsafe fn zng_inflateInit2(strm: z_streamp, windowBits: c_int) -> c_int {
    type Func = unsafe extern "C" fn(strm: z_streamp, windowBits: c_int) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "zng_inflateInit2") }, "zng_inflateInit2").as_bytes())
        .unwrap();
    f(strm, windowBits)
}
#[cfg(zng)]
#[inline(always)]
pub unsafe fn inflateInit2_(
    strm: z_streamp,
    windowBits: c_int,
    _version: *const c_char,
    _stream_size: c_int,
) -> c_int {
    zng_inflateInit2(strm, windowBits)
}
#[cfg(zng)]
#[inline(always)]
pub unsafe fn inflateInit_(strm: z_streamp, _version: *const c_char, _stream_size: c_int) -> c_int {
    zng_inflateInit(strm)
}
#[cfg(zng)]
#[inline(always)]
pub unsafe fn inflateBackInit_(
    strm: z_streamp,
    windowBits: c_int,
    window: *mut c_uchar,
    _version: *const c_char,
    _stream_size: c_int,
) -> c_int {
    zng_inflateBackInit(strm, windowBits, window)
}
#[cfg(zng)]
#[inline(always)]
pub unsafe fn deflateInit2_(
    strm: z_streamp,
    level: c_int,
    method: c_int,
    windowBits: c_int,
    memLevel: c_int,
    strategy: c_int,
    _version: *const c_char,
    _stream_size: c_int,
) -> c_int {
    zng_deflateInit2(strm, level, method, windowBits, memLevel, strategy)
}
#[cfg(zng)]
#[inline]
pub unsafe fn deflateInit_(
    strm: z_streamp,
    level: c_int,
    _version: *const c_char,
    _stream_size: c_int,
) -> c_int {
    zng_deflateInit(strm, level)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn adler32_combine(adler1: z_checksum, adler2: z_checksum, len2: z_off_t) -> z_checksum {
    type Func =
        unsafe extern "C" fn(adler1: z_checksum, adler2: z_checksum, len2: z_off_t) -> z_checksum;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "adler32_combine") }, "adler32_combine").as_bytes())
        .unwrap();
    f(adler1, adler2, len2)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn compress(
    dest: *mut Bytef,
    destLen: *mut z_size,
    source: *const Bytef,
    sourceLen: z_size,
) -> c_int {
    type Func = unsafe extern "C" fn(
        dest: *mut Bytef,
        destLen: *mut z_size,
        source: *const Bytef,
        sourceLen: z_size,
    ) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "compress") }, "compress").as_bytes())
        .unwrap();
    f(dest, destLen, source, sourceLen)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn compress2(
    dest: *mut Bytef,
    destLen: *mut z_size,
    source: *const Bytef,
    sourceLen: z_size,
    level: c_int,
) -> c_int {
    type Func = unsafe extern "C" fn(
        dest: *mut Bytef,
        destLen: *mut z_size,
        source: *const Bytef,
        sourceLen: z_size,
        level: c_int,
    ) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "compress2") }, "compress2").as_bytes())
        .unwrap();
    f(dest, destLen, source, sourceLen, level)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn compressBound(sourceLen: z_size) -> z_size {
    type Func = unsafe extern "C" fn(sourceLen: z_size) -> z_size;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "compressBound") }, "compressBound").as_bytes())
        .unwrap();
    f(sourceLen)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn crc32_combine(crc1: z_checksum, crc2: z_checksum, len2: z_off_t) -> z_checksum {
    type Func =
        unsafe extern "C" fn(crc1: z_checksum, crc2: z_checksum, len2: z_off_t) -> z_checksum;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "crc32_combine") }, "crc32_combine").as_bytes())
        .unwrap();
    f(crc1, crc2, len2)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn gzdirect(file: gzFile) -> c_int {
    type Func = unsafe extern "C" fn(file: gzFile) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "gzdirect") }, "gzdirect").as_bytes())
        .unwrap();
    f(file)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn gzdopen(fd: c_int, mode: *const c_char) -> gzFile {
    type Func = unsafe extern "C" fn(fd: c_int, mode: *const c_char) -> gzFile;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "gzdopen") }, "gzdopen").as_bytes())
        .unwrap();
    f(fd, mode)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn gzclearerr(file: gzFile) {
    type Func = unsafe extern "C" fn(file: gzFile);
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "gzclearerr") }, "gzclearerr").as_bytes())
        .unwrap();
    f(file)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn gzclose(file: gzFile) -> c_int {
    type Func = unsafe extern "C" fn(file: gzFile) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "gzclose") }, "gzclose").as_bytes())
        .unwrap();
    f(file)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn gzeof(file: gzFile) -> c_int {
    type Func = unsafe extern "C" fn(file: gzFile) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "gzeof") }, "gzeof").as_bytes())
        .unwrap();
    f(file)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn gzerror(file: gzFile, errnum: *mut c_int) -> *const c_char {
    type Func = unsafe extern "C" fn(file: gzFile, errnum: *mut c_int) -> *const c_char;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "gzerror") }, "gzerror").as_bytes())
        .unwrap();
    f(file, errnum)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn gzflush(file: gzFile, flush: c_int) -> c_int {
    type Func = unsafe extern "C" fn(file: gzFile, flush: c_int) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "gzflush") }, "gzflush").as_bytes())
        .unwrap();
    f(file, flush)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn gzgetc(file: gzFile) -> c_int {
    type Func = unsafe extern "C" fn(file: gzFile) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "gzgetc") }, "gzgetc").as_bytes())
        .unwrap();
    f(file)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn gzgets(file: gzFile, buf: *mut c_char, len: c_int) -> *mut c_char {
    type Func = unsafe extern "C" fn(file: gzFile, buf: *mut c_char, len: c_int) -> *mut c_char;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "gzgets") }, "gzgets").as_bytes())
        .unwrap();
    f(file, buf, len)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn gzopen(path: *const c_char, mode: *const c_char) -> gzFile {
    type Func = unsafe extern "C" fn(path: *const c_char, mode: *const c_char) -> gzFile;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "gzopen") }, "gzopen").as_bytes())
        .unwrap();
    f(path, mode)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn gzputc(file: gzFile, c: c_int) -> c_int {
    type Func = unsafe extern "C" fn(file: gzFile, c: c_int) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "gzputc") }, "gzputc").as_bytes())
        .unwrap();
    f(file, c)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn gzputs(file: gzFile, s: *const c_char) -> c_int {
    type Func = unsafe extern "C" fn(file: gzFile, s: *const c_char) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "gzputs") }, "gzputs").as_bytes())
        .unwrap();
    f(file, s)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn gzread(file: gzFile, buf: voidp, len: c_uint) -> c_int {
    type Func = unsafe extern "C" fn(file: gzFile, buf: voidp, len: c_uint) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "gzread") }, "gzread").as_bytes())
        .unwrap();
    f(file, buf, len)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn gzrewind(file: gzFile) -> c_int {
    type Func = unsafe extern "C" fn(file: gzFile) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "gzrewind") }, "gzrewind").as_bytes())
        .unwrap();
    f(file)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn gzseek(file: gzFile, offset: z_off_t, whence: c_int) -> z_off_t {
    type Func = unsafe extern "C" fn(file: gzFile, offset: z_off_t, whence: c_int) -> z_off_t;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "gzseek") }, "gzseek").as_bytes())
        .unwrap();
    f(file, offset, whence)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn gzsetparams(file: gzFile, level: c_int, strategy: c_int) -> c_int {
    type Func = unsafe extern "C" fn(file: gzFile, level: c_int, strategy: c_int) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "gzsetparams") }, "gzsetparams").as_bytes())
        .unwrap();
    f(file, level, strategy)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn gztell(file: gzFile) -> z_off_t {
    type Func = unsafe extern "C" fn(file: gzFile) -> z_off_t;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "gztell") }, "gztell").as_bytes())
        .unwrap();
    f(file)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn gzungetc(c: c_int, file: gzFile) -> c_int {
    type Func = unsafe extern "C" fn(c: c_int, file: gzFile) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "gzungetc") }, "gzungetc").as_bytes())
        .unwrap();
    f(c, file)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn gzwrite(file: gzFile, buf: voidpc, len: c_uint) -> c_int {
    type Func = unsafe extern "C" fn(file: gzFile, buf: voidpc, len: c_uint) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "gzwrite") }, "gzwrite").as_bytes())
        .unwrap();
    f(file, buf, len)
}
#[cfg(any(zng, feature = "libc"))]
pub unsafe fn uncompress(
    dest: *mut Bytef,
    destLen: *mut z_size,
    source: *const Bytef,
    sourceLen: z_size,
) -> c_int {
    type Func = unsafe extern "C" fn(
        dest: *mut Bytef,
        destLen: *mut z_size,
        source: *const Bytef,
        sourceLen: z_size,
    ) -> c_int;
    let f: libloading::Symbol<Func> = dynamic_library()
        .get(if_zng!({ concat!("zng_", "uncompress") }, "uncompress").as_bytes())
        .unwrap();
    f(dest, destLen, source, sourceLen)
}
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
pub const Z_NO_COMPRESSION: c_int = 0;
pub const Z_BEST_SPEED: c_int = 1;
pub const Z_BEST_COMPRESSION: c_int = 9;
pub const Z_DEFAULT_COMPRESSION: c_int = -1;
pub const Z_FILTERED: c_int = 1;
pub const Z_HUFFMAN_ONLY: c_int = 2;
pub const Z_RLE: c_int = 3;
pub const Z_FIXED: c_int = 4;
pub const Z_DEFAULT_STRATEGY: c_int = 0;
pub const Z_BINARY: c_int = 0;
pub const Z_TEXT: c_int = 1;
pub const Z_ASCII: c_int = Z_TEXT;
pub const Z_UNKNOWN: c_int = 2;
pub const Z_DEFLATED: c_int = 8;
pub(crate) fn dynamic_library() -> &'static libloading::Library {
    let path = match std::env::var("DYNAMIC_LIBZ_SYS") {
        Ok(path) => path,
        Err(e) => panic!("could not read env var DYNAMIC_LIBZ_SYS: {e}"),
    };
    use std::sync::OnceLock;
    static LIB: OnceLock<libloading::Library> = OnceLock::new();
    LIB.get_or_init(|| unsafe { libloading::Library::new(path) }.unwrap())
}
