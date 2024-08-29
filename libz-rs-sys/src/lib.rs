#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![cfg_attr(not(feature = "std"), no_std)]
#![doc = include_str!("../README.md")]

//! # Safety
//!
//! Most of the functions in this module are `unsafe fn`s, meaning that their behavior may be
//! undefined if certain assumptions are broken by the caller. In most cases, documentation
//! in this module refers to the safety assumptions of standard library functions.
//!
//! In most cases, pointers must be either `NULL` or satisfy the requirements of `&*ptr` or `&mut
//! *ptr`. This requirement maps to the requirements of [`pointer::as_ref`] and [`pointer::as_mut`]
//! for immutable and mutable pointers respectively.
//!
//! For pointer and length pairs, describing some sequence of elements in memory, the requirements
//! of [`core::slice::from_raw_parts`] or [`core::slice::from_raw_parts_mut`] apply. In some cases,
//! the element type `T` is converted into `MaybeUninit<T>`, meaning that while the slice must be
//! valid, the elements in the slice can be uninitialized. Using uninitialized buffers for output
//! is more performant.
//!
//! Finally, some functions accept a string argument, which must either be `NULL` or satisfy the
//! requirements of [`core::ffi::CStr::from_ptr`].
//!
//! [`pointer::as_ref`]: https://doc.rust-lang.org/core/primitive.pointer.html#method.as_ref
//! [`pointer::as_mut`]: https://doc.rust-lang.org/core/primitive.pointer.html#method.as_mut

use core::mem::MaybeUninit;

use core::ffi::{c_char, c_int, c_long, c_uchar, c_uint, c_ulong, c_void};

use zlib_rs::{
    deflate::{DeflateConfig, DeflateStream, Method, Strategy},
    inflate::{InflateConfig, InflateStream},
    DeflateFlush, InflateFlush, ReturnCode,
};

pub use zlib_rs::c_api::*;

#[cfg(feature = "custom-prefix")]
macro_rules! prefix {
    ($name:expr) => {
        concat!(env!("LIBZ_RS_SYS_PREFIX"), stringify!($name))
    };
}

#[cfg(all(
    not(feature = "custom-prefix"),
    not(any(test, feature = "testing-prefix"))
))]
macro_rules! prefix {
    ($name:expr) => {
        stringify!($name)
    };
}

#[cfg(all(not(feature = "custom-prefix"), any(test, feature = "testing-prefix")))]
macro_rules! prefix {
    ($name:expr) => {
        concat!("LIBZ_RS_SYS_TEST_", stringify!($name))
    };
}

#[cfg(all(feature = "rust-allocator", feature = "c-allocator"))]
const _: () =
    compile_error!("Only one of `rust-allocator` and `c-allocator` can be enabled at a time");

// In spirit this type is `libc::off_t`, but it would be our only libc dependency, and so we
// hardcode the type here. This should be correct on most operating systems. If we ever run into
// issues with it, we can either special-case or add a feature flag to force a particular width
pub type z_off_t = c_long;

/// Calculates the [crc32](https://en.wikipedia.org/wiki/Computation_of_cyclic_redundancy_checks#CRC-32_algorithm) checksum
/// of a sequence of bytes.
///
/// When the pointer argument is `NULL`, the initial checksum value is returned.
///
/// # Safety
///
/// The caller must guarantee that either:
///
/// - `buf` is `NULL`
/// - `buf` and `len` satisfy the requirements of [`core::slice::from_raw_parts`]
///
/// # Example
///
/// ```
/// use libz_rs_sys::crc32;
///
/// unsafe {
///     assert_eq!(crc32(0, core::ptr::null(), 0), 0);
///     assert_eq!(crc32(1, core::ptr::null(), 32), 0);
///
///     let input = [1,2,3];
///     assert_eq!(crc32(0, input.as_ptr(), input.len() as _), 1438416925);
/// }
/// ```
#[export_name = prefix!(crc32)]
pub unsafe extern "C-unwind" fn crc32(crc: c_ulong, buf: *const Bytef, len: uInt) -> c_ulong {
    match unsafe { slice_from_raw_parts(buf, len as usize) } {
        Some(buf) => zlib_rs::crc32(crc as u32, buf) as c_ulong,
        None => 0,
    }
}

/// Combines the checksum of two slices into one.
///
/// The combined value is equivalent to calculating the checksum of the whole input.
///
/// This function can be used when input arrives in chunks, or when different threads
/// calculate the checksum of different sections of the input.
///
/// # Example
///
/// ```
/// use libz_rs_sys::{crc32, crc32_combine};
///
/// let input = [1, 2, 3, 4, 5, 6, 7, 8];
/// let lo = &input[..4];
/// let hi = &input[4..];
///
/// unsafe {
///     let full = crc32(0, input.as_ptr(), input.len() as _);
///
///     let crc1 = crc32(0, lo.as_ptr(), lo.len() as _);
///     let crc2 = crc32(0, hi.as_ptr(), hi.len() as _);
///
///     let combined = crc32_combine(crc1, crc2, hi.len() as _);
///
///     assert_eq!(full, combined);
/// }
/// ```
#[export_name = prefix!(crc32_combine)]
pub extern "C-unwind" fn crc32_combine(crc1: c_ulong, crc2: c_ulong, len2: z_off_t) -> c_ulong {
    zlib_rs::crc32_combine(crc1 as u32, crc2 as u32, len2 as u64) as c_ulong
}

/// Calculates the [adler32](https://en.wikipedia.org/wiki/Adler-32) checksum
/// of a sequence of bytes.
///
/// When the pointer argument is `NULL`, the initial checksum value is returned.
///
/// # Safety
///
/// The caller must guarantee that either:
///
/// - `buf` is `NULL`
/// - `buf` and `len` satisfy the requirements of [`core::slice::from_raw_parts`]
///
/// # Example
///
/// ```
/// use libz_rs_sys::adler32;
///
/// unsafe {
///     assert_eq!(adler32(0, core::ptr::null(), 0), 1);
///     assert_eq!(adler32(1, core::ptr::null(), 32), 1);
///
///     let input = [1,2,3];
///     assert_eq!(adler32(0, input.as_ptr(), input.len() as _), 655366);
/// }
/// ```
#[export_name = prefix!(adler32)]
pub unsafe extern "C-unwind" fn adler32(adler: c_ulong, buf: *const Bytef, len: uInt) -> c_ulong {
    match unsafe { slice_from_raw_parts(buf, len as usize) } {
        Some(buf) => zlib_rs::adler32(adler as u32, buf) as c_ulong,
        None => 1,
    }
}

/// Combines the checksum of two slices into one.
///
/// The combined value is equivalent to calculating the checksum of the whole input.
///
/// This function can be used when input arrives in chunks, or when different threads
/// calculate the checksum of different sections of the input.
///
/// # Example
///
/// ```
/// use libz_rs_sys::{adler32, adler32_combine};
///
/// let input = [1, 2, 3, 4, 5, 6, 7, 8];
/// let lo = &input[..4];
/// let hi = &input[4..];
///
/// unsafe {
///     let full = adler32(1, input.as_ptr(), input.len() as _);
///
///     let adler1 = adler32(1, lo.as_ptr(), lo.len() as _);
///     let adler2 = adler32(1, hi.as_ptr(), hi.len() as _);
///
///     let combined = adler32_combine(adler1, adler2, hi.len() as _);
///
///     assert_eq!(full, combined);
/// }
/// ```
#[export_name = prefix!(adler32_combine)]
pub extern "C-unwind" fn adler32_combine(
    adler1: c_ulong,
    adler2: c_ulong,
    len2: z_off_t,
) -> c_ulong {
    match u64::try_from(len2) {
        Ok(len2) => zlib_rs::adler32_combine(adler1 as u32, adler2 as u32, len2) as c_ulong,
        Err(_) => {
            // for negative len, return invalid adler32 as a clue for debugging
            0xFFFF_FFFF
        }
    }
}

/// Inflates `source` into `dest`, and writes the final inflated size into `destLen`.
///
/// Upon entry, `destLen` is the total size of the destination buffer, which must be large enough to hold the entire
/// uncompressed data. (The size of the uncompressed data must have been saved previously by the compressor and
/// transmitted to the decompressor by some mechanism outside the scope of this compression library.)
/// Upon exit, `destLen` is the actual size of the uncompressed data.
///
/// # Returns
///
/// * [`Z_OK`] if success
/// * [`Z_MEM_ERROR`] if there was not enough memory
/// * [`Z_BUF_ERROR`] if there was not enough room in the output buffer
/// * [`Z_DATA_ERROR`] if the input data was corrupted or incomplete
///
/// In the case where there is not enough room, [`uncompress`] will fill the output buffer with the uncompressed data up to that point.
///
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `destLen` is `NULL`
///     - `destLen` satisfies the requirements of `&mut *destLen`
/// * Either
///     - `dest` is `NULL`
///     - `dest` and `*destLen` satisfy the requirements of [`core::slice::from_raw_parts_mut::<MaybeUninit<u8>>`]
/// * Either
///     - `source` is `NULL`
///     - `source` and `sourceLen` satisfy the requirements of [`core::slice::from_raw_parts::<u8>`]
///
/// # Example
///
/// ```
/// use libz_rs_sys::{Z_OK, uncompress};
///
/// let source = [120, 156, 115, 75, 45, 42, 202, 44, 6, 0, 8, 6, 2, 108];
///
/// let mut dest = vec![0u8; 100];
/// let mut dest_len = dest.len() as _;
///
/// let err = unsafe {
///     uncompress(
///         dest.as_mut_ptr(),
///         &mut dest_len,
///         source.as_ptr(),
///         source.len() as _,
///     )
/// };
///
/// assert_eq!(err, Z_OK);
/// assert_eq!(dest_len, 6);
///
/// dest.truncate(dest_len as usize);
/// assert_eq!(dest, b"Ferris");
/// ```
#[export_name = prefix!(uncompress)]
pub unsafe extern "C-unwind" fn uncompress(
    dest: *mut u8,
    destLen: *mut c_ulong,
    source: *const u8,
    sourceLen: c_ulong,
) -> c_int {
    // stock zlib will just dereference a NULL pointer: that's UB.
    // Hence us returning an error value is compatible
    let Some(destLen) = (unsafe { destLen.as_mut() }) else {
        return ReturnCode::StreamError as _;
    };

    let Some(output) = (unsafe { slice_from_raw_parts_uninit_mut(dest, *destLen as usize) }) else {
        return ReturnCode::StreamError as _;
    };

    let Some(input) = (unsafe { slice_from_raw_parts(source, sourceLen as usize) }) else {
        return ReturnCode::StreamError as _;
    };

    let config = InflateConfig::default();
    let (output, err) = zlib_rs::inflate::uncompress(output, input, config);

    *destLen = output.len() as c_ulong;

    err as c_int
}

/// Decompresses as much data as possible, and stops when the input buffer becomes empty or the output buffer becomes full.
///
/// # Returns
///
/// - [`Z_OK`] if success
/// - [`Z_STREAM_END`] if the end of the compressed data has been reached and all uncompressed output has been produced
/// - [`Z_NEED_DICT`] if a preset dictionary is needed at this point
/// - [`Z_STREAM_ERROR`] if the stream state was inconsistent
/// - [`Z_DATA_ERROR`] if the input data was corrupted
/// - [`Z_MEM_ERROR`] if there was not enough memory
/// - [`Z_BUF_ERROR`] if no progress was possible or if there was not enough room in the output buffer when [`Z_FINISH`] is used
///
/// Note that [`Z_BUF_ERROR`] is not fatal, and [`inflate`] can be called again with more input and more output space to continue decompressing.
/// If [`Z_DATA_ERROR`] is returned, the application may then call [`inflateSync`] to look for a good compression block if a partial recovery of the data is to be attempted.
///
/// # Safety
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`inflateInit_`] or similar
#[export_name = prefix!(inflate)]
pub unsafe extern "C-unwind" fn inflate(strm: *mut z_stream, flush: i32) -> i32 {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        let flush = InflateFlush::try_from(flush).unwrap_or_default();
        zlib_rs::inflate::inflate(stream, flush) as _
    } else {
        ReturnCode::StreamError as _
    }
}

/// Deallocates all dynamically allocated data structures for this stream.
///
/// This function discards any unprocessed input and does not flush any pending output.
///
/// # Returns
///
/// - [`Z_OK`] if success
/// - [`Z_STREAM_ERROR`] if the stream state was inconsistent
///
/// # Safety
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`inflateInit_`] or similar
#[export_name = prefix!(inflateEnd)]
pub unsafe extern "C-unwind" fn inflateEnd(strm: *mut z_stream) -> i32 {
    match InflateStream::from_stream_mut(strm) {
        Some(stream) => {
            zlib_rs::inflate::end(stream);
            ReturnCode::Ok as _
        }
        None => ReturnCode::StreamError as _,
    }
}

/// Initializes the state for decompression
///
/// # Returns
///
/// - [`Z_OK`] if success
/// - [`Z_MEM_ERROR`] if there was not enough memory
/// - [`Z_VERSION_ERROR`] if the zlib library version is incompatible with the version assumed by the caller
/// - [`Z_STREAM_ERROR`] if a parameter is invalid, such as a null pointer to the structure
///
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm`
/// * Either
///     - `version` is NULL
///     - `version` satisfies the requirements of [`core::ffi::CStr::from_ptr`]
/// * If `strm` is not `NULL`, the following fields contain valid values
///     - `zalloc`
///     - `zfree`
///     - `opaque`
#[export_name = prefix!(inflateBackInit_)]
pub unsafe extern "C-unwind" fn inflateBackInit_(
    _strm: z_streamp,
    _windowBits: c_int,
    _window: *mut c_uchar,
    _version: *const c_char,
    _stream_size: c_int,
) -> c_int {
    todo!("inflateBack is not implemented yet")
}

/// Decompresses as much data as possible, and stops when the input buffer becomes empty or the output buffer becomes full.
///
/// ## Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`inflateBackInit_`]
#[export_name = prefix!(inflateBack)]
pub unsafe extern "C-unwind" fn inflateBack(
    _strm: z_streamp,
    _in: in_func,
    _in_desc: *mut c_void,
    _out: out_func,
    _out_desc: *mut c_void,
) -> c_int {
    todo!("inflateBack is not implemented yet")
}

/// Deallocates all dynamically allocated data structures for this stream.
///
/// This function discards any unprocessed input and does not flush any pending output.
///
/// ## Returns
///
/// - [`Z_OK`] if success
/// - [`Z_STREAM_ERROR`] if the stream state was inconsistent
///
/// ## Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`inflateBackInit_`]
#[export_name = prefix!(inflateBackEnd)]
pub unsafe extern "C-unwind" fn inflateBackEnd(_strm: z_streamp) -> c_int {
    todo!("inflateBack is not implemented yet")
}

/// Sets the destination stream as a complete copy of the source stream.
///
/// This function can be useful when randomly accessing a large stream.
/// The first pass through the stream can periodically record the inflate state,
/// allowing restarting inflate at those points when randomly accessing the stream.
///
/// # Returns
///
/// - [`Z_OK`] if success
/// - [`Z_MEM_ERROR`] if there was not enough memory
/// - [`Z_STREAM_ERROR`] if the source stream state was inconsistent (such as zalloc being NULL)
///
/// The `msg` field is left unchanged in both source and destination.
///
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `dest` is `NULL`
///     - `dest` satisfies the requirements of `&mut *(dest as *mut MaybeUninit<z_stream>)`
/// * Either
///     - `source` is `NULL`
///     - `source` satisfies the requirements of `&mut *strm` and was initialized with [`inflateInit_`] or similar
#[export_name = prefix!(inflateCopy)]
pub unsafe extern "C-unwind" fn inflateCopy(dest: *mut z_stream, source: *const z_stream) -> i32 {
    let Some(dest) = (unsafe { dest.cast::<MaybeUninit<InflateStream>>().as_mut() }) else {
        return ReturnCode::StreamError as _;
    };

    let Some(source) = (unsafe { InflateStream::from_stream_ref(source) }) else {
        return ReturnCode::StreamError as _;
    };

    zlib_rs::inflate::copy(dest, source) as _
}

/// Gives information about the current location of the input stream.
///
/// This function marks locations in the input data for random access, which may be at bit positions, and notes those cases where the output of a code may span boundaries of random access blocks. The current location in the input stream can be determined from `avail_in` and `data_type` as noted in the description for the [`Z_BLOCK`] flush parameter for [`inflate`].
///
/// A code is being processed if [`inflate`] is waiting for more input to complete decoding of the code, or if it has completed decoding but is waiting for more output space to write the literal or match data.
///
/// # Returns
///
/// This function returns two values, one in the lower 16 bits of the return value, and the other in the remaining upper bits, obtained by shifting the return value down 16 bits.
///
/// - If the upper value is `-1` and the lower value is zero, then [`inflate`] is currently decoding information outside of a block.
/// - If the upper value is `-1` and the lower value is non-zero, then [`inflate`] is in the middle of a stored block, with the lower value equaling the number of bytes from the input remaining to copy.
/// - If the upper value is not `-1`, then it is the number of bits back from the current bit position in the input of the code (literal or length/distance pair) currently being processed. In that case the lower value is the number of bytes already emitted for that code.
/// - `-65536` if the provided source stream state was inconsistent.
///
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`inflateInit_`] or similar
#[export_name = prefix!(inflateMark)]
pub unsafe extern "C-unwind" fn inflateMark(strm: *const z_stream) -> c_long {
    if let Some(stream) = InflateStream::from_stream_ref(strm) {
        zlib_rs::inflate::mark(stream)
    } else {
        -65536
    }
}

/// Skips invalid compressed data until
///
/// Skip invalid compressed data until a possible full flush point (see the description of deflate with [`Z_FULL_FLUSH`]) can be found,
/// or until all available input is skipped. No output is provided.
///
/// [`inflateSync`] searches for a `00 00 FF FF` pattern in the compressed data.
/// All full flush points have this pattern, but not all occurrences of this pattern are full flush points.
///
/// # Returns
///
/// - [`Z_OK`] if a possible full flush point has been found
/// - [`Z_BUF_ERROR`] if no more input was provided
/// - [`Z_DATA_ERROR`] if no flush point has been found
/// - [`Z_STREAM_ERROR`] if the stream structure was inconsistent
///
/// In the success case, the application may save the current value of `total_in` which indicates where valid compressed data was found.
/// In the error case, the application may repeatedly call [`inflateSync`], providing more input each time, until success or end of the input data.
///
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`inflateInit_`] or similar
#[export_name = prefix!(inflateSync)]
pub unsafe extern "C-unwind" fn inflateSync(strm: *mut z_stream) -> i32 {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        zlib_rs::inflate::sync(stream) as _
    } else {
        ReturnCode::StreamError as _
    }
}

#[doc(hidden)]
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`inflateInit_`] or similar
#[export_name = prefix!(inflateSyncPoint)]
pub unsafe extern "C-unwind" fn inflateSyncPoint(strm: *mut z_stream) -> i32 {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        zlib_rs::inflate::sync_point(stream) as i32
    } else {
        ReturnCode::StreamError as _
    }
}

/// Initializes the state for decompression
///
/// A call to [`inflateInit_`] is equivalent to [`inflateInit2_`] where `windowBits` is 15.
///
/// # Returns
///
/// - [`Z_OK`] if success
/// - [`Z_MEM_ERROR`] if there was not enough memory
/// - [`Z_VERSION_ERROR`] if the zlib library version is incompatible with the version assumed by the caller
/// - [`Z_STREAM_ERROR`] if a parameter is invalid, such as a null pointer to the structure
///
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm`
/// * Either
///     - `version` is NULL
///     - `version` satisfies the requirements of [`core::ffi::CStr::from_ptr`]
/// * If `strm` is not `NULL`, the following fields contain valid values
///     - `zalloc`
///     - `zfree`
///     - `opaque`
#[export_name = prefix!(inflateInit_)]
pub unsafe extern "C-unwind" fn inflateInit_(
    strm: z_streamp,
    version: *const c_char,
    stream_size: c_int,
) -> c_int {
    let config = InflateConfig::default();
    unsafe { inflateInit2_(strm, config.window_bits, version, stream_size) }
}

/// Initializes the state for decompression
///
/// # Returns
///
/// - [`Z_OK`] if success
/// - [`Z_MEM_ERROR`] if there was not enough memory
/// - [`Z_VERSION_ERROR`] if the zlib library version is incompatible with the version assumed by the caller
/// - [`Z_STREAM_ERROR`] if a parameter is invalid, such as a null pointer to the structure
///
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm`
/// * Either
///     - `version` is NULL
///     - `version` satisfies the requirements of [`core::ffi::CStr::from_ptr`]
/// * If `strm` is not `NULL`, the following fields contain valid values
///     - `zalloc`
///     - `zfree`
///     - `opaque`
#[export_name = prefix!(inflateInit2_)]
pub unsafe extern "C-unwind" fn inflateInit2_(
    strm: z_streamp,
    windowBits: c_int,
    version: *const c_char,
    stream_size: c_int,
) -> c_int {
    if !is_version_compatible(version, stream_size) {
        ReturnCode::VersionError as _
    } else {
        inflateInit2(strm, windowBits)
    }
}

/// Helper that implements the actual initialization logic
///
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm`
/// * If `strm` is not `NULL`, the following fields contain valid values
///     - `zalloc`
///     - `zfree`
///     - `opaque`
unsafe extern "C-unwind" fn inflateInit2(strm: z_streamp, windowBits: c_int) -> c_int {
    let Some(strm) = (unsafe { strm.as_mut() }) else {
        return ReturnCode::StreamError as _;
    };

    let config = InflateConfig {
        window_bits: windowBits,
    };

    zlib_rs::inflate::init(strm, config) as _
}

/// Inserts bits in the inflate input stream.
///
/// The intent is that this function is used to start inflating at a bit position in the middle of a byte.
/// The provided bits will be used before any bytes are used from next_in.
/// This function should only be used with raw inflate, and should be used before the first [`inflate`] call after [`inflateInit2_`] or [`inflateReset`].
/// bits must be less than or equal to 16, and that many of the least significant bits of value will be inserted in the input.
///
/// If bits is negative, then the input stream bit buffer is emptied. Then [`inflatePrime`] can be called again to put bits in the buffer.
/// This is used to clear out bits leftover after feeding inflate a block description prior to feeding inflate codes.
///
/// # Returns
///
/// - [`Z_OK`] if success
/// - [`Z_STREAM_ERROR`] if the source stream state was inconsistent
///
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`inflateInit_`] or similar
#[export_name = prefix!(inflatePrime)]
pub unsafe extern "C-unwind" fn inflatePrime(strm: *mut z_stream, bits: i32, value: i32) -> i32 {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        zlib_rs::inflate::prime(stream, bits, value) as _
    } else {
        ReturnCode::StreamError as _
    }
}

/// Equivalent to [`inflateEnd`] followed by [`inflateInit_`], but does not free and reallocate the internal decompression state.
///
/// The stream will keep attributes that may have been set by [`inflateInit2_`].
/// The stream's `total_in`, `total_out`, `adler`, and `msg` fields are initialized.
///
/// # Returns
///
/// - [`Z_OK`] if success
/// - [`Z_STREAM_ERROR`] if the source stream state was inconsistent
///
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`inflateInit_`] or similar
#[export_name = prefix!(inflateReset)]
pub unsafe extern "C-unwind" fn inflateReset(strm: *mut z_stream) -> i32 {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        zlib_rs::inflate::reset(stream) as _
    } else {
        ReturnCode::StreamError as _
    }
}

/// This function is the same as [`inflateReset`], but it also permits changing the wrap and window size requests.
///
/// The `windowBits` parameter is interpreted the same as it is for [`inflateInit2_`].
/// If the window size is changed, then the memory allocated for the window is freed, and the window will be reallocated by [`inflate`] if needed.
///
/// # Returns
///
/// - [`Z_OK`] if success
/// - [`Z_STREAM_ERROR`] if the source stream state was inconsistent, or if the `windowBits`
///     parameter is invalid
///
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`inflateInit_`] or similar
#[export_name = prefix!(inflateReset2)]
pub unsafe extern "C-unwind" fn inflateReset2(strm: *mut z_stream, windowBits: c_int) -> i32 {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        let config = InflateConfig {
            window_bits: windowBits,
        };
        zlib_rs::inflate::reset_with_config(stream, config) as _
    } else {
        ReturnCode::StreamError as _
    }
}

/// Initializes the decompression dictionary from the given uncompressed byte sequence.
///
/// This function must be called immediately after a call of [`inflate`], if that call returned [`Z_NEED_DICT`].
/// The dictionary chosen by the compressor can be determined from the Adler-32 value returned by that call of inflate.
/// The compressor and decompressor must use exactly the same dictionary (see [`deflateSetDictionary`]).
/// For raw inflate, this function can be called at any time to set the dictionary.
/// If the provided dictionary is smaller than the window and there is already data in the window, then the provided dictionary will amend what's there.
/// The application must insure that the same dictionary that was used for compression is provided.
///
/// [`inflateSetDictionary`] does not perform any decompression: this will be done by subsequent calls of [`inflate`].
///
/// # Returns
///
/// - [`Z_OK`] if success
/// - [`Z_STREAM_ERROR`] if the source stream state was inconsistent or `dictionary` is `NULL`
/// - [`Z_DATA_ERROR`] if the given dictionary doesn't match the expected one (i.e. it has an incorrect Adler-32 value).
///
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`inflateInit_`] or similar
/// * Either
///     - `dictionary` is `NULL`
///     - `dictionary` and `dictLength` satisfy the requirements of [`core::slice::from_raw_parts_mut::<u8>`]
#[export_name = prefix!(inflateSetDictionary)]
pub unsafe extern "C-unwind" fn inflateSetDictionary(
    strm: *mut z_stream,
    dictionary: *const u8,
    dictLength: c_uint,
) -> c_int {
    let Some(stream) = InflateStream::from_stream_mut(strm) else {
        return ReturnCode::StreamError as _;
    };

    let dict = match dictLength {
        0 => &[],
        _ => unsafe { slice_from_raw_parts(dictionary, dictLength as usize) }.unwrap_or(&[]),
    };

    zlib_rs::inflate::set_dictionary(stream, dict) as _
}

/// Requests that gzip header information be stored in the provided [`gz_header`] structure.
///
/// The [`inflateGetHeader`] function may be called after [`inflateInit2_`] or [`inflateReset`], and before the first call of [`inflate`].
/// As [`inflate`] processes the gzip stream, `head.done` is zero until the header is completed, at which time `head.done` is set to one.
/// If a zlib stream is being decoded, then `head.done` is set to `-1` to indicate that there will be no gzip header information forthcoming.
/// Note that [`Z_BLOCK`] can be used to force [`inflate`] to return immediately after header processing is complete and before any actual data is decompressed.
///
/// - The `text`, `time`, `xflags`, and `os` fields are filled in with the gzip header contents.
/// - `hcrc` is set to true if there is a header CRC. (The header CRC was valid if done is set to one.)
/// - If `extra` is not `NULL`, then `extra_max` contains the maximum number of bytes to write to extra.
///     Once `done` is `true`, `extra_len` contains the actual extra field length,
///     and `extra` contains the extra field, or that field truncated if `extra_max` is less than `extra_len`.
/// - If `name` is not `NULL`, then up to `name_max` characters are written there, terminated with a zero unless the length is greater than `name_max`.
/// - If `comment` is not `NULL`, then up to `comm_max` characters are written there, terminated with a zero unless the length is greater than `comm_max`.
///
/// When any of `extra`, `name`, or `comment` are not `NULL` and the respective field is not present in the header, then that field is set to `NULL` to signal its absence.
/// This allows the use of [`deflateSetHeader`] with the returned structure to duplicate the header. However if those fields are set to allocated memory,
/// then the application will need to save those pointers elsewhere so that they can be eventually freed.
///
/// If [`inflateGetHeader`] is not used, then the header information is simply discarded. The header is always checked for validity, including the header CRC if present.
/// [`inflateReset`] will reset the process to discard the header information.
/// The application would need to call [`inflateGetHeader`] again to retrieve the header from the next gzip stream.
///
/// # Returns
///
/// - [`Z_OK`] if success
/// - [`Z_STREAM_ERROR`] if the source stream state was inconsistent (such as zalloc being NULL)
///
/// # Safety
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`inflateInit_`] or similar
/// * Either
///     - `head` is `NULL`
///     - `head` satisfies the requirements of `&mut *head`
/// * If `head` is not `NULL`:
///     - if `head.extra` is not NULL, it must be writable for at least `head.extra_max` bytes
///     - if `head.name` is not NULL, it must be writable for at least `head.name_max` bytes
///     - if `head.comment` is not NULL, it must be writable for at least `head.comm_max` bytes
#[export_name = prefix!(inflateGetHeader)]
pub unsafe extern "C-unwind" fn inflateGetHeader(strm: z_streamp, head: gz_headerp) -> c_int {
    let Some(stream) = (unsafe { InflateStream::from_stream_mut(strm) }) else {
        return ReturnCode::StreamError as _;
    };

    // SAFETY: the caller guarantees the safety of `&mut *`
    let header = unsafe { head.as_mut() };

    zlib_rs::inflate::get_header(stream, header) as i32
}

#[doc(hidden)]
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`inflateInit_`] or similar
#[export_name = prefix!(inflateUndermine)]
pub unsafe extern "C-unwind" fn inflateUndermine(strm: *mut z_stream, subvert: i32) -> c_int {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        zlib_rs::inflate::undermine(stream, subvert) as i32
    } else {
        ReturnCode::StreamError as _
    }
}

#[doc(hidden)]
/// ## Safety
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`inflateInit_`] or similar
#[export_name = prefix!(inflateResetKeep)]
pub unsafe extern "C-unwind" fn inflateResetKeep(strm: *mut z_stream) -> c_int {
    if let Some(stream) = InflateStream::from_stream_mut(strm) {
        zlib_rs::inflate::reset_keep(stream) as _
    } else {
        ReturnCode::StreamError as _
    }
}

// undocumented but exposed function
#[doc(hidden)]
/// Returns the number of codes used
///
/// # Safety
///
/// The caller must guarantee that either:
///
/// - `buf` is `NULL`
/// - `buf` and `len` satisfy the requirements of [`core::slice::from_raw_parts`]
#[export_name = prefix!(inflateCodesUsed)]
pub unsafe extern "C-unwind" fn inflateCodesUsed(_strm: *mut z_stream) -> c_ulong {
    todo!()
}

/// Compresses as much data as possible, and stops when the input buffer becomes empty or the output buffer becomes full.
///
/// # Returns
///
/// - [`Z_OK`] if success
/// - [`Z_STREAM_END`] if the end of the compressed data has been reached and all uncompressed output has been produced
/// - [`Z_STREAM_ERROR`] if the stream state was inconsistent
/// - [`Z_BUF_ERROR`] if no progress was possible or if there was not enough room in the output buffer when [`Z_FINISH`] is used
///
/// Note that [`Z_BUF_ERROR`] is not fatal, and [`deflate`] can be called again with more input and more output space to continue decompressing.
///
/// # Safety
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`deflateInit_`] or similar
#[export_name = prefix!(deflate)]
pub unsafe extern "C-unwind" fn deflate(strm: *mut z_stream, flush: i32) -> c_int {
    if let Some(stream) = DeflateStream::from_stream_mut(strm) {
        match DeflateFlush::try_from(flush) {
            Ok(flush) => zlib_rs::deflate::deflate(stream, flush) as _,
            Err(()) => ReturnCode::StreamError as _,
        }
    } else {
        ReturnCode::StreamError as _
    }
}

/// Provides gzip header information for when a gzip stream is requested by [`deflateInit2_`].
///
/// [`deflateSetHeader`] may be called after [`deflateInit2_`] or [`deflateReset`]) and before the first call of [`deflate`]. The header's `text`, `time`, `os`, `extra`, `name`, and `comment` fields in the provided [`gz_header`] structure are written to the gzip header (xflag is ignored — the extra flags are set according to the compression level).
///
/// The caller must assure that, if not `NULL`, `name` and `comment` are terminated with a zero byte, and that if `extra` is not NULL, that `extra_len` bytes are available there.
/// If `hcrc` is true, a gzip header crc is included.
///
/// If [`deflateSetHeader`] is not used, the default gzip header has text false, the time set to zero, and os set to the current operating system, with no extra, name, or comment fields. The gzip header is returned to the default state by [`deflateReset`].
///
/// # Returns
///
/// - [`Z_OK`] if success
/// - [`Z_STREAM_ERROR`] if the stream state was inconsistent
///
/// # Safety
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`deflateInit_`] or similar
/// * Either
///     - `head` is `NULL`
///     - `head` satisfies the requirements of `&mut *head`
#[export_name = prefix!(deflateSetHeader)]
pub unsafe extern "C-unwind" fn deflateSetHeader(strm: *mut z_stream, head: gz_headerp) -> c_int {
    let Some(stream) = (unsafe { DeflateStream::from_stream_mut(strm) }) else {
        return ReturnCode::StreamError as _;
    };

    let header = unsafe { head.as_mut() };

    zlib_rs::deflate::set_header(stream, header) as _
}

/// Returns an upper bound on the compressed size after deflation of `sourceLen` bytes.
///
/// This function must be called after [`deflateInit_`] or [`deflateInit2_`].
/// This would be used to allocate an output buffer for deflation in a single pass, and so would be called before [`deflate`].
/// If that first [`deflate`] call is provided the `sourceLen` input bytes, an output buffer allocated to the size returned by [`deflateBound`],
/// and the flush value [`Z_FINISH`], then [`deflate`] is guaranteed to return [`Z_STREAM_END`].
///
/// Note that it is possible for the compressed size to be larger than the value returned by [`deflateBound`]
/// if flush options other than [`Z_FINISH`] or [`Z_NO_FLUSH`] are used.
///
/// ## Safety
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`deflateInit_`] or similar
#[export_name = prefix!(deflateBound)]
pub unsafe extern "C-unwind" fn deflateBound(strm: *mut z_stream, sourceLen: c_ulong) -> c_ulong {
    zlib_rs::deflate::bound(DeflateStream::from_stream_mut(strm), sourceLen as usize) as c_ulong
}

/// Compresses `source` into `dest`, and writes the final deflated size into `destLen`.
///
///`sourceLen` is the byte length of the source buffer.
/// Upon entry, `destLen` is the total size of the destination buffer,
/// which must be at least the value returned by [`compressBound`]`(sourceLen)`.
/// Upon exit, `destLen` is the actual size of the compressed data.
///
/// A call to [`compress`] is equivalent to [`compress2`] with a level parameter of [`Z_DEFAULT_COMPRESSION`].
///
/// # Returns
///
/// * [`Z_OK`] if success
/// * [`Z_MEM_ERROR`] if there was not enough memory
/// * [`Z_BUF_ERROR`] if there was not enough room in the output buffer
///
/// # Safety
///
/// The caller must guarantee that
///
/// * The `destLen` pointer satisfies the requirements of [`core::ptr::read`]
/// * Either
///     - `dest` is `NULL`
///     - `dest` and `*destLen` satisfy the requirements of [`core::slice::from_raw_parts_mut::<MaybeUninit<u8>>`]
/// * Either
///     - `source` is `NULL`
///     - `source` and `sourceLen` satisfy the requirements of [`core::slice::from_raw_parts`]
///
/// # Example
///
/// ```
/// use libz_rs_sys::{Z_OK, compress};
///
/// let source = b"Ferris";
///
/// let mut dest = vec![0u8; 100];
/// let mut dest_len = dest.len() as _;
///
/// let err = unsafe {
///     compress(
///         dest.as_mut_ptr(),
///         &mut dest_len,
///         source.as_ptr(),
///         source.len() as _,
///     )
/// };
///
/// assert_eq!(err, Z_OK);
/// assert_eq!(dest_len, 14);
///
/// dest.truncate(dest_len as usize);
/// assert_eq!(dest, [120, 156, 115, 75, 45, 42, 202, 44, 6, 0, 8, 6, 2, 108]);
/// ```
#[export_name = prefix!(compress)]
pub unsafe extern "C-unwind" fn compress(
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

/// Compresses `source` into `dest`, and writes the final deflated size into `destLen`.
///
/// The level parameter has the same meaning as in [`deflateInit_`].
/// `sourceLen` is the byte length of the source buffer.
/// Upon entry, `destLen` is the total size of the destination buffer,
/// which must be at least the value returned by [`compressBound`]`(sourceLen)`.
/// Upon exit, `destLen` is the actual size of the compressed data.
///
/// # Returns
///
/// * [`Z_OK`] if success
/// * [`Z_MEM_ERROR`] if there was not enough memory
/// * [`Z_BUF_ERROR`] if there was not enough room in the output buffer
///
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `destLen` is `NULL`
///     - `destLen` satisfies the requirements of `&mut *destLen`
/// * Either
///     - `dest` is `NULL`
///     - `dest` and `*destLen` satisfy the requirements of [`core::slice::from_raw_parts_mut::<MaybeUninit<u8>>`]
/// * Either
///     - `source` is `NULL`
///     - `source` and `sourceLen` satisfy the requirements of [`core::slice::from_raw_parts`]
#[export_name = prefix!(compress2)]
pub unsafe extern "C-unwind" fn compress2(
    dest: *mut Bytef,
    destLen: *mut c_ulong,
    source: *const Bytef,
    sourceLen: c_ulong,
    level: c_int,
) -> c_int {
    // stock zlib will just dereference a NULL pointer: that's UB.
    // Hence us returning an error value is compatible
    let Some(destLen) = (unsafe { destLen.as_mut() }) else {
        return ReturnCode::StreamError as _;
    };

    let Some(output) = (unsafe { slice_from_raw_parts_uninit_mut(dest, *destLen as usize) }) else {
        return ReturnCode::StreamError as _;
    };

    let Some(input) = (unsafe { slice_from_raw_parts(source, sourceLen as usize) }) else {
        return ReturnCode::StreamError as _;
    };

    let config = DeflateConfig::new(level);
    let (output, err) = zlib_rs::deflate::compress(output, input, config);

    *destLen = output.len() as c_ulong;

    err as c_int
}

/// Returns an upper bound on the compressed size after [`compress`] or [`compress2`] on `sourceLen` bytes.
///
/// Can be used before a [`compress`] or [`compress2`] call to allocate the destination buffer.
#[export_name = prefix!(compressBound)]
pub extern "C-unwind" fn compressBound(sourceLen: c_ulong) -> c_ulong {
    zlib_rs::deflate::compress_bound(sourceLen as usize) as c_ulong
}

/// Deallocates all dynamically allocated data structures for this stream.
///
/// This function discards any unprocessed input and does not flush any pending output.
///
/// # Returns
///
/// - [`Z_OK`] if success
/// - [`Z_STREAM_ERROR`] if the stream state was inconsistent
/// - [`Z_DATA_ERROR`] if the stream was freed prematurely (some input or output was discarded)
///
/// In the error case, `strm.msg` may be set but then points to a static string (which must not be deallocated).
///
/// # Safety
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`deflateInit_`] or similar
#[export_name = prefix!(deflateEnd)]
pub unsafe extern "C-unwind" fn deflateEnd(strm: *mut z_stream) -> i32 {
    match DeflateStream::from_stream_mut(strm) {
        Some(stream) => match zlib_rs::deflate::end(stream) {
            Ok(_) => ReturnCode::Ok as _,
            Err(_) => ReturnCode::DataError as _,
        },
        None => ReturnCode::StreamError as _,
    }
}

/// This function is equivalent to [`deflateEnd`] followed by [`deflateInit_`], but does not free and reallocate the internal compression state.
///
/// This function will leave the compression level and any other attributes that may have been set unchanged.
/// The stream's `total_in`, `total_out`, `adler`, and `msg` fields are initialized.
///
/// ## Returns
///
/// - [`Z_OK`] if success
/// - [`Z_STREAM_ERROR`] if the stream state was inconsistent
///
/// ## Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`deflateInit_`] or similar
#[export_name = prefix!(deflateReset)]
pub unsafe extern "C-unwind" fn deflateReset(strm: *mut z_stream) -> i32 {
    match DeflateStream::from_stream_mut(strm) {
        Some(stream) => zlib_rs::deflate::reset(stream) as _,
        None => ReturnCode::StreamError as _,
    }
}

/// Dynamically update the compression level and compression strategy.
///
/// This can be used to switch between compression and straight copy of the input data,
/// or to switch to a different kind of input data requiring a different strategy.
///
/// The interpretation of level and strategy is as in [`deflateInit2_`].
///
/// # Returns
///
/// - [`Z_OK`] if success
/// - [`Z_STREAM_ERROR`] if the stream state was inconsistent or if a parameter was invalid
/// - [`Z_BUF_ERROR`] if there was not enough output space to complete the compression of the available input data before a change in the strategy or approach.
///
/// Note that in the case of a [`Z_BUF_ERROR`], the parameters are not changed.
/// A return value of [`Z_BUF_ERROR`] is not fatal, in which case [`deflateParams`] can be retried with more output space.
///
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`deflateInit_`] or similar
#[export_name = prefix!(deflateParams)]
pub unsafe extern "C-unwind" fn deflateParams(
    strm: z_streamp,
    level: c_int,
    strategy: c_int,
) -> c_int {
    let Ok(strategy) = Strategy::try_from(strategy) else {
        return ReturnCode::StreamError as _;
    };

    match DeflateStream::from_stream_mut(strm) {
        Some(stream) => zlib_rs::deflate::params(stream, level, strategy) as _,
        None => ReturnCode::StreamError as _,
    }
}

/// Initializes the compression dictionary from the given byte sequence without producing any compressed output.
///
/// This function may be called after [`deflateInit_`], [`deflateInit2_`] or [`deflateReset`]) and before the first call of [`deflate`].
///
/// # Returns
///
/// - [`Z_OK`] if success
/// - [`Z_STREAM_ERROR`] if the stream state was inconsistent
///
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`deflateInit_`] or similar
/// * Either
///     - `dictionary` is `NULL`
///     - `dictionary` and `dictLength` satisfy the requirements of [`core::slice::from_raw_parts_mut::<u8>`]
#[export_name = prefix!(deflateSetDictionary)]
pub unsafe extern "C-unwind" fn deflateSetDictionary(
    strm: z_streamp,
    dictionary: *const Bytef,
    dictLength: uInt,
) -> c_int {
    let Some(dictionary) = (unsafe { slice_from_raw_parts(dictionary, dictLength as usize) })
    else {
        return ReturnCode::StreamError as _;
    };

    match DeflateStream::from_stream_mut(strm) {
        Some(stream) => zlib_rs::deflate::set_dictionary(stream, dictionary) as _,
        None => ReturnCode::StreamError as _,
    }
}

/// Inserts bits in the deflate output stream.
///
/// The intent is that this function is used to start off the deflate output with the bits leftover from a previous deflate stream when appending to it.
/// As such, this function can only be used for raw deflate, and must be used before the first [`deflate`] call after a [`deflateInit2_`] or [`deflateReset`].
/// bits must be less than or equal to 16, and that many of the least significant bits of value will be inserted in the output.
///
/// # Returns
///
/// - [`Z_OK`] if success
/// - [`Z_BUF_ERROR`] if there was not enough room in the internal buffer to insert the bits
/// - [`Z_STREAM_ERROR`] if the source stream state was inconsistent
///
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`deflateInit_`] or similar
#[export_name = prefix!(deflatePrime)]
pub unsafe extern "C-unwind" fn deflatePrime(strm: z_streamp, bits: c_int, value: c_int) -> c_int {
    match DeflateStream::from_stream_mut(strm) {
        Some(stream) => zlib_rs::deflate::prime(stream, bits, value) as _,
        None => ReturnCode::StreamError as _,
    }
}

/// Returns the number of bytes and bits of output that have been generated, but not yet provided in the available output.
///
/// The bytes not provided would be due to the available output space having being consumed.
/// The number of bits of output not provided are between `0` and `7`, where they await more bits to join them in order to fill out a full byte.
/// If pending or bits are `NULL`, then those values are not set.
///
/// # Returns
///
/// - [`Z_OK`] if success
/// - [`Z_STREAM_ERROR`] if the source stream state was inconsistent
///
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`deflateInit_`] or similar
/// * Either
///     - `pending` is `NULL`
///     - `pending` satisfies the requirements of [`core::ptr::write::<c_int>`]
/// * Either
///     - `bits` is `NULL`
///     - `bits` satisfies the requirements of [`core::ptr::write::<c_int>`]
#[export_name = prefix!(deflatePending)]
pub unsafe extern "C-unwind" fn deflatePending(
    strm: z_streamp,
    pending: *mut c_uint,
    bits: *mut c_int,
) -> c_int {
    let Some(stream) = (unsafe { DeflateStream::from_stream_mut(strm) }) else {
        return ReturnCode::StreamError as _;
    };

    let (current_pending, current_bits) = stream.pending();

    if let Some(pending) = unsafe { pending.as_mut() } {
        *pending = current_pending as c_uint;
    }

    if let Some(bits) = unsafe { bits.as_mut() } {
        *bits = current_bits as c_int;
    }

    ReturnCode::Ok as _
}

/// Sets the destination stream as a complete copy of the source stream.
///
/// This function can be useful when several compression strategies will be tried, for example when there are several ways of pre-processing the input data with a filter.
/// The streams that will be discarded should then be freed by calling [`deflateEnd`].
/// Note that [`deflateCopy`] duplicates the internal compression state which can be quite large, so this strategy is slow and can consume lots of memory.
///
/// # Returns
///
/// - [`Z_OK`] if success
/// - [`Z_MEM_ERROR`] if there was not enough memory
/// - [`Z_STREAM_ERROR`] if the source stream state was inconsistent (such as zalloc being NULL)
///
/// The `msg` field is left unchanged in both source and destination.
///
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `dest` is `NULL`
///     - `dest` satisfies the requirements of `&mut *(dest as *mut MaybeUninit<z_stream>)`
/// * Either
///     - `source` is `NULL`
///     - `source` satisfies the requirements of `&mut *strm` and was initialized with [`deflateInit_`] or similar
#[export_name = prefix!(deflateCopy)]
pub unsafe extern "C-unwind" fn deflateCopy(dest: z_streamp, source: z_streamp) -> c_int {
    let Some(dest) = (unsafe { dest.cast::<MaybeUninit<DeflateStream>>().as_mut() }) else {
        return ReturnCode::StreamError as _;
    };

    let Some(source) = (unsafe { DeflateStream::from_stream_mut(source) }) else {
        return ReturnCode::StreamError as _;
    };

    zlib_rs::deflate::copy(dest, source) as _
}

/// Initializes the state for compression
///
///  The stream's `zalloc`, `zfree` and `opaque` fields must be initialized before by the caller.
///  If `zalloc` and `zfree` are set to `NULL`, [`deflateInit_`] updates them to use default allocation functions.
///  The `total_in`, `total_out`, `adler`, and `msg` fields are initialized.
///
/// The compression level must be [`Z_DEFAULT_COMPRESSION`], or between `0` and `9`:
///
/// - level `0` gives no compression at all (the input data is simply copied a block at a time)
/// - level `1` gives best speed
/// - level `9` gives best compression
/// - [`Z_DEFAULT_COMPRESSION`] requests a default compromise between speed and compression (currently equivalent to level `6`).

///
/// A call to [`inflateInit_`] is equivalent to [`inflateInit2_`] where
///
/// - `method` is `8` (deflate)
/// - `windowBits` is `15`
/// - `memLevel` is `8`
/// - `strategy` is `0` (default)
///
/// # Returns
///
/// - [`Z_OK`] if success
/// - [`Z_MEM_ERROR`] if there was not enough memory
/// - [`Z_VERSION_ERROR`] if the zlib library version is incompatible with the version assumed by the caller
/// - [`Z_STREAM_ERROR`] if a parameter is invalid, such as a null pointer to the structure
///
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm`
/// * Either
///     - `version` is NULL
///     - `version` satisfies the requirements of [`core::ffi::CStr::from_ptr`]
/// * If `strm` is not `NULL`, the following fields contain valid values
///     - `zalloc`
///     - `zfree`
///     - `opaque`
///
/// # Example
///
/// ```
/// use core::mem::MaybeUninit;
/// use libz_rs_sys::{z_stream, deflateInit_, zlibVersion, Z_OK};
///
/// // the zalloc and zfree fields are initialized as zero/NULL.
/// // `deflateInit_` will set a default allocation  and deallocation function.
/// let mut strm = MaybeUninit::zeroed();
///
/// let err = unsafe {
///     deflateInit_(
///         strm.as_mut_ptr(),
///         6,
///         zlibVersion(),
///         core::mem::size_of::<z_stream>() as _,
///     )
/// };
/// assert_eq!(err, Z_OK);
///
/// // the stream is now fully initialized. Prefer `assume_init_mut` over
/// // `assume_init` so the stream does not get moved.
/// let strm = unsafe { strm.assume_init_mut() };
/// ```
#[export_name = prefix!(deflateInit_)]
pub unsafe extern "C-unwind" fn deflateInit_(
    strm: z_streamp,
    level: c_int,
    version: *const c_char,
    stream_size: c_int,
) -> c_int {
    let config = DeflateConfig::new(level);

    unsafe {
        deflateInit2_(
            strm,
            level,
            config.method as c_int,
            config.window_bits,
            config.mem_level,
            config.strategy as c_int,
            version,
            stream_size,
        )
    }
}

/// Initializes the state for compression
///
///  The stream's `zalloc`, `zfree` and `opaque` fields must be initialized before by the caller.
///  If `zalloc` and `zfree` are set to `NULL`, [`deflateInit_`] updates them to use default allocation functions.
///  The `total_in`, `total_out`, `adler`, and `msg` fields are initialized.
///
/// The compression level must be [`Z_DEFAULT_COMPRESSION`], or between `0` and `9`:
///
/// - level `0` gives no compression at all (the input data is simply copied a block at a time)
/// - level `1` gives best speed
/// - level `9` gives best compression
/// - [`Z_DEFAULT_COMPRESSION`] requests a default compromise between speed and compression (currently equivalent to level `6`).

///
/// A call to [`inflateInit_`] is equivalent to [`inflateInit2_`] where
///
///
/// # Returns
///
/// - [`Z_OK`] if success
/// - [`Z_MEM_ERROR`] if there was not enough memory
/// - [`Z_VERSION_ERROR`] if the zlib library version is incompatible with the version assumed by the caller
/// - [`Z_STREAM_ERROR`] if a parameter is invalid, such as a null pointer to the structure
///
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm`
/// * Either
///     - `version` is NULL
///     - `version` satisfies the requirements of [`core::ffi::CStr::from_ptr`]
/// * If `strm` is not `NULL`, the following fields contain valid values
///     - `zalloc`
///     - `zfree`
///     - `opaque`
///
/// # Example
///
/// ```
/// use core::mem::MaybeUninit;
/// use libz_rs_sys::{z_stream, deflateInit2_, zlibVersion, Z_OK};
///
/// // the zalloc and zfree fields are initialized as zero/NULL.
/// // `deflateInit_` will set a default allocation  and deallocation function.
/// let mut strm = MaybeUninit::zeroed();
///
/// let err = unsafe {
///     deflateInit2_(
///         strm.as_mut_ptr(),
///         6,
///         8,
///         15,
///         8,
///         0,
///         zlibVersion(),
///         core::mem::size_of::<z_stream>() as _,
///     )
/// };
/// assert_eq!(err, Z_OK);
///
/// // the stream is now fully initialized. Prefer `assume_init_mut` over
/// // `assume_init` so the stream does not get moved.
/// let strm = unsafe { strm.assume_init_mut() };
/// ```
#[export_name = prefix!(deflateInit2_)]
pub unsafe extern "C-unwind" fn deflateInit2_(
    strm: z_streamp,
    level: c_int,
    method: c_int,
    windowBits: c_int,
    memLevel: c_int,
    strategy: c_int,
    version: *const c_char,
    stream_size: c_int,
) -> c_int {
    if !is_version_compatible(version, stream_size) {
        return ReturnCode::VersionError as _;
    }

    let Some(strm) = (unsafe { strm.as_mut() }) else {
        return ReturnCode::StreamError as _;
    };

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

    zlib_rs::deflate::init(strm, config) as _
}

/// Fine tune deflate's internal compression parameters.
///
/// This should only be used by someone who understands the algorithm used by zlib's deflate for searching
/// for the best matching string, and even then only by the most fanatic optimizer trying to squeeze out
/// the last compressed bit for their specific input data. Read the `deflate.rs` source code for the meaning
/// of the `max_lazy`, `good_length`, `nice_length`, and `max_chain` parameters.
///
/// ## Returns
///
/// - [`Z_OK`] if success
/// - [`Z_STREAM_ERROR`] if the stream state was inconsistent
///
/// # Safety
///
/// The caller must guarantee that
///
/// * Either
///     - `strm` is `NULL`
///     - `strm` satisfies the requirements of `&mut *strm` and was initialized with [`deflateInit_`] or similar
#[export_name = prefix!(deflateTune)]
pub unsafe extern "C-unwind" fn deflateTune(
    strm: z_streamp,
    good_length: c_int,
    max_lazy: c_int,
    nice_length: c_int,
    max_chain: c_int,
) -> c_int {
    let Some(stream) = (unsafe { DeflateStream::from_stream_mut(strm) }) else {
        return ReturnCode::StreamError as _;
    };

    zlib_rs::deflate::tune(
        stream,
        good_length as usize,
        max_lazy as usize,
        nice_length as usize,
        max_chain as usize,
    ) as _
}

/// Get the error message for an error. This could be the value returned by e.g. [`compress`] or
/// [`inflate`].
///
/// The return value is a pointer to a NULL-terminated sequence of bytes
///
/// ## Example
///
/// ```
/// use libz_rs_sys::*;
/// use core::ffi::{c_char, CStr};
///
/// fn cstr<'a>(ptr: *const c_char) -> &'a [u8] {
///     // SAFETY: we trust the input
///     unsafe { CStr::from_ptr(ptr) }.to_bytes()
/// }
///
/// // defined error values give a short message
/// assert_eq!(cstr(zError(Z_NEED_DICT)), b"need dictionary");
/// assert_eq!(cstr(zError(Z_NEED_DICT)), b"need dictionary");
/// assert_eq!(cstr(zError(Z_STREAM_END)), b"stream end");
/// assert_eq!(cstr(zError(Z_OK)), b"");
/// assert_eq!(cstr(zError(Z_ERRNO)), b"file error");
/// assert_eq!(cstr(zError(Z_STREAM_ERROR)), b"stream error");
/// assert_eq!(cstr(zError(Z_DATA_ERROR)), b"data error");
/// assert_eq!(cstr(zError(Z_MEM_ERROR)), b"insufficient memory");
/// assert_eq!(cstr(zError(Z_BUF_ERROR)), b"buffer error");
/// assert_eq!(cstr(zError(Z_VERSION_ERROR)), b"incompatible version");
///
/// // other inputs return an empty string
/// assert_eq!(cstr(zError(1234)), b"");
/// ```
#[export_name = prefix!(zError)]
pub const extern "C" fn zError(err: c_int) -> *const c_char {
    match ReturnCode::try_from_c_int(err) {
        Some(return_code) => return_code.error_message(),
        None => [0 as c_char].as_ptr(),
    }
}

macro_rules! libz_rs_sys_version {
    () => {
        concat!("1.3.0-zlib-rs-", env!("CARGO_PKG_VERSION"), "\0")
    };
}

// the first part of this version specifies the zlib that we're compatible with (in terms of
// supported functions). In practice in most cases only the major version is checked, unless
// specific functions that were added later are used.
const LIBZ_RS_SYS_VERSION: &str = concat!(libz_rs_sys_version!(), "\0");

unsafe fn is_version_compatible(version: *const c_char, stream_size: i32) -> bool {
    let Some(expected_major_version) = (unsafe { version.as_ref() }) else {
        return false;
    };

    if *expected_major_version as u8 != LIBZ_RS_SYS_VERSION.as_bytes()[0] {
        return false;
    }

    core::mem::size_of::<z_stream>() as i32 == stream_size
}

/// The version of the zlib library.
///
/// Its value is a pointer to a NULL-terminated sequence of bytes.
///
/// The version string for this release is `
#[doc = libz_rs_sys_version!()]
/// `:
///
/// - The first component is the version of stock zlib that this release is compatible with
/// - The final component is the zlib-rs version used to build this release.
#[export_name = prefix!(zlibVersion)]
pub const extern "C" fn zlibVersion() -> *const c_char {
    LIBZ_RS_SYS_VERSION.as_ptr().cast::<c_char>()
}

/// # Safety
///
/// Either
///
/// - `ptr` is `NULL`
/// - `ptr` and `len` satisfy the requirements of [`core::slice::from_raw_parts`]
unsafe fn slice_from_raw_parts<'a, T>(ptr: *const T, len: usize) -> Option<&'a [T]> {
    if ptr.is_null() {
        None
    } else {
        Some(unsafe { core::slice::from_raw_parts(ptr, len) })
    }
}

/// # Safety
///
/// Either
///
/// - `ptr` is `NULL`
/// - `ptr` and `len` satisfy the requirements of [`core::slice::from_raw_parts_mut`]
unsafe fn slice_from_raw_parts_uninit_mut<'a, T>(
    ptr: *mut T,
    len: usize,
) -> Option<&'a mut [MaybeUninit<T>]> {
    if ptr.is_null() {
        None
    } else {
        Some(unsafe { core::slice::from_raw_parts_mut(ptr.cast::<MaybeUninit<T>>(), len) })
    }
}
