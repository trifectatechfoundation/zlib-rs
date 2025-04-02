#![warn(unsafe_op_in_unsafe_fn)]

use zlib_rs::allocate::*;
pub use zlib_rs::c_api::*;

use core::ffi::{c_char, c_int, c_uint, CStr};
use core::ptr;
use libc::{O_APPEND, O_CREAT, O_EXCL, O_RDONLY, O_TRUNC, O_WRONLY, SEEK_CUR, SEEK_END};
use zlib_rs::deflate::Strategy;

/// In the zlib C API, this structure exposes just enough of the internal state
/// of an open gzFile to support the gzgetc() C macro. Since Rust code won't be
/// using that C macro, we define gzFile_s as an empty structure. The first fields
/// in GzState match what would be in the C version of gzFile_s.
pub enum gzFile_s {}

/// File handle for an open gzip file.
pub type gzFile = *mut gzFile_s;

// The internals of a gzip file handle (the thing gzFile actually points to, with the
// public gzFile_s part at the front for ABI compatibility).
#[repr(C)]
struct GzState {
    // Public interface:
    // These first three fields must match the structure gzFile_s in the C version
    // of zlib. In the C library, a macro called gzgetc() reads and writes these
    // fields directly.
    have: c_uint,       // number of bytes available at next
    next: *const Bytef, // next byte of uncompressed data
    pos: u64,           // current offset in uncompressed data stream

    // End of public interface:
    // All fields after this point are opaque to C code using this library,
    // so they can be rearranged without breaking compatibility.

    // Fields used for both reading and writing
    mode: GzMode,
    fd: c_int, // file descriptor
    path: *const c_char,
    size: usize,        // buffer size; can be 0 if not yet allocated
    want: usize,        // requested buffer size
    input: *mut Bytef,  // input buffer
    output: *mut Bytef, // output buffer
    direct: bool,       // true in pass-through mode, false if processing gzip data

    // Fields used just for reading
    // FIXME: add the 'how' field when read support is implemented
    start: i64,
    eof: bool,  // whether we have reached the end of the input file
    past: bool, // whether a read past the end has been requested

    // Fields used just for writing
    level: i8,
    strategy: Strategy,
    reset: bool, // whether a reset is pending after a Z_FINISH

    // Fields used for seek requests
    skip: i64,  // amount to skip (already rewound if backwards)
    seek: bool, // whether a seek request is pending

    // Error information
    err: c_int, // last error (0 if no error)
    msg: *const c_char, // error message from last error (NULL if none)

                // FIXME: add the zstream field when read/write support is implemented
}

// Gzip operating modes
// NOTE: These values match what zlib-ng uses.
#[derive(PartialEq, Eq)]
enum GzMode {
    GZ_NONE = 0,
    GZ_READ = 7247,
    GZ_WRITE = 31153,
    GZ_APPEND = 1,
}

const GZBUFSIZE: usize = 128 * 1024;

#[cfg(feature = "rust-allocator")]
const ALLOCATOR: &Allocator = &Allocator::RUST;

#[cfg(not(feature = "rust-allocator"))]
#[cfg(feature = "c-allocator")]
const ALLOCATOR: &Allocator = &Allocator::C;

#[cfg(not(feature = "rust-allocator"))]
#[cfg(not(feature = "c-allocator"))]
compile_error!("Either rust-allocator or c-allocator feature is required");

/// Open a gzip file for reading or writing.
///
/// # Safety
///
/// The caller must ensure that path and mode point to valid C strings. If the
/// return value is non-NULL, caller must delete it using only [`gzclose`].
#[cfg_attr(feature = "export-symbols", export_name = crate::prefix!(gzopen))]
pub unsafe extern "C-unwind" fn gzopen(path: *const c_char, mode: *const c_char) -> gzFile {
    unsafe { gzopen_help(path, -1, mode) }
}

/// Internal implementation shared by gzopen and gzdopen.
///
/// # Safety
/// The caller must ensure that path and mode are NULL or point to valid C strings.
unsafe fn gzopen_help(path: *const c_char, fd: c_int, mode: *const c_char) -> gzFile {
    if path.is_null() || mode.is_null() {
        return ptr::null_mut();
    }

    let Some(state) = ALLOCATOR.allocate_zeroed_raw::<GzState>() else {
        return ptr::null_mut();
    };
    // Safety: the allocate_zeroed_raw call above ensures that the allocated block
    // has the right size and alignment to be used as a GzState.
    let state = unsafe { state.cast::<GzState>().as_mut() };
    state.size = 0;
    state.want = GZBUFSIZE;
    state.msg = ptr::null();

    state.mode = GzMode::GZ_NONE;
    state.level = crate::Z_DEFAULT_COMPRESSION as i8;
    state.strategy = Strategy::Default;
    state.direct = false;

    let mut exclusive = false;
    #[cfg(target_os = "linux")]
    let mut cloexec = false;
    // Safety: We checked that mode is non-null earlier. The caller is responsible for
    // ensuring that it points to a valid C string.
    let mode = unsafe { CStr::from_ptr(mode) };
    for &ch in mode.to_bytes() {
        if ch.is_ascii_digit() {
            state.level = (ch - b'0') as i8;
        } else {
            match ch {
                b'r' => state.mode = GzMode::GZ_READ,
                b'w' => state.mode = GzMode::GZ_WRITE,
                b'a' => state.mode = GzMode::GZ_APPEND,
                b'+' => {
                    // Read+Write mode isn't supported
                    // Safety: we know state is a valid pointer because it was allocated earlier
                    // in this function, and it is not used after the free because we return
                    // immediately afterward.
                    unsafe { free_state(state) };
                    return ptr::null_mut();
                }
                b'b' => {} // binary mode is the default
                #[cfg(target_os = "linux")]
                b'e' => cloexec = true,
                b'x' => exclusive = true,
                b'f' => state.strategy = Strategy::Filtered,
                b'h' => state.strategy = Strategy::HuffmanOnly,
                b'R' => state.strategy = Strategy::Rle,
                b'F' => state.strategy = Strategy::Fixed,
                b'T' => state.direct = true,
                _ => {} // for compatibility with zlib-ng, ignore unexpected characters in the mode
            }
        }
    }

    // Must specify read, write, or append
    if state.mode == GzMode::GZ_NONE {
        // Safety: we know state is a valid pointer because it was allocated earlier in this
        // function, and it is not used after the free because we return immediately afterward.
        unsafe { free_state(state) };
        return ptr::null_mut();
    }

    // Can't force transparent read
    if state.mode == GzMode::GZ_READ {
        if state.direct {
            // Safety: we know state is a valid pointer because it was allocated earlier in this
            // function, and it is not used after the free because we return immediately afterward.
            unsafe { free_state(state) };
            return ptr::null_mut();
        }
        state.direct = true;  // Assume an empty file for now. Later, we'll check for a gzip header.
    }

    // Save the path name for error messages
    // FIXME: support Windows wide characters for compatibility with zlib-ng
    // Safety: The caller is reponsible for ensuring that path points to a valid C string.
    // We also checked it for null at the start of this function.
    let len = unsafe { libc::strlen(path) } + 1;
    let Some(path_copy) = ALLOCATOR.allocate_slice_raw::<c_char>(len) else {
        // Safety: we know state is a valid pointer because it was allocated earlier in this
        // function, and it is not used after the free because we return immediately afterward.
        unsafe { free_state(state) };
        return ptr::null_mut();
    };
    let path_copy = path_copy.as_ptr().cast::<c_char>();
    // Safety: The allocation of path_copy is checked above. The caller is responsible for
    // ensuring that path points to a valid C string.
    unsafe { libc::strncpy(path_copy, path, len); }
    state.path = path_copy;

    // Open the file unless the caller passed a file descriptor.
    if fd > -1 {
        state.fd = fd;
    } else {
        let mut oflag = 0;

        #[cfg(target_os = "linux")]
        {
            oflag |= libc::O_LARGEFILE;
        }

        #[cfg(target_os = "linux")]
        if cloexec {
            oflag |= libc::O_CLOEXEC;
        }

        if state.mode == GzMode::GZ_READ {
            oflag |= O_RDONLY;
        } else {
            oflag |= O_WRONLY | O_CREAT;
            if exclusive {
                oflag |= O_EXCL;
            }
            if state.mode == GzMode::GZ_WRITE {
                oflag |= O_TRUNC;
            } else {
                oflag |= O_APPEND;
            }
        }
        // FIXME: support _wopen for WIN32
        // Safety: We constructed state.path as a valid C string above.
        state.fd = unsafe { libc::open(state.path, oflag, 0o666) };
        if state.fd == -1 {
            // Safety: we know state is a valid pointer because it was allocated earlier in this
            // function, and it is not used after the free because we return immediately afterward.
            unsafe { free_state(state) };
            return ptr::null_mut();
        }
    }

    if state.mode == GzMode::GZ_APPEND {
        unsafe { libc::lseek(state.fd, 0, SEEK_END) };  // so gzoffset() is correct
        state.mode = GzMode::GZ_WRITE;       // simplify later checks
    }

    if state.mode == GzMode::GZ_READ {
        // Save the current position for rewinding
        unsafe { state.start = libc::lseek(state.fd, 0, SEEK_CUR) as _ };
        if state.start == -1 {
            state.start = 0;
        }
    }

    // FIXME verify the file headers, and initialize the inflate/deflate state

    // FIXME change this to core::ptr::from_mut(state).cast::<gzFile_s>() once MSRV >= 1.76
    (state as *mut GzState).cast::<gzFile_s>()
}

// Deallocate a GzState structure and all heap-allocated fields inside it.
//
// # Safety
//
// The caller must not use the state after passing it to this function.
unsafe fn free_state(state: &mut GzState) {
    unsafe {
        if !state.path.is_null() {
            ALLOCATOR.deallocate::<c_char>(state.path.cast_mut(), libc::strlen(state.path) + 1);
        }
        ALLOCATOR.deallocate::<GzState>(state, 1);
    }
}

/// Close an open gzip file and free the internal data structures referenced by the file handle.
///
/// # Returns
///
/// * [`Z_ERRNO`] if closing the file failed
/// * [`Z_OK`] otherwise
///
/// # Safety
///
/// This function may be called at most once for any file handle. The caller is responsible
/// for ensuring that the file handle either points to a GzState object or is null.
#[cfg_attr(feature = "export-symbols", export_name = crate::prefix!(gzclose))]
pub unsafe extern "C-unwind" fn gzclose(file: gzFile) -> c_int {
    let Some(state) = (unsafe { file.cast::<GzState>().as_mut() }) else {
        return Z_STREAM_ERROR;
    };

    // FIXME: once read/write support is added, clean up internal buffers

    let err = unsafe { libc::close(state.fd) };
    unsafe { free_state(state) };
    if err == 0 {
        Z_OK
    } else {
        Z_ERRNO
    }
}
