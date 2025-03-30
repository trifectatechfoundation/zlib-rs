use zlib_rs::allocate::*;
pub use zlib_rs::c_api::*;

use core::ffi::{c_char, c_int, c_uint, CStr };

use core::ptr;
use libc::{O_APPEND, O_CLOEXEC, O_CREAT, O_EXCL, O_RDONLY, O_TRUNC, O_WRONLY, SEEK_CUR, SEEK_END};
use zlib_rs::deflate::Strategy;

/// For compatibility with the zlib C API, this structure exposes just enough of the
/// internal state of an open gzFile to support the gzgetc() C macro.
#[repr(C)]
#[derive(Copy, Clone)]
pub struct gzFile_s {
    have: c_uint,        // number of bytes available at next
    next: *const Bytef,  // next byte of uncompressed data
    pos: u64,            // current offset in uncompressed data stream
}

// File handle for an open gzip file.
pub type gzFile = *mut gzFile_s;

// The internals of a gzip file handle (the thing gzFile actually points to, with the
// public gzFile_s part at the front for ABI compatibility).
#[repr(C)]
struct gz_state {
    x: gzFile_s,         // "x" for the exposed part of the data structure

    // Fields used for both reading and writing
    mode: GzMode,
    fd: c_int,           // file descriptor
    path: *const c_char,
    size: usize,         // buffer size; can be 0 if not yet allocated
    want: usize,         // requested buffer size
    input: *mut Bytef,   // input buffer
    output: *mut Bytef,  // output buffer
    direct: bool,        // true in pass-through mode, false if processing gzip data

    // Fields used just for reading
    // FIXME: add the 'how' field when read support is implemented
    start: i64,
    eof: bool,           // whether we have reached the end of the input file
    past: bool,          // whether a read past the end has been requested

    // Fields used just for writing
    level: i8,
    strategy: Strategy,
    reset: bool,         // whether a reset is pending after a Z_FINISH

    // Fields used for seek requests
    skip: i64,           // amount to skip (already rewound if backwards)
    seek: bool,          // whether a seek request is pending

    // Error information
    err: c_int,          // last error (0 if no error)
    msg: *const c_char,  // error message from last error (NULL if none)

    // FIXME: add the zstream field when read/write support is implemented
}

// Gzip operating modes
// NOTE: These values match what zlib-ng uses.
#[derive(Eq, PartialEq)]
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
/// The caller must ensure that path and mode point to valid C strings. If the
/// return value is non-NULL, caller must delete it using only [`gzclose`].
#[cfg_attr(feature = "export-symbols", export_name = crate::prefix!(gzopen))]
pub unsafe extern "C-unwind" fn gzopen(path: *const c_char, mode: *const c_char) -> gzFile {
    gzopen_help(path, -1, mode)
}

/// Internal implementation shared by gzopen and gzdopen.
///
/// # Safety
/// The caller must ensure that path and mode are NULL or point to valid C strings.
unsafe fn gzopen_help(path: *const c_char, fd: c_int, mode: *const c_char) -> gzFile {
    if path.is_null() || mode.is_null() {
        return ptr::null_mut();
    }

    let Some(state) = ALLOCATOR.allocate_zeroed(size_of::<gz_state>()) else {
        return ptr::null_mut();
    };
    let state = state.cast::<gz_state>().as_mut();
    state.size = 0;
    state.want = GZBUFSIZE;
    state.msg = ptr::null();

    state.mode = GzMode::GZ_NONE;
    state.level = crate::Z_DEFAULT_COMPRESSION as i8;
    state.strategy = Strategy::Default;
    state.direct = false;

    let mut cloexec = false;
    let mut exclusive = false;
    let mode = CStr::from_ptr(mode);
    for &ch in mode.to_bytes() {
        if ch.is_ascii_digit() {
            state.level = (ch - b'0') as i8;
        } else {
            match ch {
                b'r' => state.mode = GzMode::GZ_READ,
                b'w' => state.mode = GzMode::GZ_WRITE,
                b'a' => state.mode = GzMode::GZ_APPEND,
                b'b' => {}, // binary mode is the default
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
        ALLOCATOR.deallocate(state, 1);

        return ptr::null_mut();
    }

    // Can't force transparent read
    if state.mode == GzMode::GZ_READ {
        if state.direct {
            ALLOCATOR.deallocate(state, 1);
            return ptr::null_mut();
        }
        state.direct = true;
    }

    // Save the path name for error messages
    // FIXME: support Windows wide characters for compatibility with zlib-ng
    state.path = libc::strdup(path);

    // Open the file unless the caller passed a file descriptor.
    if fd > -1 {
        state.fd = fd;
    } else {
        let mut oflag: c_int = 0;
        if cloexec {
            oflag |= O_CLOEXEC;
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
        state.fd = libc::open(state.path, oflag, 0o666);
        if state.fd == -1 {
            ALLOCATOR.deallocate(state.path.cast_mut(), 1);
            ALLOCATOR.deallocate(state, 1);
            return ptr::null_mut();
        }
        if state.mode == GzMode::GZ_APPEND {
            libc::lseek(state.fd, 0, SEEK_END);
            state.mode = GzMode::GZ_WRITE;
        }
    }

    if state.mode == GzMode::GZ_READ {
        // Save the current position for rewinding
        state.start = libc::lseek(state.fd, 0, SEEK_CUR) as _;
        if state.start == -1 {
            state.start = 0;
        }
    }

    // FIXME verify the file headers, and initialize the inflate/deflate state

    // FIXME change this to core::ptr::from_mut(state).cast::<gzFile_s>() once MSRV >= 1.76
    (state as *mut gz_state).cast::<gzFile_s>()
}

/// Close an open gzip file and free the internal data structures referenced by the file handle.
///
/// # Safety
/// This function may be called at most once for any file handle.
#[cfg_attr(feature = "export-symbols", export_name = crate::prefix!(gzclose))]
pub unsafe extern "C-unwind" fn gzclose(file: gzFile) -> c_int{
    let Some(state) = file.cast::<gz_state>().as_mut() else {
        return Z_STREAM_ERROR;
    };

    // FIXME: once read/write support is added, clean up internal buffers

    let err = libc::close(state.fd);
    ALLOCATOR.deallocate(state.path.cast_mut(), 1);
    ALLOCATOR.deallocate(state, 1);
    if err == 0 {
        Z_OK
    } else {
        Z_ERRNO
    }
}
