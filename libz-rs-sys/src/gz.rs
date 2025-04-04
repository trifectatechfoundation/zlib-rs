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
    fd: c_int,          // file descriptor
    path: *const c_char,
    size: usize,        // buffer size; can be 0 if not yet allocated
    want: usize,        // requested buffer size
    input: *mut Bytef,  // input buffer
    output: *mut Bytef, // output buffer
    direct: bool,       // true in pass-through mode, false if processing gzip data

    // Fields used just for reading
    how: How,
    start: i64,
    eof: bool,          // whether we have reached the end of the input file
    past: bool,         // whether a read past the end has been requested

    // Fields used just for writing
    level: i8,
    strategy: Strategy,
    reset: bool,        // whether a reset is pending after a Z_FINISH

    // Fields used for seek requests
    skip: i64,          // amount to skip (already rewound if backwards)
    seek: bool,         // whether a seek request is pending

    // Error information
    err: c_int,         // last error (0 if no error)
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

// gzip read strategies
// NOTE: These values match what zlib-ng uses.
enum How {
    Look = 0,      // look for a gzip header
    // FIXME Remove "allow(dead_code)" when code using COPY & GZIP is added.
    #[allow(dead_code)]
    Copy = 1,      // copy input directly
    #[allow(dead_code)]
    Gzip = 2,      // decompress a gzip stream
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
/// # Returns
///
/// * If successful, an opaque handle that the caller can later free with [`gzfree`]
/// * On error, a null pointer
///
/// # Safety
///
/// The caller must ensure that `path` and `mode` point to valid C strings. If the
/// return value is non-NULL, caller must delete it using only [`gzclose`].
#[cfg_attr(feature = "export-symbols", export_name = crate::prefix!(gzopen))]
pub unsafe extern "C-unwind" fn gzopen(path: *const c_char, mode: *const c_char) -> gzFile {
    unsafe { gzopen_help(path, -1, mode) }
}

/// Given an open file descriptor, prepare to read or write a gzip file.
/// NOTE: This is similar to [`gzopen`], but for cases where the caller already
/// has the file open.
///
/// # Returns
///
/// * If successful, an opaque handle that the caller can later free with [`gzfree`]
/// * On error, a null pointer
///
/// # Safety
///
/// The caller must ensure that `path` points to a valid C string. If the
/// return value is non-NULL, caller must delete it using only [`gzclose`].
#[cfg_attr(feature = "export-symbols", export_name = crate::prefix!(gzdopen))]
pub unsafe extern "C-unwind" fn gzdopen(fd: c_int, mode: *const c_char) -> gzFile {
    let path = format!("<fd:{}>\0", fd);
    // Safety: `path` was constructed above as a C string with a null terminator,
    // and the caller is responsible for `mode` being a non-null C string.
    unsafe { gzopen_help(path.as_ptr().cast::<c_char>(), fd, mode) }
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
    // has the right size and alignment to be used as a GzState. And because the
    // allocator zeroes the allocated space, all the GzState fields are initialized.
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
    state.path = unsafe { gz_strdup(path) };
    if state.path.is_null() {
        // Safety: we know state is a valid pointer because it was allocated earlier in this
        // function, and it is not used after the free because we return immediately afterward.
        unsafe { free_state(state) };
        return ptr::null_mut();
    };

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

    // Initialize stream
    gz_reset(state);

    // FIXME change this to core::ptr::from_mut(state).cast::<gzFile_s>() once MSRV >= 1.76
    (state as *mut GzState).cast::<gzFile_s>()
}

// Reset the internal state of an open gzip stream according to
// its mode (read or write)
fn gz_reset(state: &mut GzState) {
    state.have = 0;                 // no output data available
    if state.mode == GzMode::GZ_READ {
        state.eof = false;          // not at end of file
        state.past = false;         // have not read past end yet
        state.how = How::Look;      // look for gzip header
    } else {
        state.reset = false;        // no deflateReset pending
    }
    state.seek = false;             // no seek request pending
    // Safety: It is valid to pass a null msg pointer to `gz_error`.
    unsafe { gz_error(state, Z_OK, ptr::null_mut()) };    // clear error status
    state.pos = 0;                  // no uncompressed data yet
    // FIXME add once the deflate state is implemented:
    // state->strm.avail_in = 0;       /* no input data yet */
}

// Set the error message for a gzip stream, and deallocate any
// previously set error message.
//
// # Safety
//
// - `state` must be a properly constructed `GzState`, e.g. as produced by [`gzopen`]
// - `msg` must be null or a valid C string. If `msg` is non-null, this function will
//   make a copy if it, and the caller will retain ownership of the original.
unsafe fn gz_error(state: &mut GzState, err: c_int, msg: *const c_char) {
    if !state.msg.is_null() {
        // NOTE: zlib-ng skips the deallocation here if state.err == Z_MEM_ERROR.
        // Instead, we maintain the invariant that `state.msg`, if non-null, is
        // allocated using `ALLOCATOR`, and we assume that the heap is coherent
        // enough that we can (and should) free the old value even if a new alloc
        // just failed.
        unsafe { deallocate_cstr(state.msg.cast_mut()) };
        state.msg = ptr::null_mut();
    }

    // On error, set state.have to 0 so that the `gzgetc()` C macro fails
    if err != Z_OK && err != Z_BUF_ERROR {
        state.have = 0;
    }

    // Set the error code, and if no message, then done
    state.err = err;
    if msg.is_null() {
        return;
    }

    // For an out of memory error, don't bother trying to allocate space for an error string.
    // ([`gzerror`] will provide literal string as a special case for OOM errors.)
    if err == Z_MEM_ERROR {
        return;
    }

    // Format the error string to include the file path.
    // Safety: `gzopen` and `gzdopen` ensure that `state.path` is a non-null C string.
    let Ok(path) = (unsafe { CStr::from_ptr(state.path).to_str() }) else {
        return;
    };
    // Safety: the caller of this function is reponsible for ensuring that `msg` is a C string,
    // and we exit this function above if `msg` is null.
    let Ok(msg) = (unsafe { CStr::from_ptr(msg).to_str() }) else {
        return;
    };
    let err_msg = format!("{}: {}\0", path, msg);
    // Safety: `err_msg` is formatted as a C string, with a null byte at the end.
    state.msg = unsafe { gz_strdup(err_msg.as_ptr().cast::<c_char>()) };
    if state.msg.is_null() {
        state.err = Z_MEM_ERROR;
    }
}

// Deallocate a GzState structure and all heap-allocated fields inside it.
//
// # Safety
//
// - The `state` object and all heap-allocated fields within it must have been obtained
//   using `ALLOCATOR`.
// - The caller must not use the `state` after passing it to this function.
unsafe fn free_state(state: &mut GzState) {
    // Safety: `deallocate_cstr` accepts null pointers or C strings, and in this
    // module we use only `ALLOCATOR` to allocate strings assigned to these fields.
    unsafe {
        deallocate_cstr(state.path.cast_mut());
        deallocate_cstr(state.msg.cast_mut());
    }
    // Safety: The caller has ensured that `state` was allocated using `ALLOCATOR`.
    unsafe {
        ALLOCATOR.deallocate::<GzState>(state, 1);
    }
}

// Free a string that was allocated with `ALLOCATOR`
//
// # Safety
//
// `s` must be either null or a null-terminated C string that was allocated with `ALLOCATOR`.
unsafe fn deallocate_cstr(s: *mut c_char) {
    if s.is_null() {
       return;
    }
    // Safety: We checked above that `s` is non-null, and the caller ensured it
    // is a C string allocated with `ALLOCATOR`.
    unsafe { ALLOCATOR.deallocate::<c_char>(s, libc::strlen(s) + 1) };
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
/// `file` must be one of the following:
/// - A file handle must have been obtained from a function in this library, such as [`gzopen`].
/// - A null pointer.
///
/// This function may be called at most once for any file handle.
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

/// Retrieve the zlib error code and a human-readable string description of
/// the most recent error on a gzip file stream.
///
/// # Arguments
///
/// * `file` - A gzip file handle, or null
/// * `errnum` - A pointer to a C integer in which the zlib error code should be
///   written, or null if the caller does not need the numeric error code.
///
/// # Returns
///
/// * A pointer to a null-terminated C string describing the error, if `file` is non-null
///   and has an error
/// * A pointer to an empty (zero-length), null-terminated C string, if `file` is non-null
///   but has no error
/// * Null otherwise
///
/// # Safety
///
/// `file` must be one of the following:
/// - A file handle must have been obtained from a function in this library, such as [`gzopen`].
/// - A null pointer.
///
/// If this function returns a non-null string, the caller must not modifiy or
/// deallocate the string.
///
/// If `errnum` is non-null, it must point to an address at which an integer may be written.
#[cfg_attr(feature = "export-symbols", export_name = crate::prefix!(gzerror))]
pub unsafe extern "C-unwind" fn gzerror(file: gzFile, errnum: *mut c_int) -> *const c_char {
    // Get internal structure and check integrity
    if file.is_null() {
        return ptr::null();
    }
    let Some(state) = (unsafe { file.cast::<GzState>().as_ref() }) else {
        return ptr::null();
    };
    if state.mode != GzMode::GZ_READ && state.mode != GzMode::GZ_WRITE {
        return ptr::null();
    }

    // Return error information
    if !errnum.is_null() {
        // Safety:
        // * `errnum` is non-null
        // * The caller is responsible for ensuring that `errnum` points to writable
        //   memory with proper alignment.
        unsafe { *errnum = state.err };
    }
    if state.err == Z_MEM_ERROR {
        b"out of memory\0".as_ptr().cast::<c_char>()
    } else if state.msg.is_null() {
        b"\0".as_ptr().cast::<c_char>()
    } else {
        state.msg
    }
}

// Create a deep copy of a C string using `ALLOCATOR`
//
// # Safety
//
// The caller must ensure that s is either null or a pointer to a null-terminated C string.
unsafe fn gz_strdup(s: *const c_char) -> *mut c_char {
    if s.is_null() {
        return ptr::null_mut();
    }
    // Safety
    let len = unsafe { libc::strlen(s) } + 1;
    let Some(buf) = ALLOCATOR.allocate_slice_raw::<c_char>(len) else {
        return ptr::null_mut();
    };
    let s_copy = buf.as_ptr().cast::<c_char>();
    // Safety: The allocation of s_copy is checked above. The caller is responsible for
    // ensuring that path points to a valid C string.
    unsafe { libc::strncpy(s_copy, s, len); }
    s_copy
}
