#![warn(unsafe_op_in_unsafe_fn)]

use zlib_rs::allocate::*;
pub use zlib_rs::c_api::*;

use crate::{deflateEnd, inflateEnd, inflateInit2, inflateReset};
use core::ffi::{c_char, c_int, c_uint, CStr};
use core::ptr;
use libc::{size_t, O_APPEND, O_CREAT, O_EXCL, O_RDONLY, O_TRUNC, O_WRONLY, SEEK_CUR, SEEK_END};
use zlib_rs::deflate::Strategy;
use zlib_rs::MAX_WBITS;

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
    source: Source,
    want: usize,     // requested buffer size, default is GZBUFSIZE
    input: *mut u8,  // input buffer (double-sized when writing)
    in_size: usize,  // size of *input
    output: *mut u8, // output buffer (double-sized when reading)
    out_size: usize, // size of *output
    direct: bool,    // true in pass-through mode, false if processing gzip data

    // Fields used just for reading
    how: How,
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
    err: c_int,         // last error (0 if no error)
    msg: *const c_char, // error message from last error (NULL if none)

    // zlib inflate or deflate stream
    stream: z_stream,
}

impl GzState {
    fn configure(&mut self, mode: &[u8]) -> Result<(bool, bool), ()> {
        let mut exclusive = false;
        let mut cloexec = false;

        for &ch in mode {
            if ch.is_ascii_digit() {
                self.level = (ch - b'0') as i8;
            } else {
                match ch {
                    b'r' => self.mode = GzMode::GZ_READ,
                    b'w' => self.mode = GzMode::GZ_WRITE,
                    b'a' => self.mode = GzMode::GZ_APPEND,
                    b'+' => {
                        // Read+Write mode isn't supported
                        return Err(());
                    }
                    b'b' => {} // binary mode is the default
                    b'e' => cloexec = true,
                    b'x' => exclusive = true,
                    b'f' => self.strategy = Strategy::Filtered,
                    b'h' => self.strategy = Strategy::HuffmanOnly,
                    b'R' => self.strategy = Strategy::Rle,
                    b'F' => self.strategy = Strategy::Fixed,
                    b'T' => self.direct = true,
                    _ => {} // for compatibility with zlib-ng, ignore unexpected characters in the mode
                }
            }
        }

        Ok((exclusive, cloexec))
    }
}

// Gzip operating modes
// NOTE: These values match what zlib-ng uses.
#[derive(Debug, PartialEq, Eq)]
enum GzMode {
    GZ_NONE = 0,
    GZ_READ = 7247,
    GZ_WRITE = 31153,
    GZ_APPEND = 1,
}

// gzip read strategies
// NOTE: These values match what zlib-ng uses.
#[derive(Debug, PartialEq, Eq)]
enum How {
    Look = 0, // look for a gzip header
    Copy = 1, // copy input directly
    Gzip = 2, // decompress a gzip stream
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

// The different ways to specify the source for gzopen_help
enum Source {
    Path(*const c_char),
    Fd(c_int),
}

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
    if path.is_null() {
        return ptr::null_mut();
    }
    let source = Source::Path(path);
    unsafe { gzopen_help(source, mode) }
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
/// The caller must ensure that `mode` points to a valid C string. If the
/// return value is non-NULL, caller must delete it using only [`gzclose`].
#[cfg_attr(feature = "export-symbols", export_name = crate::prefix!(gzdopen))]
pub unsafe extern "C-unwind" fn gzdopen(fd: c_int, mode: *const c_char) -> gzFile {
    // Safety: the caller is responsible for `mode` being a non-null C string.
    unsafe { gzopen_help(Source::Fd(fd), mode) }
}

/// Internal implementation shared by gzopen and gzdopen.
///
/// # Safety
///
/// The caller must ensure that mode points to a valid C string.
unsafe fn gzopen_help(source: Source, mode: *const c_char) -> gzFile {
    if mode.is_null() {
        return ptr::null_mut();
    }

    let Some(state) = ALLOCATOR.allocate_zeroed_raw::<GzState>() else {
        return ptr::null_mut();
    };
    // Safety: the allocate_zeroed_raw call above ensures that the allocated block
    // has the right size and alignment to be used as a GzState. And because the
    // allocator zeroes the allocated space, all the GzState fields are initialized.
    let state = unsafe { state.cast::<GzState>().as_mut() };
    state.in_size = 0;
    state.out_size = 0;
    state.want = GZBUFSIZE;
    state.msg = ptr::null();

    state.mode = GzMode::GZ_NONE;
    state.level = crate::Z_DEFAULT_COMPRESSION as i8;
    state.strategy = Strategy::Default;
    state.direct = false;

    state.stream = z_stream::default();
    state.stream.zalloc = Some(ALLOCATOR.zalloc);
    state.stream.zfree = Some(ALLOCATOR.zfree);
    state.stream.opaque = ALLOCATOR.opaque;

    let mode = unsafe { CStr::from_ptr(mode) };
    let Ok((exclusive, cloexec)) = state.configure(mode.to_bytes()) else {
        // Safety: state is a valid pointer allocated in this function and not used after freeing
        unsafe { free_state(state) };
        return ptr::null_mut();
    };

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
        state.direct = true; // Assume an empty file for now. Later, we'll check for a gzip header.
    }

    // Open the file unless the caller passed a file descriptor.
    match source {
        Source::Fd(fd) => {
            state.fd = fd;
            state.source = Source::Fd(fd);
        }
        Source::Path(path) => {
            // Save the path name for error messages
            // FIXME: support Windows wide characters for compatibility with zlib-ng
            let cloned_path = unsafe { gz_strdup(path) };
            if cloned_path.is_null() {
                unsafe { free_state(state) };
                return ptr::null_mut();
            }
            state.source = Source::Path(cloned_path);
            let mut oflag = 0;

            #[cfg(target_os = "linux")]
            {
                oflag |= libc::O_LARGEFILE;
            }
            if cloexec {
                #[cfg(target_os = "linux")]
                {
                    oflag |= libc::O_CLOEXEC;
                }
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
            state.fd = unsafe { libc::open(cloned_path, oflag, 0o666) };
        }
    }

    if state.fd == -1 {
        // Safety: we know state is a valid pointer because it was allocated earlier in this
        // function, and it is not used after the free because we return immediately afterward.
        unsafe { free_state(state) };
        return ptr::null_mut();
    }

    if state.mode == GzMode::GZ_APPEND {
        unsafe { libc::lseek(state.fd, 0, SEEK_END) }; // so gzoffset() is correct
        state.mode = GzMode::GZ_WRITE; // simplify later checks
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

// Format a fake file path corresponding to an fd, for use in error messages.
fn fd_path(buf: &mut [u8; 27], fd: c_int) -> &CStr {
    // This is equivalent to `format!("<fd:{}>\0", fd)`, but without the dependency on std.

    use core::fmt::Write;

    // The array size is chosen so that any file descriptor value will fit. We need space for 6
    // characters, plus space for the largest decimal value for the `c_int` type. On some systems
    // the c_int type can actually be 64 bits. The `i64::MIN` value has 20 digits, and the minus
    // sign, for a total of 6 + 20 + 1 = 27.
    #[cfg(feature = "std")]
    debug_assert!(format!("<fd:{}>\0", i64::MIN).len() <= buf.len());

    struct Writer<'a> {
        buf: &'a mut [u8; 27],
        len: usize,
    }

    impl Write for Writer<'_> {
        fn write_str(&mut self, s: &str) -> core::fmt::Result {
            let Some(dst) = self.buf.get_mut(self.len..self.len + s.len()) else {
                return Err(core::fmt::Error);
            };

            dst.copy_from_slice(s.as_bytes());
            self.len += s.len();

            Ok(())
        }
    }

    let mut w = Writer { buf, len: 0 };

    write!(w, "<fd:{fd}>\0").unwrap();

    unsafe { CStr::from_ptr(w.buf[..w.len].as_ptr().cast()) }
}

// Reset the internal state of an open gzip stream according to
// its mode (read or write)
fn gz_reset(state: &mut GzState) {
    state.have = 0; // no output data available
    if state.mode == GzMode::GZ_READ {
        state.eof = false; // not at end of file
        state.past = false; // have not read past end yet
        state.how = How::Look; // look for gzip header
    } else {
        state.reset = false; // no deflateReset pending
    }
    state.seek = false; // no seek request pending
                        // Safety: It is valid to pass a null msg pointer to `gz_error`.
    unsafe { gz_error(state, None) }; // clear error status
    state.pos = 0; // no uncompressed data yet
    state.stream.avail_in = 0; // no input data yet
}

// Set the error message for a gzip stream, and deallocate any
// previously set error message.
//
// # Arguments
//
// * `state` - An initialized stream state.
// * `err_msg` - `None` or `Some(err, msg)`. In the latter case, this function will
//   make a deep copy of the `msg` string, so `msg` need not remain in scope after the
//   call to this function.
//
// # Safety
//
// - `state` must be a properly constructed `GzState`, e.g. as produced by [`gzopen`]
unsafe fn gz_error(state: &mut GzState, err_msg: Option<(c_int, &str)>) {
    if !state.msg.is_null() {
        // NOTE: zlib-ng has a special case here: it skips the deallocation if
        // state.err == Z_MEM_ERROR. However, we always set state.msg to null
        // when state.err is set to Z_MEM_ERROR, so that case is unreachable
        // here.
        unsafe { deallocate_cstr(state.msg.cast_mut()) };
        state.msg = ptr::null_mut();
    }

    match err_msg {
        None => {
            state.err = Z_OK;
        }
        Some((err, msg)) => {
            // On error, set state.have to 0 so that the `gzgetc()` C macro fails
            if err != Z_OK && err != Z_BUF_ERROR {
                state.have = 0;
            }

            // Set the error code
            state.err = err;

            // For an out of memory error, don't bother trying to allocate space for an error string.
            // ([`gzerror`] will provide literal string as a special case for OOM errors.)
            if err == Z_MEM_ERROR {
                return;
            }

            // Format the error string to include the file path.
            // Safety: `gzopen` and `gzdopen` ensure that `state.path` is a non-null C string
            let sep = ": ";
            let buf = &mut [0u8; 27];
            state.msg = match state.source {
                Source::Path(path) => unsafe {
                    gz_strcat(&[CStr::from_ptr(path).to_str().unwrap(), sep, msg])
                },
                Source::Fd(fd) => unsafe {
                    gz_strcat(&[fd_path(buf, fd).to_str().unwrap(), sep, msg])
                },
            };

            if state.msg.is_null() {
                state.err = Z_MEM_ERROR;
            }
        }
    }
}

// Deallocate a GzState structure and all heap-allocated fields inside it.
//
// # Safety
//
// - The `state` object and all heap-allocated fields within it must have been obtained
//   using `ALLOCATOR`.
// - The caller must not use the `state` after passing it to this function.
unsafe fn free_state(state: *mut GzState) {
    if state.is_null() {
        return;
    }
    // Safety: `deallocate_cstr` accepts null pointers or C strings, and in this
    // module we use only `ALLOCATOR` to allocate strings assigned to these fields.
    unsafe {
        match (*state).source {
            Source::Path(path) => deallocate_cstr(path.cast_mut()),
            Source::Fd(_) => { /* fd is owned by the caller */ }
        }
        deallocate_cstr((*state).msg.cast_mut());
    }
    // Safety: state is a valid GzState, and free_buffers checks for null
    // input and output pointers internally.
    unsafe { free_buffers(state.as_mut().unwrap()) };

    // Safety: The caller has ensured that `state` was allocated using `ALLOCATOR`.
    unsafe { ALLOCATOR.deallocate(state, 1) };
}

// Deallocate the input and output buffers in a GzState.
//
// # Safety
//
// * `state` must have been obtained from [`gzopen`] or [`gzdopen`].
unsafe fn free_buffers(state: &mut GzState) {
    if !state.input.is_null() {
        // Safety: state.input is always allocated using ALLOCATOR, and
        // its allocation size is stored in state.in_size.
        unsafe { ALLOCATOR.deallocate(state.input, state.in_size) };
        state.input = ptr::null_mut();
    }
    state.in_size = 0;
    if !state.output.is_null() {
        // Safety: state.input is always allocated using ALLOCATOR, and
        // its allocation size is stored in state.in_size.
        unsafe { ALLOCATOR.deallocate(state.output, state.out_size) };
        state.output = ptr::null_mut();
    }
    state.out_size = 0;
}

// Free a string that was allocated with `ALLOCATOR`
//
// # Safety
//
// * `s` must be either null or a null-terminated C string that was allocated with `ALLOCATOR`.
// * If `s` is not null, the length of the string (including the null terminator byte) must
//   exactly match the allocation size.
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
    let ret = match unsafe { file.cast::<GzState>().as_mut() } {
        Some(state) => match state.mode {
            GzMode::GZ_READ => unsafe { gzclose_r(state) },
            _ => gzclose_w(state),
        },
        None => Z_STREAM_ERROR,
    };

    unsafe { free_state(file.cast::<GzState>()) };

    ret
}

// Close a gzip file that was opened for reading.
//
// # Returns
//
// * Z_OK if `state` has no outstanding error and the file is closed successfully.
// * A Z_ error code if the `state` is null or the file close operation fails.
//
// # Safety
//
// `state` must have been properly initialized, e.g. by [`gzopen_help`].
unsafe fn gzclose_r(state: &mut GzState) -> c_int {
    if state.mode != GzMode::GZ_READ {
        return Z_STREAM_ERROR;
    }

    // Free buffers and close the file
    if state.in_size != 0 {
        // Safety: state.stream was properly initialized as a z_stream in gzopen_help.
        unsafe { inflateEnd(&mut state.stream as *mut z_stream) };
    }
    let err = match state.err {
        Z_BUF_ERROR => Z_BUF_ERROR,
        _ => Z_OK,
    };
    match unsafe { libc::close(state.fd) } {
        0 => err,
        _ => Z_ERRNO,
    }
}

// Close a gzip file that was opened for writing.
//
// # Returns
//
// * Z_OK if `state` has no outstanding error and the file is closed successfully.
// * A Z_ error code if the `state` is null or the file close operation fails.
//
// # Safety
//
// `state` must have been properly initialized, e.g. by [`gzopen`] or [`gzdopen`].
fn gzclose_w(state: &mut GzState) -> c_int {
    if state.mode != GzMode::GZ_WRITE {
        return Z_STREAM_ERROR;
    }

    let mut ret = Z_OK;

    // Check for a pending seek request
    if state.seek {
        state.seek = false;
        // FIXME add a call to gz_zero(state, state.skip) when write support is implemented.
        ret = state.err;
    }

    // FIXME add a call go gz_comp(state, Z_FINISH) when write support is implemented.

    if state.in_size != 0 && !state.direct {
        // Safety: state.stream was properly initialized as a z_stream in gzopen_help.
        unsafe { deflateEnd(&mut state.stream as *mut z_stream) };
    }
    if unsafe { libc::close(state.fd) } == -1 {
        ret = Z_ERRNO;
    }

    ret
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
/// - A file handle obtained from [`gzopen`] or [`gzdopen`].
/// - A null pointer.
///
/// If this function returns a non-null string, the caller must not modifiy or
/// deallocate the string.
///
/// If `errnum` is non-null, it must point to an address at which a [`c_int`] may be written.
#[cfg_attr(feature = "export-symbols", export_name = crate::prefix!(gzerror))]
pub unsafe extern "C-unwind" fn gzerror(file: gzFile, errnum: *mut c_int) -> *const c_char {
    // Get internal structure and check integrity
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

/// Clear the error and end-of-file state for `file`.
///
/// # Arguments
///
/// * `file` - A gzip file handle, or null
///
/// # Safety
///
/// `file`, if non-null, must be an open file handle obtained from [`gzopen`] or [`gzdopen`].
#[cfg_attr(feature = "export-symbols", export_name = crate::prefix!(gzclearerr))]
pub unsafe extern "C-unwind" fn gzclearerr(file: gzFile) {
    // Get internal structure and check integrity
    let Some(state) = (unsafe { file.cast::<GzState>().as_mut() }) else {
        return;
    };
    if state.mode != GzMode::GZ_READ && state.mode != GzMode::GZ_WRITE {
        return;
    }

    // Clear error and EOF
    if state.mode == GzMode::GZ_READ {
        state.eof = false;
        state.past = false;
    }

    // Safety: we've checked state, and gz_error supports a null message argument.
    unsafe { gz_error(state, None) };
}

/// Check whether a read operation has tried to read beyond the end of `file`.
///
/// # Returns
///
/// * 1 if the end-of-file indicator is set. Note that this indicator is set only
///   if a read tries to go past the end of the input. If the last read request
///   attempted to read exactly the number of bytes remaining in the file, the
///   end-of-file indicator will not be set.
/// * 0 the end-of-file indicator is not set or `file` is null
///
/// # Arguments
///
/// * `file` - A gzip file handle, or null
///
/// # Safety
///
/// `file`, if non-null, must be an open file handle obtained from [`gzopen`] or [`gzdopen`].
#[cfg_attr(feature = "export-symbols", export_name = crate::prefix!(gzeof))]
pub unsafe extern "C-unwind" fn gzeof(file: gzFile) -> c_int {
    // Get internal structure and check integrity
    let Some(state) = (unsafe { file.cast::<GzState>().as_ref() }) else {
        return 0;
    };
    if state.mode != GzMode::GZ_READ {
        return 0;
    }

    // Per the semantics described in the function comments above, the return value
    // is based on state.past, rather than state.eof.
    state.past as _
}

/// Check whether `file` is in direct mode (reading or writing literal bytes without compression).
///
/// NOTE: If `gzdirect` is called immediately after [`gzopen`] or [`gzdopen`], it may allocate
/// buffers internally to read the file header and determine whether the content is a gzip file.
/// If [`gzbuffer`] is used, it should be called before `gzdirect`.
///
/// # Returns
///
/// 0 if `file` is null.
///
/// If `file` is being read,
/// * 1 if the contents are being read directly, without decompression.
/// * 0 if the contents are being decompressed when read.
///
/// If `file` is being written,
/// * 1 if transparent mode was requested upon open (with the `"wT"` mode flag for [`gzopen`]).
/// * 0 otherwise.
///
/// # Arguments
///
/// * `file` - A gzip file handle, or null
///
/// # Safety
///
/// `file`, if non-null, must be an open file handle obtained from [`gzopen`] or [`gzdopen`].
#[cfg_attr(feature = "export-symbols", export_name = crate::prefix!(gzdirect))]
pub unsafe extern "C-unwind" fn gzdirect(file: gzFile) -> c_int {
    let Some(state) = (unsafe { file.cast::<GzState>().as_mut() }) else {
        return 0;
    };

    // In write mode, the direct flag was set in `gzopen_help`. In read mode, the
    // direct status is determined lazily on the first read operation. If the first
    // read hasn't happened yet, we can look at the header now to determine if the
    // file is in gzip format.
    if state.mode == GzMode::GZ_READ && state.how == How::Look && state.have == 0 {
        _ = unsafe { gz_look(state) };
    }

    state.direct as _
}

// Given a gzip file opened for reading, check for a gzip header, and set
// `state.direct` accordingly.
//
// # Returns
//
// * 0 on success
// * -1 on failure
//
// # Safety
//
// `state` must have been properly initialized, e.g. by [`gzopen_help`].
unsafe fn gz_look(state: &mut GzState) -> c_int {
    // Allocate buffers if needed.
    if state.input.is_null() {
        state.in_size = state.want;
        let Some(input) = ALLOCATOR.allocate_slice_raw::<u8>(state.in_size) else {
            // Safety:
            unsafe { gz_error(state, Some((Z_MEM_ERROR, "out of memory"))) };
            return -1;
        };
        state.input = input.as_ptr();
    }
    if state.output.is_null() {
        state.out_size = state.want * 2;
        let Some(output) = ALLOCATOR.allocate_slice_raw::<u8>(state.out_size) else {
            // Safety: The caller confirmed the validity of state, and free_buffers checks
            // for null input and output pointers internally.
            unsafe { free_buffers(state) };
            // Safety: The caller confirmed the validity of state.
            unsafe { gz_error(state, Some((Z_MEM_ERROR, "out of memory"))) };
            return -1;
        };
        state.output = output.as_ptr();
    }

    // Allocate memory for inflate.
    state.stream.avail_in = 0;
    state.stream.next_in = ptr::null_mut();
    // Safety: `gzopen_help` initialized `state.stream`'s `zalloc`, `zfree`, and
    // `opaque` fields as needed by `inflateInit2`.
    if unsafe { inflateInit2(&mut state.stream as *mut z_stream, MAX_WBITS + 16) } != Z_OK {
        // Safety: The caller confirmed the validity of `state`, and `free_buffers` checks
        // for null input and output pointers internally.
        unsafe { free_buffers(state) };
        // Safety: The caller confirmed the validity of `state`.
        unsafe { gz_error(state, Some((Z_MEM_ERROR, "out of memory"))) };
        return -1;
    }

    // Get at least the magic bytes in the input buffer.
    if state.stream.avail_in < 2 {
        // Safety: The caller confirmed the validity of `state`.
        if unsafe { gz_avail(state) } == -1 {
            return -1;
        }
        if state.stream.avail_in == 0 {
            return 0;
        }
    }

    // Look for gzip magic bytes.
    // Safety: `gz_avail` ensures that `next_in` points to at least `avail_in` readable bytes.
    if state.stream.avail_in > 1
        && unsafe { *state.stream.next_in } == 31
        && unsafe { *state.stream.next_in.add(1) } == 139
    {
        // Safety: We initialized `state.stream` with `inflateInit2` above.
        unsafe { inflateReset(&mut state.stream as *mut z_stream) };
        state.how = How::Gzip;
        state.direct = false;
        return 0;
    }

    // No gzip header. If we were decoding gzip before, the remaining bytes
    // are trailing garbage that can be ignored.
    if !state.direct {
        state.stream.avail_in = 0;
        state.eof = true;
        state.have = 0;
        return 0;
    }

    // The file is not in gzip format, so enable direct mode, and copy all
    // buffered input to the output.
    // Safety:
    // * `state.output` was allocated above.
    // * `gz_avail` ensures that `next_in` points to at least `avail_in` readable bytes.
    unsafe {
        ptr::copy_nonoverlapping(
            state.stream.next_in,
            state.output,
            state.stream.avail_in as usize,
        )
    };
    state.next = state.output;
    state.have = state.stream.avail_in;
    state.stream.avail_in = 0;
    state.how = How::Copy;
    state.direct = true;

    0
}

// Load data into the input buffer and set the eof flag if the last of the data has been
// loaded.
//
// # Returns
//
// * -1 if an error occurs.
// * 0 otherwise.
//
// # Safety
//
// `state` must have been properly initialized, e.g. by [`gzopen_help`].
unsafe fn gz_avail(state: &mut GzState) -> c_int {
    if state.err != Z_OK && state.err != Z_BUF_ERROR {
        return -1;
    }
    if !state.eof {
        if state.stream.avail_in != 0 {
            // Copy any remaining input to the start.
            unsafe {
                ptr::copy_nonoverlapping(
                    state.stream.next_in,
                    state.input,
                    state.stream.avail_in as usize,
                )
            };
        }
        let Ok(got) = (unsafe {
            gz_load(
                state,
                state.input.add(state.stream.avail_in as usize),
                state.in_size - state.stream.avail_in as usize,
            )
        }) else {
            return -1;
        };
        state.stream.avail_in += got as uInt;
        state.stream.next_in = state.input;
    }
    0
}

unsafe fn gz_load(state: &mut GzState, buf: *mut u8, len: size_t) -> Result<size_t, ()> {
    let mut have = 0;
    let mut ret = 0;
    while have < len {
        ret = unsafe { libc::read(state.fd, buf.add(have).cast::<_>(), (len - have) as _) };
        if ret <= 0 {
            break;
        }
        have += ret as size_t;
    }
    if ret < 0 {
        unsafe { gz_error(state, Some((Z_ERRNO, "read error"))) }; // FIXME implement `zstrerror`
        return Err(());
    }
    if ret == 0 {
        state.eof = true;
    }
    Ok(have)
}

// Create a deep copy of a C string using `ALLOCATOR`
//
// # Safety
//
// The caller must ensure that s is either null or a pointer to a null-terminated C string.
unsafe fn gz_strdup(src: *const c_char) -> *mut c_char {
    if src.is_null() {
        return ptr::null_mut();
    }

    // SAFETY: the caller must ensure this is a valid C string
    let src = unsafe { CStr::from_ptr(src) };

    let len = src.to_bytes_with_nul().len();
    let Some(dst) = ALLOCATOR.allocate_slice_raw::<c_char>(len) else {
        return ptr::null_mut();
    };

    // SAFETY: src and dst don't overlap, because dst was just allocated. src is valid for a read
    // of len bytes, and dst is valid for a write of len bytes.
    unsafe { core::ptr::copy_nonoverlapping(src.as_ptr(), dst.as_ptr(), len) };

    dst.as_ptr()
}

// Create a new C string, allocated using `ALLOCATOR`, that contains the
// concatenation of zero or more C strings.
//
// # Returns
//
// * A pointer to a C string, for which the caller receives ownership,
// * Or a null pointer upon error.
//
// # Safety
//
// * The return value, if non-null, must be freed using `ALLOCATOR`.
unsafe fn gz_strcat(strings: &[&str]) -> *mut c_char {
    let mut len = 1; // 1 for null terminator
    for src in strings {
        len += src.len();
    }
    let Some(buf) = ALLOCATOR.allocate_slice_raw::<c_char>(len) else {
        return ptr::null_mut();
    };
    let start = buf.as_ptr().cast::<c_char>();
    let mut dst = start.cast::<u8>();
    for src in strings {
        let size = src.len();
        unsafe {
            ptr::copy_nonoverlapping(src.as_ptr(), dst, size);
        };
        dst = unsafe { dst.add(size) };
    }
    unsafe { *dst = 0 };
    start
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_configure() {
        let mut state = core::mem::MaybeUninit::<GzState>::zeroed();
        let state = unsafe { state.assume_init_mut() };

        state.configure(b"r").unwrap();
        assert_eq!(state.mode, GzMode::GZ_READ);
        state.configure(b"rw").unwrap();
        assert_eq!(state.mode, GzMode::GZ_WRITE);
        state.configure(b"wr").unwrap();
        assert_eq!(state.mode, GzMode::GZ_READ);

        state.configure(b"4").unwrap();
        assert_eq!(state.level, 4);
        state.configure(b"64").unwrap();
        assert_eq!(state.level, 4);

        state.configure(b"f").unwrap();
        assert_eq!(state.strategy, Strategy::Filtered);
        state.configure(b"h").unwrap();
        assert_eq!(state.strategy, Strategy::HuffmanOnly);
        state.configure(b"R").unwrap();
        assert_eq!(state.strategy, Strategy::Rle);
        state.configure(b"F").unwrap();
        assert_eq!(state.strategy, Strategy::Fixed);

        // Unknown characters are ignored.
        state.configure(b"xqz").unwrap();

        // Plus errors (read + write mode is not supported)
        state.configure(b"123+").unwrap_err();

        assert_eq!(state.configure(b""), Ok((false, false)));
        assert_eq!(state.configure(b"x"), Ok((true, false)));
        assert_eq!(state.configure(b"e"), Ok((false, true)));
        assert_eq!(state.configure(b"xe"), Ok((true, true)));
    }

    // Map a byte string literal to a C string
    // FIXME: switch to c"example" format once MSRV >= 1.77
    macro_rules! c {
        ($s:literal) => {{
            $s.as_ptr().cast::<c_char>()
        }};
    }

    #[test]
    fn gzdopen_invalid_fd() {
        assert_eq!(unsafe { gzdopen(-1, c!(b"r\0")) }, core::ptr::null_mut())
    }

    #[test]
    fn gzopen_path_null() {
        assert_eq!(
            unsafe { gzopen(core::ptr::null(), c!(b"r\0")) },
            core::ptr::null_mut()
        )
    }

    #[test]
    fn gzopen_mode_null() {
        assert_eq!(
            unsafe { gzopen(c!(b"/foo/bar\0"), core::ptr::null(),) },
            core::ptr::null_mut()
        )
    }

    #[test]
    fn test_gz_strdup() {
        let src = ptr::null();
        let dup = unsafe { gz_strdup(src) };
        assert!(dup.is_null());

        let src = b"\0";
        let dup = unsafe { gz_strdup(src.as_ptr().cast::<c_char>()) };
        assert!(!dup.is_null());
        assert_eq!(unsafe { CStr::from_ptr(dup) }.to_bytes_with_nul(), src);
        unsafe { ALLOCATOR.deallocate(dup, libc::strlen(dup) + 1) };

        let src = b"example\0";
        let dup = unsafe { gz_strdup(src.as_ptr().cast::<c_char>()) };
        assert!(!dup.is_null());
        assert_eq!(unsafe { CStr::from_ptr(dup) }.to_bytes_with_nul(), src);
        unsafe { ALLOCATOR.deallocate(dup, libc::strlen(dup) + 1) };
    }

    #[test]
    fn test_gz_strcat() {
        let src = [];
        let dup = unsafe { gz_strcat(&src) };
        assert!(!dup.is_null());
        assert_eq!(unsafe { libc::strlen(dup) }, 0);
        unsafe { ALLOCATOR.deallocate(dup, libc::strlen(dup) + 1) };

        let src = ["example"];
        let dup = unsafe { gz_strcat(&src) };
        assert!(!dup.is_null());
        assert_eq!(
            unsafe { CStr::from_ptr(dup) }.to_bytes_with_nul(),
            b"example\0"
        );
        unsafe { ALLOCATOR.deallocate(dup, libc::strlen(dup) + 1) };

        let src = ["hello", "", ",", "world"];
        let dup = unsafe { gz_strcat(&src) };
        assert!(!dup.is_null());
        assert_eq!(
            unsafe { CStr::from_ptr(dup) }.to_bytes_with_nul(),
            b"hello,world\0"
        );
        unsafe { ALLOCATOR.deallocate(dup, libc::strlen(dup) + 1) };
    }

    #[test]
    fn test_fd_path() {
        let mut buf = [0u8; 27];
        assert_eq!(fd_path(&mut buf, 0).to_bytes(), b"<fd:0>");
        assert_eq!(fd_path(&mut buf, 9).to_bytes(), b"<fd:9>");
        assert_eq!(fd_path(&mut buf, -1).to_bytes(), b"<fd:-1>");
        assert_eq!(
            fd_path(&mut buf, i32::MIN).to_bytes(),
            format!("<fd:{}>", i32::MIN).as_bytes(),
        );
    }

    #[test]
    #[cfg_attr(not(target_os = "linux"), ignore = "lseek is not implemented")]
    fn test_gz_error() {
        // Open a gzip stream with an invalid file handle. Initially, no error
        // status should be set.
        let handle = unsafe { gzdopen(-2, c!(b"r\0")) };
        assert!(!handle.is_null());

        let state = (unsafe { handle.cast::<GzState>().as_mut() }).unwrap();
        assert_eq!(state.err, Z_OK);
        assert!(state.msg.is_null());
        let mut err = Z_ERRNO;
        let msg = unsafe { gzerror(handle, &mut err as *mut c_int) };
        assert_eq!(unsafe { CStr::from_ptr(msg) }.to_bytes_with_nul(), b"\0");
        assert_eq!(err, Z_OK);

        // When an error is set, the path should be prepended to the error message automatically.
        let state = (unsafe { handle.cast::<GzState>().as_mut() }).unwrap();
        unsafe { gz_error(state, Some((Z_ERRNO, "example error"))) };
        assert_eq!(state.err, Z_ERRNO);
        assert_eq!(
            unsafe { CStr::from_ptr(state.msg) }.to_bytes_with_nul(),
            b"<fd:-2>: example error\0"
        );
        let mut err = Z_OK;
        let msg = unsafe { gzerror(handle, &mut err as *mut c_int) };
        assert_eq!(
            unsafe { CStr::from_ptr(msg) }.to_bytes_with_nul(),
            b"<fd:-2>: example error\0"
        );
        assert_eq!(err, Z_ERRNO);

        // Setting the error message to null should clear the old error message.
        let state = (unsafe { handle.cast::<GzState>().as_mut() }).unwrap();
        unsafe { gz_error(state, None) };
        assert_eq!(state.err, Z_OK);
        assert!(state.msg.is_null());
        let mut err = Z_ERRNO;
        let msg = unsafe { gzerror(handle, &mut err as *mut c_int) };
        assert_eq!(unsafe { CStr::from_ptr(msg) }.to_bytes_with_nul(), b"\0");
        assert_eq!(err, Z_OK);

        // Setting the error code to Z_MEM_ERROR should clear the internal error message
        // (because gz_error doesn't try to allocate space for a copy of the message if
        // the reason for the error is that allocations are failing).
        let state = (unsafe { handle.cast::<GzState>().as_mut() }).unwrap();
        unsafe { gz_error(state, Some((Z_MEM_ERROR, "should be ignored"))) };
        assert_eq!(state.err, Z_MEM_ERROR);
        assert!(state.msg.is_null());
        let mut err = Z_OK;
        let msg = unsafe { gzerror(handle, &mut err as *mut c_int) };
        assert_eq!(
            unsafe { CStr::from_ptr(msg) }.to_bytes_with_nul(),
            b"out of memory\0"
        );
        assert_eq!(err, Z_MEM_ERROR);

        // gzclose should return an error because the fd is invalid.
        assert_eq!(unsafe { gzclose(handle) }, Z_ERRNO);
    }

    #[test]
    #[cfg_attr(not(target_os = "linux"), ignore = "lseek is not implemented")]
    fn test_gzclearerr() {
        // gzclearerr on a null file handle should return quietly.
        unsafe { gzclearerr(ptr::null_mut()) };

        // Open a gzip stream with an invalid file handle. Initially, no error
        // status should be set.
        let handle = unsafe { gzdopen(-2, c!(b"r\0")) };
        assert!(!handle.is_null());

        // gzclearerr should reset the eof and past flags.
        unsafe { handle.cast::<GzState>().as_mut().unwrap().eof = true };
        unsafe { handle.cast::<GzState>().as_mut().unwrap().past = true };
        unsafe { gzclearerr(handle) };
        assert!(!unsafe { handle.cast::<GzState>().as_ref().unwrap().eof });
        assert!(!unsafe { handle.cast::<GzState>().as_ref().unwrap().past });

        // Set an error flag and message.
        unsafe {
            gz_error(
                handle.cast::<GzState>().as_mut().unwrap(),
                Some((Z_STREAM_ERROR, "example error")),
            )
        };
        let mut err = Z_OK;
        let msg = unsafe { gzerror(handle, &mut err as *mut c_int) };
        assert_eq!(err, Z_STREAM_ERROR);
        assert_eq!(
            unsafe { CStr::from_ptr(msg) }.to_bytes_with_nul(),
            b"<fd:-2>: example error\0"
        );

        // gzclearerr should clear the error flag and message.
        unsafe { gzclearerr(handle) };
        let msg = unsafe { gzerror(handle, &mut err as *mut c_int) };
        assert_eq!(err, Z_OK);
        assert_eq!(unsafe { CStr::from_ptr(msg) }.to_bytes_with_nul(), b"\0");

        // gzclose should return an error because the fd is invalid.
        assert_eq!(unsafe { gzclose(handle) }, Z_ERRNO);

        // Test the write and append modes, where gzclearerr should not clear eof or past.
        for mode in [c!(b"w\0"), c!(b"a\0")] {
            // Open a gzip stream for write with an invalid file handle. gzeof should return 0.
            let handle = unsafe { gzdopen(-2, mode) };
            assert!(!handle.is_null());
            assert_eq!(unsafe { gzeof(handle) }, 0);

            // gzclearerr should not reset the eof and past flags in write or append mode.
            unsafe { handle.cast::<GzState>().as_mut().unwrap().eof = true };
            unsafe { handle.cast::<GzState>().as_mut().unwrap().past = true };
            unsafe { gzclearerr(handle) };
            assert!(unsafe { handle.cast::<GzState>().as_ref().unwrap().eof });
            assert!(unsafe { handle.cast::<GzState>().as_ref().unwrap().past });

            // Set an error flag and message.
            unsafe {
                gz_error(
                    handle.cast::<GzState>().as_mut().unwrap(),
                    Some((Z_STREAM_ERROR, "example error")),
                )
            };

            // gzclearerr should clear the error flag and message.
            unsafe { gzclearerr(handle) };
            let msg = unsafe { gzerror(handle, &mut err as *mut c_int) };
            assert_eq!(err, Z_OK);
            assert_eq!(unsafe { CStr::from_ptr(msg) }.to_bytes_with_nul(), b"\0");

            // gzclose should return an error because the fd is invalid.
            assert_eq!(unsafe { gzclose(handle) }, Z_ERRNO);
        }
    }

    #[test]
    #[cfg_attr(not(target_os = "linux"), ignore = "lseek is not implemented")]
    fn test_gzeof() {
        // gzeof on a null file handle should return false.
        assert_eq!(unsafe { gzeof(ptr::null_mut()) }, 0);

        // Open a gzip stream for read with an invalid file handle. gzeof should return 0.
        let handle = unsafe { gzdopen(-2, c!(b"r\0")) };
        assert!(!handle.is_null());
        assert_eq!(unsafe { gzeof(handle) }, 0);

        // gzeof should return 1 only if there was a read attempt past the end of the stream.
        unsafe { handle.cast::<GzState>().as_mut().unwrap().eof = true };
        assert_eq!(unsafe { gzeof(handle) }, 0);
        unsafe { handle.cast::<GzState>().as_mut().unwrap().past = true };
        assert_eq!(unsafe { gzeof(handle) }, 1);

        // gzclose should return an error because the fd is invalid.
        assert_eq!(unsafe { gzclose(handle) }, Z_ERRNO);

        // Test the write and append modes, where gzeof should always return 0.
        for mode in [c!(b"w\0"), c!(b"a\0")] {
            // Open a gzip stream for write with an invalid file handle. gzeof should return 0.
            let handle = unsafe { gzdopen(-2, mode) };
            assert!(!handle.is_null());
            assert_eq!(unsafe { gzeof(handle) }, 0);

            // Even with the past flag set, gzeof should still return 0 in write or append mode.
            unsafe { handle.cast::<GzState>().as_mut().unwrap().past = true };
            assert_eq!(unsafe { gzeof(handle) }, 0);

            // gzclose should return an error because the fd is invalid.
            assert_eq!(unsafe { gzclose(handle) }, Z_ERRNO);
        }
    }
}
