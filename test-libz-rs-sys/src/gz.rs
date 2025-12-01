use z_rs::{
    gzFile_s, gzbuffer, gzclearerr, gzclose, gzclose_r, gzclose_w, gzdirect, gzdopen, gzerror,
    gzflush, gzfread, gzfwrite, gzgetc, gzgetc_, gzgets, gzoffset, gzopen, gzopen64, gzputc,
    gzputs, gzread, gzrewind, gzseek, gzsetparams, gztell, gzungetc, gzwrite,
};
use zlib_rs::c_api::*;

use libc::size_t;
use std::ffi::{c_char, c_int, c_uint, c_void, CStr, CString};
use std::path::{Path, PathBuf};
use std::ptr;

// Generate a file path relative to the project's root
fn crate_path(file: &str) -> String {
    path(Path::new(env!("CARGO_MANIFEST_DIR")), file)
}

// Generate a file path relative to a specified directory prefix
fn path(prefix: &Path, file: &str) -> String {
    let mut path_buf = prefix.to_path_buf();
    path_buf.push(file);
    path_buf.as_path().to_str().unwrap().to_owned()
}

// Return an open(2) mode, modified as needed to support binary files on the target platform.
fn binary_mode(mode: c_int) -> c_int {
    #[cfg(target_os = "windows")]
    {
        mode | libc::O_BINARY
    }

    #[cfg(not(target_os = "windows"))]
    {
        mode
    }
}

// Try to gzopen a file path with a specified mode, and panic if the result is unexpected.
// NOTE: This is a macro, instead of a function, so that the test runner will report errors
// with the line number where it is invoked.
macro_rules! test_open {
    ($path:expr, $mode:expr, $should_succeed:expr) => {
        let cpath = CString::new($path).unwrap();
        let cmode = CString::new($mode).unwrap();
        let handle = unsafe { gzopen(cpath.as_ptr(), cmode.as_ptr()) };
        assert_eq!(
            $should_succeed,
            !handle.is_null(),
            "gzopen({}, {})",
            $path,
            $mode
        );
        if !handle.is_null() {
            assert_eq!(unsafe { gzclose(handle) }, Z_OK, "gzclose({}) error", $path);
        }
    };
}

// Variant of `test_open` that takes a file descriptor
macro_rules! test_fdopen {
    ($fd:expr, $mode:expr, $should_succeed:expr) => {
        let cmode = CString::new($mode).unwrap();
        let handle = unsafe { gzdopen($fd, cmode.as_ptr()) };
        assert_eq!(
            $should_succeed,
            !handle.is_null(),
            "gzdopen({}, {})",
            $fd,
            $mode
        );
        if !handle.is_null() {
            assert_eq!(unsafe { gzclose(handle) }, Z_OK, "gzclose({}) error", $fd);
        }
    };
}
#[test]
fn open_close() {
    // Open a valid file for reading
    test_open!(crate_path("src/test-data/issue-109.gz"), "r", true);

    // "b" for binary mode is optional
    test_open!(crate_path("src/test-data/issue-109.gz"), "rb", true);

    // Mode must include r, w, or a
    test_open!(crate_path("src/test-data/issue-109.gz"), "", false);
    test_open!(crate_path("src/test-data/issue-109.gz"), "e", false);

    // "+" (read plus write) mode isn't supported
    test_open!(crate_path("src/test-data/issue-109.gz"), "+", false);

    // For zlib-ng compatibility, mode can't specify transparent read
    test_open!(crate_path("src/test-data/issue-109.gz"), "Tr", false);

    // Read of a nonexistent file should fail
    test_open!(crate_path("src/test-data/no-such-file.gz"), "r", false);

    // Closing a null file handle should return an error instead of crashing
    assert_eq!(unsafe { gzclose(ptr::null_mut()) }, Z_STREAM_ERROR);

    // Initialize a gzip stream for reading using an open file descriptor
    let cpath = CString::new(crate_path("src/test-data/issue-109.gz")).unwrap();
    let fd = unsafe { libc::open(cpath.as_ptr(), libc::O_RDONLY) };
    assert_ne!(fd, -1);
    test_fdopen!(fd, "r", true);
}

#[test]
fn gzclose_error() {
    // gzclose_r and gzclose_w should return Z_STREAM_ERROR for a null file handle.
    assert_eq!(unsafe { gzclose_r(ptr::null_mut()) }, Z_STREAM_ERROR);
    assert_eq!(unsafe { gzclose_w(ptr::null_mut()) }, Z_STREAM_ERROR);

    // gzclose_r should return Z_STREAM_ERROR when called with a file handle opened for writing.
    let file = unsafe { gzdopen(-2, CString::new("w").unwrap().as_ptr()) };
    assert!(!file.is_null());
    assert_eq!(unsafe { gzclose_r(file) }, Z_STREAM_ERROR);
    // gzclose_w should return Z_ERRNO for this file handle (because we gave it an invalid fd).
    assert_eq!(unsafe { gzclose_w(file) }, Z_ERRNO);

    // gzclose_w should return Z_STREAM_ERROR when called with a file handle opened for reading.
    let file = unsafe { gzdopen(-2, CString::new("r").unwrap().as_ptr()) };
    assert!(!file.is_null());
    assert_eq!(unsafe { gzclose_w(file) }, Z_STREAM_ERROR);
    // gzclose_r should return Z_ERRNO for this file handle (because we gave it an invalid fd).
    assert_eq!(unsafe { gzclose_r(file) }, Z_ERRNO);
}

#[test]
fn create() {
    // Create a temporary directory that will be automatically removed when
    // temp_dir goes out of scope.
    let temp_dir_path = temp_base();
    let temp_dir = tempfile::TempDir::new_in(temp_dir_path).unwrap();
    let temp_path = temp_dir.path();

    // Creation of a new file should work
    test_open!(path(temp_path, "new.gz"), "w", true);

    // File creation should fail with "x" (exclusive) flag if the file already exists
    test_open!(path(temp_path, "new.gz"), "wx", false);
    test_open!(path(temp_path, "different_file.gz"), "wx", true);

    // "e" flag should open for writing with O_CLOEXEC (close file descriptor on exec)
    // on compatible systems and should be silently ignored on other systems.
    test_open!(path(temp_path, "new.gz"), "ew", true);

    // Append mode should create a new file if needed
    test_open!(path(temp_path, "new2.gz"), "a", true);

    // "x" flag should work for append mode, too
    test_open!(path(temp_path, "new2.gz"), "ax", false);
    test_open!(path(temp_path, "new3.gz"), "ax", true);

    // "+" (read plus write) mode isn't supported
    test_open!(path(temp_path, "new4.gz"), "+", false);
}

// Return a platform-appropriate temporary directory prefix.
fn temp_base() -> PathBuf {
    if cfg!(target_os = "wasi") {
        std::path::PathBuf::from("/tmp/")
    } else {
        std::env::temp_dir()
    }
}

#[test]
fn gz_error_access() {
    const UNSET_ERRNO: c_int = -12345;

    // gz_error should return null when given a null file handle
    assert!(unsafe { gzerror(ptr::null_mut::<gzFile_s>(), ptr::null_mut()).is_null() });

    // When the file handle is null, gz_error should not modify the errno
    let mut gz_errno: c_int = UNSET_ERRNO;
    assert!(unsafe { gzerror(ptr::null_mut::<gzFile_s>(), &mut gz_errno as *mut c_int).is_null() });
    assert_eq!(gz_errno, UNSET_ERRNO);

    // Open a valid gzip file; the error should be an empty string
    let path = CString::new(crate_path("src/test-data/issue-109.gz")).unwrap();
    let mode = CString::new("r").unwrap();
    let handle = unsafe { gzopen64(path.as_ptr(), mode.as_ptr()) };
    assert!(!handle.is_null());
    let mut gz_errno: c_int = UNSET_ERRNO;
    let err = unsafe { gzerror(handle, &mut gz_errno as *mut c_int) };
    assert!(!err.is_null());
    assert_eq!(unsafe { *err }, 0 as c_char);
    assert_eq!(unsafe { gzclose(handle) }, Z_OK);
}

#[test]
fn gz_direct_write() {
    // Create a temporary directory that will be automatically removed when
    // temp_dir goes out of scope.
    let temp_dir_path = if cfg!(target_os = "wasi") {
        std::path::PathBuf::from("/tmp/")
    } else {
        std::env::temp_dir()
    };
    let temp_dir = tempfile::TempDir::new_in(temp_dir_path).unwrap();
    let temp_path = temp_dir.path();

    // Test write and append modes, where direct mode is specified by the call to `gzopen`.
    for mode in ["w", "a"] {
        // Open a new file without the "T" mode flag. It should be in non-direct mode.
        let file = unsafe {
            gzopen(
                CString::new(path(temp_path, "compressed.gz"))
                    .unwrap()
                    .as_ptr(),
                CString::new(mode).unwrap().as_ptr(),
            )
        };
        assert!(!file.is_null());
        assert_eq!(unsafe { gzdirect(file) }, 0);
        assert_eq!(unsafe { gzclose(file) }, Z_OK);

        // Open a new file with the "T" mode flag. It should be in direct mode.
        let file = unsafe {
            gzopen(
                CString::new(path(temp_path, "direct.gz")).unwrap().as_ptr(),
                CString::new("T".to_owned() + mode).unwrap().as_ptr(),
            )
        };
        assert!(!file.is_null());
        assert_eq!(unsafe { gzdirect(file) }, 1);
        assert_eq!(unsafe { gzclose(file) }, Z_OK);
    }
}

#[test]
fn gz_direct_read() {
    // gzdirect(null) should return 0.
    assert_eq!(unsafe { gzdirect(ptr::null_mut()) }, 0);

    // Open a gzip file for reading. gzdirect should return 0.
    let file = unsafe {
        gzopen(
            CString::new(crate_path("src/test-data/issue-109.gz"))
                .unwrap()
                .as_ptr(),
            CString::new("r").unwrap().as_ptr(),
        )
    };
    assert!(!file.is_null());
    assert_eq!(unsafe { gzdirect(file) }, 0);
    assert_eq!(unsafe { gzclose(file) }, Z_OK);

    // Open a non-gzip file for reading. gzdirect should return 1.
    let file = unsafe {
        gzopen(
            CString::new(crate_path("src/test-data/issue-169.js"))
                .unwrap()
                .as_ptr(),
            CString::new("r").unwrap().as_ptr(),
        )
    };
    assert!(!file.is_null());
    assert_eq!(unsafe { gzdirect(file) }, 1);
    assert_eq!(unsafe { gzclose(file) }, Z_OK);

    // Open a gzip stream from an invalid file descriptor. gzdirect should return 1, but
    // it should cause an error condition to be set within the stream.
    let file = unsafe { gzdopen(-2, CString::new("r").unwrap().as_ptr()) };
    assert!(!file.is_null());
    assert_eq!(unsafe { gzdirect(file) }, 1);
    let mut err = Z_OK;
    let msg = unsafe { gzerror(file, &mut err as *mut c_int) };
    assert!(!msg.is_null());
    assert_eq!(err, Z_ERRNO);
    assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);
}

#[test]
fn gzread_special_cases() {
    let mut buf = [0u8; 10];

    // gzread on an null file handle should return -1.
    assert_eq!(
        unsafe {
            gzread(
                ptr::null_mut(),
                buf.as_mut_ptr().cast::<c_void>(),
                buf.len() as c_uint,
            )
        },
        -1
    );

    // Open a gzip file for writing. gzread should return -1.
    for mode in ["w", "a"] {
        // The fd here is invalid, but it's enough to construct a gzFile so we can exercise
        // the code path in gzread that checks for read mode.
        let file = unsafe { gzdopen(-2, CString::new(mode).unwrap().as_ptr()) };
        assert!(!file.is_null());
        assert_eq!(
            unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), buf.len() as c_uint) },
            -1
        );
        assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);
    }

    // Create a gzip file with some junk data after the end of the deflate stream.
    // gzread should ignore the junk data.
    // Create a temporary directory that will be automatically removed when
    // temp_dir goes out of scope.
    let temp_dir_path = temp_base();
    let temp_dir = tempfile::TempDir::new_in(temp_dir_path).unwrap();
    let temp_path = temp_dir.path();
    let file_name = path(temp_path, "output.gz");
    let file = unsafe {
        gzopen(
            CString::new(file_name.as_str()).unwrap().as_ptr(),
            CString::new("w").unwrap().as_ptr(),
        )
    };
    assert!(!file.is_null());
    const STRING1: &[u8] = b"deflated contents";
    const STRING2: &[u8] = b"\ntrailing data";
    assert_eq!(
        unsafe { gzwrite(file, STRING1.as_ptr().cast::<c_void>(), STRING1.len() as _) },
        STRING1.len() as _
    );
    assert_eq!(unsafe { gzclose(file) }, Z_OK);
    let old_size = file_size(&file_name).unwrap();
    let fd = unsafe {
        libc::open(
            CString::new(file_name.as_str()).unwrap().as_ptr(),
            binary_mode(libc::O_APPEND | libc::O_WRONLY),
        )
    };
    assert_ne!(fd, -1);
    assert_eq!(
        unsafe { libc::write(fd, STRING2.as_ptr().cast::<c_void>(), STRING2.len() as _) },
        STRING2.len() as _
    );
    assert_eq!(unsafe { libc::close(fd) }, 0);
    let new_size = file_size(&file_name).unwrap();
    assert_eq!(new_size, old_size + STRING2.len());
    let file = unsafe {
        gzopen(
            CString::new(file_name.as_str()).unwrap().as_ptr(),
            CString::new("r").unwrap().as_ptr(),
        )
    };
    assert!(!file.is_null());
    // Read more than expected to make sure we get everything.
    let mut buf = [0u8; STRING1.len() + 1];
    assert_eq!(
        unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), buf.len() as _) },
        STRING1.len() as _
    );
    assert_eq!(unsafe { gzclose(file) }, Z_OK);
}

#[test]
fn gzread_gzip() {
    const BUF_SIZE: usize = 128;
    const MAX_READ_SIZE: usize = 256;
    let mut buf = [0u8; MAX_READ_SIZE];

    // Open a valid gzip file for reading.
    let file = unsafe {
        gzopen(
            CString::new(crate_path("src/test-data/issue-109.gz"))
                .unwrap()
                .as_ptr(),
            CString::new("r").unwrap().as_ptr(),
        )
    };
    assert!(!file.is_null());
    // Set a small buffer size to help exercise the various buffer-refilling code paths.
    assert_eq!(unsafe { gzbuffer(file, BUF_SIZE as c_uint) }, 0);
    // Try to read more bytes than can be represented by a c_int. gzread should return -1.
    let len = c_int::MAX as c_uint + 1;
    assert_eq!(
        unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), len as c_uint) },
        -1
    );
    let mut err = Z_OK;
    assert!(!unsafe { gzerror(file, &mut err as *mut c_int) }.is_null());
    assert_eq!(err, Z_STREAM_ERROR);
    // Try a read of a more reasonable number of bytes. This should fail because the last
    // gzread set the file handle's internal error status.
    assert_eq!(
        unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), 1) },
        -1
    );
    // Clear the error state an retry the read. This time the read should succeed.
    unsafe { gzclearerr(file) };
    assert_eq!(
        unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), 10) },
        10
    );
    assert_eq!(&buf[..10], b"#mtree\n/se");
    // Read until we hit the end of the file. The number of bytes obtained by gz_read should match the decompressed file size.
    let mut bytes_read: usize = 10;
    loop {
        // Do large reads, compared to the file handle's internal buffer size, to exercise the
        // optimized code paths that bypass the intermediate buffer.
        let ret = unsafe {
            gzread(
                file,
                buf.as_mut_ptr().cast::<c_void>(),
                MAX_READ_SIZE as c_uint,
            )
        };
        assert!(ret >= 0);
        if ret == 0 {
            break;
        }
        bytes_read += ret as usize;
    }
    assert_eq!(bytes_read, 126094);

    assert_eq!(unsafe { gzclose(file) }, Z_OK);
}

#[test]
fn gzread_direct() {
    const BUF_SIZE: usize = 24;
    const MAX_READ_SIZE: usize = 256;
    let mut buf = [0u8; MAX_READ_SIZE];

    // Open a non-gzip file for reading.
    let path = crate_path("src/test-data/issue-169.js");
    let file = unsafe {
        gzopen(
            CString::new(path.as_str()).unwrap().as_ptr(),
            CString::new("r").unwrap().as_ptr(),
        )
    };
    assert!(!file.is_null());
    // Set a small buffer size to help exercise the various buffer-refilling code paths.
    assert_eq!(unsafe { gzbuffer(file, BUF_SIZE as c_uint) }, 0);
    // gzread should fetch the specified number of bytes from the start of the file.
    assert_eq!(
        unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), 20 as c_uint) },
        20 as c_int
    );
    assert_eq!(&buf[..20], b"// This file was pro");
    // A zero-byte gzread should return zero.
    assert_eq!(
        unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), 0) },
        0
    );
    // Do a small read to get more of the data in the file handle's output buffer.
    assert_eq!(
        unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), 2 as c_uint) },
        2 as c_int
    );
    assert_eq!(&buf[..2], b"ce");
    // Do another read. This should be satisfied partially by the bytes remaining in the file
    // handle's output buffer, with the remaining bytes being read from the file.
    assert_eq!(
        unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), 20 as c_uint) },
        20 as c_int
    );
    assert_eq!(&buf[..20], b"durally generated fr");
    // Read until we hit the end of the file. The number of bytes obtained by gz_read should match the file size.
    let mut bytes_read: usize = 20 + 2 + 20;
    loop {
        // Do large reads, compared to the file handle's internal buffer size, to exercise the
        // optimized code paths that bypass the intermediate buffer.
        let ret = unsafe {
            gzread(
                file,
                buf.as_mut_ptr().cast::<c_void>(),
                MAX_READ_SIZE as c_uint,
            )
        };
        assert!(ret >= 0);
        if ret == 0 {
            break;
        }
        bytes_read += ret as usize;
    }
    let Ok(size) = file_size(path.as_str()) else {
        panic!("Could not find size of file {path}");
    };
    assert_eq!(bytes_read, size);

    assert_eq!(unsafe { gzclose(file) }, Z_OK);
}

#[test]
fn gzwrite_basic() {
    // Create a temporary directory that will be automatically removed when
    // temp_dir goes out of scope.
    let temp_dir_path = temp_base();
    let temp_dir = tempfile::TempDir::new_in(temp_dir_path).unwrap();
    let temp_path = temp_dir.path();

    // Test both compressed and direct (uncompressed) write modes.
    for (filename, mode) in [("direct", "wT"), ("compressed", "w")] {
        // Open a new file for writing.
        let temp_file = path(temp_path, filename);
        let file = unsafe {
            gzopen(
                CString::new(temp_file.clone()).unwrap().as_ptr(),
                CString::new(mode).unwrap().as_ptr(),
            )
        };
        assert!(!file.is_null());

        let direct = unsafe { gzdirect(file) } == 1;
        assert_eq!(direct, mode.contains('T'));

        // Set a small buffer size to help exercise the various buffer-refilling code paths.
        assert_eq!(unsafe { gzbuffer(file, 20 as c_uint) }, 0);

        const STRING1: &[u8] = b"small write ";
        const STRING2: &[u8] = b"second write ";
        const STRING3: &[u8] = b"0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
        const COPIES: usize = 2;
        for _ in 0..COPIES {
            // A zero-byte write should return zero.
            assert_eq!(
                unsafe { gzwrite(file, STRING1.as_ptr().cast::<c_void>(), 0) },
                0
            );
            // Do a small write that will fit in the input buffer.
            assert_eq!(
                unsafe {
                    gzwrite(
                        file,
                        STRING1.as_ptr().cast::<c_void>(),
                        STRING1.len() as c_uint,
                    )
                },
                STRING1.len() as c_int
            );
            // Fill the input buffer.
            assert_eq!(
                unsafe {
                    gzwrite(
                        file,
                        STRING2.as_ptr().cast::<c_void>(),
                        STRING2.len() as c_uint,
                    )
                },
                STRING2.len() as c_int
            );
            // Do a large write (bigger than the input buffer).
            assert_eq!(
                unsafe {
                    gzwrite(
                        file,
                        STRING3.as_ptr().cast::<c_void>(),
                        STRING3.len() as c_uint,
                    )
                },
                STRING3.len() as c_int
            );
        }

        // Close the file handle to flush any buffered data.
        assert_eq!(unsafe { gzclose(file) }, Z_OK);

        // Verify that the expected data was written to the file.
        let input_len = (STRING1.len() + STRING2.len() + STRING3.len()) * COPIES;
        let size = file_size(&temp_file).unwrap();
        if direct {
            assert_eq!(size, input_len);
        } else {
            // There is enough repetition in the input text to allow for some compression.
            assert!(size < input_len);
        }
        let file = unsafe {
            gzopen(
                CString::new(temp_file.clone()).unwrap().as_ptr(),
                CString::new("r").unwrap().as_ptr(),
            )
        };
        assert!(!file.is_null());
        let mut buf = [0u8; 1024];
        for _ in 0..COPIES {
            for expected in [STRING1, STRING2, STRING3] {
                assert_eq!(
                    unsafe {
                        gzread(
                            file,
                            buf.as_mut_ptr().cast::<c_void>(),
                            expected.len() as c_uint,
                        )
                    },
                    expected.len() as c_int
                );
                assert_eq!(buf[..expected.len()], *expected);
            }
        }
        assert_eq!(
            unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), 1) },
            0
        );
        assert_eq!(unsafe { gzclose(file) }, Z_OK);
    }
}

#[test]
fn gzwrite_error() {
    const STRING1: &[u8] = b"sample data";

    // gzwrite on a null file handle should return 0.
    assert_eq!(
        unsafe {
            gzwrite(
                ptr::null_mut(),
                STRING1.as_ptr().cast::<c_void>(),
                STRING1.len() as _,
            )
        },
        0
    );

    // gzwrite on a read-only file should return 0.
    let file = unsafe {
        gzopen(
            CString::new(crate_path("src/test-data/issue-109.gz"))
                .unwrap()
                .as_ptr(),
            CString::new("r").unwrap().as_ptr(),
        )
    };
    assert_eq!(
        unsafe { gzwrite(file, STRING1.as_ptr().cast::<c_void>(), STRING1.len() as _) },
        0
    );
    assert_eq!(unsafe { gzclose(file) }, Z_OK);

    // gzwrite should return 0 if the requested length does not fit in the return type.
    let len = c_int::MAX as c_uint + 1;
    let file = unsafe { gzdopen(-2, CString::new("w").unwrap().as_ptr()) };
    assert!(!file.is_null());
    assert_eq!(
        unsafe { gzwrite(file, STRING1.as_ptr().cast::<c_void>(), len) },
        0
    );
    let mut err = Z_OK;
    assert!(!unsafe { gzerror(file, &mut err as *mut c_int) }.is_null());
    assert_eq!(err, Z_DATA_ERROR);
    assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);
}

#[test]
fn gzflush_basic() {
    // Create a temporary directory that will be automatically removed when
    // temp_dir goes out of scope.
    let temp_dir_path = temp_base();
    let temp_dir = tempfile::TempDir::new_in(temp_dir_path).unwrap();
    let temp_path = temp_dir.path();

    // Test both compressed and direct (uncompressed) write modes.
    for (filename, mode) in [("direct", "wT"), ("compressed", "w")] {
        // Open a new file for writing.
        let temp_file = path(temp_path, filename);
        let file = unsafe {
            gzopen(
                CString::new(temp_file.clone()).unwrap().as_ptr(),
                CString::new(mode).unwrap().as_ptr(),
            )
        };
        assert!(!file.is_null());

        // Set a buffer size that is large enough to hold all the writes that follow,
        // so that we can check for changes in the file size to verify whether gzflush worked.
        assert_eq!(unsafe { gzbuffer(file, 1024) }, 0);

        const STRING1: &[u8] = b"first write ";
        const STRING2: &[u8] = b"second write ";
        const STRING3: &[u8] = b"third write";
        let mut size_before = file_size(&temp_file).unwrap();
        assert_eq!(size_before, 0);
        // Do a small gzwrite. It should not cause any file I/O yet.
        assert_eq!(
            unsafe { gzwrite(file, STRING1.as_ptr().cast::<c_void>(), STRING1.len() as _) },
            STRING1.len() as _
        );
        let mut size_after = file_size(&temp_file).unwrap();
        assert_eq!(size_before, size_after);
        size_before = size_after;

        // Call gzflush. The buffered data should be written to the file.
        assert_eq!(unsafe { gzflush(file, Z_SYNC_FLUSH) }, Z_OK);
        size_after = file_size(&temp_file).unwrap();
        assert!(size_after > size_before);
        size_before = size_after;

        // Do another small write. It should be buffered and not yet flushed to the file.
        assert_eq!(
            unsafe { gzwrite(file, STRING2.as_ptr().cast::<c_void>(), STRING2.len() as _) },
            STRING2.len() as _
        );
        let mut size_after = file_size(&temp_file).unwrap();
        assert_eq!(size_before, size_after);
        size_before = size_after;

        // Call gzflush with the Z_FINISH option. The buffered data should be written to the file.
        assert_eq!(unsafe { gzflush(file, Z_FINISH) }, Z_OK);
        size_after = file_size(&temp_file).unwrap();
        assert!(size_after > size_before);
        size_before = size_after;

        // After the gzflush(Z_FINISH), it should be possible to do additional writes.
        assert_eq!(
            unsafe { gzwrite(file, STRING3.as_ptr().cast::<c_void>(), STRING3.len() as _) },
            STRING3.len() as _
        );
        size_after = file_size(&temp_file).unwrap();
        assert_eq!(size_before, size_after);
        size_before = size_after;

        // Close the file. This should write out the remaining buffered data.
        assert_eq!(unsafe { gzclose(file) }, Z_OK);
        size_after = file_size(&temp_file).unwrap();
        assert!(size_after > size_before);

        // Try to read the newly created file. Despite containing two distinct gzip segments
        // due to the Z_FINISH, it should be readable.
        let file = unsafe {
            gzopen(
                CString::new(temp_file.clone()).unwrap().as_ptr(),
                CString::new("r").unwrap().as_ptr(),
            )
        };
        assert!(!file.is_null());
        let mut buf = [0u8; 1024];
        for expected in [STRING1, STRING2, STRING3] {
            assert_eq!(
                unsafe {
                    gzread(
                        file,
                        buf.as_mut_ptr().cast::<c_void>(),
                        expected.len() as c_uint,
                    )
                },
                expected.len() as c_int
            );
            assert_eq!(buf[..expected.len()], *expected);
        }

        assert_eq!(unsafe { gzclose(file) }, Z_OK);
    }
}

#[test]
fn gzflush_error() {
    // gzflush on a null file handle should return Z_STREAM_ERROR.
    assert_eq!(
        unsafe { gzflush(ptr::null_mut(), Z_NO_FLUSH) },
        Z_STREAM_ERROR
    );

    // gzflush on a read-only file should return Z_STREAM_ERROR.
    let file = unsafe {
        gzopen(
            CString::new(crate_path("src/test-data/issue-109.gz"))
                .unwrap()
                .as_ptr(),
            CString::new("r").unwrap().as_ptr(),
        )
    };
    assert_eq!(unsafe { gzflush(file, Z_NO_FLUSH) }, Z_STREAM_ERROR);
    assert_eq!(unsafe { gzclose(file) }, Z_OK);

    // gzflush with an invalid flush type should return Z_STREAM_ERROR;
    let file = unsafe { gzdopen(-2, CString::new("w").unwrap().as_ptr()) };
    assert!(!file.is_null());
    assert_eq!(unsafe { gzflush(file, -1) }, Z_STREAM_ERROR);
    assert_eq!(unsafe { gzflush(file, Z_FINISH + 1) }, Z_STREAM_ERROR);
    assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);
}

#[test]
fn gzoffset_gztell_read() {
    // Create a temporary directory that will be automatically removed when
    // temp_dir goes out of scope.
    let temp_dir_path = temp_base();
    let temp_dir = tempfile::TempDir::new_in(temp_dir_path).unwrap();
    let temp_path = temp_dir.path();
    let file_name = path(temp_path, "output.gz");

    // Make a copy of a gzip file with some junk data prepended in front of the gzip
    // header. This should affect the return values of gzoffset but not gztell.
    const OFFSET: usize = 123;
    const PADDING: &[u8] = &[0u8; OFFSET];
    let fd = unsafe {
        libc::open(
            CString::new(file_name.as_str()).unwrap().as_ptr(),
            binary_mode(libc::O_CREAT | libc::O_WRONLY),
            0o644,
        )
    };
    assert_ne!(fd, -1);
    assert_eq!(
        unsafe { libc::write(fd, PADDING.as_ptr().cast::<c_void>(), OFFSET as _) },
        OFFSET as _
    );
    let source_name = crate_path("src/test-data/issue-109.gz");
    let mode = binary_mode(libc::O_RDONLY);
    let source_fd =
        unsafe { libc::open(CString::new(source_name.as_str()).unwrap().as_ptr(), mode) };
    assert_ne!(source_fd, -1);
    let mut buf = [0u8; 1024];
    loop {
        let ret = unsafe { libc::read(source_fd, buf.as_mut_ptr().cast(), buf.len() as _) };
        assert_ne!(ret, -1);
        if ret == 0 {
            break;
        }
        assert_eq!(
            unsafe { libc::write(fd, buf.as_ptr().cast(), ret as _) },
            ret as _
        );
    }
    assert_eq!(unsafe { libc::close(source_fd) }, 0);
    assert_eq!(unsafe { libc::close(fd) }, 0);
    assert_eq!(
        file_size(&file_name).unwrap(),
        file_size(&source_name).unwrap() + OFFSET
    );

    // Open the newly created file, seek past the prepended junk to the gzip header, and
    // use gzdopen to turn the fd into a gzip file handle.
    let fd = unsafe { libc::open(CString::new(file_name.as_str()).unwrap().as_ptr(), mode) };
    assert_ne!(fd, -1);
    assert_eq!(
        unsafe { libc::lseek(fd, OFFSET as _, libc::SEEK_SET) },
        OFFSET as _
    );
    let file = unsafe { gzdopen(fd, CString::new(b"r").unwrap().as_ptr()) };
    assert!(!file.is_null());
    const BUFFER_SIZE: usize = 2048;
    assert_eq!(unsafe { gzbuffer(file, BUFFER_SIZE as _) }, 0);

    // With the file just opened for read, gztell should return 0, while gzoffset should return
    // the offset from the start of the file.
    assert_eq!(unsafe { gztell(file) }, 0);
    assert_eq!(unsafe { gzoffset(file) }, OFFSET as _);

    // Consume N bytes of decompressed data. The output of gztell should increase by N.
    assert_eq!(
        unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), buf.len() as _) },
        buf.len() as _
    );
    assert_eq!(unsafe { gztell(file) }, buf.len() as _);
    // After the read, the output of gzoffset should be OFFSET + BUFFER_SIZE minus the amount
    // of the compressed input that was consumed to produce the requested amount of decompressed
    // output. This works out to 1910 bytes, determined by running the C equivalent of these
    // operations against zlib-ng and original zlib.
    const EXPECTED: i64 = 1910;
    assert_eq!(unsafe { gzoffset(file) }, EXPECTED as _);

    // The file should close cleanly, as gztell and gzoffset do not set the internal error state.
    assert_eq!(unsafe { gzclose(file) }, Z_OK);
}

#[test]
fn gzoffset_gztell_write() {
    // Create a temporary directory that will be automatically removed when
    // temp_dir goes out of scope.
    let temp_dir_path = temp_base();
    let temp_dir = tempfile::TempDir::new_in(temp_dir_path).unwrap();
    let temp_path = temp_dir.path();
    let file_name = path(temp_path, "output.gz");

    // Open a new gzip file for writing.
    let file = unsafe {
        gzopen(
            CString::new(file_name.as_str()).unwrap().as_ptr(),
            CString::new("w").unwrap().as_ptr(),
        )
    };
    assert!(!file.is_null());
    const BUFFER_SIZE: usize = 2048;
    assert_eq!(unsafe { gzbuffer(file, BUFFER_SIZE as _) }, 0);

    // With the new file just opened for write, gztell and gzoffset both should return zero.
    assert_eq!(unsafe { gztell(file) }, 0);
    assert_eq!(unsafe { gzoffset(file) }, 0);

    // Write some data, but not enough to fill the file handle's internal buffers.
    let buf = [0u8; 1024];
    assert_eq!(
        unsafe { gzwrite(file, buf.as_ptr().cast::<c_void>(), buf.len() as _) },
        buf.len() as _
    );
    // gztell should return the number of bytes written into the file handle.
    assert_eq!(unsafe { gztell(file) }, buf.len() as _);
    // gzoffset should return zero because nothing has been written to the file yet.
    assert_eq!(unsafe { gzoffset(file) }, 0);
    // Force a write to the file.
    assert_eq!(unsafe { gzflush(file, Z_SYNC_FLUSH) }, Z_OK);
    // gztell should still return the number of bytes written into the file handle.
    assert_eq!(unsafe { gztell(file) }, buf.len() as _);
    // gzoffset should indicate that some data has been written to the file.
    assert!(unsafe { gzoffset(file) } > 0);

    let size = file_size(&file_name).unwrap();

    // Close the file and reopen it in append mode.
    assert_eq!(unsafe { gzclose(file) }, Z_OK);
    let file = unsafe {
        gzopen(
            CString::new(file_name.as_str()).unwrap().as_ptr(),
            CString::new("a").unwrap().as_ptr(),
        )
    };

    // After opening in append mode, gztell should return zero.
    assert_eq!(unsafe { gztell(file) }, 0);
    // But gzoffset should return the number of bytes that were already in the file, plus
    // 10 bytes for a new gzip header.
    assert_eq!(unsafe { gzoffset(file) }, (size + 10) as _);

    assert_eq!(unsafe { gzclose(file) }, Z_OK);
}

#[test]
fn gzoffset_gztell_error() {
    // gzoffset(null) should return -1.
    assert_eq!(unsafe { gzoffset(ptr::null_mut()) }, -1);
    // gztell(null) should return -1.
    assert_eq!(unsafe { gztell(ptr::null_mut()) }, -1);

    // Open an invalid file descriptor as a gzip stream. gztell should return zero, but
    // gzoffset should return -1 because it actually checks the location in the file.
    for mode in [b"r", b"w", b"a"] {
        let file = unsafe { gzdopen(-2, CString::new(mode).unwrap().as_ptr()) };
        assert!(!file.is_null());
        assert_eq!(unsafe { gztell(file) }, 0);
        assert_eq!(unsafe { gzoffset(file) }, -1);
        assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);
    }
}

#[test]
fn gzputc_basic() {
    // Create a temporary directory that will be automatically removed when
    // temp_dir goes out of scope.
    let temp_dir_path = temp_base();
    let temp_dir = tempfile::TempDir::new_in(temp_dir_path).unwrap();
    let temp_path = temp_dir.path();
    let file_name = path(temp_path, "output");

    // Open a new gzip file for writing. Use direct (uncompressed) mode to make validation easier.
    let file = unsafe {
        gzopen(
            CString::new(file_name.as_str()).unwrap().as_ptr(),
            CString::new("wT").unwrap().as_ptr(),
        )
    };
    assert!(!file.is_null());
    // Set a small buffer size to exercise more internal code paths.
    assert_eq!(unsafe { gzbuffer(file, 8) }, 0);

    // Write to the file one byte at a time, using gzputc.
    const CONTENT: &[u8] = b"sample text to test gzputc implementation";
    for c in CONTENT {
        assert_eq!(unsafe { gzputc(file, *c as _) }, *c as _);
    }

    // Close the file to flush any buffered writes.
    assert_eq!(unsafe { gzclose(file) }, Z_OK);

    // Validate that the file contains the expected bytes.
    let actual = std::fs::read(file_name).unwrap();
    assert_eq!(actual, CONTENT);
}

#[test]
fn gzputc_error() {
    // gzputc on a null file handle should return -1.
    assert_eq!(unsafe { gzputc(ptr::null_mut(), 1) }, -1);

    // gzputc on a read-only file handle should return -1.
    let file = unsafe { gzdopen(-2, CString::new("r").unwrap().as_ptr()) };
    assert!(!file.is_null());
    assert_eq!(unsafe { gzputc(ptr::null_mut(), 1) }, -1);
    assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);

    // Open an invalid file descriptor as a gzip write stream, with a small buffer,
    // and use gzputc to write enough bytes to overflow the buffer and cause file I/O.
    // The last gzputc call should return -1.
    let file = unsafe { gzdopen(-2, CString::new("wT").unwrap().as_ptr()) };
    const BUF_SIZE: usize = 10;
    assert_eq!(unsafe { gzbuffer(file, BUF_SIZE as _) }, 0);
    for _ in 0..BUF_SIZE {
        assert_eq!(unsafe { gzputc(file, 1) }, 1);
    }
    assert_eq!(unsafe { gzputc(file, 1) }, -1);
    assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);
}

#[test]
fn gzputs_basic() {
    // Create a temporary directory that will be automatically removed when
    // temp_dir goes out of scope.
    let temp_dir_path = temp_base();
    let temp_dir = tempfile::TempDir::new_in(temp_dir_path).unwrap();
    let temp_path = temp_dir.path();
    let file_name = path(temp_path, "output");

    // Open a new gzip file for writing. Use direct (uncompressed) mode to make validation easier.
    let file = unsafe {
        gzopen(
            CString::new(file_name.as_str()).unwrap().as_ptr(),
            CString::new("wT").unwrap().as_ptr(),
        )
    };
    assert!(!file.is_null());
    // Set a small buffer size to exercise more internal code paths.
    assert_eq!(unsafe { gzbuffer(file, 8) }, 0);

    // gzputs of a null string should return -1 rather than crashing.
    assert_eq!(unsafe { gzputs(file, ptr::null()) }, -1);

    // Write some data to the file using gzputs.
    const CONTENT: [&str; 3] = ["zlib ", "", "string larger than the buffer size"];
    for s in CONTENT {
        assert_eq!(
            unsafe { gzputs(file, CString::new(s).unwrap().as_ptr()) },
            s.len() as _
        );
    }

    // Close the file to flush any buffered writes.
    assert_eq!(unsafe { gzclose(file) }, Z_OK);

    // Validate that the file contains the expected bytes.
    const EXPECTED: &str = "zlib string larger than the buffer size";
    let actual = std::fs::read(file_name).unwrap();
    assert_eq!(actual, EXPECTED.as_bytes());
}

#[test]
fn gzputs_error() {
    const CONTENT: &[u8] = b"example\0";

    // gzputs on a null file handle should return -1.
    assert_eq!(
        unsafe { gzputs(ptr::null_mut(), CONTENT.as_ptr().cast::<c_char>()) },
        -1
    );

    // gzputs on a read-only file handle should return -1.
    let file = unsafe { gzdopen(-2, CString::new("r").unwrap().as_ptr()) };
    assert!(!file.is_null());
    assert_eq!(
        unsafe { gzputs(ptr::null_mut(), CONTENT.as_ptr().cast::<c_char>()) },
        -1
    );
    assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);
}

#[test]
fn gzgetc_basic() {
    // Read data from a gzip file one byte at a time using gzgetc, and verify that
    // the expected content is returned.
    // FIXME: Replace these closures with simple function pointers once the project MSRV
    // includes a fix for https://github.com/rust-lang/rust/issues/140293
    for gzgetc_fn in [|x| unsafe { gzgetc(x) }, |x| unsafe { gzgetc_(x) }] {
        let file_name = crate_path("src/test-data/text.gz");
        let file = unsafe {
            gzopen(
                CString::new(file_name.as_str()).unwrap().as_ptr(),
                CString::new("r").unwrap().as_ptr(),
            )
        };
        assert!(!file.is_null());
        assert_eq!(unsafe { gzbuffer(file, 8) }, 0);
        const EXPECTED: &str = "gzip\nexample data\nfor tests";
        let mut content = String::with_capacity(EXPECTED.len());
        for _ in 0..EXPECTED.len() {
            // Safety: `file` was initialized by `gzopen`.
            let ch = gzgetc_fn(file);
            assert_ne!(ch, -1);
            content.push(ch as u8 as char);
        }
        // We should be at the end, so the next gzgetc should return -1.
        assert_eq!(gzgetc_fn(file), -1);
        assert_eq!(unsafe { gzclose(file) }, Z_OK);
        assert_eq!(content.as_str(), EXPECTED);
    }
}

#[test]
fn gzgetc_error() {
    // FIXME: Replace these closures with simple function pointers once the project MSRV
    // includes a fix for https://github.com/rust-lang/rust/issues/140293
    for gzgetc_fn in [|x| unsafe { gzgetc(x) }, |x| unsafe { gzgetc_(x) }] {
        // gzgetc on a null file handle should return -1.
        assert_eq!(gzgetc_fn(ptr::null_mut()), -1);

        // gzgetc on a write-only file handle should return -1.
        let file = unsafe { gzdopen(-2, CString::new("w").unwrap().as_ptr()) };
        assert_eq!(gzgetc_fn(file), -1);
        assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);

        // Open an invalid file descriptor as a gzip read stream. gzgetc should return -1.
        let file = unsafe { gzdopen(-2, CString::new("r").unwrap().as_ptr()) };
        assert_eq!(gzgetc_fn(file), -1);
        assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);
    }
}

#[test]
fn gzungetc_basic() {
    // Open a gzip file for reading.
    let file_name = crate_path("src/test-data/text.gz");
    let file = unsafe {
        gzopen(
            CString::new(file_name.as_str()).unwrap().as_ptr(),
            CString::new("r").unwrap().as_ptr(),
        )
    };
    assert!(!file.is_null());

    // Set a small buffer size to make it easier to exercise all the edge cases.
    // Since file is in read mode, `gzbuffer(file, 8)` will result in an input
    // buffer of 8 bytes and an output buffer of 16 bytes. gzungetc operates
    // on the output buffer, so the operations that follow are working with a
    // 16 byte buffer.
    assert_eq!(unsafe { gzbuffer(file, 8) }, 0);

    // Call gzungetc before doing any read operations on the file. It should return the
    // character pushed. Because the output buffer size is 16 bytes (based on the gzbuffer
    // call above), gzungetc should work exactly 16 times before we do any reads.
    const CONTENT: &[u8] = b"0123456789abcdef";
    for c in CONTENT.iter().rev() {
        assert_eq!(unsafe { gzungetc(*c as c_int, file) }, *c as c_int);
    }

    // gzread should return the characters we pushed into the buffer with gzungetc.
    // Note that we looped through CONTENT in reverse when doing the gzungetc, so
    // the result of this read should match CONTENT.
    let mut buf = [0u8; CONTENT.len()];
    assert_eq!(
        unsafe {
            gzread(
                file,
                buf.as_mut_ptr().cast::<c_void>(),
                CONTENT.len() as c_uint,
            )
        },
        CONTENT.len() as _
    );
    assert_eq!(&buf, CONTENT);

    // Do a large read to skip toward the end of the file. This will leave the output buffer empty.
    assert_eq!(
        unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), 16) },
        16
    );
    assert_eq!(&buf, b"gzip\nexample dat");

    // The number of bytes remaining to decompress from the file is smaller than the output
    // buffer. Do a one-byte gzread which will uncompress the remainder of the file into
    // the output buffer and then consume the first byte.
    assert_eq!(
        unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), 1) },
        1
    );
    assert_eq!(buf[0], b'a');

    // After the last gzread, the 16-byte output buffer should consist of:
    // - 1 unused byte (that held the 'a' we consumed in the last `gzread`).
    // - 10 bytes of decompressed output ("\nfor tests").
    // - 5 unused bytes.
    //
    // Call gzungetc twice. The first call will be able to write into the available
    // byte at the start. The second call will have to shift the content to the end
    // of the output buffer to make room.
    assert_eq!(unsafe { gzungetc('6' as c_int, file) }, '6' as c_int);
    assert_eq!(unsafe { gzungetc('5' as c_int, file) }, '5' as c_int);

    // The output buffer should now contain:
    // - 4 unused bytes.
    // - The last character pushed using gzungetc, '6'.
    // - The previous character pushed using gzungetc, '5'.
    // - The content that was already in the buffer, "\nfor tests".
    //
    // We should be able to push 4 more bytes with gzungetc to fill up the
    // available space at the start.
    for c in ['4', '3', '2', '1'] {
        assert_eq!(unsafe { gzungetc(c as c_int, file) }, c as c_int);
    }

    // gzread should yield the remaining 10 bytes of uncompressed content from the file,
    // preceded by the 6 bytes we just pushed with gzungetc, for a total of 16 bytes.
    const EXPECTED1: &[u8] = b"123456\nfor tests";
    // Read more than expected to make sure there's no other output following it.
    let mut buf = [0u8; EXPECTED1.len() + 1];
    assert_eq!(
        unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), buf.len() as _) },
        EXPECTED1.len() as _
    );
    assert_eq!(&buf[..EXPECTED1.len()], EXPECTED1);

    // The 16-byte output buffer is now empty. Call gzungetc 17 times. The first
    // 16 calls should succeed, and the last one should fail and set an error.
    let mut err = Z_OK;
    assert!(!unsafe { gzerror(file, &mut err) }.is_null());
    assert_eq!(err, Z_OK);
    for _ in 0..16 {
        assert_eq!(unsafe { gzungetc('-' as c_int, file) }, '-' as c_int);
    }
    assert_eq!(unsafe { gzungetc('-' as c_int, file) }, -1);
    assert!(!unsafe { gzerror(file, &mut err) }.is_null());
    assert_eq!(err, Z_DATA_ERROR);

    // Clear the error state, rewind to the start of the file, and read some data
    // to refill the output buffer.
    unsafe { gzclearerr(file) };
    unsafe { gzrewind(file) };
    assert!(!unsafe { gzerror(file, &mut err) }.is_null());
    assert_eq!(err, Z_OK);
    assert_eq!(unsafe { gzgetc(file) }, 'g' as c_int);
    assert_eq!(unsafe { gzgetc(file) }, 'z' as c_int);

    // Push a character back into the output buffer with gzungetc, issue a seek
    // request to move to another part of the output stream, and then do another
    // gzungetc. gzread should then return the character pushed in the second
    // gzungetc call, followed by the data at the seek target location. The
    // character pushed by the first gzungetc call should be discarded.
    assert_eq!(unsafe { gzungetc('7' as c_int, file) }, '7' as c_int);
    assert_eq!(unsafe { gzseek(file, 1, libc::SEEK_CUR) }, 2);
    assert_eq!(unsafe { gzungetc('8' as c_int, file) }, '8' as c_int);
    const EXPECTED2: &[u8] = b"8ip\nexample";
    let mut buf = [0u8; EXPECTED2.len()];
    assert_eq!(
        unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), buf.len() as _) },
        EXPECTED2.len() as _
    );
    assert_eq!(&buf, EXPECTED2);

    assert_eq!(unsafe { gzclose(file) }, Z_OK);
}

#[test]
fn gzungetc_error() {
    // gzungetc on a null file handle should return -1.
    assert_eq!(unsafe { gzungetc('*' as c_int, ptr::null_mut()) }, -1);

    // gzgetc on a write-only file handle should return -1.
    let file = unsafe { gzdopen(-2, CString::new("w").unwrap().as_ptr()) };
    assert_eq!(unsafe { gzungetc('*' as c_int, file) }, -1);
    assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);

    // gzgetc with a negative character value should return -1.
    let file_name = crate_path("src/test-data/text.gz");
    let file = unsafe {
        gzopen(
            CString::new(file_name.as_str()).unwrap().as_ptr(),
            CString::new("r").unwrap().as_ptr(),
        )
    };
    assert!(!file.is_null());
    assert_eq!(unsafe { gzungetc(-1 as c_int, file) }, -1);
    assert_eq!(unsafe { gzclose(file) }, Z_OK);
}

#[test]
fn gzgets_basic() {
    // Open a file containing gzip-compressed text.
    let file_name = crate_path("src/test-data/text.gz");
    let file = unsafe {
        gzopen(
            CString::new(file_name.as_str()).unwrap().as_ptr(),
            CString::new("r").unwrap().as_ptr(),
        )
    };
    assert!(!file.is_null());

    // gzgets with a buffer too small to hold the next line should fetch len-1 bytes and
    // add a null terminator. Note: we fill the output buffer with a nonzero value before
    // the call to make sure gzgets null-terminates properly.
    let mut buf = [127 as c_char; 4];
    let ret = unsafe { gzgets(file, buf.as_mut_ptr(), buf.len() as _) };
    assert!(!ret.is_null());
    assert_eq!(
        unsafe { CStr::from_ptr(buf.as_ptr()).to_str().unwrap() },
        "gzi"
    );

    // gzgets with a bigger buffer should fetch (only) the remainder of the line, up to and
    // including the '\n'.
    let mut buf = [127 as c_char; 100];
    let ret = unsafe { gzgets(file, buf.as_mut_ptr(), buf.len() as _) };
    assert!(!ret.is_null());
    assert_eq!(
        unsafe { CStr::from_ptr(buf.as_ptr()).to_str().unwrap() },
        "p\n"
    );

    // gzgets with len=1 should return a string consisting of just a null terminator.
    let mut buf = [127 as c_char; 1];
    let ret = unsafe { gzgets(file, buf.as_mut_ptr(), buf.len() as _) };
    assert!(!ret.is_null());
    assert_eq!(buf[0], 0 as c_char);

    // Read the next line with gzgets, using a buffer just big enough.
    let mut buf = [127 as c_char; 14];
    let ret = unsafe { gzgets(file, buf.as_mut_ptr(), buf.len() as _) };
    assert!(!ret.is_null());
    assert!(!ret.is_null());
    assert_eq!(
        unsafe { CStr::from_ptr(buf.as_ptr()).to_str().unwrap() },
        "example data\n"
    );

    // Read the final line of the file, which is not terminated by a newline character.
    let mut buf = [127 as c_char; 100];
    let ret = unsafe { gzgets(file, buf.as_mut_ptr(), buf.len() as _) };
    assert!(!ret.is_null());
    assert!(!ret.is_null());
    assert_eq!(
        unsafe { CStr::from_ptr(buf.as_ptr()).to_str().unwrap() },
        "for tests"
    );

    // gzgets at the end of the file should return null.
    let mut buf = [127 as c_char; 100];
    let ret = unsafe { gzgets(file, buf.as_mut_ptr(), buf.len() as _) };
    assert!(ret.is_null());

    assert_eq!(unsafe { gzclose(file) }, Z_OK);
}

#[test]
fn gzgets_error() {
    let mut buf = [0 as c_char; 16];

    // gzgets on a null file handle should return null.
    assert!(unsafe { gzgets(ptr::null_mut(), buf.as_mut_ptr(), buf.len() as _) }.is_null());

    // gzgets on a write-only file handle should return null.
    let file = unsafe { gzdopen(-2, CString::new("w").unwrap().as_ptr()) };
    assert!(unsafe { gzgets(ptr::null_mut(), buf.as_mut_ptr(), buf.len() as _) }.is_null());
    assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);

    // Open an invalid file descriptor as a gzip read stream. gzgets should return null.
    let file = unsafe { gzdopen(-2, CString::new("r").unwrap().as_ptr()) };
    assert!(unsafe { gzgets(ptr::null_mut(), buf.as_mut_ptr(), buf.len() as _) }.is_null());
    assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);

    // Test invalid gzgets parameters with a valid input file.
    let file_name = crate_path("src/test-data/issue-109.gz");
    let file = unsafe {
        gzopen(
            CString::new(file_name.as_str()).unwrap().as_ptr(),
            CString::new("r").unwrap().as_ptr(),
        )
    };
    assert!(!file.is_null());
    // gzgets with a null buffer should return null.
    assert!(unsafe { gzgets(ptr::null_mut(), ptr::null_mut(), 1) }.is_null());
    // gzgets with a nonpositive len should return null.
    assert!(unsafe { gzgets(ptr::null_mut(), buf.as_mut_ptr(), 0) }.is_null());
    assert!(unsafe { gzgets(ptr::null_mut(), buf.as_mut_ptr(), -1) }.is_null());
    assert_eq!(unsafe { gzclose(file) }, Z_OK);
}

#[test]
fn gzfread_basic() {
    let mut buf = [0u8; 32];
    let file_name = crate_path("src/test-data/text.gz");
    let file = unsafe {
        gzopen(
            CString::new(file_name.as_str()).unwrap().as_ptr(),
            CString::new("r").unwrap().as_ptr(),
        )
    };
    assert!(!file.is_null());

    // When there is enough data remaining in the file, gzfread should transfer exactly
    // size * nitems bytes into the buffer.
    assert_eq!(
        unsafe { gzfread(buf.as_mut_ptr().cast::<c_void>(), 4, 5, file) },
        5
    );
    assert_eq!(&buf[..20], b"gzip\nexample data\nfo");
    assert_eq!(buf[20], 0);

    // gzfread with a null buffer should return 0.
    assert_eq!(unsafe { gzfread(ptr::null_mut(), 1, 1, file) }, 0);

    // When there is not enough data remaining in the file, gzfread should transfer as many
    // units of size as possible.
    let mut buf = [0u8; 32];
    assert_eq!(
        unsafe { gzfread(buf.as_mut_ptr().cast::<c_void>(), 4, 5, file) },
        1
    );
    assert_eq!(&buf[..4], b"r te");
    // gzfread should have partially filled the next item.
    assert_eq!(&buf[4..8], b"sts\0");

    assert_eq!(unsafe { gzclose(file) }, Z_OK);
}

#[test]
fn gzfread_error() {
    let mut buf = [0u8; 10];

    // gzfread on a null file handle should return 0.
    assert_eq!(
        unsafe { gzfread(buf.as_mut_ptr().cast::<c_void>(), 1, 1, ptr::null_mut()) },
        0
    );

    // gzfread with a size or nitems of 0 should return 0.
    let file = unsafe { gzdopen(-2, CString::new("r").unwrap().as_ptr()) };
    assert!(!file.is_null());
    assert_eq!(
        unsafe { gzfread(buf.as_mut_ptr().cast::<c_void>(), 0, 1, file) },
        0
    );
    assert_eq!(
        unsafe { gzfread(buf.as_mut_ptr().cast::<c_void>(), 1, 0, file) },
        0
    );

    // gzfread should return 0 if size * nitems is too big to fit in a size_t.
    assert_eq!(
        unsafe { gzfread(buf.as_mut_ptr().cast::<c_void>(), size_t::MAX, 2, file) },
        0
    );
    assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);

    // gzfread on a write-only file handle should return 0.
    let file = unsafe { gzdopen(-2, CString::new("w").unwrap().as_ptr()) };
    assert_eq!(
        unsafe { gzfread(buf.as_mut_ptr().cast::<c_void>(), 1, 1, file) },
        0
    );
    assert!(!file.is_null());
    assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);
}

#[test]
fn gzfwrite_basic() {
    // Create a temporary directory that will be automatically removed when
    // temp_dir goes out of scope.
    let temp_dir_path = temp_base();
    let temp_dir = tempfile::TempDir::new_in(temp_dir_path).unwrap();
    let temp_path = temp_dir.path();
    let file_name = path(temp_path, "output");

    // Open a file for writing, using direct (uncompressed) mode to make it easier
    // to verify the output.
    let file = unsafe {
        gzopen(
            CString::new(file_name.as_str()).unwrap().as_ptr(),
            CString::new("wT").unwrap().as_ptr(),
        )
    };
    assert!(!file.is_null());

    // gzfwrite of a single object should return 1.
    assert_eq!(
        unsafe { gzfwrite(b"test".as_ptr().cast::<c_void>(), 4, 1, file) },
        1
    );
    // gzfwrite of n objects should return n.
    assert_eq!(
        unsafe { gzfwrite(b" of gzfwrite...".as_ptr().cast::<c_void>(), 4, 3, file) },
        3
    );

    // gzfwrite with a null buffer should return 0.
    assert_eq!(unsafe { gzfread(ptr::null_mut(), 1, 1, file) }, 0);

    // After the gzfwrite calls, the file should close cleanly.
    assert_eq!(unsafe { gzclose(file) }, Z_OK);

    // Read in the file and verify that the contents match what was passed to gzfwrite.
    const EXPECTED: &[u8] = b"test of gzfwrite";
    let actual = std::fs::read(file_name).unwrap();
    assert_eq!(actual, EXPECTED);
}

#[test]
fn gzfwrite_error() {
    let mut buf = [0u8; 10];

    // gzfwrite on a null file handle should return 0.
    assert_eq!(
        unsafe { gzfwrite(buf.as_mut_ptr().cast::<c_void>(), 1, 1, ptr::null_mut()) },
        0
    );

    // gzfwrite with a size or nitems of 0 should return 0.
    let file = unsafe { gzdopen(-2, CString::new("w").unwrap().as_ptr()) };
    assert!(!file.is_null());
    assert_eq!(
        unsafe { gzfwrite(buf.as_mut_ptr().cast::<c_void>(), 0, 1, file) },
        0
    );
    assert_eq!(
        unsafe { gzfwrite(buf.as_mut_ptr().cast::<c_void>(), 1, 0, file) },
        0
    );

    // gzfwrite should return 0 if size * nitems is too big to fit in a size_t.
    assert_eq!(
        unsafe { gzfwrite(buf.as_mut_ptr().cast::<c_void>(), size_t::MAX, 2, file) },
        0
    );
    assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);

    // gzfwrite on a read-only file handle should return 0.
    let file = unsafe { gzdopen(-2, CString::new("r").unwrap().as_ptr()) };
    assert_eq!(
        unsafe { gzfwrite(buf.as_mut_ptr().cast::<c_void>(), 1, 1, file) },
        0
    );
    assert!(!file.is_null());
    assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);
}

#[test]
fn gzsetparams_basic() {
    // Create a temporary directory that will be automatically removed when
    // temp_dir goes out of scope.
    let temp_dir_path = temp_base();
    let temp_dir = tempfile::TempDir::new_in(temp_dir_path).unwrap();
    let temp_path = temp_dir.path();
    let file_name = path(temp_path, "output.gz");

    // Open a file for writing.
    let file = unsafe {
        gzopen(
            CString::new(file_name.as_str()).unwrap().as_ptr(),
            CString::new("w").unwrap().as_ptr(),
        )
    };
    assert!(!file.is_null());

    // Call gzsetparams before doing any writes. It should return Z_OK and should
    // not cause any file I/O yet.
    assert_eq!(unsafe { gzsetparams(file, 2, 0) }, Z_OK);
    assert_eq!(file_size(&file_name).unwrap(), 0);

    // Provide some data to the file descriptor for compression. The deflate output
    // should be buffered, so the file size should not increase yet.
    const STRING1: &[u8] = b"first write ";
    assert_eq!(
        unsafe { gzwrite(file, STRING1.as_ptr().cast::<c_void>(), STRING1.len() as _) },
        STRING1.len() as _
    );
    assert_eq!(file_size(&file_name).unwrap(), 0);

    // Call gzsetparams to change the deflate parameters. It should return Z_OK and should
    // flush some of the previously buffered data to the file.
    assert_eq!(unsafe { gzsetparams(file, 9, 0) }, Z_OK);
    let size = file_size(&file_name).unwrap();
    assert!(size > 0);

    // Write more data to the file descriptor.
    const STRING2: &[u8] = b"second write ";
    assert_eq!(
        unsafe { gzwrite(file, STRING2.as_ptr().cast::<c_void>(), STRING2.len() as _) },
        STRING2.len() as _
    );

    // Call gzsetparams with the same parameters as last time. This should be a no-op, returning
    // Z_OK and not forcing a flush of the buffered write.
    assert_eq!(unsafe { gzsetparams(file, 9, 0) }, Z_OK);
    assert_eq!(file_size(&file_name).unwrap(), size);

    // Call gzsetparams with different parameters. This should return Z_OK and should flush
    // the previously buffered output to the file.
    assert_eq!(unsafe { gzsetparams(file, 9, 1) }, Z_OK);
    let new_size = file_size(&file_name).unwrap();
    assert!(new_size > size);
    let size = new_size;

    // Do another write, and close the file.
    const STRING3: &[u8] = b"third write";
    assert_eq!(
        unsafe { gzwrite(file, STRING3.as_ptr().cast::<c_void>(), STRING3.len() as _) },
        STRING3.len() as _
    );
    assert_eq!(unsafe { gzclose(file) }, Z_OK);
    let new_size = file_size(&file_name).unwrap();
    assert!(new_size > size);

    // Read and uncompress the file. We should get back the concatenation of all the
    // content written to it.
    let file = unsafe {
        gzopen(
            CString::new(file_name.as_str()).unwrap().as_ptr(),
            CString::new("r").unwrap().as_ptr(),
        )
    };
    assert!(!file.is_null());
    const EXPECTED: &[u8] = b"first write second write third write";
    let mut buf = [0u8; EXPECTED.len() + 1];
    assert_eq!(
        unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), buf.len() as _) },
        EXPECTED.len() as _
    );
    assert_eq!(&buf[..EXPECTED.len()], EXPECTED);
    assert_eq!(unsafe { gzclose(file) }, Z_OK);
}

#[test]
fn gzsetparams_error() {
    // gzsetparams on a null file handle should return Z_STREAM_ERROR.
    assert_eq!(
        unsafe { gzsetparams(ptr::null_mut(), 1, 0) },
        Z_STREAM_ERROR
    );

    // gzsetparams on a read-only file should return Z_STREAM_ERROR.
    let file = unsafe { gzdopen(-2, CString::new("r").unwrap().as_ptr()) };
    assert!(!file.is_null());
    assert_eq!(unsafe { gzsetparams(file, 1, 0) }, Z_STREAM_ERROR);
    assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);

    // gzsetparams on a file in direct-write mode should return Z_STREAM_ERROR.
    let file = unsafe { gzdopen(-2, CString::new("wT").unwrap().as_ptr()) };
    assert!(!file.is_null());
    assert_eq!(unsafe { gzsetparams(file, 1, 0) }, Z_STREAM_ERROR);
    assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);

    // gzsetparams with an invalid strategy should return Z_STREAM_ERROR.
    let file = unsafe { gzdopen(-2, CString::new("w").unwrap().as_ptr()) };
    assert!(!file.is_null());
    assert_eq!(unsafe { gzsetparams(file, 1, 0) }, Z_OK);
    assert_eq!(unsafe { gzsetparams(file, 1, -1) }, Z_STREAM_ERROR);
    assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);

    // Open a gzip file handle around an invalid file descriptor, do a buffered write,
    // and then call gzsetparams with valid parameters. This should trigger a file
    // write that fails, resulting in an error code bubbling back up through
    // through gzsetparams.
    let file = unsafe { gzdopen(-2, CString::new("w").unwrap().as_ptr()) };
    assert!(!file.is_null());
    const CONTENT: &[u8] = b"0123456789";
    assert_eq!(
        unsafe { gzwrite(file, CONTENT.as_ptr().cast::<c_void>(), CONTENT.len() as _) },
        CONTENT.len() as _
    );
    assert_eq!(unsafe { gzsetparams(file, 1, 2) }, Z_ERRNO);
    assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);
}

#[test]
fn gzseek_read() {
    // Create a temporary directory that will be automatically removed when
    // temp_dir goes out of scope.
    let temp_dir_path = temp_base();
    let temp_dir = tempfile::TempDir::new_in(temp_dir_path).unwrap();
    let temp_path = temp_dir.path();

    // Create compressed and non-compressed versions of the same file, to exercise
    // all the implementation paths for gzseek.
    let gzip_file_name = crate_path("src/test-data/issue-109.gz");
    let direct_file_name = path(temp_path, "uncompressed");
    let fd = unsafe {
        libc::open(
            CString::new(direct_file_name.as_str()).unwrap().as_ptr(),
            binary_mode(libc::O_CREAT | libc::O_WRONLY),
            0o644,
        )
    };
    assert_ne!(fd, -1);
    let file = unsafe {
        gzopen(
            CString::new(gzip_file_name.as_str()).unwrap().as_ptr(),
            CString::new("r").unwrap().as_ptr(),
        )
    };
    assert!(!file.is_null());
    let mut buf = [0u8; 65536];
    loop {
        let ret = unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), buf.len() as _) };
        assert_ne!(ret, -1);
        if ret == 0 {
            break;
        }
        assert_eq!(
            unsafe { libc::write(fd, buf.as_mut_ptr().cast::<c_void>(), ret as _) },
            ret as _
        );
    }
    assert_eq!(unsafe { gzclose(file) }, Z_OK);
    assert_eq!(unsafe { libc::close(fd) }, 0);

    for file_name in [direct_file_name, gzip_file_name] {
        let file = unsafe {
            gzopen(
                CString::new(file_name.as_str()).unwrap().as_ptr(),
                CString::new("r").unwrap().as_ptr(),
            )
        };
        assert!(!file.is_null());

        // Set a small buffer size to help exercise all the code paths.
        assert_eq!(unsafe { gzbuffer(file, 16) }, 0);

        // Issue an absolute seek, and then a read. The read should return content from the proper offset.
        assert_eq!(unsafe { gzseek(file, 10, libc::SEEK_SET) }, 10);
        assert_eq!(unsafe { gztell(file) }, 10);
        let mut buf = [0u8; 16];
        assert_eq!(
            unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), buf.len() as _) },
            buf.len() as _
        );
        assert_eq!(&buf, b"t type=file uid=");
        assert_eq!(unsafe { gztell(file) }, 26);

        // Issue a relative seek forward, and then a read. We previously moved to offset 10 and
        // read 16 bytes, so we're starting at offset 26. Seeking 24 bytes forward should take us
        // to offset 50.
        assert_eq!(unsafe { gzseek(file, 24, libc::SEEK_CUR) }, 50);
        assert_eq!(unsafe { gztell(file) }, 50);
        let mut buf = [0u8; 16];
        assert_eq!(
            unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), buf.len() as _) },
            buf.len() as _
        );
        assert_eq!(&buf, b"DINFO time=16815");
        assert_eq!(unsafe { gztell(file) }, 66);

        // Issue a relative seek backwards, and then a read. We previously moved to offset 50 and
        // read 16 bytes, so we're starting at offset 66. Seeking 26 bytes backwards should take us
        // to offset 40.
        assert_eq!(unsafe { gzseek(file, -26, libc::SEEK_CUR) }, 40);
        assert_eq!(unsafe { gztell(file) }, 40);
        let mut buf = [0u8; 16];
        assert_eq!(
            unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), buf.len() as _) },
            buf.len() as _
        );
        assert_eq!(&buf, b"44\n./.BUILDINFO ");
        assert_eq!(unsafe { gztell(file) }, 56);

        // Attempt an absolute seek to a location before the start. gzseek should return -1,
        // and the position in the uncompressed data stream should remain unchanged.
        assert_eq!(unsafe { gzseek(file, -2, libc::SEEK_SET) }, -1);
        assert_eq!(unsafe { gztell(file) }, 56);

        // Attempt a relative seek to a location before the start. gzseek should return -1,
        // and the position in the uncompressed data stream should remain unchanged.
        assert_eq!(unsafe { gzseek(file, -57, libc::SEEK_SET) }, -1);
        assert_eq!(unsafe { gztell(file) }, 56);

        // After those failed seeks, we should be able to seek to a valid location and read
        // the data there. This time, use gzgets to cover its implementation of the seek logic.
        // (When reading compressed data, gzseek only schedules the seek; the various read
        // functions actually act upon it.)
        assert_eq!(unsafe { gzseek(file, 10000, libc::SEEK_SET) }, 10000);
        assert_eq!(unsafe { gztell(file) }, 10000);
        let mut buf = [127u8; 16];
        let ret = unsafe { gzgets(file, buf.as_mut_ptr().cast::<c_char>(), buf.len() as _) };
        assert!(!ret.is_null());
        assert_eq!(&buf, b"681598407.0 siz\0");

        // Use gzrewind to seek back to the start, and verify that gzgetc returns the
        // beginning of the uncompressed content.
        assert_eq!(unsafe { gzrewind(file) }, 0);
        const EXPECTED: &[u8] = b"#mtre";
        for &c in EXPECTED {
            assert_eq!(unsafe { gzgetc(file) }, c as _);
        }

        // Call gzseek twice and then do a read. The read should begin at the sum of the
        // offsets of the gzseek calls.
        assert_eq!(unsafe { gztell(file) }, 5);
        assert_eq!(unsafe { gzseek(file, 1, libc::SEEK_CUR) }, 6);
        assert_eq!(unsafe { gzseek(file, 2, libc::SEEK_CUR) }, 8);
        let mut buf = [0u8; 5];
        assert_eq!(
            unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), buf.len() as _) },
            buf.len() as _
        );
        assert_eq!(&buf, b"set t");

        assert_eq!(unsafe { gzclose(file) }, Z_OK);
    }
}

#[test]
fn gzseek_write() {
    // Create a temporary directory that will be automatically removed when
    // temp_dir goes out of scope.
    let temp_dir_path = temp_base();
    let temp_dir = tempfile::TempDir::new_in(temp_dir_path).unwrap();
    let temp_path = temp_dir.path();

    // Test both compressed and direct (non-compressed) writes.
    for mode in ["w", "wT"] {
        // Open a file handle for writing.
        let file_name = path(temp_path, "output");
        let file = unsafe {
            gzopen(
                CString::new(file_name.as_str()).unwrap().as_ptr(),
                CString::new(mode).unwrap().as_ptr(),
            )
        };
        assert!(!file.is_null());

        // Set a small buffer size to help exercise all the code paths.
        const BUF_SIZE: c_uint = 8;
        assert_eq!(unsafe { gzbuffer(file, BUF_SIZE) }, 0);

        // gzseek forward a few bytes immediately.
        assert_eq!(unsafe { gzseek(file, 3, libc::SEEK_SET) }, 3);
        assert_eq!(unsafe { gztell(file) }, 3);

        // Write some data, with gzseek calls interleaved. Note: Part of the internal seek
        // implementation is done lazily in the next write call, so we use a combination
        // of all the write functions: gzwrite, gzputc, gzputs, and gzflush. gzsetparams
        // also implements the pending seek, but it is not supported in direct-mode
        // (non-compressed) files, so it is tested separately in the function
        // gzseek_gzsetparams.
        const STRING1: &[u8] = b"0123";
        assert_eq!(
            unsafe { gzwrite(file, STRING1.as_ptr().cast::<c_void>(), STRING1.len() as _) },
            STRING1.len() as _
        );
        assert_eq!(unsafe { gztell(file) }, 7);
        assert_eq!(unsafe { gzseek(file, 1, libc::SEEK_CUR) }, 8);
        assert_eq!(unsafe { gzseek(file, 12, libc::SEEK_SET) }, 12);
        assert_eq!(unsafe { gztell(file) }, 12);
        const STRING2: &[u8] = b"456\0";
        assert_eq!(
            unsafe { gzputs(file, STRING2.as_ptr().cast::<c_char>()) },
            (STRING2.len() - 1) as _
        );
        assert_eq!(unsafe { gztell(file) }, 15);
        assert_eq!(unsafe { gzseek(file, 2, libc::SEEK_CUR) }, 17);
        assert_eq!(unsafe { gzputc(file, b'7' as _) }, b'7' as _);
        assert_eq!(unsafe { gztell(file) }, 18);
        assert_eq!(unsafe { gzseek(file, 1, libc::SEEK_CUR) }, 19);
        assert_eq!(unsafe { gzflush(file, Z_SYNC_FLUSH) }, Z_OK);
        assert_eq!(unsafe { gzputc(file, b'8' as _) }, b'8' as _);

        // Do one more gzseek at the end, and then close the file handle. This should produce
        // the specified number of zero bytes at the end of the uncompressed data stream.
        assert_eq!(
            unsafe { gzseek(file, (BUF_SIZE * 3 + 1) as _, libc::SEEK_CUR) },
            (20 + BUF_SIZE * 3 + 1) as _
        );
        assert_eq!(unsafe { gzclose(file) }, Z_OK);

        // Read the file and confirm that each of the gzeek calls produced the expected
        // number of zero bytes.
        let file = unsafe {
            gzopen(
                CString::new(file_name.as_str()).unwrap().as_ptr(),
                CString::new("r").unwrap().as_ptr(),
            )
        };
        assert!(!file.is_null());
        const EXPECTED1: &[u8] = b"\x00\x00\x000123\x00\x00\x00\x00\x00456\x00\x007\x008";
        let mut buf = [127u8; EXPECTED1.len()];
        assert_eq!(
            unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), buf.len() as _) },
            buf.len() as _
        );
        assert_eq!(&buf, EXPECTED1);
        const EXPECTED2: &[u8] = &[0u8; BUF_SIZE as usize * 3 + 1];
        let mut buf = [127u8; EXPECTED2.len() + 1];
        assert_eq!(
            unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), buf.len() as _) },
            EXPECTED2.len() as _
        );
        assert_eq!(&buf[..EXPECTED2.len()], EXPECTED2);
        assert_eq!(unsafe { gzclose(file) }, Z_OK);
    }
}

#[test]
fn gzseek_gzsetparams() {
    // Create a temporary directory that will be automatically removed when
    // temp_dir goes out of scope.
    let temp_dir_path = temp_base();
    let temp_dir = tempfile::TempDir::new_in(temp_dir_path).unwrap();
    let temp_path = temp_dir.path();

    // Open a file handle for writing in compressed mode.
    let file_name = path(temp_path, "output.gz");
    let file = unsafe {
        gzopen(
            CString::new(file_name.as_str()).unwrap().as_ptr(),
            CString::new("w").unwrap().as_ptr(),
        )
    };
    assert!(!file.is_null());

    // Write some content to the file handle.
    const STRING1: &[u8] = b"hello";
    assert_eq!(
        unsafe { gzwrite(file, STRING1.as_ptr().cast::<c_void>(), STRING1.len() as _) },
        STRING1.len() as _
    );

    // Call gzseek to schedule a pending write of some zeros to the compressed stream.
    const SEEK_AMOUNT: usize = 4;
    assert_eq!(unsafe { gzseek(file, SEEK_AMOUNT as _, libc::SEEK_CUR) }, 9);

    // Before doing another write, call gzsetparams. This should write the pending zeros
    // to the current gzip stream before closing the stream and starting a new one.
    assert_eq!(unsafe { gzsetparams(file, 9, 2) }, Z_OK);
    assert_eq!(unsafe { gztell(file) }, (STRING1.len() + SEEK_AMOUNT) as _);

    // Write some more content to the file handle. This will end up in the second gzip stream
    // in the file.
    const STRING2: &[u8] = b"world";
    assert_eq!(
        unsafe { gzwrite(file, STRING2.as_ptr().cast::<c_void>(), STRING2.len() as _) },
        STRING2.len() as _
    );

    // Close the file handle to flush any buffered output to the file.
    assert_eq!(unsafe { gzclose(file) }, Z_OK);

    // Open the newly created file for reading.
    let file = unsafe {
        gzopen(
            CString::new(file_name.as_str()).unwrap().as_ptr(),
            CString::new("r").unwrap().as_ptr(),
        )
    };
    assert!(!file.is_null());

    // Read back the content to validate that it was written correctly.
    let mut buf = [127u8; STRING1.len()];
    assert_eq!(
        unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), buf.len() as _) },
        buf.len() as _
    );
    assert_eq!(&buf, STRING1);
    for _ in 0..SEEK_AMOUNT {
        assert_eq!(unsafe { gzgetc(file) }, 0);
    }
    let mut buf = [127u8; STRING2.len() + 1];
    assert_eq!(
        unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), buf.len() as _) },
        (buf.len() - 1) as _
    );
    assert_eq!(&buf[..STRING2.len()], STRING2);

    assert_eq!(unsafe { gzclose(file) }, Z_OK);
}

#[test]
fn gzseek_error() {
    // gzseek on a null file handle should return -1.
    assert_eq!(unsafe { gzseek(ptr::null_mut(), 0, libc::SEEK_CUR) }, -1);

    // Open a gzip file handle around an invalid file descriptor, do a buffered write,
    // and then call gzflush to force a file write. This will set an error condition
    // inside the file handle, after which gzseek should return -1.
    let file = unsafe { gzdopen(-2, CString::new("w").unwrap().as_ptr()) };
    assert!(!file.is_null());
    const CONTENT: &[u8] = b"0123456789";
    assert_eq!(
        unsafe { gzwrite(file, CONTENT.as_ptr().cast::<c_void>(), CONTENT.len() as _) },
        CONTENT.len() as _
    );
    assert_eq!(unsafe { gzflush(file, Z_SYNC_FLUSH) }, Z_ERRNO);
    assert_eq!(unsafe { gzseek(file, 0, libc::SEEK_CUR) }, -1);

    // Clear the error condition and call gzseek with the unsupported mode SEEK_END.
    // This should return -1.
    unsafe { gzclearerr(file) };
    assert_eq!(unsafe { gzseek(file, 0, libc::SEEK_END) }, -1);

    assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);
}

#[test]
fn gzrewind_error() {
    // gzrewind on a null file handle should return -1.
    assert_eq!(unsafe { gzrewind(ptr::null_mut()) }, -1);

    // gzrewind on a write-only file should return -1.
    let file = unsafe { gzdopen(-2, CString::new("w").unwrap().as_ptr()) };
    assert!(!file.is_null());
    assert_eq!(unsafe { gzrewind(file) }, -1);
    assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);

    // Open a gzip file handle for reading using an invalid file descriptor. gzrewind
    // should return -1.
    let file = unsafe { gzdopen(-2, CString::new("r").unwrap().as_ptr()) };
    assert!(!file.is_null());
    assert_eq!(unsafe { gzrewind(file) }, -1);
    assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);

    // Open a gzip file handle for reading using an invalid file descriptor, and
    // attempt a read to set the handle's internal error status. After that,
    // gzrewind should return -1.
    let file = unsafe { gzdopen(-2, CString::new("r").unwrap().as_ptr()) };
    assert!(!file.is_null());
    let mut buf = [0u8; 16];
    assert_eq!(
        unsafe { gzread(file, buf.as_mut_ptr().cast::<c_void>(), buf.len() as _) },
        -1
    );
    assert_eq!(unsafe { gzrewind(file) }, -1);
    assert_eq!(unsafe { gzclose(file) }, Z_ERRNO);
}

// Get the size in bytes of a file.
//
// # Returns
//
// - `Ok(size)` on success.
// - `Err` on error.
fn file_size(path: &str) -> Result<usize, std::io::Error> {
    Ok(std::fs::metadata(path)?.len() as usize)
}

#[cfg(feature = "gzprintf")]
mod gzprintf {
    use super::*;
    use z_rs::{gzeof, gzprintf};

    macro_rules! cstr {
        ($str:literal) => {
            CStr::from_ptr(concat!($str, "\0").as_ptr().cast::<c_char>())
        };
    }

    macro_rules! gz_error {
        ($fmt:literal, $err:expr) => {
            panic!($fmt, CStr::from_ptr($err).to_str().unwrap());
        };
    }

    #[test]
    fn test_gzio() {
        let temp_dir_path = temp_base();
        let temp_dir = tempfile::TempDir::new_in(temp_dir_path).unwrap();
        let temp_path = temp_dir.path();

        let path = CString::from_vec_with_nul(path(temp_path, "test_gzio.gz\0").into()).unwrap();
        let fname = &path;
        let mut uncompr = vec![0i8; 10000 * size_of::<i32>()];

        test_gzio_help(fname, &mut uncompr);
    }

    fn test_gzio_help(fname: &CStr, uncompr: &mut [i8]) {
        unsafe {
            let hello = cstr!("hello, hello!");

            let mut err: i32 = 0;
            let len: usize = hello.to_bytes_with_nul().len();
            let pos: i64;

            /* Write gz file with test data */
            let mut file = gzopen(fname.as_ptr(), cstr!("wb").as_ptr());
            if file.is_null() {
                panic!("gzopen error");
            }

            /* Write hello, hello! using gzputs and gzprintf */
            gzputc(file, b'h' as i32);
            if gzputs(file, cstr!("ello").as_ptr()) != 4 {
                gz_error!("gzputs err: {}", gzerror(file, &mut err));
            }
            if gzprintf(file, cstr!(", %s!").as_ptr(), cstr!("hello").as_ptr()) != 8 {
                gz_error!("gzprintf err: {}", gzerror(file, &mut err));
            }
            /* Write string null-teriminator using gzseek */
            if gzseek(file, 1, libc::SEEK_CUR) < 0 {
                panic!("gzseek error, gztell={}", gztell(file));
            }
            /* Write hello, hello! using gzfwrite using best compression level */
            if gzsetparams(file, Z_BEST_COMPRESSION, Z_DEFAULT_STRATEGY) != Z_OK {
                gz_error!("gzsetparams err: {}", gzerror(file, &mut err));
            }
            if gzfwrite(hello.as_ptr().cast(), len, 1, file) == 0 {
                gz_error!("gzfwrite err: {}", gzerror(file, &mut err));
            }
            /* Flush compressed bytes to file */
            if gzflush(file, Z_SYNC_FLUSH) != Z_OK {
                gz_error!("gzflush err: {}", gzerror(file, &mut err));
            }
            let compr_len = gzoffset(file);
            if compr_len <= 0 {
                gz_error!("gzoffset err: {}", gzerror(file, &mut err));
            }
            gzclose(file);

            /* Open gz file we previously wrote */
            file = gzopen(fname.as_ptr(), cstr!("rb").as_ptr());
            if file.is_null() {
                gz_error!("gzopen error: {}", gzerror(file, &mut err));
            }

            /* Read uncompressed data - hello, hello! string twice */
            libc::strcpy(uncompr.as_mut_ptr(), cstr!("garbages").as_ptr());
            if gzread(file, uncompr.as_mut_ptr().cast(), uncompr.len() as _) as usize != (len + len)
            {
                gz_error!("gzread err: {}", gzerror(file, &mut err));
            }
            if libc::strcmp(uncompr.as_ptr(), hello.as_ptr()) != 0 {
                panic!(
                    "bad gzread: {}",
                    CStr::from_ptr(uncompr.as_ptr()).to_str().unwrap()
                );
            } else {
                println!(
                    "gzread(): {}",
                    CStr::from_ptr(uncompr.as_ptr()).to_str().unwrap()
                );
            }
            /* Check position at the end of the gz file */
            if gzeof(file) != 1 {
                panic!("gzeof err: not reporting end of stream");
            }

            /* Seek backwards mid-string and check char reading with gzgetc and gzungetc */
            pos = gzseek(file, -22, libc::SEEK_CUR);
            if pos != 6 || gztell(file) != pos {
                panic!("gzseek error, pos={}, gztell={}", pos, gztell(file));
            }
            if gzgetc(file) != i32::from(b' ') {
                panic!("gzgetc error");
            }
            if gzungetc(i32::from(b' '), file) != i32::from(b' ') {
                panic!("gzungetc error");
            }
            /* Read first hello, hello! string with gzgets */
            libc::strcpy(uncompr.as_mut_ptr(), cstr!("garbages").as_ptr());
            gzgets(file, uncompr.as_mut_ptr(), uncompr.len() as _);
            if libc::strlen(uncompr.as_mut_ptr()) != 7
            /* " hello!" */
            {
                gz_error!("gzgets err after gzseek: {}", gzerror(file, &mut err));
            }
            if libc::strcmp(uncompr.as_mut_ptr(), hello.as_ptr().add(6)) != 0 {
                panic!("bad gzgets after gzseek");
            } else {
                println!(
                    "gzgets() after gzseek: {}",
                    CStr::from_ptr(uncompr.as_ptr()).to_str().unwrap()
                );
            }
            /* Seek to second hello, hello! string */
            let pos = gzseek(file, 14, libc::SEEK_SET);
            if pos != 14 || gztell(file) != pos {
                panic!("gzseek error, pos={}, gztell={}", pos, gztell(file));
            }
            /* Check position not at end of file */
            if gzeof(file) != 0 {
                panic!("gzeof err: reporting end of stream");
            }
            /* Read first hello, hello! string with gzfread */
            libc::strcpy(uncompr.as_mut_ptr(), cstr!("garbages").as_ptr());
            let read = gzfread(uncompr.as_mut_ptr().cast(), uncompr.len() as _, 1, file);
            if libc::strcmp(uncompr.as_mut_ptr(), hello.as_ptr()) != 0 {
                panic!("bad gzgets");
            } else {
                println!(
                    "gzgets(): {}",
                    CStr::from_ptr(uncompr.as_ptr()).to_str().unwrap()
                );
            }
            let pos = gzoffset(file);
            if pos < 0 || pos != (compr_len + 10) {
                panic!("gzoffset err: wrong offset at end");
            }
            /* Trigger an error and clear it with gzclearerr */
            gzfread(uncompr.as_mut_ptr().cast(), usize::MAX, usize::MAX, file);
            gzerror(file, &mut err);
            if err == 0 {
                panic!("gzerror err: no error returned");
            }
            gzclearerr(file);
            gzerror(file, &mut err);
            if err != 0 {
                panic!("gzclearerr err: not zero {}", err);
            }

            gzclose(file);

            if gzclose(core::ptr::null_mut()) != Z_STREAM_ERROR {
                panic!("gzclose unexpected return when handle null");
            }

            let _ = read;
        }
    }
}
