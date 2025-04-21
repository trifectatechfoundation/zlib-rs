use zlib_rs::c_api::*;

use libz_rs_sys::{
    gzFile_s, gzbuffer, gzclearerr, gzclose, gzclose_r, gzclose_w, gzdirect, gzdopen, gzerror,
    gzflush, gzopen, gzread, gzwrite,
};

use std::ffi::{c_char, c_int, c_uint, c_void, CString};
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
    let handle = unsafe { gzopen(path.as_ptr(), mode.as_ptr()) };
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
        panic!("Could not find size of file {}", path);
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

// Get the size in bytes of a file.
//
// # Returns
//
// - `Ok(size)` on success.
// - `Err` on error.
fn file_size(path: &str) -> Result<usize, ()> {
    let mut result = Err(());
    let stat_ptr = unsafe { libc::calloc(1, core::mem::size_of::<libc::stat>() as _) };
    if stat_ptr.is_null() {
        return result;
    }
    let ret = unsafe {
        libc::stat(
            CString::new(path).unwrap().as_ptr(),
            stat_ptr.cast::<libc::stat>(),
        )
    };
    if ret == 0 {
        if let Some(stat_info) = unsafe { stat_ptr.cast::<libc::stat>().as_ref() } {
            result = Ok(stat_info.st_size as usize);
        }
    }
    unsafe { libc::free(stat_ptr) };
    result
}
