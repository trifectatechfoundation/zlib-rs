use zlib_rs::c_api::*;

use libz_rs_sys::{gzFile_s, gzclose, gzdirect, gzdopen, gzerror, gzopen};

use std::ffi::{c_char, c_int, CString};
use std::path::Path;
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
fn create() {
    // Create a temporary directory that will be automatically removed when
    // temp_dir goes out of scope.
    let temp_dir_path = if cfg!(target_os = "wasi") {
        std::path::PathBuf::from("/tmp/")
    } else {
        std::env::temp_dir()
    };

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
                CString::new(path(temp_path, "compressed.gz")).unwrap().as_ptr(),
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
    // Open a gzip file for reading. gzdirect should return 0.
    let file = unsafe { gzopen(CString::new(crate_path("src/test-data/issue-109.gz")).unwrap().as_ptr(), CString::new("r").unwrap().as_ptr()) };
    assert!(!file.is_null());
    assert_eq!(unsafe { gzdirect(file) }, 0);
    assert_eq!(unsafe { gzclose(file) }, Z_OK);

    // Open a non-gzip file for reading. gzdirect should return 1.
    let file = unsafe { gzopen(CString::new(crate_path("src/test-data/issue-169.js")).unwrap().as_ptr(), CString::new("r").unwrap().as_ptr()) };
    assert!(!file.is_null());
    assert_eq!(unsafe { gzdirect(file) }, 1);
    assert_eq!(unsafe { gzclose(file) }, Z_OK);
}
