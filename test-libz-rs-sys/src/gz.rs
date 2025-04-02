use zlib_rs::c_api::*;

use libz_rs_sys::gzclose;
use libz_rs_sys::gzopen;
use std::ffi::CString;
use std::ptr;
use tempfile;

// Turn a Rust string into a C string
macro_rules! cs {
    ($str:expr) => {
        CString::new($str).unwrap().as_ptr()
    };
}

// Generate a file path relative to the project's root
macro_rules! crate_path {
    ($str:expr) => {
        concat!(env!("CARGO_MANIFEST_DIR"), "/", $str)
    };
}

// Generate a file path relative to a specified prefix
macro_rules! path {
    ($prefix:expr, $str:expr) => {
        {
            let mut path_buf = $prefix.to_path_buf();
            path_buf.push($str);
            path_buf.as_path().to_str().unwrap().to_owned()
        }
    };
}

/*fn test_open(path: &str, mode: &str, should_succeed: bool) {
    let handle = unsafe { gzopen(cs!(path), cs!(mode)) };
    assert_eq!(should_succeed, !handle.is_null(), "{path} {mode}");
    if !handle.is_null() {
        assert_eq!(unsafe { gzclose(handle) }, Z_OK);
    }
}
*/

macro_rules! test_open {
    ($path:expr, $mode:expr, $should_succeed:expr) => {
        {
            let handle = unsafe { gzopen(cs!($path), cs!($mode)) };
            assert_eq!($should_succeed, !handle.is_null(), "{} {}", $path, $mode);
            if !handle.is_null() {
                assert_eq!(unsafe { gzclose(handle) }, Z_OK);
            }
        }
    };
}

#[test]
fn open_close() {
    // Open a valid file for reading
    test_open!(crate_path!("src/test-data/issue-109.gz"), "r", true);

    // "b" for binary mode is optional
    test_open!(crate_path!("src/test-data/issue-109.gz"), "rb", true);

    // Mode must include r, w, or a
    test_open!(crate_path!("src/test-data/issue-109.gz"), "", false);
    test_open!(crate_path!("src/test-data/issue-109.gz"), "e", false);

    // For zlib-ng compatibility, mode can't specify transparent read
    test_open!(crate_path!("src/test-data/issue-109.gz"), "Tr", false);

    // Read of a nonexistent file should fail
    test_open!(crate_path!("src/test-data/no-such-file.gz"), "r", false);

    // Closing a null file handle should return an error instead of crashing
    assert_eq!(unsafe { gzclose(ptr::null_mut()) }, Z_STREAM_ERROR);
}

#[test]
fn create() {
    // Create a temporary directory that will be automatically removed when
    // temp_dir goes out of scope.
    let Ok(temp_dir) = tempfile::tempdir() else {
        panic!("Could not create temporary directory.");
    };
    let temp_path = temp_dir.path();

    // Creation of a new file should work
    test_open!(path!(temp_path, "new.gz"), "w", true);

    // File creation should fail with "x" (exclusive) flag if the file already exists
    test_open!(path!(&temp_path, "new.gz"), "wx", false);
    test_open!(path!(&temp_path, "different_file.gz"), "wx", true);

    // "e" flag should open for writing with O_CLOEXEC (close file descriptor on exec)
    // on compatible systems and should be silently ignored on other systems.
    test_open!(path!(&temp_path, "new.gz"), "ew", true);

    // Append mode should create a new file if needed
    test_open!(path!(temp_path, "new2.gz"), "a", true);

    // "x" flag should work for append mode, too
    test_open!(path!(&temp_path, "new2.gz"), "ax", false);
    test_open!(path!(&temp_path, "new3.gz"), "ax", true);
}
