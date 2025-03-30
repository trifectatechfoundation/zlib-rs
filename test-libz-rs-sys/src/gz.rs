use zlib_rs::c_api::*;

use libz_rs_sys::gzclose;
use libz_rs_sys::gzopen;
use std::ffi::CString;
use std::ptr;

// Turn a Rust string into a C string
macro_rules! cs {
    ($str:expr) => {
        CString::new($str).unwrap().as_ptr()
    };
}

// Generate a file path relative to the project's root
macro_rules! path {
    ($str:expr) => {
        concat!(env!("CARGO_MANIFEST_DIR"), "/", $str)
    };
}

unsafe fn test_open(path: &str, mode: &str, should_succeed: bool) {
    let handle = gzopen(cs!(path), cs!(mode));
    assert_eq!(should_succeed, !handle.is_null(), "{path} {mode}");
    if !handle.is_null() {
        assert_eq!(gzclose(handle), Z_OK);
    }
}

#[test]
fn open_close() {
    unsafe {
        // Open a valid file for reading
        test_open(path!("src/test-data/issue-109.gz"), "r", true);

        // "b" for binary mode is optional
        test_open(path!("src/test-data/issue-109.gz"), "rb", true);

        // Mode must include r, w, or a
        test_open(path!("src/test-data/issue-109.gz"), "", false);
        test_open(path!("src/test-data/issue-109.gz"), "e", false);

        // For zlib-ng compatibility, mode can't specify transparent read
        test_open(path!("src/test-data/issue-109.gz"), "Tr", false);

        // Read of a nonexistent file should fail
        test_open(path!("src/test-data/no-such-file.gz"), "r", false);

        // Closing a null file handle should return an error instead of crashing
        assert_eq!(gzclose(ptr::null_mut()), Z_STREAM_ERROR);
    }
}
