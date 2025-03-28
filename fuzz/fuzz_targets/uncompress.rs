#![no_main]
use libfuzzer_sys::fuzz_target;

use zlib_rs::ReturnCode;

fn uncompress_help(input: &[u8]) -> Vec<u8> {
    let mut dest_vec = vec![0u8; 1 << 16];

    let mut dest_len = dest_vec.len() as std::ffi::c_ulong;
    let dest = dest_vec.as_mut_ptr();

    let source = input.as_ptr();
    let source_len = input.len() as _;

    let err = unsafe { ::libz_rs_sys::uncompress(dest, &mut dest_len, source, source_len) };

    if err != 0 {
        panic!("error {:?}", ReturnCode::from(err));
    }

    dest_vec.truncate(dest_len as usize);

    dest_vec
}

fuzz_target!(|data: String| {
    // first, deflate the data using the standard zlib
    let mut length = 8 * 1024;
    let mut deflated = vec![0; length];

    let error = unsafe {
        libz_ng_sys::compress(
            deflated.as_mut_ptr().cast(),
            &mut length,
            data.as_ptr().cast(),
            data.len(),
        )
    };

    let error = ReturnCode::from(error as i32);
    assert_eq!(ReturnCode::Ok, error);

    deflated.truncate(length as _);

    let output = uncompress_help(&deflated);

    if output != data.as_bytes() {
        let path = std::env::temp_dir().join("deflate.txt");
        std::fs::write(&path, &data).unwrap();
        eprintln!("saved input file to {path:?}");
    }

    assert_eq!(output, data.as_bytes());
});
