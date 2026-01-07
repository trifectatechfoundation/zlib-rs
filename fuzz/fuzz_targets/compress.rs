#![no_main]
use libfuzzer_sys::fuzz_target;

use zlib_rs::ReturnCode;

fuzz_target!(|data: &[u8]| {
    // first, deflate the data using the standard zlib
    let mut length = 8 * 1024;
    let mut deflated = vec![0; length as usize];
    let error = unsafe {
        libz_rs_sys::compress(
            deflated.as_mut_ptr().cast(),
            &mut length,
            data.as_ptr().cast(),
            data.len() as _,
        )
    };

    let error = ReturnCode::from(error as i32);
    if error != ReturnCode::Ok && data.len() > 6 * 1024 {
        return;
    }
    assert_eq!(ReturnCode::Ok, error);

    deflated.truncate(length as usize);

    let mut output = vec![0u8; data.len()];
    let config = zlib_rs::inflate::InflateConfig { window_bits: 15 };
    let (output, error) = zlib_rs::inflate::uncompress_slice(&mut output, &deflated, config);
    assert_eq!(ReturnCode::Ok, error);

    if output != data {
        let path = std::env::temp_dir().join("deflate.txt");
        std::fs::write(&path, data).unwrap();
        eprintln!("saved input file to {path:?}");
    }

    assert_eq!(output, data);
});
