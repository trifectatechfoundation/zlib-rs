#![no_main]
use libfuzzer_sys::fuzz_target;

use zlib::ReturnCode;

fuzz_target!(|data: String| {
    // first, deflate the data using the standard zlib
    let length = 8 * 1024;
    let mut deflated = vec![0; length as usize];
    let mut length = length as u64;
    let error = unsafe {
        zlib::compress(
            deflated.as_mut_ptr().cast(),
            &mut length,
            data.as_ptr().cast(),
            data.len() as _,
        )
    };

    let error = zlib::ReturnCode::from(error as i32);
    assert_eq!(ReturnCode::Ok, error);

    deflated.truncate(length as usize);

    let output = zlib::uncompress_help(&deflated).unwrap();

    if output != data.as_bytes() {
        let path = std::env::temp_dir().join("deflate.txt");
        std::fs::write(&path, &data).unwrap();
        eprintln!("saved input file to {path:?}");
    }

    assert_eq!(output, data.as_bytes());
});
