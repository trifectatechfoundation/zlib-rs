#![no_main]
use libfuzzer_sys::fuzz_target;

use std::mem::MaybeUninit;
use zlib_rs::deflate::{compress, DeflateConfig};
use zlib_rs::ReturnCode;

// Round-trip test for deflate and inflate
fuzz_target!(|data: &[u8]| {
    let mut deflated = vec![MaybeUninit::zeroed(); 8 * 1024];
    let (deflated, error) = compress(&mut deflated, data, DeflateConfig::new(-1));

    if error != ReturnCode::Ok && data.len() > 6 * 1024 {
        // This can fail if the input data doesn't fit into the deflate buffer.
        // We are generous here and say that any <6kb input will always fit into
        // an 8kb deflate stream.
        return;
    }
    assert_eq!(ReturnCode::Ok, error);

    let mut output = vec![0u8; data.len()];
    let config = zlib_rs::inflate::InflateConfig { window_bits: 15 };
    let (output, error) = zlib_rs::inflate::uncompress_slice(&mut output, deflated, config);
    assert_eq!(ReturnCode::Ok, error);

    if output != data {
        let path = std::env::temp_dir().join("deflate.txt");
        std::fs::write(&path, data).unwrap();
        eprintln!("saved input file to {path:?}");
    }

    assert_eq!(output, data);
});
