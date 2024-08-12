use zlib_rs::{deflate::DeflateConfig, inflate::InflateConfig, ReturnCode};

use crate::helpers::{compress_slice_ng, uncompress_slice_ng};

#[test]
fn end_to_end() {
    ::quickcheck::quickcheck(test as fn(_, _) -> _);
}

pub fn test(data: String, config: DeflateConfig) -> bool {
    if !(0..32).contains(&config.window_bits) {
        // lower is raw, higher is gzip
        return true;
    }

    // only proceed if this is a valid config
    let mut stream = libz_rs_sys::z_stream::default();
    let err = zlib_rs::deflate::init(&mut stream, config);
    if err != ReturnCode::Ok {
        return true;
    }
    unsafe { libz_rs_sys::deflateEnd(&mut stream) };

    const LENGTH: usize = 1 << 17;

    // first, deflate the data using the standard zlib
    let mut deflated_rs = [0; LENGTH];
    let (deflated_rs, error) =
        zlib_rs::deflate::compress_slice(&mut deflated_rs, data.as_bytes(), config);

    let error = ReturnCode::from(error as i32);
    assert_eq!(ReturnCode::Ok, error);

    let mut deflated_ng = [0; LENGTH];
    let (deflated_ng, error) = compress_slice_ng(&mut deflated_ng, data.as_bytes(), config);

    let error = ReturnCode::from(error as i32);
    assert_eq!(ReturnCode::Ok, error);

    assert_eq!(deflated_rs, deflated_ng);

    let config = InflateConfig {
        window_bits: match config.window_bits {
            8 => 9,
            n => n,
        },
    };

    let mut dest_vec_ng = vec![0u8; data.len()];
    let (output_ng, error) = uncompress_slice_ng(&mut dest_vec_ng, deflated_rs, config);

    if error != ReturnCode::Ok || output_ng != data.as_bytes() {
        let path = std::env::temp_dir().join("deflate.txt");
        std::fs::write(&path, &data).unwrap();
        let path = std::env::temp_dir().join("inflate.txt");
        // see https://github.com/rust-lang/rust-clippy/pull/12892
        #[allow(clippy::needless_borrows_for_generic_args)]
        std::fs::write(&path, &deflated_rs).unwrap();
        eprintln!("saved deflated file to {path:?}");
    }

    assert_eq!(ReturnCode::Ok, error);
    assert_eq!(
        output_ng,
        data.as_bytes(),
        "zlib-ng cannot decode these bytes!"
    );

    let mut dest_vec_rs = vec![0u8; data.len()];
    let (output_rs, error) =
        zlib_rs::inflate::uncompress_slice(&mut dest_vec_rs, deflated_rs, config);

    if error != ReturnCode::Ok || output_rs != data.as_bytes() {
        let path = std::env::temp_dir().join("deflate.txt");
        std::fs::write(&path, &data).unwrap();
        let path = std::env::temp_dir().join("inflate.txt");
        // see https://github.com/rust-lang/rust-clippy/pull/12892
        #[allow(clippy::needless_borrows_for_generic_args)]
        std::fs::write(&path, &deflated_rs).unwrap();
        eprintln!("saved deflated file to {path:?}");
    }

    assert_eq!(ReturnCode::Ok, error);
    assert_eq!(output_rs, data.as_bytes());

    true
}
