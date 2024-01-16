#![no_main]
use libfuzzer_sys::{arbitrary, fuzz_target};

use zlib::ReturnCode;

const BYTES: &[u8] = include_bytes!("../../silesia-small.tar");

#[derive(Debug)]
struct Length(usize);

impl<'a> arbitrary::Arbitrary<'a> for Length {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(Length(u.int_in_range(0..=BYTES.len())?))
    }
}

#[derive(Debug)]
struct Level(i32);

impl<'a> arbitrary::Arbitrary<'a> for Level {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(Level(u.int_in_range(0..=9)?))
    }
}

fuzz_target!(|input: (Level, Length)| {
    let (level, n) = input;
    let level = level.0;

    let data = &BYTES[..n.0];

    if data.len() == 0 {
        return;
    }

    let output_length = 2 * BYTES.len();

    let mut deflated_rs = vec![0; output_length as usize];
    let mut deflated_len_rs = output_length;
    let error = zlib::compress_rs(&mut deflated_rs, &mut deflated_len_rs, data, level);
    assert_eq!(ReturnCode::Ok, error);
    deflated_rs.truncate(deflated_len_rs);

    let mut deflated_ng = vec![0; output_length as usize];
    let mut deflated_len_ng = output_length;
    let error = zlib::compress_ng(&mut deflated_ng, &mut deflated_len_ng, data, level);
    assert_eq!(ReturnCode::Ok, error);
    deflated_ng.truncate(deflated_len_ng);

    assert_eq!(&deflated_rs, &deflated_ng);

    match uncompress_help_ng(&deflated_ng) {
        Err(err) => {
            let raw_path = std::env::temp_dir().join("failed-inflate-raw.dat");
            std::fs::write(&raw_path, &data).unwrap();

            let deflated_path = std::env::temp_dir().join("failed-inflate-deflated.dat");
            std::fs::write(&deflated_path, &deflated_ng).unwrap();

            eprintln!("saved files\n    raw:      {raw_path:?}\n    deflated: {deflated_path:?}");

            panic!("uncompress error {:?}", err);
        }
        Ok(output) => {
            if output != data {
                let path = std::env::temp_dir().join("deflate.txt");
                std::fs::write(&path, &data).unwrap();
                eprintln!("saved input file to {path:?}");
            }

            assert_eq!(output, data);
        }
    }
});

#[allow(unused)]
fn uncompress_help_ng(input: &[u8]) -> Result<Vec<u8>, ReturnCode> {
    let mut dest_vec = vec![0u8; BYTES.len()];

    let mut dest_len = dest_vec.len();
    let dest = dest_vec.as_mut_ptr();

    let source = input.as_ptr();
    let source_len = input.len();

    let err = unsafe { libz_ng_sys::uncompress(dest, &mut dest_len, source, source_len) };

    if err != 0 {
        Err(ReturnCode::from(err))
    } else {
        dest_vec.truncate(dest_len);

        Ok(dest_vec)
    }
}
