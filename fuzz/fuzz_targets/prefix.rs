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

    match zlib::uncompress_help(&deflated_ng) {
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
