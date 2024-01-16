#![no_main]
use libfuzzer_sys::{arbitrary, fuzz_target};

use zlib::ReturnCode;

#[derive(Debug, arbitrary::Arbitrary)]
enum Level {
    Three = 3,
    Four = 4,
    Five = 5,
    Six = 6,
}

fuzz_target!(|input: (Level, String)| {
    let (level, data) = input;

    let output_length = 2 * 4096;
    let data = data.as_bytes();
    let level = level as i32;

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
