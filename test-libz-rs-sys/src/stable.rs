use zlib_rs::{Inflate, InflateConfig, InflateError, InflateFlush, ReturnCode, Status};

use crate::helpers::{
    gzip_wbits_15_lvl_9, gzip_wbits_9_lvl_9, input_text, raw_wbits_15_lvl_9, raw_wbits_9_lvl_9,
    uncompress_slice_ng, zlib_wbits_15_lvl_9, zlib_wbits_9_lvl_9,
};

#[track_caller]
fn assert_inflates(expect_header: bool, window_bits: u8, input: &[u8], expected: &[u8]) {
    let mut d = Inflate::new(expect_header, window_bits);
    let mut out = vec![0u8; expected.len()];

    let status = d.decompress(input, &mut out, InflateFlush::Finish).unwrap();

    assert_eq!(status, Status::StreamEnd);
    assert_eq!(out, expected);
}

#[test]
fn inflate_new_valid_inputs() {
    let expected = input_text();

    assert_inflates(false, 9, &raw_wbits_9_lvl_9(), expected.as_bytes());
    assert_inflates(false, 15, &raw_wbits_15_lvl_9(), expected.as_bytes());

    assert_inflates(true, 9, &zlib_wbits_9_lvl_9(), expected.as_bytes());
    assert_inflates(true, 15, &zlib_wbits_15_lvl_9(), expected.as_bytes());

    assert_inflates(true, 16 + 9, &gzip_wbits_9_lvl_9(), expected.as_bytes());
    assert_inflates(true, 16 + 15, &gzip_wbits_15_lvl_9(), expected.as_bytes());

    assert_inflates(true, 32 + 9, &zlib_wbits_9_lvl_9(), expected.as_bytes());
    assert_inflates(true, 32 + 15, &zlib_wbits_15_lvl_9(), expected.as_bytes());
    assert_inflates(true, 32 + 9, &gzip_wbits_9_lvl_9(), expected.as_bytes());
    assert_inflates(true, 47, &zlib_wbits_15_lvl_9(), expected.as_bytes());
    assert_inflates(true, 47, &gzip_wbits_9_lvl_9(), expected.as_bytes());

    // Using a window bits value of 0 will read the window bits from the zlib header.
    assert_inflates(true, 0, &zlib_wbits_9_lvl_9(), expected.as_bytes());
    assert_inflates(true, 0, &zlib_wbits_15_lvl_9(), expected.as_bytes());

    // 32 working is a quirk in the logic, and we don't document that this works.
    assert_inflates(true, 32, &gzip_wbits_9_lvl_9(), expected.as_bytes());
    assert_inflates(true, 32, &gzip_wbits_15_lvl_9(), expected.as_bytes());
}

#[test]
#[should_panic]
fn new_panics_for_raw_window_bits_too_small() {
    let _ = Inflate::new(false, 7);
}

#[test]
#[should_panic]
fn new_panics_for_raw_window_bits_too_large() {
    let _ = Inflate::new(false, 16);
}

#[test]
#[should_panic]
fn new_panics_for_header_window_bits_too_small() {
    let _ = Inflate::new(true, 7);
}

#[test]
#[should_panic]
fn new_panics_for_header_window_bits_in_gap_before_gzip_range_upper_end() {
    let _ = Inflate::new(true, 16 + 7);
}

#[test]
#[should_panic]
fn new_panics_for_header_window_bits_in_gap_before_auto_range_upper_end() {
    let _ = Inflate::new(true, 32 + 7);
}

#[test]
#[should_panic]
fn new_panics_for_header_window_bits_too_large() {
    let _ = Inflate::new(true, 32 + 16);
}

#[test]
#[should_panic]
fn raw_window_bits_0() {
    // Window bits do not match the input.
    assert_inflates(true, 0, &raw_wbits_15_lvl_9(), input_text().as_bytes());
}

#[test]
#[should_panic]
fn gzip_window_bits_0() {
    // This is a quirk of the logic, 32 does work (becomes 0 internally), but 0 itself does not
    // work, due to its "wrap" value being different.
    assert_inflates(true, 32, &gzip_wbits_9_lvl_9(), input_text().as_bytes());
    assert_inflates(true, 32, &gzip_wbits_15_lvl_9(), input_text().as_bytes());

    let mut output = vec![0u8; 1 << 16];
    let (_, result) = uncompress_slice_ng(
        &mut output,
        &gzip_wbits_9_lvl_9(),
        InflateConfig { window_bits: 0 },
    );
    assert_eq!(result, ReturnCode::DataError);

    assert_inflates(true, 00, &gzip_wbits_9_lvl_9(), input_text().as_bytes());
}

#[test]
fn gzip_inflate_rejects_distance_beyond_configured_window() {
    let mut inflate = Inflate::new(true, 16 + 9);
    let input = gzip_wbits_15_lvl_9();
    let mut output = [0; 256];

    for _ in 0..input_text().len() {
        let input = &input[inflate.total_in() as usize..];
        match inflate.decompress(input, &mut output, InflateFlush::NoFlush) {
            Err(error) => {
                assert_eq!(error, InflateError::DataError);
                return;
            }
            Ok(Status::StreamEnd) => panic!("accepted a distance beyond the configured window"),
            Ok(Status::Ok | Status::BufError) => {}
        }
    }

    panic!("inflate made no terminal progress");
}
