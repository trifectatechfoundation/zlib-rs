use zlib_rs::{
    compress_slice, DeflateConfig, Inflate, InflateConfig, InflateFlush, ReturnCode, Status,
};

use std::sync::LazyLock;

use crate::helpers::uncompress_slice_ng;

#[track_caller]
fn assert_inflates(expect_header: bool, window_bits: u8, input: &[u8], expected: &[u8]) {
    let mut d = Inflate::new(expect_header, window_bits);
    let mut out = vec![0u8; expected.len()];

    let status = d.decompress(input, &mut out, InflateFlush::Finish).unwrap();

    assert_eq!(status, Status::StreamEnd);
    assert_eq!(out, expected);
}

static INPUT_TEXT: LazyLock<Vec<u8>> = LazyLock::new(|| {
    // Has some long sequences that repeat.
    let paragraph = concat!(
        "Tweebuffelsmeteenslagmorsdoodgeskietfontein is 'n plaas wat in die provinsie Noordwes ongeveer 200 km wes van ", 
        "Pretoria lê en amptelik by die Landmeter-Generaal geregistreer is.", 
        "\n\n", 
        "Die naam illustreer die saamflansende kenmerk van 'n Afrikaanse samestelling: ", 
        "al die beskrywende aspekte van 'n konsep kan in die algemeen saamgebind word in een lang woord. ", 
        "'n Ander voorbeeld sou wees wildewaatlemoenkonfytkompetisiebeoordelaarshandleiding of wat van een met so 70 letters: ", 
        "maketaanwaatlemoenkonfyttentoonstellingbeoordelaarshandleidingsboekies. ", 
        "Sulke uiterste gebruik is egter nie algemeen nie, en daar is ooreenstemming dat woorde met koppeltekens geskei word", 
        "indien dit té lank begin raak."
    );

    // We want a reasonably sized input so that the window size actually has some effect.
    paragraph.repeat(64).into_bytes()
});

fn compress(window_bits: i32, level: i32) -> Vec<u8> {
    let mut output = vec![0; 1 << 16];

    let config = DeflateConfig {
        level,
        window_bits,

        ..Default::default()
    };
    let (slice, ret) = compress_slice(&mut output, &INPUT_TEXT, config);
    assert_eq!(ret, ReturnCode::Ok);
    let n = slice.len();
    output.truncate(n);

    output
}

static RAW_WBITS_9_LVL_9: LazyLock<Vec<u8>> = LazyLock::new(|| compress(-9, 9));
static RAW_WBITS_15_LVL_9: LazyLock<Vec<u8>> = LazyLock::new(|| compress(-15, 9));

static ZLIB_WBITS_9_LVL_9: LazyLock<Vec<u8>> = LazyLock::new(|| compress(9, 9));
static ZLIB_WBITS_15_LVL_9: LazyLock<Vec<u8>> = LazyLock::new(|| compress(15, 9));

static GZIP_WBITS_9_LVL_9: LazyLock<Vec<u8>> = LazyLock::new(|| compress(16 + 9, 9));
static GZIP_WBITS_15_LVL_9: LazyLock<Vec<u8>> = LazyLock::new(|| compress(16 + 15, 9));

#[test]
fn inflate_new_valid_inputs() {
    let expected = &*INPUT_TEXT;

    assert_inflates(false, 9, &RAW_WBITS_9_LVL_9, expected);
    assert_inflates(false, 15, &RAW_WBITS_15_LVL_9, expected);

    assert_inflates(true, 9, &ZLIB_WBITS_9_LVL_9, expected);
    assert_inflates(true, 15, &ZLIB_WBITS_15_LVL_9, expected);

    assert_inflates(true, 16 + 9, &GZIP_WBITS_9_LVL_9, expected);
    assert_inflates(true, 16 + 15, &GZIP_WBITS_15_LVL_9, expected);

    assert_inflates(true, 32 + 9, &ZLIB_WBITS_9_LVL_9, expected);
    assert_inflates(true, 32 + 15, &ZLIB_WBITS_15_LVL_9, expected);
    assert_inflates(true, 32 + 9, &GZIP_WBITS_9_LVL_9, expected);
    assert_inflates(true, 47, &ZLIB_WBITS_15_LVL_9, expected);
    assert_inflates(true, 47, &GZIP_WBITS_9_LVL_9, expected);

    // Using a window bits value of 0 will read the window bits from the zlib header.
    assert_inflates(true, 0, &ZLIB_WBITS_9_LVL_9, expected);
    assert_inflates(true, 0, &ZLIB_WBITS_15_LVL_9, expected);

    // 32 working is a quirk in the logic, and we don't document that this works.
    assert_inflates(true, 32, &GZIP_WBITS_9_LVL_9, expected);
    assert_inflates(true, 32, &GZIP_WBITS_15_LVL_9, expected);
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
    let _ = assert_inflates(true, 0, &RAW_WBITS_15_LVL_9, &INPUT_TEXT);
}

#[test]
#[should_panic]
fn gzip_window_bits_0() {
    // This is a quirk of the logic, 32 does work (becomes 0 internally), but 0 itself does not
    // work, due to its "wrap" value being different.
    let _ = assert_inflates(true, 32, &GZIP_WBITS_9_LVL_9, &INPUT_TEXT);
    let _ = assert_inflates(true, 32, &GZIP_WBITS_15_LVL_9, &INPUT_TEXT);

    let mut output = vec![0u8; 1 << 16];
    let (_, result) = uncompress_slice_ng(
        &mut output,
        &GZIP_WBITS_9_LVL_9,
        InflateConfig { window_bits: 0 },
    );
    assert_eq!(result, ReturnCode::DataError);

    let _ = assert_inflates(true, 00, &GZIP_WBITS_9_LVL_9, &INPUT_TEXT);
}
