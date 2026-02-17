//! Test that internal counters do not overflow. This is an issue especially on windows, where the
//! total_in and total_out fields are u32. Here we test specifically with an input length larget
//! than u32::MAX.
use zlib_rs::{Deflate, DeflateFlush, Inflate, InflateFlush, Status};

pub fn main() {
    const TOTAL: u64 = 5 * 1024 * 1024 * 1024;
    const IN_CHUNK: usize = 1 << 20;
    const OUT_CHUNK: usize = 1 << 20;

    let zeros = vec![0u8; IN_CHUNK];

    let mut def = Deflate::new(1, true, 15);
    let mut compressed = Vec::new();

    let mut left = TOTAL;
    while left > 0 {
        let n = (left.min(IN_CHUNK as u64)) as usize;
        let mut in_pos = 0;
        while in_pos < n {
            let mut out = vec![0u8; OUT_CHUNK];
            let in_before = def.total_in();
            let out_before = def.total_out();

            let st = def
                .compress(&zeros[in_pos..n], &mut out, DeflateFlush::NoFlush)
                .unwrap();

            let consumed = (def.total_in() - in_before) as usize;
            let produced = (def.total_out() - out_before) as usize;

            if consumed == 0 && produced == 0 {
                panic!("deflate made no progress: {st:?}");
            }

            in_pos += consumed;
            compressed.extend_from_slice(&out[..produced]);
        }
        left -= n as u64;
    }

    loop {
        let mut out = vec![0u8; OUT_CHUNK];
        let out_before = def.total_out();
        let st = def.compress(&[], &mut out, DeflateFlush::Finish).unwrap();
        let produced = (def.total_out() - out_before) as usize;
        compressed.extend_from_slice(&out[..produced]);
        if matches!(st, Status::StreamEnd) {
            break;
        }
        if produced == 0 && !matches!(st, Status::Ok | Status::BufError) {
            panic!("deflate finish made no progress: {st:?}");
        }
    }

    assert_eq!(def.total_in(), TOTAL);
    let total_compressed = def.total_out();

    let mut inf = Inflate::new(true, 15);
    let mut off = 0usize;

    loop {
        let mut out = vec![0u8; OUT_CHUNK];
        let in_before = inf.total_in();
        let out_before = inf.total_out();

        let st = inf
            .decompress(&compressed[off..], &mut out, InflateFlush::NoFlush)
            .unwrap();

        let consumed = (inf.total_in() - in_before) as usize;
        let produced = (inf.total_out() - out_before) as usize;

        if consumed == 0 && produced == 0 {
            panic!("inflate made no progress: {st:?}");
        }

        off += consumed;

        if out[..produced].iter().any(|&b| b != 0) {
            panic!("non-zero byte at offset {}", out_before);
        }

        if matches!(st, Status::StreamEnd) {
            break;
        }
    }

    assert_eq!(inf.total_in(), total_compressed);
    assert_eq!(inf.total_out(), TOTAL);
}
