#![no_main]

//! the tests provide good coverage, the purose of this fuzzer is to
//! discover memory safety issues in the SIMD implementations.

use libfuzzer_sys::fuzz_target;

use zlib_rs::deflate::DeflateConfig;
use zlib_rs::inflate::InflateConfig;
use zlib_rs::{Flush, ReturnCode};

use std::ffi::{c_char, c_int, c_uint};

fuzz_target!(|input: (Vec<u8>, u32)| {
    let (input, start) = input;

    {
        let expected = {
            let mut h = crc32fast::Hasher::new_with_initial(start);
            h.update(&input[..]);
            h.finalize()
        };

        let actual = zlib_rs::crc32::crc32(input.as_slice(), start);

        assert_eq!(expected, actual);
    }

    {
        let mut dst = [0; 1 << 16];

        let expected = {
            let mut h = crc32fast::Hasher::new_with_initial(0);
            h.update(&input[..]);
            h.finalize()
        };

        let actual = zlib_rs::crc32::crc32_copy(&mut dst[..input.len()], input.as_slice());

        assert_eq!(expected, actual);

        assert_eq!(input, &dst[..input.len()]);
    }
});
