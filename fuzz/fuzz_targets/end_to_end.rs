#![no_main]
use libfuzzer_sys::fuzz_target;

use zlib_rs::deflate::DeflateConfig;

fuzz_target!(|input: (String, DeflateConfig)| {
    test_libz_rs_sys::end_to_end::test(input.0, input.1);
});
