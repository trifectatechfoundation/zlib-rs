#![no_main]

//! the tests provide good coverage, the purpose of this fuzzer is to
//! discover memory safety issues in the SIMD implementations.

use libfuzzer_sys::fuzz_target;

fuzz_target!(|input: (Vec<u8>, u32)| {
    let (input, start) = input;

    {
        let expected = {
            let mut h = crc32fast::Hasher::new_with_initial(start);
            h.update(&input[..]);
            h.finalize()
        };

        let actual = zlib_rs::crc32::crc32(start, input.as_slice());

        assert_eq!(expected, actual);
    }

    {
        use zlib_rs::{crc32, crc32_combine};

        let data = &input;

        let Some(buf_len) = data.first().copied() else {
            return;
        };

        let buf_size = Ord::max(buf_len, 1) as usize;

        let crc0 = 0;
        let mut crc1 = crc0;
        let mut crc2 = crc0;

        for chunk in data.chunks(buf_size) {
            let crc3 = crc32(0, chunk);
            let crc4 = crc32_combine(crc1, crc3, chunk.len() as u64);

            crc1 = crc32(crc1, chunk);

            assert_eq!(crc1, crc4);
        }

        crc2 = crc32(crc2, data);

        assert_eq!(crc1, crc2);

        let combine1 = crc32_combine(crc1, crc2, data.len() as _);
        let combine2 = crc32_combine(crc1, crc1, data.len() as _);
        assert_eq!(combine1, combine2);
    }

    {
        use zlib_rs::{adler32, adler32_combine};

        let data = &input;

        let Some(buf_len) = data.first().copied() else {
            return;
        };

        let buf_size = Ord::max(buf_len, 1) as usize;

        let mut adler1 = 1;
        let mut adler2 = 1;

        for chunk in data.chunks(buf_size) {
            let adler3 = adler32(1, chunk);
            let adler4 = adler32_combine(adler1, adler3, chunk.len() as u64);

            adler1 = adler32(adler1, chunk);

            assert_eq!(adler1, adler4);
        }

        adler2 = adler32(adler2, data);

        assert_eq!(adler1, adler2);

        let combine1 = adler32_combine(adler1, adler2, data.len() as _);
        let combine2 = adler32_combine(adler1, adler1, data.len() as _);
        assert_eq!(combine1, combine2);
    }
});
