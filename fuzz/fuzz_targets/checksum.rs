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

        let actual = zlib_rs::crc32::crc32(input.as_slice(), start);

        assert_eq!(expected, actual);
    }

    {
        let expected = {
            let mut h = crc32fast::Hasher::new_with_initial(0);
            h.update(&input[..]);
            h.finalize()
        };

        let mut buf = [0; 1 << 16];
        let mut dst = zlib_rs::read_buf::ReadBuf::new(&mut buf[..input.len()]);

        let actual = zlib_rs::crc32::crc32_copy(&mut dst, input.as_slice());

        assert_eq!(expected, actual);

        assert_eq!(input, dst.filled());
    }

    {
        use zlib_rs::{adler32, adler32_combine};

        let data = input;

        let Some(buf_len) = data.first().copied() else {
            return;
        };

        let buf_size = Ord::max(buf_len, 1) as usize;

        let mut adler1 = 1;
        let mut adler2 = 1;

        for chunk in data.chunks(buf_size) {
            adler1 = adler32(adler1, chunk);
        }

        adler2 = adler32(adler2, &data);

        assert_eq!(adler1, adler2);

        let combine1 = adler32_combine(adler1, adler2, data.len() as _);
        let combine2 = adler32_combine(adler1, adler1, data.len() as _);
        assert_eq!(combine1, combine2);
    }
});
