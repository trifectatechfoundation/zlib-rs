#![no_main]

//! the tests provide good coverage, the purpose of this fuzzer is to
//! discover memory safety issues in the SIMD implementations.

use libfuzzer_sys::fuzz_target;

const BASE: u32 = 65521;

fn adler32_scalar(start: u32, data: &[u8]) -> u32 {
    let mut s1 = start & 0xffff;
    let mut s2 = (start >> 16) & 0xffff;
    for &b in data {
        s1 = (s1 + b as u32) % BASE;
        s2 = (s2 + s1) % BASE;
    }
    (s2 << 16) | s1
}

fuzz_target!(|input: (Vec<u8>, u8, u32)| {
    let (input, offset, start) = input;

    // Add an offset to the input because some of the checksum algorithms are sensitive to
    // alignment.
    let offset = offset & 15;
    let Some(input) = input.get(usize::from(offset)..) else {
        return;
    };

    {
        let expected = {
            let mut h = crc32fast::Hasher::new_with_initial(start);
            h.update(input);
            h.finalize()
        };

        let actual = zlib_rs::crc32::crc32(start, input);

        assert_eq!(expected, actual);
    }

    {
        use zlib_rs::crc32::{crc32, crc32_combine};

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
        use zlib_rs::adler32::adler32;
        assert_eq!(adler32(start, &input), adler32_scalar(start, &input));
    }

    {
        use zlib_rs::adler32::{adler32, adler32_combine};

        let data = &input;

        assert_eq!(adler32(start, data), adler32_scalar(start, data));

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
