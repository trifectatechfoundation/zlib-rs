use libz_sys as libz_ng_sys;

fn iter_files() -> impl Iterator<Item = &'static [u8]> {
    static DATA: &[u8] = include_bytes!("../src/test-data/gix-blame-readme.bin");

    let mut cursor = DATA;

    std::iter::from_fn(move || {
        let [a, b, c, d, rest @ ..] = cursor else {
            return None;
        };

        let length = u32::from_le_bytes([*a, *b, *c, *d]) as usize;

        let (input, rest) = rest.split_at(length);

        cursor = rest;

        Some(input)
    })
}

fn main() {
    match std::env::args().nth(1).unwrap().as_str() {
        "ng" => {
            for _ in 0..100 {
                decompress_files_ng(iter_files());
            }
        }
        "rs" => {
            for _ in 0..100 {
                decompress_files_rs(iter_files());
            }
        }
        other => panic!("invalid variant: {other}"),
    }
}

fn decompress_files_rs<'a>(inputs: impl Iterator<Item = &'a [u8]>) {
    use libz_rs_sys::*;

    let mut outbuf = vec![0u8; 1 << 20];

    unsafe {
        let mut stream = std::mem::MaybeUninit::zeroed();
        let ret = inflateInit2_(
            stream.as_mut_ptr(),
            15, // windowBits = 15 (default)
            zlibVersion(),
            std::mem::size_of::<z_stream>() as i32,
        );
        assert_eq!(ret, Z_OK);
        let stream = stream.assume_init_mut();

        for input in inputs {
            stream.next_in = input.as_ptr() as *mut u8;
            stream.avail_in = input.len() as u32;
            stream.next_out = outbuf.as_mut_ptr();
            stream.avail_out = outbuf.len() as u32;

            let ret = inflate(stream, Z_NO_FLUSH);
            assert!(ret == Z_STREAM_END || ret == Z_OK);

            // Reset stream for next input
            let ret = inflateReset(stream);
            assert_eq!(ret, Z_OK);
        }

        inflateEnd(stream);
    }
}

fn decompress_files_ng<'a>(inputs: impl Iterator<Item = &'a [u8]>) {
    use libz_ng_sys::*;

    let mut outbuf = vec![0u8; 1 << 20];

    unsafe {
        let mut stream = std::mem::MaybeUninit::zeroed();
        let ret = inflateInit2_(
            stream.as_mut_ptr(),
            15, // windowBits = 15 (default)
            zlibVersion(),
            std::mem::size_of::<z_stream>() as i32,
        );
        assert_eq!(ret, Z_OK);
        let stream = stream.assume_init_mut();

        for input in inputs {
            stream.next_in = input.as_ptr() as *mut u8;
            stream.avail_in = input.len() as u32;
            stream.next_out = outbuf.as_mut_ptr();
            stream.avail_out = outbuf.len() as u32;

            let _ret = inflate(stream, Z_FINISH);
            // assert!(ret == Z_STREAM_END || ret == Z_OK,);

            // Reset stream for next input
            let ret = inflateReset(stream);
            assert_eq!(ret, Z_OK);
        }

        inflateEnd(stream);
    }
}
