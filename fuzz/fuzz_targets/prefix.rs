#![no_main]
use libfuzzer_sys::{arbitrary, fuzz_target};

use zlib::{Flush, ReturnCode};

const BYTES: &[u8] = include_bytes!("../../silesia-small.tar");

#[derive(Debug)]
struct Length(usize);

impl<'a> arbitrary::Arbitrary<'a> for Length {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(Length(u.int_in_range(0..=BYTES.len())?))
    }
}

#[derive(Debug, arbitrary::Arbitrary)]
enum Level {
    Zero = 0,
    One = 1,
    Two = 2,
    Three = 3,
    Four = 4,
    Five = 5,
    Six = 6,
    Seven = 7,
    Eight = 8,
    Nine = 9,
}

fuzz_target!(|input: (Level, Length)| {
    let (level, n) = input;
    let level = level as i32;

    let data = &BYTES[..n.0];

    if data.len() == 0 {
        return;
    }

    let output_length = 2 * BYTES.len();

    let mut deflated_rs = vec![0; output_length as usize];
    let mut deflated_len_rs = output_length;
    let error = compress_rs(&mut deflated_rs, &mut deflated_len_rs, data, level);
    assert_eq!(ReturnCode::Ok, error);
    deflated_rs.truncate(deflated_len_rs);

    let mut deflated_ng = vec![0; output_length as usize];
    let mut deflated_len_ng = output_length;
    let error = compress_ng(&mut deflated_ng, &mut deflated_len_ng, data, level);
    assert_eq!(ReturnCode::Ok, error);
    deflated_ng.truncate(deflated_len_ng);

    assert_eq!(&deflated_rs, &deflated_ng);

    match uncompress_help(&deflated_ng) {
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

fn uncompress_help(input: &[u8]) -> Result<Vec<u8>, ReturnCode> {
    let mut dest_vec = vec![0u8; BYTES.len()];

    let mut dest_len = dest_vec.len();
    let dest = dest_vec.as_mut_ptr();

    let source = input.as_ptr();
    let source_len = input.len();

    let err = unsafe { libz_ng_sys::uncompress(dest, &mut dest_len, source, source_len) };

    if err != 0 {
        Err(zlib::ReturnCode::from(err))
    } else {
        dest_vec.truncate(dest_len as usize);

        Ok(dest_vec)
    }
}

fn compress_rs(
    dest: &mut [u8],
    dest_len: &mut usize,
    source: &[u8],
    //
    level: i32,
) -> ReturnCode {
    use zlib::{deflate, deflateEnd, deflateInit2_, z_stream};

    const VERSION: *const libc::c_char = "2.3.0\0".as_ptr() as *const libc::c_char;
    const STREAM_SIZE: libc::c_int = std::mem::size_of::<z_stream>() as libc::c_int;

    let mut stream = z_stream {
        next_in: source.as_ptr() as *mut u8,
        avail_in: 0, // for special logic in the first  iteration
        total_in: 0,
        next_out: dest.as_mut_ptr(),
        avail_out: 0, // for special logic on the first iteration
        total_out: 0,
        msg: std::ptr::null_mut(),
        state: std::ptr::null_mut(),
        zalloc: Some(zlib::allocate::zcalloc),
        zfree: Some(zlib::allocate::zcfree),
        opaque: std::ptr::null_mut(),
        data_type: 0,
        adler: 0,
        reserved: 0,
    };

    let method = zlib::Z_DEFLATED;
    let window_bits = 15;
    let mem_level = 8;
    let strategy = zlib::Z_DEFAULT_STRATEGY;

    let err = {
        let strm: *mut z_stream = &mut stream;
        unsafe {
            deflateInit2_(
                strm,
                level,
                method,
                window_bits,
                mem_level,
                strategy,
                VERSION,
                STREAM_SIZE,
            )
        }
    };

    if ReturnCode::from(err) != ReturnCode::Ok as _ {
        return ReturnCode::from(err);
    }

    let max = libc::c_uint::MAX as usize;

    let mut left = dest.len();
    let mut source_len = source.len();

    loop {
        if stream.avail_out == 0 {
            stream.avail_out = Ord::min(left, max) as _;
            left -= stream.avail_out as usize;
        }

        if stream.avail_in == 0 {
            stream.avail_in = Ord::min(source_len, max) as _;
            source_len -= stream.avail_in as usize;
        }

        let flush = if source_len > 0 {
            Flush::NoFlush
        } else {
            Flush::Finish
        };

        let err = unsafe { deflate(&mut stream, flush as i32) };
        if ReturnCode::from(err) != ReturnCode::Ok {
            break;
        }
    }

    *dest_len = stream.total_out as _;

    unsafe { deflateEnd(&mut stream) };

    ReturnCode::Ok
}

fn compress_ng(
    dest: &mut [u8],
    dest_len: &mut usize,
    source: &[u8],
    //
    level: i32,
) -> ReturnCode {
    use libz_ng_sys::{deflate, deflateEnd, deflateInit2_, z_stream};

    const VERSION: *const libc::c_char = "2.3.0\0".as_ptr() as *const libc::c_char;
    const STREAM_SIZE: libc::c_int = std::mem::size_of::<z_stream>() as libc::c_int;

    let mut stream = z_stream {
        next_in: source.as_ptr() as *mut u8,
        avail_in: 0, // for special logic in the first  iteration
        total_in: 0,
        next_out: dest.as_mut_ptr(),
        avail_out: 0, // for special logic on the first iteration
        total_out: 0,
        msg: std::ptr::null_mut(),
        state: std::ptr::null_mut(),
        zalloc: zlib::allocate::zcalloc,
        zfree: zlib::allocate::zcfree,
        opaque: std::ptr::null_mut(),
        data_type: 0,
        adler: 0,
        reserved: 0,
    };

    let method = zlib::Z_DEFLATED;
    let window_bits = 15;
    let mem_level = 8;
    let strategy = zlib::Z_DEFAULT_STRATEGY;

    let err = {
        let strm: *mut z_stream = &mut stream;
        unsafe {
            deflateInit2_(
                strm,
                level,
                method,
                window_bits,
                mem_level,
                strategy,
                VERSION,
                STREAM_SIZE,
            )
        }
    };

    if ReturnCode::from(err) != ReturnCode::Ok as _ {
        return ReturnCode::from(err);
    }

    let max = libc::c_uint::MAX as usize;

    let mut left = dest.len();
    let mut source_len = source.len();

    loop {
        if stream.avail_out == 0 {
            stream.avail_out = Ord::min(left, max) as _;
            left -= stream.avail_out as usize;
        }

        if stream.avail_in == 0 {
            stream.avail_in = Ord::min(source_len, max) as _;
            source_len -= stream.avail_in as usize;
        }

        let flush = if source_len > 0 {
            Flush::NoFlush
        } else {
            Flush::Finish
        };

        let err = unsafe { deflate(&mut stream, flush as i32) };
        if ReturnCode::from(err) != ReturnCode::Ok {
            break;
        }
    }

    *dest_len = stream.total_out as _;

    unsafe { deflateEnd(&mut stream) };

    ReturnCode::Ok
}
