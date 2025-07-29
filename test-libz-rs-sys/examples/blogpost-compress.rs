use std::ffi::{c_int, c_uint};

// we use the libz_sys but configure zlib-ng in zlib compat mode
use libz_sys as libz_ng_sys;

use zlib_rs::{DeflateFlush, ReturnCode};

fn main() {
    let mut it = std::env::args();

    // skips the program name
    let _ = it.next().unwrap();

    let level: i32 = it.next().unwrap().parse().unwrap();

    let mut dest_vec = vec![0u8; 1 << 28];
    let mut dest_len = dest_vec.len();

    match it.next().unwrap().as_str() {
        "ng" => {
            let path = it.next().unwrap();
            let input = std::fs::read(path).unwrap();

            let err = compress_ng(&mut dest_vec, &mut dest_len, &input, level);
            assert_eq!(ReturnCode::Ok, err);
        }
        "rs" => {
            let path = it.next().unwrap();
            let input = std::fs::read(path).unwrap();

            let err = compress_rs(&mut dest_vec, &mut dest_len, &input, level);
            assert_eq!(ReturnCode::Ok, err);
        }
        other => panic!("invalid input: {other:?}"),
    }
}

const METHOD: i32 = zlib_rs::c_api::Z_DEFLATED;
const WINDOW_BITS: i32 = 15;
const MEM_LEVEL: i32 = 8;
const STRATEGY: i32 = zlib_rs::c_api::Z_DEFAULT_STRATEGY;

fn compress_rs(
    dest: &mut [u8],
    dest_len: &mut usize,
    source: &[u8],
    //
    level: i32,
) -> ReturnCode {
    use libz_rs_sys::{deflate, deflateEnd, deflateInit2_, z_stream, zlibVersion};

    let mut stream = z_stream {
        next_in: source.as_ptr() as *mut u8,
        avail_in: 0, // for special logic in the first  iteration
        total_in: 0,
        next_out: dest.as_mut_ptr(),
        avail_out: 0, // for special logic on the first iteration
        total_out: 0,
        msg: std::ptr::null_mut(),
        state: std::ptr::null_mut(),
        zalloc: Some(zlib_rs::allocate::C.zalloc),
        zfree: Some(zlib_rs::allocate::C.zfree),
        opaque: std::ptr::null_mut(),
        data_type: 0,
        adler: 0,
        reserved: 0,
    };

    let err = {
        let strm: *mut z_stream = &mut stream;
        unsafe {
            deflateInit2_(
                strm,
                level,
                METHOD,
                WINDOW_BITS,
                MEM_LEVEL,
                STRATEGY,
                zlibVersion(),
                std::mem::size_of::<z_stream>() as c_int,
            )
        }
    };

    if ReturnCode::from(err) != ReturnCode::Ok as _ {
        return ReturnCode::from(err);
    }

    let max = c_uint::MAX as usize;

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
            DeflateFlush::NoFlush
        } else {
            DeflateFlush::Finish
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
    use libz_ng_sys::{deflate, deflateEnd, deflateInit2_, z_stream, zlibVersion};

    let mut stream = z_stream {
        next_in: source.as_ptr() as *mut u8,
        avail_in: 0, // for special logic in the first  iteration
        total_in: 0,
        next_out: dest.as_mut_ptr(),
        avail_out: 0, // for special logic on the first iteration
        total_out: 0,
        msg: std::ptr::null_mut(),
        state: std::ptr::null_mut(),
        zalloc: zlib_rs::allocate::C.zalloc,
        zfree: zlib_rs::allocate::C.zfree,
        opaque: std::ptr::null_mut(),
        data_type: 0,
        adler: 0,
        reserved: 0,
    };

    let err = {
        let strm: *mut z_stream = &mut stream;
        unsafe {
            deflateInit2_(
                strm,
                level,
                METHOD,
                WINDOW_BITS,
                MEM_LEVEL,
                STRATEGY,
                zlibVersion(),
                std::mem::size_of::<z_stream>() as c_int,
            )
        }
    };

    if ReturnCode::from(err) != ReturnCode::Ok as _ {
        return ReturnCode::from(err);
    }

    let max = c_uint::MAX as usize;

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
            DeflateFlush::NoFlush
        } else {
            DeflateFlush::Finish
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
