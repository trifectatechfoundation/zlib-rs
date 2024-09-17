//! a binary just so we can look at the optimized  assembly

// we use the libz_sys but configure zlib-ng in zlib compat mode
use libz_sys as libz_ng_sys;

macro_rules! chunked_body {
    ($chunk_size:expr, $dest:expr, $source:expr) => {{
        use core::ffi::*;
        use zlib_rs::{inflate::InflateConfig, InflateFlush, ReturnCode};

        let mut stream = core::mem::MaybeUninit::zeroed();

        let err = inflateInit2_(
            stream.as_mut_ptr(),
            InflateConfig::default().window_bits,
            zlibVersion(),
            core::mem::size_of::<z_stream>() as c_int,
        );
        assert_eq!(err, 0);

        let stream = stream.assume_init_mut();

        stream.avail_out = $dest.len() as _;
        stream.next_out = $dest.as_mut_ptr();

        for chunk in $source.chunks($chunk_size) {
            stream.next_in = chunk.as_ptr() as *mut u8;
            stream.avail_in = chunk.len() as _;

            let err = inflate(stream, InflateFlush::NoFlush as _);

            if err == ReturnCode::StreamEnd as i32 {
                break;
            }

            assert_eq!(err, ReturnCode::Ok as i32);
        }

        let err = inflateEnd(stream);
        assert_eq!(err, ReturnCode::Ok as i32);

        stream.total_out
    }};
}

macro_rules! chunked {
    ("chunked-ng", $chunk_size:expr, $dest:expr, $source:expr) => {{
        use libz_sys::*;

        unsafe { chunked_body!($chunk_size, $dest, $source) }
    }};
    ("chunked-rs", $chunk_size:expr, $dest:expr, $source:expr) => {{
        use libz_rs_sys::*;

        unsafe { chunked_body!($chunk_size, $dest, $source) }
    }};
}

fn main() {
    let mut it = std::env::args();

    let _ = it.next().unwrap();

    let mut dest_vec = vec![0u8; 1 << 28];

    let mut dest_len = dest_vec.len() as std::ffi::c_ulong;
    let dest = dest_vec.as_mut_ptr();

    let silesia_small_tar_gz = include_bytes!("../../silesia-small.tar.gz");

    match it.next().unwrap().as_str() {
        "ng" => {
            let path = it.next().unwrap();
            let input = std::fs::read(&path).unwrap();

            let source = input.as_ptr();
            let source_len = input.len() as _;

            let err = unsafe { libz_ng_sys::uncompress(dest, &mut dest_len, source, source_len) };
            assert_eq!(err, 0);
        }
        "rs" => {
            let path = it.next().unwrap();
            let input = std::fs::read(&path).unwrap();

            let source = input.as_ptr();
            let source_len = input.len() as _;

            let err = unsafe { ::libz_rs_sys::uncompress(dest, &mut dest_len, source, source_len) };
            assert_eq!(err, 0);
        }
        "ng-chunked" => {
            let chunk_log: usize = it.next().unwrap().parse().unwrap();

            let _ = chunked!(
                "chunked-ng",
                1 << chunk_log,
                &mut dest_vec,
                &silesia_small_tar_gz
            );
        }
        "rs-chunked" => {
            let chunk_log: usize = it.next().unwrap().parse().unwrap();

            let _ = chunked!(
                "chunked-rs",
                1 << chunk_log,
                &mut dest_vec,
                &silesia_small_tar_gz
            );
        }
        "ng-adler" => {
            let chunk_log: usize = it.next().unwrap().parse().unwrap();

            let path = it.next().unwrap();
            let input = std::fs::read(&path).unwrap();

            let mut adler = unsafe { libz_sys::adler32(0, core::ptr::null(), 0) };

            for chunk in input.chunks(1 << chunk_log) {
                adler = unsafe { libz_sys::adler32(adler, chunk.as_ptr(), chunk.len() as _) };
            }
        }
        "rs-adler" => {
            let chunk_log: usize = it.next().unwrap().parse().unwrap();

            let path = it.next().unwrap();
            let input = std::fs::read(&path).unwrap();

            let mut adler = unsafe { libz_rs_sys::adler32(0, core::ptr::null(), 0) };

            for chunk in input.chunks(1 << chunk_log) {
                adler = unsafe { libz_rs_sys::adler32(adler, chunk.as_ptr(), chunk.len() as _) };
            }
        }
        other => panic!("invalid option '{other}', expected one of 'rs' or 'ng'"),
    }
}
