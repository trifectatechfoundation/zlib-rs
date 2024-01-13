//! a binary just so we can look at the optimized assembly

use std::path::PathBuf;

use zlib::{Flush, ReturnCode};

fn main() {
    let mut it = std::env::args();

    let _ = it.next().unwrap();

    let level = 8;

    match it.next().unwrap().as_str() {
        "ng" => {
            let path = it.next().unwrap();
            let input = std::fs::read(&path).unwrap();

            let mut dest_vec = vec![0u8; 1 << 28];
            let mut dest_len = dest_vec.len();

            let err = compress_ng(&mut dest_vec, &mut dest_len, &input, level);

            if err != ReturnCode::Ok {
                panic!("error {err:?}");
            }

            dest_vec.truncate(dest_len);

            let path = PathBuf::from(path);
            std::fs::write(path.with_extension(""), &dest_vec).unwrap();

            drop(dest_vec)
        }
        "rs" => {
            let path = it.next().unwrap();
            let Ok(input) = std::fs::read(&path) else {
                panic!("could not read file {path:?}");
            };

            let mut dest_vec = vec![0u8; 1 << 28];
            let mut dest_len = dest_vec.len();

            let err = compress_rs(&mut dest_vec, &mut dest_len, &input, level);

            if err != ReturnCode::Ok {
                panic!("error {err:?}");
            }

            dest_vec.truncate(dest_len as usize);

            let path = PathBuf::from(path);
            std::fs::write(path.with_extension(""), &dest_vec).unwrap();

            drop(dest_vec)
        }
        "xx" => {
            let path = it.next().unwrap();
            let Ok(input) = std::fs::read(&path) else {
                panic!("could not read file {path:?}");
            };

            let mut dest_vec = vec![0u8; 1 << 28];
            let mut dest_len = dest_vec.len();

            let err = compress_dynamic(&mut dest_vec, &mut dest_len, &input, level);

            if err != ReturnCode::Ok {
                panic!("error {err:?}");
            }

            dest_vec.truncate(dest_len as usize);

            let path = PathBuf::from(path);
            std::fs::write(path.with_extension(""), &dest_vec).unwrap();

            drop(dest_vec)
        }
        other => panic!("invalid option '{other}', expected one of 'rs' or 'ng'"),
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

fn compress_dynamic(
    dest: &mut [u8],
    dest_len: &mut usize,
    source: &[u8],
    //
    level: i32,
) -> ReturnCode {
    use libz_ng_sys::{deflateEnd, z_stream};

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
            deflateInit2__dynamic(
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

        let err = unsafe { deflate_dynamic(&mut stream, flush as i32) };
        if ReturnCode::from(err) != ReturnCode::Ok {
            break;
        }
    }

    *dest_len = stream.total_out as _;

    unsafe { deflateEnd(&mut stream) };

    ReturnCode::Ok
}

pub unsafe fn deflate_dynamic(strm: *mut libz_ng_sys::z_stream, flush: i32) -> std::ffi::c_int {
    const LIBZ_NG_SO: &str = "/home/folkertdev/rust/zlib-ng/libz-ng.so";

    let lib = libloading::Library::new(LIBZ_NG_SO).unwrap();

    type Func = unsafe extern "C" fn(strm: *mut libz_ng_sys::z_stream, flush: i32) -> i32;

    let f: libloading::Symbol<Func> = lib.get(b"zng_deflate").unwrap();

    f(strm, flush)
}

pub unsafe fn deflateInit2__dynamic(
    strm: *mut libz_ng_sys::z_stream,
    level: libc::c_int,
    method: libc::c_int,
    windowBits: libc::c_int,
    memLevel: libc::c_int,
    strategy: libc::c_int,
    version: *const libc::c_char,
    stream_size: libc::c_int,
) -> std::ffi::c_int {
    const LIBZ_NG_SO: &str = "/home/folkertdev/rust/zlib-ng/libz-ng.so";

    let lib = libloading::Library::new(LIBZ_NG_SO).unwrap();

    type Func = unsafe extern "C" fn(
        strm: *mut libz_ng_sys::z_stream,
        level: libc::c_int,
        method: libc::c_int,
        windowBits: libc::c_int,
        memLevel: libc::c_int,
        strategy: libc::c_int,
        version: *const libc::c_char,
        stream_size: libc::c_int,
    ) -> i32;

    let f: libloading::Symbol<Func> = lib.get(b"zng_deflateInit2_").unwrap();

    f(
        strm,
        level,
        method,
        windowBits,
        memLevel,
        strategy,
        version,
        stream_size,
    )
}
