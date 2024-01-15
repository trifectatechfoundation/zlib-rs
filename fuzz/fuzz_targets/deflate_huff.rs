#![no_main]
use libfuzzer_sys::fuzz_target;

use libc::{c_char, c_int};
use zlib::ReturnCode;

fn uncompress_help(input: &[u8]) -> Vec<u8> {
    let mut dest_vec = vec![0u8; 1 << 16];

    let mut dest_len = dest_vec.len();
    let dest = dest_vec.as_mut_ptr();

    let source = input.as_ptr();
    let source_len = input.len();

    let err = unsafe { libz_ng_sys::uncompress(dest, &mut dest_len, source, source_len) };

    if err != 0 {
        panic!("error {:?}", zlib::ReturnCode::from(err));
    }

    dest_vec.truncate(dest_len as usize);

    dest_vec
}

pub(crate) fn compress3<'a>(
    output: &'a mut [u8],
    input: &[u8],
    level: i32,
) -> (&'a mut [u8], ReturnCode) {
    let method = zlib::Z_DEFLATED;
    let window_bits = 15;
    let mem_level = 8;
    let strategy = zlib::Z_HUFFMAN_ONLY;

    let mut stream = zlib::z_stream {
        next_in: input.as_ptr() as *mut u8,
        avail_in: 0, // for special logic in the first  iteration
        total_in: 0,
        next_out: output.as_mut_ptr(),
        avail_out: 0, // for special logic on the first iteration
        total_out: 0,
        msg: std::ptr::null_mut(),
        state: std::ptr::null_mut(),
        zalloc: None,
        zfree: None,
        opaque: std::ptr::null_mut(),
        data_type: 0,
        adler: 0,
        reserved: 0,
    };

    const VERSION: *const c_char = "2.3.0\0".as_ptr() as *const c_char;
    const STREAM_SIZE: c_int = std::mem::size_of::<zlib::z_stream>() as c_int;

    let err = unsafe {
        zlib::deflateInit2_(
            &mut stream,
            level,
            method,
            window_bits,
            mem_level,
            strategy,
            VERSION,
            STREAM_SIZE,
        )
    };

    let err = ReturnCode::from(err);

    if err != ReturnCode::Ok {
        return (output, err);
    }

    let max = libc::c_uint::MAX as usize;

    let mut left = output.len();
    let mut source_len = input.len();

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
            zlib::Flush::NoFlush
        } else {
            zlib::Flush::Finish
        };

        let err = unsafe { zlib::deflate(&mut stream, flush as i32) };
        let err = ReturnCode::from(err);

        if err != ReturnCode::Ok {
            break;
        }
    }

    let output = &mut output[..stream.total_out as usize];

    unsafe { zlib::deflateEnd(&mut stream) };

    (output, ReturnCode::Ok)
}

fuzz_target!(|data: String| {
    // first, deflate the data using the standard zlib
    let length = 8 * 1024;
    let mut deflated = vec![0; length as usize];
    let (deflated, error) = compress3(&mut deflated, data.as_bytes(), 6);

    assert_eq!(ReturnCode::Ok, error);
    let output = uncompress_help(&deflated);

    if output != data.as_bytes() {
        let path = std::env::temp_dir().join("deflate.txt");
        std::fs::write(&path, &data).unwrap();
        eprintln!("saved input file to {path:?}");
    }

    assert_eq!(output, data.as_bytes());
});
