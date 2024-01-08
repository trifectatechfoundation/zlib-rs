#![no_main]
use libfuzzer_sys::fuzz_target;
use zlib::ReturnCode;

const VERSION: *const libc::c_char = "2.3.0\0".as_ptr() as *const libc::c_char;
const STREAM_SIZE: libc::c_int = std::mem::size_of::<zlib::z_stream>() as libc::c_int;

fn deflate_ng(data: &[u8], window_bits: i32) -> Vec<u8> {
    // first, deflate the data using the standard zlib
    let length = 8 * 1024;
    let mut deflated = vec![0u8; length as usize];

    let mut stream = libz_ng_sys::z_stream {
        next_in: data.as_ptr() as *mut u8,
        avail_in: data.len() as _,
        total_in: 0,
        next_out: deflated.as_mut_ptr(),
        avail_out: deflated.len() as _,
        total_out: 0,
        msg: std::ptr::null_mut(),
        state: std::ptr::null_mut(),
        zalloc: ::zlib::allocate::zcalloc,
        zfree: ::zlib::allocate::zcfree,
        opaque: std::ptr::null_mut(),
        data_type: 0,
        adler: 0,
        reserved: 0,
    };

    let level = 9;
    let method = 8; // deflate
    let mem_level = 8;
    let strategy = 0;

    unsafe {
        let err = libz_ng_sys::deflateInit2_(
            &mut stream,
            level,
            method,
            window_bits as i32,
            mem_level,
            strategy,
            b"1.3.0\0".as_ptr() as *const i8,
            std::mem::size_of::<libz_ng_sys::z_stream>() as i32,
        );
        let return_code = ReturnCode::from(err);

        assert_eq!(ReturnCode::Ok, return_code);
    };

    let error = unsafe { libz_ng_sys::deflate(&mut stream, zlib::Flush::Finish as _) };

    let error: ReturnCode = ReturnCode::from(error as i32);
    assert_eq!(ReturnCode::StreamEnd, error);

    deflated.truncate(stream.total_out as _);

    unsafe {
        let err = libz_ng_sys::deflateEnd(&mut stream);
        let return_code: ReturnCode = ReturnCode::from(err);
        assert_eq!(ReturnCode::Ok, return_code);
    }

    deflated
}

fuzz_target!(|input: (String, usize)| {
    let (data, chunk_size) = input;

    // any other value seems to be kind of broken?
    let window_bits = 15;

    if chunk_size == 0 {
        return;
    }

    let deflated = deflate_ng(data.as_bytes(), window_bits as i32);

    let mut stream = zlib::z_stream::default();

    unsafe {
        let err = zlib::inflateInit2_(&mut stream, window_bits as i32, VERSION, STREAM_SIZE);
        let return_code: ReturnCode = ReturnCode::from(err);

        assert_eq!(ReturnCode::Ok, return_code);
    };

    let mut output = vec![0; 1 << 15];
    stream.next_out = output.as_mut_ptr();
    stream.avail_out = output.len() as _;

    for chunk in deflated.as_slice().chunks(chunk_size) {
        stream.next_in = chunk.as_ptr() as *mut u8;
        stream.avail_in = chunk.len() as _;

        let err = unsafe { zlib::inflate(&mut stream, ::zlib::Flush::NoFlush as _) };
        let return_code: ReturnCode = ReturnCode::from(err);

        match return_code {
            ReturnCode::Ok => continue,
            ReturnCode::StreamEnd => continue,
            _ => {
                if stream.msg.is_null() {
                    panic!("{:?}: <no error message>", return_code)
                } else {
                    let msg = unsafe { std::ffi::CStr::from_ptr(stream.msg) };
                    panic!("{:?}: {:?}", return_code, msg)
                }
            }
        }
    }

    output.truncate(stream.total_out as usize);
    let output = String::from_utf8(output).unwrap();

    unsafe {
        let err = zlib::inflateEnd(&mut stream);
        let return_code: ReturnCode = ReturnCode::from(err);
        assert_eq!(ReturnCode::Ok, return_code);
    }

    if output != data {
        let path = std::env::temp_dir().join("deflate.txt");
        std::fs::write(&path, &data).unwrap();
        eprintln!("saved input file to {path:?}");
    }

    assert_eq!(output, data);
});
