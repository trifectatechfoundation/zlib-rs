#![no_main]
use libfuzzer_sys::fuzz_target;
use zlib_rs::ReturnCode;

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
        zalloc: ::zlib_rs::allocate::C.zalloc,
        zfree: ::zlib_rs::allocate::C.zfree,
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
            window_bits,
            mem_level,
            strategy,
            libz_ng_sys::zlibVersion(),
            std::mem::size_of::<libz_ng_sys::z_stream>() as i32,
        );
        let return_code = ReturnCode::from(err);

        assert_eq!(ReturnCode::Ok, return_code);
    };

    let error = unsafe { libz_ng_sys::deflate(&mut stream, libz_ng_sys::Z_FINISH) };

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

    let deflated = deflate_ng(data.as_bytes(), window_bits);

    let mut stream = libz_rs_sys::z_stream::default();

    unsafe {
        let err = libz_rs_sys::inflateInit2_(
            &mut stream,
            window_bits,
            libz_rs_sys::zlibVersion(),
            core::mem::size_of::<libz_rs_sys::z_stream>() as i32,
        );
        let return_code: ReturnCode = ReturnCode::from(err);

        assert_eq!(ReturnCode::Ok, return_code);
    };

    let mut output = vec![0; 1 << 15];
    stream.next_out = output.as_mut_ptr();
    stream.avail_out = output.len() as _;

    for chunk in deflated.as_slice().chunks(chunk_size) {
        stream.next_in = chunk.as_ptr() as *mut u8;
        stream.avail_in = chunk.len() as _;

        let err = unsafe { libz_rs_sys::inflate(&mut stream, ::libz_rs_sys::Z_NO_FLUSH) };
        let return_code: ReturnCode = ReturnCode::from(err);

        match return_code {
            ReturnCode::Ok => continue,
            ReturnCode::StreamEnd => continue,
            _ => {
                if stream.msg.is_null() {
                    panic!("{return_code:?}: <no error message>")
                } else {
                    let msg = unsafe { std::ffi::CStr::from_ptr(stream.msg) };
                    panic!("{return_code:?}: {msg:?}")
                }
            }
        }
    }

    output.truncate(stream.total_out as usize);
    let output = String::from_utf8(output).unwrap();

    unsafe {
        let err = libz_rs_sys::inflateEnd(&mut stream);
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
