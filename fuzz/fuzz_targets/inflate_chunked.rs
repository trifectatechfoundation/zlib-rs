#![no_main]
use libfuzzer_sys::fuzz_target;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(i32)]
enum ReturnCode {
    Ok = 0,
    StreamEnd = 1,
    NeedDict = 2,
    ErrNo = -1,
    StreamError = -2,
    DataError = -3,
    MemError = -4,
    BufError = -5,
    VersionError = -6,
}

unsafe fn deflate(strm: *mut libz_ng_sys::z_stream, flush: i32) -> std::ffi::c_int {
    let lib = libloading::Library::new("/home/folkertdev/rust/zlib-ng/libz-ng.so").unwrap();

    type Func =
        unsafe extern "C" fn(strm: *mut libz_ng_sys::z_stream, flush: i32) -> std::ffi::c_int;

    let f: libloading::Symbol<Func> = lib.get(b"zng_deflate").unwrap();

    f(strm, flush)
}

unsafe fn inflate(strm: *mut libz_ng_sys::z_stream, flush: i32) -> std::ffi::c_int {
    let lib = libloading::Library::new("/home/folkertdev/rust/zlib-ng/libz-ng.so").unwrap();

    type Func =
        unsafe extern "C" fn(strm: *mut libz_ng_sys::z_stream, flush: i32) -> std::ffi::c_int;

    let f: libloading::Symbol<Func> = lib.get(b"zng_inflate").unwrap();

    f(strm, flush)
}

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
        let return_code = std::mem::transmute::<_, ReturnCode>(err);

        assert_eq!(ReturnCode::Ok, return_code);
    };

    let error = unsafe { libz_ng_sys::deflate(&mut stream, zlib::Flush::Finish as _) };

    let error: ReturnCode = unsafe { std::mem::transmute(error as i32) };
    assert_eq!(ReturnCode::StreamEnd, error);

    deflated.truncate(stream.total_out as _);

    unsafe {
        let err = libz_ng_sys::deflateEnd(&mut stream);
        let return_code = std::mem::transmute::<_, ReturnCode>(err);
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
        let err = zlib::inflate::inflateInit2(&mut stream, window_bits as i32);
        let return_code = std::mem::transmute::<_, ReturnCode>(err);

        assert_eq!(ReturnCode::Ok, return_code);
    };

    let mut output = vec![0; 1 << 15];
    stream.next_out = output.as_mut_ptr();
    stream.avail_out = output.len() as _;

    for chunk in deflated.as_slice().chunks(chunk_size) {
        stream.next_in = chunk.as_ptr() as *mut u8;
        stream.avail_in = chunk.len() as _;

        let err = unsafe { zlib::inflate::inflate(&mut stream, ::zlib::Flush::NoFlush as _) };
        let return_code = unsafe { std::mem::transmute::<_, ReturnCode>(err) };

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
        let err = zlib::inflate::inflateEnd(&mut stream);
        let return_code = std::mem::transmute::<_, ReturnCode>(err);
        assert_eq!(ReturnCode::Ok, return_code);
    }

    if output != data {
        let path = std::env::temp_dir().join("deflate.txt");
        std::fs::write(&path, &data).unwrap();
        eprintln!("saved input file to {path:?}");
    }

    assert_eq!(output, data);
});
