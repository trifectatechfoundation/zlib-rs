#![no_main]

use zlib_rs::ReturnCode;

use libfuzzer_sys::fuzz_target;

use zlib_rs::c_api::gz_header;

const VERSION: *const libc::c_char = "2.3.0\0".as_ptr() as *const libc::c_char;
const STREAM_SIZE: libc::c_int = std::mem::size_of::<libz_ng_sys::z_stream>() as libc::c_int;

fuzz_target!(|data: &[u8]| {

    let window_bits = 47;

    let mut stream = libz_rs_sys::z_stream::default();
    stream.next_in = data.as_ptr() as *mut u8;
    stream.avail_in = data.len() as _;

    unsafe {
        let err = libz_rs_sys::inflateInit2_(&mut stream, window_bits as i32, VERSION, STREAM_SIZE);
        let return_code: ReturnCode = ReturnCode::from(err);

        assert_eq!(ReturnCode::Ok, return_code);
    };

    let extra: [u8; 14] = [0; 14];
    let name: [u8; 9] = [0; 9];
    let comment: [u8; 10] = [0; 10];

    let mut header = gz_header {
        text: 0,
        time: 0,
        xflags: 0,
        os: 0,
        extra: extra.as_ptr() as *mut u8,
        extra_len: 0,
        extra_max: 14,
        name: name.as_ptr() as *mut u8,
        name_max: 9,
        comment: comment.as_ptr() as *mut u8,
        comm_max: 10,
        hcrc: 0,
        done: 0,
    };

    let ret = unsafe { libz_rs_sys::inflateGetHeader(&mut stream, &mut header) };
    assert_eq!(ReturnCode::from(ret), ReturnCode::Ok);

    let mut output = vec![0; 1 << 15];
    stream.next_out = output.as_mut_ptr();
    stream.avail_out = output.len() as _;

    let err = unsafe { libz_rs_sys::inflate(&mut stream, ::libz_rs_sys::Z_NO_FLUSH) };
    let return_code_rs: ReturnCode = ReturnCode::from(err);

    match return_code_rs {
        ReturnCode::Ok => (),
        ReturnCode::StreamEnd => (),
        ReturnCode::BufError => (),
        ReturnCode::DataError => (),
        ReturnCode::NeedDict => (),
        ReturnCode::StreamError => (),
        ReturnCode::MemError => (),
        ReturnCode::ErrNo => (),
        ReturnCode::VersionError => (),
    }

    unsafe {
        let err = libz_rs_sys::inflateEnd(&mut stream);
        let return_code: ReturnCode = ReturnCode::from(err);
        assert_eq!(ReturnCode::Ok, return_code);
    }
});
