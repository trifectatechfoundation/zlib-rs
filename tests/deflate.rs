use std::{
    ffi::CStr,
    sync::atomic::{AtomicU8, Ordering},
};

use zlib::*;

use libc::{c_char, c_int};

const VERSION: *const c_char = "2.3.0\0".as_ptr() as *const c_char;
const STREAM_SIZE: c_int = std::mem::size_of::<zlib::z_stream>() as c_int;

pub mod quick {
    use super::*;

    #[rustfmt::skip]
    const BI_VALID_INPUT: [u8; 554] = [
        0x8d, 0xff, 0xff, 0xff, 0xa2, 0x00, 0x00, 0xff, 0x00, 0x15, 0x1b, 0x1b, 0xa2, 0xa2, 0xaf, 0xa2,
        0xa2, 0x00, 0x00, 0x00, 0x02, 0x00, 0x1b, 0x3f, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x0b,
        0x00, 0xab, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x01, 0x2b, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x01, 0x1e, 0x00, 0x00, 0x01, 0x40, 0x00, 0x00, 0x00, 0x07, 0x01, 0x18, 0x00, 0x22, 0x00,
        0x00, 0x00, 0xfd, 0x39, 0xff, 0x00, 0x00, 0x00, 0x1b, 0xfd, 0x3b, 0x00, 0x68, 0x00, 0x00, 0x01,
        0xff, 0xff, 0xff, 0x57, 0xf8, 0x1e, 0x00, 0x00, 0xf2, 0xf2, 0xf2, 0xf2, 0xfa, 0xff, 0xff, 0xff,
        0xff, 0x7e, 0x00, 0x00, 0x4a, 0x00, 0xc5, 0x00, 0x41, 0x00, 0x00, 0x00, 0x01, 0x01, 0x00, 0x00,
        0x00, 0x02, 0x01, 0x01, 0x00, 0xa2, 0x08, 0x00, 0x00, 0x00, 0x00, 0x27, 0x4a, 0x4a, 0x4a, 0x32,
        0x00, 0xf9, 0xff, 0x00, 0x02, 0x9a, 0xff, 0x00, 0x00, 0x3f, 0x50, 0x00, 0x03, 0x00, 0x00, 0x00,
        0x3d, 0x00, 0x08, 0x2f, 0x20, 0x00, 0x23, 0x00, 0x00, 0x00, 0x00, 0x23, 0x00, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0x7a, 0x7a, 0x9e, 0xff, 0xff, 0x00, 0x1b, 0x1b, 0x04, 0x00, 0x1b, 0x1b,
        0x1b, 0x1b, 0x00, 0x00, 0x00, 0xaf, 0xad, 0xaf, 0x00, 0x00, 0xa8, 0x00, 0x00, 0x00, 0x2e, 0xff,
        0xff, 0x2e, 0xc1, 0x00, 0x10, 0x00, 0x00, 0x00, 0x06, 0x70, 0x00, 0x00, 0x00, 0xda, 0x67, 0x01,
        0x47, 0x00, 0x00, 0x00, 0x0c, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0x00, 0x01, 0x00, 0x3f,
        0x54, 0x00, 0x00, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x5c, 0x00, 0x00, 0x34, 0x3e, 0xc5, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x7a, 0x00, 0x00, 0x00, 0x0a, 0x01, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x7a, 0x7a, 0x7a, 0x7a, 0x7a, 0x00, 0x00, 0x00, 0x40, 0x1b, 0x1b, 0x88, 0x1b, 0x1b,
        0x1b, 0x1b, 0x1b, 0x1b, 0x1b, 0x1f, 0x1b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0b, 0x00, 0x00, 0x00,
        0x00, 0x04, 0x00, 0x00, 0x50, 0x3e, 0x7a, 0x7a, 0x00, 0x00, 0x40, 0x00, 0x40, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x08, 0x87, 0x00, 0x00, 0xff, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x01, 0x00, 0xff, 0x3d, 0x00, 0x11, 0x4d, 0x00, 0x00, 0x01, 0xd4, 0xd4, 0xd4, 0xd4, 0x2d, 0xd4,
        0xd4, 0xff, 0xff, 0xff, 0xfa, 0x01, 0xd4, 0x00, 0xd4, 0x00, 0x00, 0xd4, 0xd4, 0xd4, 0xd4, 0xd4,
        0xd4, 0x1e, 0x1e, 0x1e, 0x1e, 0x00, 0x00, 0xfe, 0xf9, 0x1e, 0x1e, 0x1e, 0x1e, 0x1e, 0x1e, 0x00,
        0x16, 0xd4, 0xd4, 0xd4, 0xd4, 0xd4, 0xd4, 0xd4, 0xd4, 0xd4, 0x00, 0x00, 0x80, 0x20, 0x00, 0x00,
        0xff, 0x2b, 0x2b, 0x2b, 0x2b, 0x35, 0xd4, 0xd4, 0x47, 0x3f, 0xd4, 0xd4, 0xd6, 0xd4, 0xd4, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x32, 0x4a, 0x4a, 0x4a, 0x4a, 0x71, 0x00, 0x1b, 0x1b, 0x1b, 0x1b, 0x1b,
        0x1f, 0x1b, 0x1b, 0x1b, 0x57, 0x57, 0x57, 0x57, 0x00, 0x00, 0x1b, 0x08, 0x2b, 0x16, 0xc3, 0x00,
        0x00, 0x00, 0x29, 0x30, 0x03, 0xff, 0x03, 0x03, 0x03, 0x03, 0x07, 0x00, 0x00, 0x01, 0x0b, 0xff,
        0xff, 0xf5, 0xf5, 0xf5, 0x00, 0x00, 0xfe, 0xfa, 0x0f, 0x0f, 0x08, 0x00, 0xff, 0x00, 0x53, 0x3f,
        0x00, 0x04, 0x5d, 0xa8, 0x2e, 0xff, 0xff, 0x00, 0x2f, 0x2f, 0x05, 0xff, 0xff, 0xff, 0x2f, 0x2f,
        0x2f, 0x0a, 0x0a, 0x0a, 0x0a, 0x30, 0xff, 0xff, 0xff, 0xf0, 0x0a, 0x0a, 0x0a, 0x00, 0xff, 0x3f,
        0x4f, 0x00, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x71, 0x00, 0x2e, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x71, 0x71, 0x00, 0x71, 0x71, 0x71, 0xf5, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0xff,
        0xff, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0xdb, 0x3f, 0x00, 0xfa, 0x71, 0x71, 0x71, 0x00, 0x00,
        0x00, 0x01, 0x00, 0x00, 0x00, 0x71, 0x71, 0x71, 0x71, 0x71
    ];

    #[test]
    fn bi_valid() {
        let mut stream = zlib::z_stream {
            next_in: std::ptr::null_mut(),
            avail_in: 0,
            total_in: 0,
            next_out: std::ptr::null_mut(),
            avail_out: 0,
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

        let err = unsafe {
            deflateInit2_(
                &mut stream,
                1,
                Z_DEFLATED,
                31,
                1,
                Z_FILTERED,
                VERSION,
                STREAM_SIZE,
            )
        };

        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        stream.next_in = &BI_VALID_INPUT as *const u8 as *mut u8;
        let mut next_out = [0u8; 1236];
        stream.next_out = next_out.as_mut_ptr();

        stream.avail_in = 554;
        stream.avail_out = 31;

        let err = unsafe { deflate(&mut stream, Flush::Finish as i32) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        stream.avail_in = 0;
        stream.avail_out = 498;
        let err = unsafe { deflate(&mut stream, Flush::Finish as i32) };
        assert_eq!(ReturnCode::from(err), ReturnCode::StreamEnd);

        let err = unsafe { deflateEnd(&mut stream) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);
    }

    #[rustfmt::skip]
    const BLOCK_OPEN_INPUT: [u8; 495] = [ 
        0x1d, 0x1d, 0x00, 0x00, 0x00, 0x4a, 0x4a, 0x4a, 0xaf, 0xaf, 0xaf, 0xaf, 0x4a, 0x4a, 0x4a, 0x4a,
        0x3f, 0x3e, 0xaf, 0xff, 0xff, 0xff, 0x11, 0xff, 0xff, 0xff, 0xff, 0xdf, 0x00, 0x00, 0x00, 0x01,
        0x3f, 0x7d, 0x00, 0x50, 0x00, 0x00, 0xc8, 0x01, 0x2b, 0x60, 0xc8, 0x00, 0x24, 0x06, 0xff, 0xff,
        0x4a, 0x4e, 0x4a, 0x7d, 0xc8, 0x01, 0xf1, 0x2b, 0x28, 0xb2, 0xb2, 0x60, 0x25, 0xc8, 0x06, 0x00,
        0x00, 0x00, 0x31, 0x00, 0x01, 0xb2, 0xb2, 0xb2, 0xff, 0xff, 0xfd, 0xb2, 0xb2, 0x40, 0xff, 0x7d,
        0x3b, 0x34, 0x3e, 0xff, 0xff, 0x4a, 0x4a, 0x01, 0xf1, 0xff, 0x02, 0xff, 0x3f, 0xff, 0x02, 0xff,
        0xff, 0xff, 0xbf, 0x0a, 0xff, 0x00, 0x01, 0x3f, 0xb3, 0xff, 0x26, 0x00, 0x00, 0x13, 0x00, 0xc8,
        0x3e, 0x3e, 0x3e, 0x4a, 0x76, 0x4a, 0x4a, 0x2e, 0x7d, 0x3e, 0x3e, 0x3e, 0x3e, 0x1d, 0x1d, 0x1d,
        0xfe, 0xea, 0xef, 0x80, 0x01, 0x00, 0x00, 0x40, 0x00, 0x00, 0xba, 0x00, 0x06, 0xfa, 0xb9, 0x11,
        0xbf, 0x98, 0xee, 0x45, 0x7e, 0x04, 0x00, 0xff, 0xff, 0xff, 0x67, 0xc3, 0xc3, 0xc3, 0xc3, 0x00,
        0x1d, 0x1d, 0xe1, 0xe3, 0x00, 0xc3, 0x1d, 0x98, 0x1d, 0x1d, 0x1d, 0x1d, 0x1d, 0x00, 0x00, 0x00,
        0x02, 0x00, 0x00, 0x00, 0xe8, 0x00, 0x00, 0x1d, 0x1d, 0x1d, 0xfa, 0x1e, 0x12, 0xff, 0xff, 0xff,
        0x00, 0x01, 0xa7, 0xff, 0xff, 0xff, 0x1d, 0x1d, 0x1d, 0x63, 0xff, 0xff, 0xff, 0x1f, 0x00, 0x00,
        0x10, 0x40, 0x00, 0x00, 0xad, 0xff, 0xff, 0x3f, 0x51, 0x00, 0xf8, 0xff, 0xff, 0x8a, 0x01, 0x05,
        0x00, 0x00, 0x03, 0x00, 0x00, 0xff, 0x00, 0x00, 0x00, 0x05, 0x40, 0x1f, 0x08, 0x0a, 0x00, 0xff,
        0xff, 0x01, 0x00, 0x12, 0x00, 0x00, 0x01, 0x00, 0x3f, 0x40, 0x1d, 0x1d, 0x1d, 0x1d, 0x1d, 0x1d,
        0x21, 0x00, 0x1d, 0x00, 0x00, 0x00, 0xe4, 0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0xe6, 0xe6, 0x34,
        0xe6, 0xe6, 0xe6, 0xe6, 0xff, 0x2b, 0xee, 0x1d, 0x1d, 0x1d, 0x93, 0x1d, 0x1d, 0x1d, 0xee, 0x2b,
        0xee, 0x01, 0x81, 0x1d, 0x00, 0x00, 0x58, 0x00, 0x00, 0x01, 0x14, 0x00, 0x1b, 0x00, 0x00, 0x2c,
        0x00, 0x00, 0x00, 0xdb, 0x00, 0x45, 0x7e, 0x00, 0x00, 0x00, 0xfb, 0xbd, 0x00, 0x06, 0x21, 0xd3,
        0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x49, 0x49, 0xc9, 0x49, 0x3d, 0x00, 0x34, 0x01, 0x00,
        0x00, 0x6a, 0x2b, 0x00, 0x00, 0x50, 0x40, 0xf0, 0xf0, 0xf0, 0xf0, 0xa3, 0xa3, 0xa3, 0xa3, 0xf0,
        0xf0, 0x06, 0xfa, 0xa9, 0x01, 0x10, 0xbf, 0x98, 0x9d, 0x2b, 0xee, 0x2d, 0x21, 0x01, 0xdb, 0x00,
        0x45, 0x10, 0x00, 0x00, 0x7e, 0x00, 0x00, 0xe7, 0x00, 0xff, 0xff, 0x00, 0xf6, 0x00, 0x00, 0x00,
        0xf9, 0x00, 0x00, 0x00, 0x11, 0x00, 0x00, 0x00, 0xe2, 0x00, 0x00, 0x00, 0x2d, 0x00, 0x00, 0x00,
        0x2f, 0x00, 0x3f, 0x54, 0x1d, 0x1d, 0x1d, 0x4c, 0x4c, 0x4c, 0x4c, 0x2a, 0x4c, 0x4c, 0x10, 0xff,
        0xff, 0x1a, 0x00, 0x00, 0x01, 0xff, 0x00, 0xff, 0xf9, 0x00, 0x3f, 0x53, 0xcc, 0xcc, 0xcc, 0xcc,
        0x6e, 0x00, 0x00, 0x01, 0xf8, 0xff, 0xff, 0xff, 0x49, 0x04, 0x2c, 0x01, 0x00, 0x1d, 0x00, 0x07,
        0x01, 0xff, 0x00, 0x00, 0x00, 0xf8, 0xff, 0x09, 0x00, 0x27, 0x00, 0x08, 0x21, 0x1c, 0x00, 0x00,
        0x00, 0x00, 0x1d, 0x05, 0x00, 0x00, 0x00, 0x2c, 0x53, 0x3f, 0x00, 0x01, 0x00, 0x00, 0xe6, 0xff,
        0xff, 0xff, 0x6a, 0x2b, 0xee, 0xe6, 0x6a, 0x2b, 0xee, 0x2b, 0xee, 0xee, 0x2b, 0xee, 0x00
    ];

    #[test]
    fn block_open_quick() {
        let mut stream = zlib::z_stream {
            next_in: std::ptr::null_mut(),
            avail_in: 0,
            total_in: 0,
            next_out: std::ptr::null_mut(),
            avail_out: 0,
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

        const MAX_WBITS: i32 = 15; // 32kb LZ77 window

        let err = unsafe {
            deflateInit2_(
                &mut stream,
                1,
                Z_DEFLATED,
                -MAX_WBITS,
                1,
                Z_FILTERED,
                VERSION,
                STREAM_SIZE,
            )
        };

        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        stream.next_in = &BLOCK_OPEN_INPUT as *const u8 as *mut u8;
        let mut next_out = [0u8; 1116];
        stream.next_out = next_out.as_mut_ptr();

        stream.avail_in = BLOCK_OPEN_INPUT.len() as _;
        loop {
            let written = stream.next_out as usize - next_out.as_mut_ptr() as usize;
            stream.avail_out = (next_out.len() - written) as _;

            if stream.avail_out > 38 {
                stream.avail_out = 38;
            }

            let err = unsafe { deflate(&mut stream, Flush::Finish as i32) };
            if ReturnCode::from(err) == ReturnCode::StreamEnd {
                break;
            }

            assert_eq!(ReturnCode::from(err), ReturnCode::Ok);
        }

        let compressed_size = stream.next_out as usize - next_out.as_mut_ptr() as usize;

        let err = unsafe { deflateEnd(&mut stream) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let mut stream = zlib::z_stream {
            next_in: std::ptr::null_mut(),
            avail_in: 0,
            total_in: 0,
            next_out: std::ptr::null_mut(),
            avail_out: 0,
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

        let err = unsafe { inflateInit2_(&mut stream, -MAX_WBITS, VERSION, STREAM_SIZE) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        stream.next_in = next_out.as_mut_ptr();
        stream.avail_in = compressed_size as _;

        let mut uncompressed = [0u8; BLOCK_OPEN_INPUT.len()];
        stream.next_out = uncompressed.as_mut_ptr();
        stream.avail_out = uncompressed.len() as _;

        let err = unsafe { inflate(&mut stream, Z_NO_FLUSH) };
        assert_eq!(ReturnCode::from(err), ReturnCode::StreamEnd);

        let err = unsafe { inflateEnd(&mut stream) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        assert_eq!(uncompressed, BLOCK_OPEN_INPUT);
    }

    #[test]
    fn block_open_fast() {
        let mut stream = zlib::z_stream {
            next_in: std::ptr::null_mut(),
            avail_in: 0,
            total_in: 0,
            next_out: std::ptr::null_mut(),
            avail_out: 0,
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

        const MAX_WBITS: i32 = 15; // 32kb LZ77 window

        let err = unsafe {
            deflateInit2_(
                &mut stream,
                2, // fast
                Z_DEFLATED,
                -MAX_WBITS,
                1,
                Z_FILTERED,
                VERSION,
                STREAM_SIZE,
            )
        };

        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        stream.next_in = &BLOCK_OPEN_INPUT as *const u8 as *mut u8;
        let mut next_out = [0u8; 1116];
        stream.next_out = next_out.as_mut_ptr();

        stream.avail_in = BLOCK_OPEN_INPUT.len() as _;
        loop {
            let written = stream.next_out as usize - next_out.as_mut_ptr() as usize;
            stream.avail_out = (next_out.len() - written) as _;

            if stream.avail_out > 38 {
                stream.avail_out = 38;
            }

            let err = unsafe { deflate(&mut stream, Flush::Finish as i32) };
            if ReturnCode::from(err) == ReturnCode::StreamEnd {
                break;
            }

            assert_eq!(ReturnCode::from(err), ReturnCode::Ok);
        }

        let compressed_size = stream.next_out as usize - next_out.as_mut_ptr() as usize;

        let err = unsafe { deflateEnd(&mut stream) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let mut stream = zlib::z_stream {
            next_in: std::ptr::null_mut(),
            avail_in: 0,
            total_in: 0,
            next_out: std::ptr::null_mut(),
            avail_out: 0,
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

        let err = unsafe { inflateInit2_(&mut stream, -MAX_WBITS, VERSION, STREAM_SIZE) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        stream.next_in = next_out.as_mut_ptr();
        stream.avail_in = compressed_size as _;

        let mut uncompressed = [0u8; BLOCK_OPEN_INPUT.len()];
        stream.next_out = uncompressed.as_mut_ptr();
        stream.avail_out = uncompressed.len() as _;

        let err = unsafe { inflate(&mut stream, Z_NO_FLUSH) };
        assert_eq!(ReturnCode::from(err), ReturnCode::StreamEnd);

        let err = unsafe { inflateEnd(&mut stream) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        assert_eq!(uncompressed, BLOCK_OPEN_INPUT);
    }

    #[test]
    fn block_open_slow() {
        let mut stream = zlib::z_stream {
            next_in: std::ptr::null_mut(),
            avail_in: 0,
            total_in: 0,
            next_out: std::ptr::null_mut(),
            avail_out: 0,
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

        const MAX_WBITS: i32 = 15; // 32kb LZ77 window

        let err = unsafe {
            deflateInit2_(
                &mut stream,
                9, // fast
                Z_DEFLATED,
                -MAX_WBITS,
                1,
                Z_FILTERED,
                VERSION,
                STREAM_SIZE,
            )
        };

        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        stream.next_in = &BLOCK_OPEN_INPUT as *const u8 as *mut u8;
        let mut next_out = [0u8; 1116];
        stream.next_out = next_out.as_mut_ptr();

        stream.avail_in = BLOCK_OPEN_INPUT.len() as _;
        loop {
            let written = stream.next_out as usize - next_out.as_mut_ptr() as usize;
            stream.avail_out = (next_out.len() - written) as _;

            if stream.avail_out > 38 {
                stream.avail_out = 38;
            }

            let err = unsafe { deflate(&mut stream, Flush::Finish as i32) };
            if ReturnCode::from(err) == ReturnCode::StreamEnd {
                break;
            }

            assert_eq!(ReturnCode::from(err), ReturnCode::Ok);
        }

        let compressed_size = stream.next_out as usize - next_out.as_mut_ptr() as usize;

        let err = unsafe { deflateEnd(&mut stream) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let mut stream = zlib::z_stream {
            next_in: std::ptr::null_mut(),
            avail_in: 0,
            total_in: 0,
            next_out: std::ptr::null_mut(),
            avail_out: 0,
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

        let err = unsafe { inflateInit2_(&mut stream, -MAX_WBITS, VERSION, STREAM_SIZE) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        stream.next_in = next_out.as_mut_ptr();
        stream.avail_in = compressed_size as _;

        let mut uncompressed = [0u8; BLOCK_OPEN_INPUT.len()];
        stream.next_out = uncompressed.as_mut_ptr();
        stream.avail_out = uncompressed.len() as _;

        let err = unsafe { inflate(&mut stream, Z_NO_FLUSH) };
        assert_eq!(ReturnCode::from(err), ReturnCode::StreamEnd);

        let err = unsafe { inflateEnd(&mut stream) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        assert_eq!(uncompressed, BLOCK_OPEN_INPUT);
    }
}

#[test]
fn deflate_medium_fizzle_bug() {
    const EXPECTED: &[u8] = &[
        120, 156, 99, 96, 128, 3, 73, 6, 26, 3, 71, 218, 2, 28, 182, 214, 17, 225, 50, 85, 100, 30,
        0, 132, 7, 24, 220,
    ];

    const INPUT: &str = "\0\0\0\0\0\0\0\0\0\0\0\u{19}\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0~\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0%\0\0\0\0\0\0\0\0\0\0\0\0";

    let mut output = [0; EXPECTED.len()];
    let mut dest_len = output.len();

    let err = zlib::compress_rs(&mut output, &mut dest_len, INPUT.as_bytes(), 6);
    assert_eq!(err, ReturnCode::Ok);

    assert_eq!(&output[..dest_len], EXPECTED);
}

#[test]
fn test_deflate_concurrency() {
    static mut BUF: [u8; 8 * 1024] = [0; 8 * 1024];

    let zbuf: &mut [u8] = &mut [0; 4 * 1024];
    let tmp: &mut [u8] = &mut [0; 8 * 1024];

    const PAUSED: u8 = 0;
    const RUNNING: u8 = 1;
    const STOPPED: u8 = 2;

    static STATE: AtomicU8 = AtomicU8::new(PAUSED);
    static TARGET_STATE: AtomicU8 = AtomicU8::new(PAUSED);

    std::thread::spawn(|| loop {
        STATE.store(TARGET_STATE.load(Ordering::Relaxed), Ordering::Relaxed);

        match STATE.load(Ordering::Relaxed) {
            PAUSED => continue,
            STOPPED => break,
            _ => unsafe {
                for i in 0..BUF.len() {
                    let ptr = BUF.as_mut_ptr().add(i);
                    *ptr = *ptr + 1;
                }
            },
        }
    });

    fn transition(target_state: u8) {
        TARGET_STATE.store(target_state, Ordering::Relaxed);

        while STATE.load(Ordering::Relaxed) != target_state {
            std::hint::spin_loop()
        }
    }

    unsafe {
        let mut dstrm = zlib::z_stream::default();

        let err = deflateInit2(
            &mut dstrm,
            Z_BEST_SPEED,
            Z_DEFLATED,
            -15,
            8,
            Z_DEFAULT_STRATEGY,
        );
        assert_eq!(Z_OK, err, "{:?}", CStr::from_ptr(dstrm.msg));

        let mut istrm = zlib::z_stream::default();
        let err = inflateInit2(&mut istrm, -15);
        assert_eq!(Z_OK, err, "{:?}", CStr::from_ptr(istrm.msg));

        // Iterate for a certain amount of time.
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(1);
        while std::time::Instant::now() < deadline {
            /* Start each iteration with a fresh stream state. */
            let err = deflateReset(&mut dstrm);
            assert_eq!(Z_OK, err, "{:?}", CStr::from_ptr(dstrm.msg));

            let err = inflateReset(&mut istrm);
            assert_eq!(Z_OK, err, "{:?}", CStr::from_ptr(istrm.msg));

            // Mutate and compress the first half of buf concurrently.
            // Decompress and throw away the results, which are unpredictable.
            transition(RUNNING);
            dstrm.next_in = BUF.as_mut_ptr();
            dstrm.avail_in = (BUF.len() / 2) as _;
            while dstrm.avail_in > 0 {
                dstrm.next_out = zbuf.as_mut_ptr();
                dstrm.avail_out = zbuf.len() as _;
                let err = deflate(&mut dstrm, Z_NO_FLUSH);
                assert_eq!(Z_OK, err, "{:?}", CStr::from_ptr(dstrm.msg));
                istrm.next_in = zbuf.as_mut_ptr();
                istrm.avail_in = (zbuf.len() - dstrm.avail_out as usize) as _;
                while istrm.avail_in > 0 {
                    istrm.next_out = tmp.as_mut_ptr();
                    istrm.avail_out = tmp.len() as _;
                    let err = inflate(&mut istrm, Z_NO_FLUSH);
                    assert_eq!(Z_OK, err, "{:?}", CStr::from_ptr(istrm.msg));
                }
            }

            // Stop mutation and compress the second half of buf.
            // Decompress and check that the result matches.
            transition(PAUSED);
            dstrm.next_in = BUF.as_mut_ptr().add(BUF.len() / 2);
            dstrm.avail_in = (BUF.len() - BUF.len() / 2) as _;
            while dstrm.avail_in > 0 {
                dstrm.next_out = zbuf.as_mut_ptr();
                dstrm.avail_out = zbuf.len() as _;
                dbg!(dstrm.avail_in);
                let err = deflate(&mut dstrm, Z_FINISH);
                if err == Z_STREAM_END {
                    assert_eq!(0, dstrm.avail_in);
                } else {
                    assert_eq!(Z_OK, err, "{:?}", CStr::from_ptr(dstrm.msg));
                }
                istrm.next_in = zbuf.as_mut_ptr();
                istrm.avail_in = (zbuf.len() - dstrm.avail_out as usize) as _;
                while istrm.avail_in > 0 {
                    let orig_total_out = istrm.total_out as usize;
                    istrm.next_out = tmp.as_mut_ptr();
                    istrm.avail_out = tmp.len() as _;
                    let err = inflate(&mut istrm, Z_NO_FLUSH);
                    if err == Z_STREAM_END {
                        assert_eq!(0, istrm.avail_in);
                    } else {
                        assert_eq!(Z_OK, err, "{:?}", CStr::from_ptr(dstrm.msg));
                    }
                    let concurrent_size = BUF.len() - BUF.len() / 2;
                    if istrm.total_out as usize > concurrent_size {
                        let (tmp_offset, buf_offset, size) = if orig_total_out >= concurrent_size {
                            (
                                0,
                                orig_total_out - concurrent_size,
                                istrm.total_out as usize - orig_total_out,
                            )
                        } else {
                            (
                                concurrent_size - orig_total_out,
                                0,
                                istrm.total_out as usize - concurrent_size,
                            )
                        };

                        let a = &tmp[tmp_offset as usize..][..size];
                        let b = &BUF[BUF.len() / 2 + buf_offset..][..size];

                        assert_eq!(a, b);
                    }
                }
            }
        }
    }
}
