use std::{ffi::CString, mem::MaybeUninit};

use crate as libz_rs_sys;

use libc::{c_char, c_int};

use libz_rs_sys::{
    deflate, deflateEnd, deflateInit2_, inflate, inflateEnd, inflateInit2_, Z_DEFLATED, Z_FILTERED,
    Z_NO_FLUSH,
};
use zlib_rs::{
    deflate::{DeflateConfig, Strategy},
    Flush, ReturnCode,
};

const VERSION: *const c_char = "2.3.0\0".as_ptr() as *const c_char;
const STREAM_SIZE: c_int = std::mem::size_of::<libz_rs_sys::z_stream>() as c_int;

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
        let mut stream = libz_rs_sys::z_stream {
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
        let mut stream = libz_rs_sys::z_stream {
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

        let mut stream = libz_rs_sys::z_stream {
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
        let mut stream = libz_rs_sys::z_stream {
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

        let mut stream = libz_rs_sys::z_stream {
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
        let mut stream = libz_rs_sys::z_stream {
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

        let mut stream = libz_rs_sys::z_stream {
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
fn test_deflate_tune() {
    let mut compr = [0; 128];

    let good_length = 3;
    let max_lazy = 5;
    let nice_length = 18;
    let max_chain = 6;

    let mut c_stream = MaybeUninit::zeroed();

    let err = unsafe {
        libz_rs_sys::deflateTune(
            c_stream.as_mut_ptr(),
            good_length,
            max_lazy,
            nice_length,
            max_chain,
        )
    };
    assert_eq!(err, libz_rs_sys::Z_STREAM_ERROR);

    let err = unsafe {
        libz_rs_sys::deflateInit_(
            c_stream.as_mut_ptr(),
            libz_rs_sys::Z_BEST_COMPRESSION,
            VERSION,
            core::mem::size_of::<libz_rs_sys::z_stream>() as _,
        )
    };
    assert_eq!(err, libz_rs_sys::Z_OK);

    let c_stream = unsafe { c_stream.assume_init_mut() };

    let err = unsafe {
        libz_rs_sys::deflateTune(c_stream, good_length, max_lazy, nice_length, max_chain)
    };
    assert_eq!(err, libz_rs_sys::Z_OK);

    let input = "Hello, World!\n";

    c_stream.next_in = input.as_ptr() as *mut u8;
    c_stream.next_out = compr.as_mut_ptr();

    while c_stream.total_in as usize != input.len() && (c_stream.total_out as usize) < compr.len() {
        c_stream.avail_in = 1;
        c_stream.avail_out = 1; /* force small buffers */

        let err = unsafe { libz_rs_sys::deflate(c_stream, Z_NO_FLUSH) };
        assert_eq!(err, libz_rs_sys::Z_OK);
    }

    /* Finish the stream, still forcing small buffers: */
    loop {
        c_stream.avail_out = 1;
        let err = unsafe { libz_rs_sys::deflate(c_stream, libz_ng_sys::Z_FINISH) };
        if err == libz_ng_sys::Z_STREAM_END {
            break;
        };
        assert_eq!(err, libz_rs_sys::Z_OK);
    }

    let err = unsafe { libz_rs_sys::deflateEnd(c_stream) };
    assert_eq!(err, libz_rs_sys::Z_OK);
}

#[test]
fn deflate_medium_fizzle_bug() {
    const EXPECTED: &[u8] = &[
        120, 156, 99, 96, 128, 3, 73, 6, 26, 3, 71, 218, 2, 28, 182, 214, 17, 225, 50, 85, 100, 30,
        0, 132, 7, 24, 220,
    ];

    const INPUT: &str = "\0\0\0\0\0\0\0\0\0\0\0\u{19}\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0~\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0%\0\0\0\0\0\0\0\0\0\0\0\0";

    let mut output = [0; EXPECTED.len()];

    let config = DeflateConfig::new(6);
    let (output, err) = zlib_rs::deflate::compress_slice(&mut output, INPUT.as_bytes(), config);
    assert_eq!(err, ReturnCode::Ok);

    assert_eq!(output, EXPECTED);
}

#[test]
fn deflate_bound_correct() {
    ::quickcheck::quickcheck(test as fn(_) -> _);

    fn test((config, source_len): (DeflateConfig, u64)) -> bool {
        let rs_bound = unsafe {
            let mut strm = MaybeUninit::zeroed();

            // first validate the config
            let err = libz_rs_sys::deflateInit2_(
                strm.as_mut_ptr(),
                config.level,
                config.method as i32,
                config.window_bits,
                config.mem_level,
                config.strategy as i32,
                VERSION,
                STREAM_SIZE,
            );

            if err != 0 {
                return true;
            }

            libz_rs_sys::deflateBound(strm.as_mut_ptr(), source_len)
        };

        let ng_bound = unsafe {
            let mut strm = MaybeUninit::zeroed();

            // first validate the config
            let err = libz_ng_sys::deflateInit2_(
                strm.as_mut_ptr(),
                config.level,
                config.method as i32,
                config.window_bits,
                config.mem_level,
                config.strategy as i32,
                VERSION,
                core::mem::size_of::<libz_ng_sys::z_stream>() as _,
            );

            assert_eq!(err, 0);

            libz_ng_sys::deflateBound(strm.as_mut_ptr(), source_len)
        };

        rs_bound == ng_bound
    }
}

fn deflate_bound_gzip_header_help(
    (config, source_len, extra, name, comment): (DeflateConfig, u64, CString, CString, CString),
) -> bool {
    let rs_bound = unsafe {
        let mut strm = MaybeUninit::zeroed();

        // first validate the config
        let err = libz_rs_sys::deflateInit2_(
            strm.as_mut_ptr(),
            config.level,
            config.method as i32,
            config.window_bits,
            config.mem_level,
            config.strategy as i32,
            VERSION,
            STREAM_SIZE,
        );

        if err != 0 {
            return true;
        }

        let mut header = libz_rs_sys::gz_header {
            text: 0,
            time: 0,
            xflags: 0,
            os: 0,
            extra: extra.as_ptr() as *mut _,
            extra_len: extra.as_bytes().len() as _,
            extra_max: 0,
            name: name.as_ptr() as *mut _,
            name_max: 0,
            comment: comment.as_ptr() as *mut _,
            comm_max: 0,
            hcrc: 1,
            done: 0,
        };

        // this may fail if the config is not set up for gzip
        let _err = libz_rs_sys::deflateSetHeader(strm.as_mut_ptr(), &mut header);

        libz_rs_sys::deflateBound(strm.as_mut_ptr(), source_len)
    };

    let ng_bound = unsafe {
        let mut strm = MaybeUninit::zeroed();

        // first validate the config
        let err = libz_ng_sys::deflateInit2_(
            strm.as_mut_ptr(),
            config.level,
            config.method as i32,
            config.window_bits,
            config.mem_level,
            config.strategy as i32,
            VERSION,
            core::mem::size_of::<libz_ng_sys::z_stream>() as _,
        );

        assert_eq!(err, 0);

        let mut header = libz_ng_sys::gz_header {
            text: 0,
            time: 0,
            xflags: 0,
            os: 0,
            extra: extra.as_ptr() as *mut _,
            extra_len: extra.as_bytes().len() as _,
            extra_max: 0,
            name: name.as_ptr() as *mut _,
            name_max: 0,
            comment: comment.as_ptr() as *mut _,
            comm_max: 0,
            hcrc: 1,
            done: 0,
        };

        // this may fail if the config is not set up for gzip
        let _err = libz_ng_sys::deflateSetHeader(strm.as_mut_ptr(), &mut header);

        libz_ng_sys::deflateBound(strm.as_mut_ptr(), source_len)
    };

    assert_eq!(rs_bound, ng_bound);

    rs_bound == ng_bound
}

#[test]
fn deflate_bound_gzip_header() {
    ::quickcheck::quickcheck(deflate_bound_gzip_header_help as fn(_) -> _);
}

#[test]
fn test_compress_bound() {
    ::quickcheck::quickcheck(test as fn(_) -> _);

    fn test(source_len: usize) -> bool {
        let rs_bound = libz_rs_sys::compressBound(source_len as _);
        let ng_bound = unsafe { libz_ng_sys::compressBound(source_len) };

        assert_eq!(rs_bound, ng_bound as _);

        rs_bound == ng_bound as _
    }
}

#[test]
fn test_compress_param() {
    let mut output_rs = [0; 1024];
    let mut output_ng = [0; 1024];

    let config = DeflateConfig::new(2);

    let input =
        "Scheduling and executing async tasks is a job handled by an async runtime, such as\0";

    let n_rs = unsafe {
        let mut strm = MaybeUninit::zeroed();

        // first validate the config
        let err = libz_rs_sys::deflateInit2_(
            strm.as_mut_ptr(),
            config.level,
            config.method as i32,
            config.window_bits,
            config.mem_level,
            config.strategy as i32,
            VERSION,
            STREAM_SIZE,
        );
        assert_eq!(err, 0);

        let stream = strm.assume_init_mut();

        stream.next_out = output_rs.as_mut_ptr();
        stream.avail_out = output_rs.len() as _;

        let offset = input.len() / 2;

        stream.next_in = input.as_ptr() as *mut u8;
        stream.avail_in = offset as _;

        let err = libz_rs_sys::deflate(stream, Flush::NoFlush as i32);
        assert_eq!(err, 0);

        let err = libz_rs_sys::deflateParams(stream, 8, Strategy::Rle as i32);
        assert_eq!(err, 0);

        assert_eq!(stream.next_in as usize - input.as_ptr() as usize, offset);

        stream.avail_in = (input.len() - offset) as _;

        let err = libz_rs_sys::deflate(stream, Flush::Finish as i32);
        assert_eq!(err, ReturnCode::StreamEnd as i32);

        let err = libz_rs_sys::deflateEnd(stream);
        assert_eq!(err, 0);

        stream.total_out as usize
    };

    let n_ng = unsafe {
        let mut strm = MaybeUninit::zeroed();

        let err = libz_ng_sys::deflateParams(strm.as_mut_ptr(), 8, Strategy::Rle as i32);
        assert_eq!(err, ReturnCode::StreamError as i32);

        // first validate the config
        let err = libz_ng_sys::deflateInit2_(
            strm.as_mut_ptr(),
            config.level,
            config.method as i32,
            config.window_bits,
            config.mem_level,
            config.strategy as i32,
            VERSION,
            STREAM_SIZE,
        );
        assert_eq!(err, 0);

        let err = libz_ng_sys::deflateParams(strm.as_mut_ptr(), -1, 100);
        assert_eq!(err, ReturnCode::StreamError as i32);

        let err = libz_ng_sys::deflateParams(strm.as_mut_ptr(), 100, Strategy::Rle as i32);
        assert_eq!(err, ReturnCode::StreamError as i32);

        let stream = strm.assume_init_mut();

        stream.next_out = output_ng.as_mut_ptr();
        stream.avail_out = output_ng.len() as _;

        let offset = input.len() / 2;

        stream.next_in = input.as_ptr() as *mut u8;
        stream.avail_in = offset as _;

        let err = libz_ng_sys::deflate(stream, Flush::NoFlush as i32);
        assert_eq!(err, 0);

        let err = libz_ng_sys::deflateParams(stream, 8, Strategy::Rle as i32);
        assert_eq!(err, 0);

        assert_eq!(stream.next_in as usize - input.as_ptr() as usize, offset);

        stream.avail_in = (input.len() - offset) as _;

        let err = libz_ng_sys::deflate(stream, Flush::Finish as i32);
        assert_eq!(err, ReturnCode::StreamEnd as i32);

        let err = libz_ng_sys::deflateEnd(stream);
        assert_eq!(err, 0);

        stream.total_out
    };

    assert_eq!(&output_rs[..n_rs], &output_ng[..n_ng]);
}
