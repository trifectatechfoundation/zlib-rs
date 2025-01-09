use std::{ffi::CString, mem::MaybeUninit};

// we use the libz_sys but configure zlib-ng in zlib compat mode
use libz_sys as libz_ng_sys;

use core::ffi::{c_char, c_int, c_ulong, CStr};

use libz_rs_sys::{
    deflate, deflateEnd, deflateInit2_, inflate, inflateEnd, inflateInit2_, Z_DEFLATED, Z_FILTERED,
    Z_NO_FLUSH,
};
use zlib_rs::{
    c_api::Z_BEST_COMPRESSION,
    deflate::{DeflateConfig, Method, Strategy},
    inflate::InflateConfig,
    DeflateFlush, ReturnCode,
};

use crate::assert_eq_rs_ng;

const VERSION: *const c_char = libz_rs_sys::zlibVersion();
const STREAM_SIZE: c_int = core::mem::size_of::<libz_rs_sys::z_stream>() as c_int;

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

        let err = unsafe { deflate(&mut stream, DeflateFlush::Finish as i32) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        stream.avail_in = 0;
        stream.avail_out = 498;
        let err = unsafe { deflate(&mut stream, DeflateFlush::Finish as i32) };
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
        let next_out_start = next_out.as_ptr() as usize;
        stream.next_out = next_out.as_mut_ptr();

        stream.avail_in = BLOCK_OPEN_INPUT.len() as _;
        loop {
            let written = stream.next_out as usize - next_out_start;
            stream.avail_out = (next_out.len() - written) as _;

            if stream.avail_out > 38 {
                stream.avail_out = 38;
            }

            let err = unsafe { deflate(&mut stream, DeflateFlush::Finish as i32) };
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
        let next_out_start = next_out.as_ptr() as usize;
        stream.next_out = next_out.as_mut_ptr();

        stream.avail_in = BLOCK_OPEN_INPUT.len() as _;
        loop {
            let written = stream.next_out as usize - next_out_start;
            stream.avail_out = (next_out.len() - written) as _;

            if stream.avail_out > 38 {
                stream.avail_out = 38;
            }

            let err = unsafe { deflate(&mut stream, DeflateFlush::Finish as i32) };
            if ReturnCode::from(err) == ReturnCode::StreamEnd {
                break;
            }

            assert_eq!(ReturnCode::from(err), ReturnCode::Ok);
        }

        let compressed_size = stream.next_out as usize - next_out_start;

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
        let next_out_start = next_out.as_ptr() as usize;
        stream.next_out = next_out.as_mut_ptr();

        stream.avail_in = BLOCK_OPEN_INPUT.len() as _;
        loop {
            let written = stream.next_out as usize - next_out_start;
            stream.avail_out = (next_out.len() - written) as _;

            if stream.avail_out > 38 {
                stream.avail_out = 38;
            }

            let err = unsafe { deflate(&mut stream, DeflateFlush::Finish as i32) };
            if ReturnCode::from(err) == ReturnCode::StreamEnd {
                break;
            }

            assert_eq!(ReturnCode::from(err), ReturnCode::Ok);
        }

        let compressed_size = stream.next_out as usize - next_out_start;

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

fn assert_deflate_bound_correct((config, source_len): (DeflateConfig, c_ulong)) {
    assert_eq_rs_ng!({
        let mut strm = MaybeUninit::zeroed();

        // first validate the config
        let err = deflateInit2_(
            strm.as_mut_ptr(),
            config.level,
            config.method as i32,
            config.window_bits,
            config.mem_level,
            config.strategy as i32,
            VERSION,
            STREAM_SIZE,
        );

        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let bound = deflateBound(strm.as_mut_ptr(), source_len);

        let ret = unsafe { deflateEnd(strm.as_mut_ptr()) };
        assert_eq!(ret, Z_OK);

        bound
    });
}

#[test]
#[cfg_attr(miri, ignore = "slow")]
fn deflate_bound_correct() {
    ::quickcheck::quickcheck(test as fn(_) -> _);

    fn test(input: (DeflateConfig, c_ulong)) -> bool {
        assert_deflate_bound_correct(input);

        true
    }
}

#[test]
fn deflate_bound_correct_basic() {
    let config = DeflateConfig::default();

    assert_deflate_bound_correct((config, 42));
    assert_deflate_bound_correct((config, 123456));
    assert_deflate_bound_correct((config, 1 << 24));
}

#[test]
fn deflate_bound_correct_windows() {
    // on windows, c_ulong is just 32 bits wide. That leads to rounding that is different to what
    // we'd get when using usize in rust

    let config = DeflateConfig {
        level: 9,
        method: Method::Deflated,
        window_bits: -13,
        mem_level: 5,
        strategy: Strategy::Filtered,
    };

    // this value is dangerously close to u32::MAX, and the calculation will run into overflow
    //    u32::MAX = 4294967296
    let source_len = 4294967233;

    assert_deflate_bound_correct((config, source_len));

    let config = DeflateConfig {
        level: 0,
        method: Method::Deflated,
        window_bits: 15,
        mem_level: 5,
        strategy: Strategy::HuffmanOnly,
    };
    // this value is dangerously close to u32::MAX, and the calculation will run into overflow
    //    u32::MAX = 4294967296
    let source_len = 4294967289;

    assert_deflate_bound_correct((config, source_len));
}

fn deflate_bound_gzip_header_help(
    (config, source_len, extra, name, comment): (DeflateConfig, c_ulong, CString, CString, CString),
) -> bool {
    let extra_len = extra.as_bytes().len();

    let extra = extra.as_ptr().cast_mut().cast::<u8>();
    let name = name.as_ptr().cast_mut().cast::<u8>();
    let comment = comment.as_ptr().cast_mut().cast::<u8>();

    assert_eq_rs_ng!({
        let mut strm = MaybeUninit::zeroed();

        // first validate the config
        let err = deflateInit2_(
            strm.as_mut_ptr(),
            config.level,
            config.method as i32,
            config.window_bits,
            config.mem_level,
            config.strategy as i32,
            zlibVersion(),
            core::mem::size_of::<z_stream>() as _,
        );

        if err != 0 {
            return true;
        }

        let mut header = gz_header {
            text: 0,
            time: 0,
            xflags: 0,
            os: 0,
            extra_len: extra_len as _,
            extra,
            extra_max: 0,
            name,
            name_max: 0,
            comment,
            comm_max: 0,
            hcrc: 1,
            done: 0,
        };

        // this may fail if the config is not set up for gzip
        let _ = deflateSetHeader(strm.as_mut_ptr(), &mut header);

        let bound = deflateBound(strm.as_mut_ptr(), source_len);

        deflateEnd(strm.as_mut_ptr());

        bound
    });

    true
}

#[test]
#[cfg_attr(miri, ignore = "slow")]
fn deflate_bound_gzip_header() {
    ::quickcheck::quickcheck(deflate_bound_gzip_header_help as fn(_) -> _);
}

#[test]
fn deflate_bound_gzip_header_basic() {
    deflate_bound_gzip_header_help((
        DeflateConfig::default(),
        1234,
        CString::from_vec_with_nul("extra\0".as_bytes().to_vec()).unwrap(),
        CString::from_vec_with_nul("name\0".as_bytes().to_vec()).unwrap(),
        CString::from_vec_with_nul("comment\0".as_bytes().to_vec()).unwrap(),
    ));
}

#[test]
#[cfg_attr(
    target_endian = "big",
    ignore = "we don't support DFLTCC, which changes the bounds in zlib-ng"
)]
fn test_compress_bound_windows() {
    let source_len = 4294967289 as core::ffi::c_ulong;

    assert_eq_rs_ng!({ compressBound(source_len as _) });
}

#[test]
#[cfg_attr(
    target_endian = "big",
    ignore = "we don't support DFLTCC, which changes the bounds in zlib-ng"
)]
fn test_compress_bound() {
    ::quickcheck::quickcheck(test as fn(_) -> _);

    fn test(source_len: core::ffi::c_ulong) -> bool {
        assert_eq_rs_ng!({ compressBound(source_len as _) });

        true
    }
}

#[test]
fn test_compress_param() {
    let config = DeflateConfig::new(2);

    let input =
        "Scheduling and executing async tasks is a job handled by an async runtime, such as\0";

    assert_eq_rs_ng!({
        let mut output = [0; 1024];

        let mut strm = MaybeUninit::zeroed();

        // first validate the config
        let err = deflateInit2_(
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

        stream.next_out = output.as_mut_ptr();
        stream.avail_out = output.len() as _;

        let offset = input.len() / 2;

        stream.next_in = input.as_ptr() as *mut u8;
        stream.avail_in = offset as _;

        let err = deflate(stream, DeflateFlush::NoFlush as i32);
        assert_eq!(err, 0);

        let err = deflateParams(stream, 8, Strategy::Rle as i32);
        assert_eq!(err, 0);

        assert_eq!(stream.next_in as usize - input.as_ptr() as usize, offset);

        stream.avail_in = (input.len() - offset) as _;

        let err = deflate(stream, DeflateFlush::Finish as i32);
        assert_eq!(err, ReturnCode::StreamEnd as i32);

        let err = deflateEnd(stream);
        assert_eq!(err, 0);

        output[..stream.total_out as usize].to_vec()
    });
}

#[test]
fn test_dict_deflate() {
    let config = DeflateConfig {
        level: Z_BEST_COMPRESSION,
        ..Default::default()
    };

    const DICTIONARY: &str = "hello";
    const HELLO: &str = "hello, hello!\0";

    assert_eq_rs_ng!({
        let mut strm = MaybeUninit::zeroed();

        // first validate the config
        let err = deflateInit2_(
            strm.as_mut_ptr(),
            config.level,
            config.method as i32,
            config.window_bits,
            config.mem_level,
            config.strategy as i32,
            zlibVersion(),
            core::mem::size_of::<z_stream>() as _,
        );

        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let strm = strm.assume_init_mut();

        let err = deflateSetDictionary(strm, DICTIONARY.as_ptr(), DICTIONARY.len() as _);

        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let dict_id = strm.adler;
        let mut compr = [0; 32];
        strm.next_out = compr.as_mut_ptr();
        strm.avail_out = compr.len() as _;

        strm.next_in = HELLO.as_ptr() as *mut u8;
        strm.avail_in = HELLO.len() as _;

        let err = deflate(strm, DeflateFlush::Finish as i32);
        assert_eq!(ReturnCode::from(err), ReturnCode::StreamEnd);

        let err = deflateEnd(strm);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        (dict_id as c_ulong, compr)
    });
}

#[test]
fn test_deflate_prime() {
    unsafe fn deflate_prime_32(stream: &mut libz_rs_sys::z_stream, value: i32) -> i32 {
        // zlib's deflatePrime() takes at most 16 bits
        let err = libz_rs_sys::deflatePrime(stream, 16, value & 0xffff);
        if err != libz_rs_sys::Z_OK {
            return err;
        }

        libz_rs_sys::deflatePrime(stream, 16, value >> 16)
    }

    let config = DeflateConfig {
        level: -1,
        method: Method::Deflated,
        window_bits: -15, // deflate as raw bytes
        mem_level: 8,
        strategy: Strategy::Default,
    };

    unsafe {
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

        let strm = strm.assume_init_mut();

        /* Gzip magic number */
        use libz_rs_sys::deflatePrime;
        let mut err;
        err = deflatePrime(strm, 16, 0x8b1f);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);
        /* Gzip compression method (deflate) */
        err = deflatePrime(strm, 8, 0x08);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);
        /* Gzip flags (one byte, using two odd bit calls) */
        err = deflatePrime(strm, 3, 0x0);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);
        err = deflatePrime(strm, 5, 0x0);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);
        /* Gzip modified time */
        err = deflate_prime_32(strm, 0);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);
        /* Gzip extra flags */
        err = deflatePrime(strm, 8, 0x0);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);
        /* Gzip operating system */
        err = deflatePrime(strm, 8, 255);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        const HELLO: &str = "hello, hello!\0";

        strm.next_in = HELLO.as_ptr() as *mut u8;
        strm.avail_in = HELLO.len() as _;

        let mut compr = [0xAA; 64];
        strm.next_out = compr.as_mut_ptr();
        strm.avail_out = compr.len() as _;

        err = libz_rs_sys::deflate(strm, DeflateFlush::Finish as i32);
        assert_eq!(err, ReturnCode::StreamEnd as i32);

        dbg!(strm.total_out);

        /* Gzip uncompressed data crc32 */
        let crc = libz_rs_sys::crc32(0, HELLO.as_ptr(), HELLO.len() as _);
        err = deflate_prime_32(strm, crc as _);
        assert_eq!(err, 0);
        /* Gzip uncompressed data length */
        err = deflate_prime_32(strm, HELLO.len() as _);
        assert_eq!(err, 0);

        let total_out = strm.total_out;

        err = libz_rs_sys::deflateEnd(strm);
        assert_eq!(err, 0); // inflate with gzip header

        // now inflate it again
        let inflate_config = InflateConfig {
            window_bits: 15 + 32,
        };

        let mut strm = MaybeUninit::zeroed();

        let err = libz_rs_sys::inflateInit2_(
            strm.as_mut_ptr(),
            inflate_config.window_bits,
            VERSION,
            STREAM_SIZE,
        );
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let strm = strm.assume_init_mut();

        strm.next_in = compr.as_mut_ptr();
        strm.avail_in = total_out as _;

        let mut uncompr = vec![0; 32];
        strm.next_out = uncompr.as_mut_ptr();
        strm.avail_out = uncompr.len() as _;

        // the crc checksum is not actually in the buffer, so the check of the checksum will error
        // out with a BufError because there is insufficient input.
        let err = libz_rs_sys::inflate(strm, DeflateFlush::Finish as i32);
        assert_eq!(ReturnCode::from(err), ReturnCode::BufError);
        assert_eq!(&uncompr[..strm.total_out as usize], HELLO.as_bytes());

        inflateEnd(strm);
    }
}

#[test]
fn small_window() {
    let deflate_config = DeflateConfig {
        level: Z_BEST_COMPRESSION,
        method: Method::Deflated,
        window_bits: -9,
        mem_level: 8,
        strategy: Strategy::Default,
    };

    let inflate_config = InflateConfig {
        window_bits: deflate_config.window_bits,
    };

    let plain: [u8; 128] = std::array::from_fn(|i| i as u8);
    let dictionary1 = vec![b'a'; (1 << 9) - plain.len() / 2];

    assert_eq_rs_ng!({
        let mut strm = MaybeUninit::zeroed();

        // first validate the config
        let err = deflateInit2_(
            strm.as_mut_ptr(),
            deflate_config.level,
            deflate_config.method as i32,
            deflate_config.window_bits,
            deflate_config.mem_level,
            deflate_config.strategy as i32,
            VERSION,
            STREAM_SIZE,
        );
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let strm = strm.assume_init_mut();

        let err = deflateSetDictionary(strm, dictionary1.as_ptr(), dictionary1.len() as _);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let err = deflateSetDictionary(strm, plain.as_ptr(), plain.len() as _);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        strm.next_in = plain.as_ptr() as *mut u8;
        strm.avail_in = plain.len() as _;

        let mut compr = [0; 32];
        strm.next_out = compr.as_mut_ptr();
        strm.avail_out = compr.len() as _;

        let err = deflate(strm, DeflateFlush::Finish as i32);
        assert_eq!(ReturnCode::from(err), ReturnCode::StreamEnd);

        let err = deflateEnd(strm);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        // now inflate it again
        let mut strm = MaybeUninit::zeroed();

        let err = inflateInit2_(
            strm.as_mut_ptr(),
            inflate_config.window_bits,
            VERSION,
            STREAM_SIZE,
        );
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let strm = strm.assume_init_mut();

        let err = inflateSetDictionary(strm, dictionary1.as_ptr(), dictionary1.len() as _);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let err = inflateSetDictionary(strm, plain.as_ptr(), plain.len() as _);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        strm.next_in = compr.as_mut_ptr();
        strm.avail_in = compr.len() as _;

        let mut plain_again = vec![0; plain.len()];
        strm.next_out = plain_again.as_mut_ptr();
        strm.avail_out = plain_again.len() as _;

        let err = inflate(strm, DeflateFlush::NoFlush as i32);
        assert_eq!(ReturnCode::from(err), ReturnCode::StreamEnd);

        let err = inflateEnd(strm);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        compr
    });
}

#[test]
fn test_deflate_pending() {
    const HELLO: &str = "hello, hello!\0";

    let config = DeflateConfig::default();

    let mut strm = MaybeUninit::zeroed();

    unsafe {
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

        let strm = strm.assume_init_mut();

        let mut compr = [0; 32];
        strm.next_in = HELLO.as_ptr() as *mut u8;
        strm.next_out = compr.as_mut_ptr();

        let it_in = HELLO.as_bytes().chunks(1);
        let mut it_out = compr.chunks(1);

        for (_, _) in it_in.zip(&mut it_out) {
            strm.avail_in = 1;
            strm.avail_out = 1;
            let err = deflate(strm, DeflateFlush::NoFlush as i32);
            assert_eq!(err, 0);
        }

        let mut ped = 0;
        let mut bits = 0;

        let err = libz_rs_sys::deflatePending(strm, std::ptr::null_mut(), std::ptr::null_mut());
        assert_eq!(err, 0);

        let err = libz_rs_sys::deflatePending(strm, &mut ped, std::ptr::null_mut());
        assert_eq!(err, 0);

        let err = libz_rs_sys::deflatePending(strm, std::ptr::null_mut(), &mut bits);
        assert_eq!(err, 0);

        let err = libz_rs_sys::deflatePending(strm, &mut ped, &mut bits);
        assert_eq!(err, 0);

        assert!(bits >= 0);
        assert!(bits <= 7);

        /* Finish the stream, still forcing small buffers: */
        for _ in it_out {
            strm.avail_out = 1;
            let err = deflate(strm, libz_rs_sys::Z_FINISH);
            if err == libz_rs_sys::Z_STREAM_END {
                break;
            };
            assert_eq!(err, 0);
        }

        let err = deflateEnd(strm);
        assert_eq!(err, 0);
    }
}

/// test deflate() with DeflateFlush::Full
#[test]
fn test_flush() {
    let config = DeflateConfig::default();

    const HELLO: &str = "hello, hello!\0";
    let mut compr = [0; 32];

    unsafe {
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
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let stream = strm.assume_init_mut();

        stream.next_in = HELLO.as_ptr() as *mut u8;
        stream.next_out = compr.as_mut_ptr();

        stream.avail_in = 3;
        stream.avail_out = compr.len() as _;

        let err = libz_rs_sys::deflate(stream, DeflateFlush::FullFlush as i32);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        // force an error in the first compressed block
        compr[3] += 1;
        stream.avail_in = (HELLO.len() - 3) as _;

        let err = libz_rs_sys::deflate(stream, DeflateFlush::Finish as i32);
        assert_eq!(ReturnCode::from(err), ReturnCode::StreamEnd);

        let err = libz_rs_sys::deflateEnd(stream);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);
    }

    test_sync(&compr)
}

// test inflateSync()
fn test_sync(compr: &[u8]) {
    let mut uncompr = [0xAA; 32];

    let mut stream = MaybeUninit::zeroed();

    let config = InflateConfig::default();

    unsafe {
        let err = libz_rs_sys::inflateInit2_(
            stream.as_mut_ptr(),
            config.window_bits,
            VERSION,
            STREAM_SIZE,
        );
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let stream = stream.assume_init_mut();

        stream.next_in = compr.as_ptr() as *mut u8;
        stream.avail_in = 2; // read just the zlib header

        stream.next_out = uncompr.as_mut_ptr();
        stream.avail_out = uncompr.len() as _;

        let err = libz_rs_sys::inflate(stream, DeflateFlush::NoFlush as i32);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        stream.avail_in = (compr.len() - 2) as _; // read all compressed data
        let err = libz_rs_sys::inflateSync(stream); // but skip the damaged part (at index 3)
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let err = libz_rs_sys::inflate(stream, DeflateFlush::Finish as i32);
        assert_eq!(ReturnCode::from(err), ReturnCode::StreamEnd);

        let err = libz_rs_sys::inflateEnd(stream);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);
    }
}

#[test]
fn test_deflate_hash_head_0() {
    use libz_rs_sys::{
        deflate, deflateEnd, deflateInit2_, deflateParams, Z_DEFLATED, Z_FINISH, Z_FULL_FLUSH,
        Z_HUFFMAN_ONLY, Z_SYNC_FLUSH,
    };

    const MAX_WBITS: i32 = 15;

    let mut strm = MaybeUninit::zeroed();
    let mut err;

    err = unsafe {
        deflateInit2_(
            strm.as_mut_ptr(),
            1,
            Z_DEFLATED,
            -15,
            4,
            Z_HUFFMAN_ONLY,
            VERSION,
            STREAM_SIZE,
        )
    };
    assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

    let strm = unsafe { strm.assume_init_mut() };

    let mut next_in = [0x30; 9698];

    next_in[8193] = 0x00;
    next_in[8194] = 0x00;
    next_in[8195] = 0x00;
    next_in[8199] = 0x8a;

    strm.next_in = next_in.as_mut_ptr();
    let mut next_out = [0u8; 21572];
    strm.next_out = next_out.as_mut_ptr();

    strm.avail_in = 0;
    strm.avail_out = 1348;
    err = unsafe { deflateParams(strm, 3, Z_FILTERED) };
    assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

    strm.avail_in = 6728;
    strm.avail_out = 2696;
    err = unsafe { deflate(strm, Z_SYNC_FLUSH) };
    assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

    strm.avail_in = 15;
    strm.avail_out = 1348;
    err = unsafe { deflateParams(strm, 9, Z_FILTERED) };
    assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

    strm.avail_in = 1453;
    strm.avail_out = 1348;
    err = unsafe { deflate(strm, Z_FULL_FLUSH) };

    assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

    strm.avail_in = (next_in.as_mut_ptr() as usize + next_in.len() - strm.next_in as usize) as _;
    strm.avail_out = (next_out.as_ptr() as usize + next_out.len() - strm.next_out as usize) as _;
    err = unsafe { deflate(strm, Z_FINISH) };
    assert_eq!(ReturnCode::from(err), ReturnCode::StreamEnd);

    let compressed_size = strm.next_out as usize - next_out.as_ptr() as usize;

    err = unsafe { deflateEnd(strm) };

    assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

    let mut strm = MaybeUninit::zeroed();

    err = unsafe { inflateInit2_(strm.as_mut_ptr(), -MAX_WBITS, VERSION, STREAM_SIZE) };
    assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

    let strm = unsafe { strm.assume_init_mut() };

    strm.next_in = next_out.as_mut_ptr();
    strm.avail_in = compressed_size as _;
    let mut uncompressed = [0; 9698];
    assert_eq!(next_in.len(), uncompressed.len());

    strm.next_out = uncompressed.as_mut_ptr();
    strm.avail_out = uncompressed.len() as _;

    err = unsafe { inflate(strm, Z_NO_FLUSH) };
    assert_eq!(ReturnCode::from(err), ReturnCode::StreamEnd);

    err = unsafe { inflateEnd(strm) };
    assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

    assert_eq!(uncompressed, next_in);
}

#[test]
fn test_deflate_copy() {
    const HELLO: &str = "hello, hello!\0";

    let config = DeflateConfig::default();

    let mut strm = MaybeUninit::zeroed();

    unsafe {
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

        let strm = strm.assume_init_mut();

        let mut compr = [0; 32];
        strm.next_in = HELLO.as_ptr() as *mut u8;
        strm.next_out = compr.as_mut_ptr();

        for _ in HELLO.as_bytes() {
            strm.avail_in = 1;
            strm.avail_out = 1;

            let err = libz_rs_sys::deflate(strm, DeflateFlush::NoFlush as i32);
            assert_eq!(err, 0);
        }

        loop {
            strm.avail_out = 1;
            let err = libz_rs_sys::deflate(strm, DeflateFlush::Finish as i32);

            if ReturnCode::from(err) == ReturnCode::StreamEnd {
                break;
            }

            assert_eq!(err, 0);
        }

        let mut copy = MaybeUninit::uninit();
        let err = libz_rs_sys::deflateCopy(copy.as_mut_ptr(), strm);
        assert_eq!(err, 0);

        assert_eq!(
            *(strm.state as *const u8),
            *(((*copy.as_mut_ptr()).state) as *const u8),
        );

        let err = libz_rs_sys::deflateEnd(strm);
        assert_eq!(err, 0);

        let err = libz_rs_sys::deflateEnd(copy.as_mut_ptr());
        assert_eq!(err, 0);
    }
}

#[test]
fn version_error() {
    use libz_rs_sys::{deflateInit_, z_stream, zlibVersion, Z_OK, Z_VERSION_ERROR};

    let mut stream = core::mem::MaybeUninit::zeroed();

    let ret = unsafe {
        deflateInit_(
            stream.as_mut_ptr(),
            1,
            zlibVersion(),
            core::mem::size_of::<z_stream>() as i32,
        )
    };
    assert_eq!(ret, Z_OK);

    let ret = unsafe { deflateEnd(stream.as_mut_ptr()) };
    assert_eq!(ret, Z_OK);

    // invalid stream size
    let ret = unsafe { deflateInit_(stream.as_mut_ptr(), 1, zlibVersion(), 1) };
    assert_eq!(ret, Z_VERSION_ERROR);

    // version is null
    let ret = unsafe {
        deflateInit_(
            stream.as_mut_ptr(),
            1,
            core::ptr::null(),
            core::mem::size_of::<z_stream>() as i32,
        )
    };
    assert_eq!(ret, Z_VERSION_ERROR);

    // version is invalid
    let ret = unsafe {
        deflateInit_(
            stream.as_mut_ptr(),
            1,
            b"!\0".as_ptr() as *const c_char,
            core::mem::size_of::<z_stream>() as i32,
        )
    };
    assert_eq!(ret, Z_VERSION_ERROR);

    // version is invalid, deflateInit2_
    let ret = unsafe {
        deflateInit2_(
            stream.as_mut_ptr(),
            1,
            0,
            0,
            0,
            0,
            b"!\0".as_ptr() as *const c_char,
            core::mem::size_of::<z_stream>() as i32,
        )
    };
    assert_eq!(ret, Z_VERSION_ERROR);
}

#[test]
fn gzip_with_header() {
    let extra = "some extra stuff\0";
    let name = "nomen est omen\0";
    let comment = "such comment\0";

    let config = DeflateConfig {
        window_bits: 31,
        ..Default::default()
    };

    let output = assert_eq_rs_ng!({
        let mut stream = MaybeUninit::<z_stream>::zeroed();

        let err = unsafe {
            deflateInit2_(
                stream.as_mut_ptr(),
                config.level,
                config.method as i32,
                config.window_bits,
                config.mem_level,
                config.strategy as i32,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as c_int,
            )
        };
        assert_eq!(err, 0);

        let stream = unsafe { stream.assume_init_mut() };

        let mut header = gz_header {
            text: 0,
            time: 0,
            xflags: 0,
            os: 0,
            extra: extra.as_ptr() as *mut _,
            extra_len: extra.len() as _,
            extra_max: 0,
            name: name.as_ptr() as *mut _,
            name_max: 0,
            comment: comment.as_ptr() as *mut _,
            comm_max: 0,
            hcrc: 1,
            done: 0,
        };

        let err = unsafe { deflateSetHeader(stream, &mut header) };
        assert_eq!(err, 0);

        let input = b"Hello World\n";
        stream.next_in = input.as_ptr() as *mut _;
        stream.avail_in = input.len() as _;

        let mut output = [0u8; 256];
        stream.next_out = output.as_mut_ptr();
        stream.avail_out = output.len() as _;

        let err = unsafe { deflate(stream, DeflateFlush::Finish as _) };
        assert_eq!(err, ReturnCode::StreamEnd as i32);

        let n = stream.total_out as usize;

        let err = unsafe { deflateEnd(stream) };
        assert_eq!(err, 0);

        let output = output[..n].to_vec();

        assert_eq!(output.len(), 81);

        output
    });

    assert_eq_rs_ng!({
        let mut stream = MaybeUninit::<z_stream>::zeroed();

        let err = unsafe {
            inflateInit2_(
                stream.as_mut_ptr(),
                config.window_bits,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as c_int,
            )
        };
        assert_eq!(err, 0);

        let stream = unsafe { stream.assume_init_mut() };

        let mut input = output.clone();
        stream.next_in = input.as_mut_ptr() as _;
        stream.avail_in = input.len() as _;

        let mut output = [0u8; 12];
        stream.next_out = output.as_mut_ptr();
        stream.avail_out = output.len() as _;

        let mut extra_buf = [0u8; 64];
        let mut name_buf = [0u8; 64];
        let mut comment_buf = [0u8; 64];

        let mut header = gz_header {
            text: 0,
            time: 0,
            xflags: 0,
            os: 0,
            extra: extra_buf.as_mut_ptr(),
            extra_len: 0,
            extra_max: extra_buf.len() as _,
            name: name_buf.as_mut_ptr(),
            name_max: name_buf.len() as _,
            comment: comment_buf.as_mut_ptr(),
            comm_max: comment_buf.len() as _,
            hcrc: 0,
            done: 0,
        };

        let err = unsafe { inflateGetHeader(stream, &mut header) };
        assert_eq!(err, 0);

        let err = unsafe { inflate(stream, DeflateFlush::NoFlush as _) };
        assert_eq!(
            err,
            ReturnCode::StreamEnd as i32,
            "{:?}",
            if stream.msg.is_null() {
                None
            } else {
                Some(unsafe { CStr::from_ptr(stream.msg) })
            }
        );

        let err = inflateEnd(stream);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        assert!(!header.comment.is_null());
        assert_eq!(
            unsafe { CStr::from_ptr(header.comment.cast()) }
                .to_str()
                .unwrap(),
            comment.trim_end_matches('\0')
        );

        assert!(!header.name.is_null());
        assert_eq!(
            unsafe { CStr::from_ptr(header.name.cast()) }
                .to_str()
                .unwrap(),
            name.trim_end_matches('\0')
        );

        assert!(!header.extra.is_null());
        assert_eq!(
            unsafe { CStr::from_ptr(header.extra.cast()) }
                .to_str()
                .unwrap(),
            extra.trim_end_matches('\0')
        );
    });
}

mod fuzz_based_tests {
    use crate::helpers::compress_slice_ng;
    use libz_rs_sys::gz_header;
    use zlib_rs::{
        deflate::{compress_slice, DeflateConfig, Method, Strategy},
        ReturnCode,
    };

    fn fuzz_based_test(input: &[u8], config: DeflateConfig, expected: &[u8]) {
        let mut output_rs = [0; 1 << 17];
        let (output_rs, err) = compress_slice(&mut output_rs, input, config);
        assert_eq!(err, ReturnCode::Ok);

        if !cfg!(miri) {
            let mut output_ng = [0; 1 << 17];
            let (output_ng, err) = compress_slice_ng(&mut output_ng, input, config);
            assert_eq!(err, ReturnCode::Ok);

            assert_eq!(output_rs, output_ng);
        }

        if !expected.is_empty() {
            assert_eq!(output_rs, expected);
        }
    }

    #[cfg(not(miri))]
    quickcheck::quickcheck! {
        fn rs_is_ng(bytes: Vec<u8>) -> bool {
            fuzz_based_test(&bytes, DeflateConfig::default(), &[]);

            true
        }
    }

    const PAPER_100K: &[u8] = include_bytes!("test-data/paper-100k.pdf");
    const FIREWORKS: &[u8] = include_bytes!("test-data/fireworks.jpg");
    const LCET10: &str = include_str!("test-data/lcet10.txt");

    #[test]
    #[cfg_attr(miri, ignore = "too slow")]
    fn compress_lcet10() {
        fuzz_based_test(LCET10.as_bytes(), DeflateConfig::default(), &[])
    }

    #[test]
    #[cfg_attr(miri, ignore = "too slow")]
    #[cfg_attr(
        target_family = "wasm",
        ignore = "zlib-ng compresses differently on wasm"
    )]
    fn compress_paper_100k() {
        let mut config = DeflateConfig::default();

        for n in 0..=9 {
            config.level = n;
            fuzz_based_test(PAPER_100K, config, &[])
        }
    }

    #[test]
    #[cfg_attr(miri, ignore = "too slow")]
    fn compress_fireworks() {
        let mut config = DeflateConfig::default();

        for n in 0..=9 {
            config.level = n;
            fuzz_based_test(FIREWORKS, config, &[])
        }
    }

    #[test]
    fn simple_rle() {
        fuzz_based_test(
            "\0\0\0\0\u{6}".as_bytes(),
            DeflateConfig {
                level: -1,
                method: Method::Deflated,
                window_bits: 11,
                mem_level: 4,
                strategy: Strategy::Rle,
            },
            &[56, 17, 99, 0, 2, 54, 0, 0, 11, 0, 7],
        )
    }

    #[test]
    fn fill_window_out_of_bounds() {
        const INPUT: &[u8] = &[
            0x71, 0x71, 0x71, 0x71, 0x71, 0x6a, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71,
            0x71, 0x71, 0x71, 0x71, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x1d, 0x1d, 0x1d, 0x1d, 0x63,
            0x63, 0x63, 0x63, 0x63, 0x1d, 0x1d, 0x1d, 0x1d, 0x1d, 0x1d, 0x1d, 0x1d, 0x1d, 0x1d,
            0x1d, 0x27, 0x0, 0x0, 0x0, 0x1d, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x71, 0x71, 0x71,
            0x71, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x31, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x1d, 0x1d, 0x0, 0x0, 0x0, 0x0, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50,
            0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x48, 0x50,
            0x50, 0x50, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x2c, 0x0, 0x0, 0x0, 0x0, 0x4a,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71,
            0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x70, 0x71, 0x71, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x71, 0x71, 0x71, 0x71, 0x6a, 0x0, 0x0, 0x0, 0x0,
            0x71, 0x0, 0x71, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x31, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71,
            0x71, 0x71, 0x0, 0x4a, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x71, 0x71,
            0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71,
            0x70, 0x71, 0x71, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x71, 0x71, 0x71, 0x71,
            0x6a, 0x0, 0x0, 0x0, 0x0, 0x71, 0x0, 0x71, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x31, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x1d, 0x1d, 0x0, 0x0, 0x0, 0x0,
            0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50,
            0x50, 0x50, 0x50, 0x50, 0x48, 0x50, 0x0, 0x0, 0x71, 0x71, 0x71, 0x71, 0x3b, 0x3f, 0x71,
            0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x50, 0x50, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x2c, 0x0, 0x0, 0x0, 0x0, 0x4a, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x71,
            0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71,
            0x71, 0x70, 0x71, 0x71, 0x0, 0x0, 0x0, 0x6, 0x0, 0x0, 0x0, 0x0, 0x0, 0x71, 0x71, 0x71,
            0x71, 0x71, 0x71, 0x70, 0x71, 0x71, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x71, 0x71, 0x71, 0x71, 0x3b, 0x3f, 0x71, 0x71, 0x71,
            0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71,
            0x71, 0x3b, 0x3f, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x20, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x71, 0x75, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71,
            0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x10, 0x0, 0x71, 0x71,
            0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x3b, 0x71, 0x71, 0x71, 0x71, 0x71,
            0x71, 0x76, 0x71, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x71, 0x71, 0x71, 0x71, 0x71,
            0x71, 0x71, 0x71, 0x10, 0x0, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71,
            0x71, 0x3b, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x76, 0x71, 0x34, 0x34, 0x34, 0x34,
            0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34,
            0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x0, 0x0, 0x0, 0x0, 0x0, 0x34, 0x34, 0x34, 0x34,
            0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34,
            0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34,
            0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34,
            0x34, 0x34, 0x30, 0x34, 0x34, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x71, 0x0, 0x0, 0x0, 0x0, 0x6,
        ];

        fuzz_based_test(
            INPUT,
            DeflateConfig {
                level: -1,
                method: Method::Deflated,
                window_bits: 9,
                mem_level: 1,
                strategy: Strategy::HuffmanOnly,
            },
            &[
                0x18, 0x19, 0x4, 0xc1, 0x21, 0x1, 0xc4, 0x0, 0x10, 0x3, 0xb0, 0x18, 0x29, 0x1e,
                0x7e, 0x17, 0x83, 0xf5, 0x70, 0x6c, 0xac, 0xfe, 0xc9, 0x27, 0xdb, 0xb6, 0x6f, 0xdb,
                0xb6, 0x6d, 0xdb, 0x80, 0x24, 0xb9, 0xbb, 0xbb, 0x24, 0x49, 0x92, 0x24, 0xf, 0x2,
                0xd8, 0x36, 0x0, 0xf0, 0x3, 0x0, 0x0, 0x24, 0xd0, 0xb6, 0x6d, 0xdb, 0xb6, 0x6d,
                0xdb, 0xbe, 0x6d, 0xf9, 0x13, 0x4, 0xc7, 0x4, 0x0, 0x80, 0x30, 0x0, 0xc3, 0x22,
                0x68, 0xf, 0x36, 0x90, 0xc2, 0xb5, 0xfa, 0x7f, 0x48, 0x80, 0x81, 0xb, 0x40, 0x55,
                0x55, 0x55, 0xd5, 0x16, 0x80, 0xaa, 0x7, 0x9, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
                0xe, 0x7c, 0x82, 0xe0, 0x98, 0x0, 0x0, 0x0, 0x4, 0x60, 0x10, 0xf9, 0x8c, 0xe2,
                0xe5, 0xfa, 0x3f, 0x2, 0x54, 0x55, 0x55, 0x65, 0x0, 0xa8, 0xaa, 0xaa, 0xaa, 0xba,
                0x2, 0x50, 0xb5, 0x90, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x78, 0x82, 0xe0, 0xd0,
                0x8a, 0x41, 0x0, 0x0, 0xa2, 0x58, 0x54, 0xb7, 0x60, 0x83, 0x9a, 0x6a, 0x4, 0x96,
                0x87, 0xba, 0x51, 0xf8, 0xfb, 0x9b, 0x26, 0xfc, 0x0, 0x1c, 0x7, 0x6c, 0xdb, 0xb6,
                0x6d, 0xdb, 0xb6, 0x6d, 0xf7, 0xa8, 0x3a, 0xaf, 0xaa, 0x6a, 0x3, 0xf8, 0xc2, 0x3,
                0x40, 0x55, 0x55, 0x55, 0xd5, 0x5b, 0xf8, 0x80, 0xaa, 0x7a, 0xb, 0x0, 0x7f, 0x82,
                0xe0, 0x98, 0x0, 0x40, 0x18, 0x0, 0x82, 0xd8, 0x49, 0x40, 0x2, 0x22, 0x7e, 0xeb,
                0x80, 0xa6, 0xc, 0xa0, 0x9f, 0xa4, 0x2a, 0x38, 0xf, 0x0, 0x0, 0xe7, 0x1, 0xdc,
                0x55, 0x95, 0x17, 0x0, 0x0, 0xae, 0x0, 0x38, 0xc0, 0x67, 0xdb, 0x36, 0x80, 0x2b,
                0x0, 0xe, 0xf0, 0xd9, 0xf6, 0x13, 0x4, 0xc7, 0x4, 0x0, 0x0, 0x30, 0xc, 0x83, 0x22,
                0x69, 0x7, 0xc6, 0xea, 0xff, 0x19, 0x0, 0x0, 0x80, 0xaa, 0x0, 0x0, 0x0, 0x0, 0x0,
                0x0, 0x8e, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0x6a,
                0xf5, 0x63, 0x60, 0x60, 0x3, 0x0, 0xee, 0x8a, 0x88, 0x67,
            ],
        )
    }

    #[test]
    fn read_buf_window_uninitialized() {
        // copies more in `read_buf_window` than is initialized at that point
        const INPUT: &str = include_str!("test-data/read_buf_window_uninitialized.txt");

        fuzz_based_test(
            INPUT.as_bytes(),
            DeflateConfig {
                level: 0,
                method: Method::Deflated,
                window_bits: 10,
                mem_level: 6,
                strategy: Strategy::Default,
            },
            &[],
        )
    }

    #[test]
    fn gzip_no_header() {
        let config = DeflateConfig {
            level: 9,
            method: Method::Deflated,
            window_bits: 31, // gzip
            ..Default::default()
        };

        let input = b"Hello World!";
        let os = gz_header::OS_CODE;

        fuzz_based_test(
            input,
            config,
            &[
                31, 139, 8, 0, 0, 0, 0, 0, 2, os, 243, 72, 205, 201, 201, 87, 8, 207, 47, 202, 73,
                81, 4, 0, 163, 28, 41, 28, 12, 0, 0, 0,
            ],
        )
    }

    #[test]
    #[rustfmt::skip]
    fn gzip_stored_block_checksum() {
        fuzz_based_test(
            &[
                27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 9, 0,
            ],
            DeflateConfig {
                level: 0,
                method: Method::Deflated,
                window_bits: 26,
                mem_level: 6,
                strategy: Strategy::Default,
            },
            &[
                31, 139, 8, 0, 0, 0, 0, 0, 4, gz_header::OS_CODE, 1, 18, 0, 237, 255, 27, 27, 27, 27, 27, 27, 27,
                27, 27, 27, 27, 27, 27, 27, 27, 27, 9, 0, 60, 101, 156, 55, 18, 0, 0, 0,
            ],
        )
    }

    #[test]
    fn hash_calc_difference() {
        // exposed an issue in the crc32 acle hash calc where the incorrect instruction was used.
        // a different hash function changes which match is found first in the hash map, and
        // depending on the algorithm that can give different results. Note that in this case, the
        // output of aarch64 and x86_64 do line up with the current aarch64 hashing approach.

        // historical note: previously we used the crc32 hardware instructions, but later versions
        // of zlib-ng stopped doing that (the dispatch cost was too high), and we followed suit.
        let output = &[
            24, 149, 99, 96, 102, 24, 104, 160, 7, 38, 57, 96, 92, 117, 6, 14, 6, 38, 60, 202, 65,
            14, 86, 99, 208, 3, 3, 6, 67, 6, 38, 22, 58, 56, 17, 10, 40, 119, 41, 84, 175, 26, 0,
            172, 56, 3, 232,
        ];

        fuzz_based_test(
            &[
                0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 46, 0, 0, 0, 0, 0, 8, 0,
                0, 0, 0, 0, 0, 0, 0, 39, 0, 8, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 38, 0, 46, 46, 46, 46, 46, 46, 0,
                49, 0, 2, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                46, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 39, 0, 8, 0, 2, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 38, 0, 46,
                46, 46, 46, 46, 46, 0, 49, 0, 2, 4, 0, 0, 8, 0, 0, 0, 0, 0, 0, 38,
            ],
            DeflateConfig {
                level: -1,
                method: Method::Deflated,
                window_bits: 8,
                mem_level: 2,
                strategy: Strategy::Default,
            },
            output,
        )
    }

    #[test]
    #[cfg_attr(
        target_family = "wasm",
        ignore = "zlib-ng compresses differently on wasm"
    )]
    fn longest_match_difference() {
        // the output on aarch64 and x86_64: fully featured modern targets
        let output_other = &[
            24, 87, 83, 81, 97, 100, 96, 96, 228, 98, 0, 3, 123, 6, 6, 77, 21, 16, 67, 5, 36, 10,
            102, 73, 139, 67, 164, 24, 194, 64, 32, 144, 75, 55, 16, 5, 248, 65, 65, 52, 152, 116,
            99, 100, 96, 96, 16, 98, 96, 151, 241, 243, 243, 11, 12, 52, 128, 41, 2, 153, 206, 6,
            50, 21, 106, 20, 20, 56, 66, 40, 5, 48, 169, 98, 13, 166, 4, 24, 98, 25, 20, 192, 138,
            173, 37, 24, 184, 32, 64, 65, 26, 68, 50, 112, 128, 57, 26, 32, 83, 224, 134, 73, 162,
            154, 8, 7, 14, 40, 60, 78, 12, 121, 38, 12, 17, 6, 6, 176, 15, 144, 4, 0, 125, 74, 22,
            82,
        ];

        // longest_match does not perform unaligned 64-bit reads/writes on i686
        let output_i686 = &[
            24, 87, 83, 81, 97, 100, 96, 96, 228, 98, 0, 3, 123, 6, 6, 77, 21, 16, 67, 5, 36, 10,
            102, 73, 139, 67, 164, 24, 194, 64, 32, 144, 75, 55, 16, 5, 248, 65, 65, 52, 152, 116,
            99, 100, 96, 96, 16, 98, 96, 151, 241, 243, 243, 11, 12, 52, 128, 41, 2, 153, 206, 6,
            50, 21, 106, 20, 20, 56, 66, 40, 5, 48, 169, 98, 13, 166, 4, 24, 98, 25, 20, 192, 138,
            173, 37, 24, 184, 32, 64, 65, 26, 68, 50, 112, 128, 57, 26, 32, 83, 224, 134, 73, 162,
            154, 8, 7, 14, 40, 60, 78, 20, 30, 8, 48, 97, 136, 48, 48, 128, 125, 128, 36, 0, 0,
            125, 74, 22, 82,
        ];

        // longest_match does not perform unaligned 32-bit reads/writes on s390x
        let output_s390x = &[
            24, 87, 83, 81, 97, 100, 96, 96, 228, 98, 0, 3, 123, 6, 6, 77, 21, 16, 67, 5, 36, 10,
            102, 73, 139, 67, 164, 24, 194, 64, 32, 144, 75, 55, 16, 5, 248, 65, 65, 52, 152, 116,
            99, 100, 96, 96, 16, 98, 96, 151, 241, 243, 243, 11, 12, 52, 128, 41, 2, 153, 206, 6,
            50, 21, 106, 20, 20, 56, 66, 40, 5, 48, 169, 98, 13, 166, 4, 24, 98, 25, 20, 192, 138,
            173, 37, 24, 184, 32, 64, 65, 26, 68, 50, 112, 128, 57, 26, 32, 83, 224, 134, 73, 66,
            140, 192, 0, 14, 40, 60, 78, 20, 30, 8, 48, 97, 136, 48, 48, 128, 125, 128, 36, 0, 0,
            125, 74, 22, 82,
        ];

        assert_ne!(output_i686.as_slice(), output_other.as_slice());
        assert_ne!(output_i686.as_slice(), output_s390x.as_slice());

        fuzz_based_test(
            &[
                36, 36, 1, 0, 0, 1, 10, 0, 0, 0, 0, 0, 0, 63, 0, 0, 41, 36, 0, 0, 0, 0, 36, 36, 1,
                0, 0, 36, 0, 0, 0, 0, 27, 23, 0, 0, 0, 0, 0, 0, 0, 86, 86, 86, 86, 86, 81, 10, 45,
                81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 78, 78, 78, 78, 78, 78, 78,
                78, 91, 78, 78, 78, 78, 78, 70, 1, 0, 0, 0, 18, 0, 7, 28, 78, 78, 78, 81, 81, 48,
                81, 81, 81, 81, 81, 81, 81, 81, 10, 0, 0, 0, 6, 0, 0, 0, 36, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 65, 0, 0, 0, 0, 0, 0, 32, 0, 0, 0, 0, 0, 36, 59, 0, 0, 0, 0,
                0, 16, 0, 93, 0, 32, 0, 6, 0, 0, 0, 59, 24, 0, 10, 10, 10, 10, 10, 10, 10, 32, 27,
                10, 10, 10, 10, 0, 8, 10, 10, 10, 10, 10, 40, 0, 36, 0, 0, 6, 0, 0, 0, 36, 0, 0, 0,
                0, 25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 78, 78, 78, 78,
                78, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ],
            DeflateConfig {
                level: 4,
                method: Method::Deflated,
                window_bits: 8,
                mem_level: 6,
                strategy: Strategy::Default,
            },
            if cfg!(target_arch = "x86") {
                output_i686
            } else if cfg!(target_arch = "s390x") {
                output_s390x
            } else {
                output_other
            },
        )
    }

    // a change introduced in [commit].
    // make sure we match the new behavior, and throw an error if the test suite runs with older
    // versions
    //
    // [commit]: https://github.com/zlib-ng/zlib-ng/commit/322753f36e833343ae030e499564691da15eef32
    #[test]
    fn deflate_medium_bypass() {
        let mut input = [0u8; 268];
        input[0] = 0x16;
        input[263] = 0x5;

        fuzz_based_test(
            &input,
            DeflateConfig::default(),
            &[
                120, 156, 19, 99, 24, 5, 12, 12, 12, 172, 32, 18, 0, 24, 45, 0, 28,
            ],
        )
    }
}

#[test]
fn issue_169() {
    const INPUT: &str = include_str!("test-data/issue-169.js");
    let buf = INPUT;

    crate::assert_eq_rs_ng!({
        let mut out = [0u8; 4096];
        let mut ret;

        let mut strm = MaybeUninit::zeroed();

        // first validate the config
        ret = deflateInit2_(
            strm.as_mut_ptr(),
            Z_BEST_SPEED,
            Z_DEFLATED,
            -15,
            8,
            Z_DEFAULT_STRATEGY,
            VERSION,
            STREAM_SIZE,
        );
        assert_eq!(ret, Z_OK);

        let strm = strm.assume_init_mut();

        strm.avail_in = 2048;
        strm.avail_out = 1053;
        strm.next_in = buf.as_ptr() as *mut u8;
        strm.next_out = out.as_mut_ptr();
        ret = deflate(strm, Z_NO_FLUSH);
        assert_eq!(ret, Z_OK);

        assert_eq!(strm.avail_in, 0);
        assert_eq!(strm.avail_out, 1053);
        assert_eq!(strm.total_in, 2048);
        assert_eq!(strm.total_out, 0);

        strm.avail_in = 67;
        ret = deflate(strm, Z_FINISH);
        assert_eq!(ret, Z_OK);

        // libz-sys behavior is sligtly different on CI windows
        #[cfg(not(windows))]
        {
            assert_eq!(strm.avail_in, 67);
            assert_eq!(strm.avail_out, 0);
            assert_eq!(strm.total_in, 2048);
            assert_eq!(strm.total_out, 1053);
        }

        strm.avail_out = (out.len() as core::ffi::c_ulong - strm.total_out) as uInt;
        ret = deflate(strm, Z_FINISH);

        // libz-sys behavior is sligtly different on CI windows
        #[cfg(not(windows))]
        {
            assert_eq!(strm.avail_in, 0);
            assert_eq!(strm.avail_out, 2854);
            assert_eq!(strm.total_in, 2115);
            assert_eq!(strm.total_out, 1242);
        }

        assert_eq!(ret, Z_STREAM_END);
        assert_eq!(strm.avail_in, 0);

        deflateEnd(strm);

        out
    });
}

#[test]
fn flood_pending_buffer() {
    // this test configures a very small mem_level, resulting in a small `Pending` buffer. In
    // combination with large chunks of gzip header data, this exposed a logic error in the
    // `Pending` implementation, that resulted in out-of-bounds writes.

    const NAME_BYTES: [u8; 510] = [
        0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b,
        0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x97, 0x97, 0x97, 0x97, 0x97, 0x97,
        0x97, 0x97, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b,
        0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x97, 0x97, 0x97, 0x97, 0x97, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b,
        0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b,
        0x8b, 0x97, 0x97, 0x97, 0x97, 0x97, 0x97, 0x97, 0x97, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x97, 0x97, 0x97, 0x97, 0x97,
        0x97, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b,
        0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x97, 0x97, 0x97, 0x97,
        0x97, 0x97, 0x97, 0x97, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0x97, 0x97, 0x97, 0x97, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x99, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xbf, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x97, 0x97, 0x97, 0x8b, 0x8b,
        0x8b, 0x8b, 0x8b, 0x97, 0x97, 0x97, 0x97, 0x97, 0x97, 0x97, 0x97, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x97, 0x97, 0x97,
        0x97, 0x97, 0x97, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b,
        0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x8b, 0x97, 0x97,
        0x97, 0x97, 0x97, 0x97, 0x97, 0x97, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    ];

    let name = CString::new(NAME_BYTES).unwrap();

    let comment = CString::new("").unwrap();

    let mut extra = [
        139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 151, 151, 151, 151, 151, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 139, 0, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
        139, 139, 139, 139, 139, 223, 223, 223, 223, 223, 139, 139, 139, 255, 255, 255, 255, 255,
        193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193,
        193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 193, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 151, 151, 151, 151, 151, 151, 151, 39,
    ]
    .to_vec();

    assert_eq_rs_ng!({
        let config = DeflateConfig {
            level: Z_DEFAULT_COMPRESSION,
            method: Method::Deflated,
            window_bits: 31,
            mem_level: 1, // NOTE: this is extremely small
            strategy: Strategy::HuffmanOnly,
        };

        let mut stream = MaybeUninit::zeroed();

        let err = unsafe {
            deflateInit2_(
                stream.as_mut_ptr(),
                config.level,
                config.method as i32,
                config.window_bits,
                config.mem_level,
                config.strategy as i32,
                libz_rs_sys::zlibVersion(),
                core::mem::size_of::<z_stream>() as c_int,
            )
        };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let stream = unsafe { stream.assume_init_mut() };

        let mut header = gz_header {
            text: 0,
            time: 0,
            xflags: 0,
            os: -1953789045,
            extra: extra.as_mut_ptr(),
            extra_len: extra.len().try_into().unwrap(),
            extra_max: 0,                         // doesn't matter for writing.
            name: name.as_ptr() as *mut u8, // hack: UB if written to, but we shouldn't write during deflate.
            name_max: 0,                    // doesn't matter for writing.
            comment: comment.as_ptr() as *mut u8, // hack: UB if written to, but we shouldn't write during deflate.
            comm_max: 0,                          // doesn't matter for wr+iting.
            hcrc: 222,
            done: 0, // doesn't matter for writing.
        };

        let err = unsafe { deflateSetHeader(stream, &mut header as gz_headerp) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let mut source: [u8; 0] = [];

        let bound = unsafe { deflateBound(stream, source.len() as _) };
        let mut dest = vec![0; bound as usize];

        stream.next_in = source.as_mut_ptr().cast();
        stream.avail_in = source.len() as _;
        stream.next_out = dest.as_mut_ptr().cast();
        stream.avail_out = dest.len() as _;

        let err = unsafe { deflate(stream, DeflateFlush::NoFlush as _) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let err = unsafe { deflateEnd(stream) };
        assert_eq!(ReturnCode::from(err), ReturnCode::DataError);
    });
}

#[test]
fn copy_uninitialized_window_section() {
    // caused a panic because an attempt was made to copy uninitialized bytes within the window.
    // see also https://github.com/trifectatechfoundation/zlib-rs/issues/218
    const INPUT: [u8; 2067] = [
        0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 45, 0, 1, 1, 0, 0, 0, 9, 0, 0, 0, 39, 39, 39, 39, 39,
        45, 45, 54, 0, 54, 38, 39, 39, 0, 0, 0, 0, 0, 78, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 98, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 97, 97, 39,
        97, 97, 97, 15, 0, 0, 0, 0, 0, 5, 0, 0, 113, 0, 0, 0, 0, 25, 26, 26, 0, 0, 0, 0, 5, 0, 0,
        40, 0, 0, 0, 3, 0, 0, 0, 32, 59, 0, 0, 0, 0, 0, 0, 31, 0, 0, 0, 2, 0, 0, 39, 39, 39, 39,
        39, 39, 39, 61, 64, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 39, 39, 39, 97,
        113, 24, 113, 113, 113, 113, 113, 121, 113, 97, 97, 97, 97, 39, 97, 97, 97, 113, 113, 113,
        113, 113, 113, 113, 113, 121, 113, 44, 44, 44, 0, 0, 0, 10, 0, 0, 1, 4, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 25, 26, 65, 0, 0, 0, 0, 5, 0, 0, 40, 0, 0, 0, 3, 0, 0, 0, 32, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 35, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 40, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 39, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 58, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 108, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 121, 31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 39, 121, 31, 2, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 121, 0, 0, 0, 0, 123, 121, 0, 121, 121, 121, 0, 0, 0, 0, 0, 0, 110, 1, 8, 0,
        0, 1, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 2, 16, 122, 0, 0, 0, 0, 8, 8, 0, 31, 0, 2, 16, 9,
        0, 0, 0, 0, 0, 8, 0, 31, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 43, 0, 0, 0, 0, 0, 0, 0,
        58, 0, 0, 0, 0, 31, 39, 97, 97, 97, 97, 39, 97, 97, 97, 15, 0, 0, 0, 0, 0, 5, 0, 0, 113, 0,
        1, 39, 26, 26, 25, 48, 0, 0, 0, 0, 5, 0, 0, 40, 0, 0, 0, 3, 0, 0, 0, 32, 59, 0, 0, 0, 0, 0,
        91, 31, 0, 0, 0, 2, 0, 0, 39, 39, 39, 39, 39, 39, 39, 61, 64, 61, 61, 61, 61, 61, 61, 61,
        61, 61, 61, 61, 61, 41, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 97, 39, 97, 97, 97, 15, 0,
        0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 37, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 121, 31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 121,
        31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 121, 31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 121, 31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 121, 31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        121, 31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
        61, 39, 39, 39, 97, 113, 24, 113, 113, 113, 113, 113, 121, 113, 97, 97, 97, 97, 39, 97, 97,
        97, 113, 113, 113, 113, 113, 113, 113, 113, 121, 113, 44, 44, 44, 0, 0, 0, 10, 0, 0, 1, 4,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 25, 26, 65, 0, 0, 0, 0, 5, 0, 0, 40, 0, 0, 0, 3, 0, 0, 0, 32,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 35, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        40, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 39, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 108, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 121, 31, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 39, 121, 31, 2, 0, 0, 0, 0, 39, 49, 39, 49, 39, 39, 39, 0,
        0, 0, 0, 0, 48, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 40, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        7, 0, 46, 6, 0, 0, 0, 0, 0, 0, 31, 0, 0, 0, 0, 0, 64, 0, 0, 0, 4, 0, 0, 9, 0, 0, 0, 0, 3,
        0, 2, 61, 0, 0, 0, 0, 0, 1, 0, 0, 52, 56, 53, 53, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0,
        40, 0, 10, 4, 0, 0, 0, 0, 0, 0, 46, 121, 121, 121, 121, 121, 121, 121, 121, 0, 40, 0, 0, 0,
        0, 16, 0, 0, 0, 0,
    ];

    let config = DeflateConfig {
        level: 0,
        method: Method::Deflated,
        window_bits: 25,
        mem_level: 3,
        strategy: Strategy::Default,
    };

    assert_eq_rs_ng!({
        let mut header = gz_header {
            text: 825307441,
            time: 14641,
            xflags: 0,
            os: 0,
            extra: core::ptr::null_mut(),
            extra_len: 0,
            extra_max: 0,
            name: core::ptr::null_mut(),
            name_max: 0,
            comment: core::ptr::null_mut(),
            comm_max: 0,
            hcrc: 0,
            done: 0,
        };

        let mut stream = MaybeUninit::zeroed();

        let err = unsafe {
            deflateInit2_(
                stream.as_mut_ptr(),
                config.level,
                config.method as i32,
                config.window_bits,
                config.mem_level,
                config.strategy as i32,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as c_int,
            )
        };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let stream = unsafe { stream.assume_init_mut() };

        let err = unsafe { deflateSetHeader(stream, &mut header as gz_headerp) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let mut source = INPUT;
        let buf_size = unsafe { deflateBound(stream, source.len() as _) };

        let mut dest = vec![0; buf_size as usize];
        let chunk = 47u32;
        let flush = DeflateFlush::PartialFlush;

        stream.next_in = source.as_mut_ptr().cast();
        stream.next_out = dest.as_mut_ptr().cast();

        // Break input into chunks.
        let mut left: u32 = source.len().try_into().unwrap();
        stream.avail_out = dest.len().try_into().unwrap();
        while left > 0 {
            let avail = Ord::min(chunk, left);
            stream.avail_in = avail;
            let err = unsafe { deflate(stream, flush as i32) };
            match ReturnCode::from(err) {
                ReturnCode::Ok => {
                    left -= avail;
                }
                ReturnCode::BufError => {
                    // Ran out of space, reallocate the buffer. Worst case double the buffer size.
                    let add_space = Ord::min(chunk, buf_size as u32);
                    dest.resize(dest.len() + add_space as usize, 0);

                    // If extend() reallocates, it may have moved in memory.
                    stream.next_out = dest.as_mut_ptr();
                    stream.avail_out += add_space;

                    left -= avail - stream.avail_in;
                }
                err => panic!("fatal {:?}", err),
            }
        }

        assert_eq!(left, 0);

        // Finish the stream.
        let err = unsafe { deflate(stream, DeflateFlush::Finish as _) };
        match ReturnCode::from(err) {
            ReturnCode::Ok | ReturnCode::BufError => {
                // We might have run out of input, but still need more space to write the header.
                loop {
                    // Worst case double the buffer size.
                    let add_space = Ord::min(chunk, buf_size as u32);
                    dest.resize(dest.len() + add_space as usize, 0);

                    // If extend() reallocates, it may have moved in memory.
                    stream.next_out = dest.as_mut_ptr();
                    stream.avail_out += add_space;

                    let err = unsafe { deflate(stream, DeflateFlush::Finish as _) };
                    match ReturnCode::from(err) {
                        ReturnCode::Ok => continue,
                        ReturnCode::BufError => continue,
                        ReturnCode::StreamEnd => break,
                        _ => unreachable!(),
                    }
                }
            }
            ReturnCode::StreamEnd => { /* do nothing, we're done */ }
            err => panic!("fatal {:?}", err),
        }

        dest.truncate(stream.total_out as usize);

        let err = unsafe { deflateEnd(stream) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        dest
    });
}

#[test]
fn crc32_hash_calc_uninitialized_memory() {
    // by default the `Crc32HashCalc::insert_string` function tries to read some uninitialized
    // bytes in this case. That panicked before, and we now handle it properly.
    //
    // see https://github.com/trifectatechfoundation/zlib-rs/issues/219

    let mut source = [
        96, 48, 113, 0, 25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 71, 0, 0, 0, 0, 0, 0, 7,
        0, 0, 0, 64, 0, 0, 0, 38, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 86, 86, 86, 86, 86, 86, 86, 86,
        86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
        86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
        86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
        86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
        86, 86, 0, 0, 0, 0, 121, 31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 121, 31, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 1, 0, 0, 0, 121, 31, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 64, 0, 0, 121, 31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 121, 31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ];

    let config = DeflateConfig {
        level: 0,
        method: Method::Deflated,
        window_bits: 31,
        mem_level: 3,
        strategy: Strategy::Default,
    };

    assert_eq_rs_ng!({
        let mut stream = MaybeUninit::zeroed();

        let err = unsafe {
            deflateInit2_(
                stream.as_mut_ptr(),
                config.level,
                config.method as i32,
                config.window_bits,
                config.mem_level,
                config.strategy as i32,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as c_int,
            )
        };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let stream = unsafe { stream.assume_init_mut() };

        let buf_size = unsafe { deflateBound(stream, source.len() as _) };

        let mut dest = vec![0; buf_size as usize];
        let chunk = 2u32;
        let flush = DeflateFlush::NoFlush;

        stream.next_in = source.as_mut_ptr().cast();
        stream.avail_in = chunk; // First chunk.
        stream.next_out = dest.as_mut_ptr().cast();
        stream.avail_out = dest.len().try_into().unwrap();

        // Deflate first chunk.
        let err = unsafe { deflate(stream, flush as i32) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        // Change the parameters.
        let new_level = 4;
        let err = unsafe { deflateParams(stream, new_level, config.strategy as _) };
        match ReturnCode::from(err) {
            ReturnCode::Ok => {}
            ReturnCode::BufError => {
                // Flushing the current pending data may run us out of buffer space.
                // Worst case double the buffer size.
                let add_space = Ord::min(chunk, buf_size as u32);
                dest.resize(dest.len() + add_space as usize, 0);

                // If extend() reallocates, it may have moved in memory.
                stream.next_out = dest.as_mut_ptr();
                stream.avail_out += add_space;

                let err = unsafe { deflateParams(stream, new_level, config.strategy as _) };
                assert_eq!(ReturnCode::from(err), ReturnCode::Ok);
            }
            err => panic!("fatal {:?}", err),
        }

        // Deflate the rest in chunks.
        let mut left: u32 = source.len() as u32 - chunk;
        while left > 0 {
            // Write the chunk.
            let avail = Ord::min(chunk, left);
            stream.avail_in = avail;
            let err = unsafe { deflate(stream, flush as i32) };
            match ReturnCode::from(err) {
                ReturnCode::Ok => {
                    left -= avail;
                }
                ReturnCode::BufError => {
                    // Worst case double the buffer size.
                    let add_space = Ord::min(chunk, buf_size as u32);
                    dest.resize(dest.len() + add_space as usize, 0);

                    // If extend() reallocates, it may have moved in memory.
                    stream.next_out = dest.as_mut_ptr();
                    stream.avail_out += add_space;

                    left -= avail - stream.avail_in;
                }
                err => panic!("fatal {:?}", err),
            }
        }

        assert_eq!(left, 0);

        deflateEnd(stream);

        dest
    });
}
