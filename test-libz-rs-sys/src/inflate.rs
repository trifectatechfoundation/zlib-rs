use core::ffi::{c_char, c_int, c_uchar, c_uint, c_ulong, c_void, CStr};
use core::mem::{ManuallyDrop, MaybeUninit};

use zlib_rs::c_api::{
    Z_DATA_ERROR, Z_ERRNO, Z_NEED_DICT, Z_OK, Z_STREAM_END, Z_STREAM_ERROR, Z_VERSION_ERROR,
};
use zlib_rs::deflate::{compress_slice, DeflateConfig};
use zlib_rs::inflate::{set_mode_dict, uncompress_slice, InflateConfig, INFLATE_STATE_SIZE};
use zlib_rs::{InflateFlush, ReturnCode, MAX_WBITS};

use crate::assert_eq_rs_ng;

const VERSION: *const c_char = libz_rs_sys::zlibVersion();
const STREAM_SIZE: c_int = core::mem::size_of::<libz_rs_sys::z_stream>() as c_int;

#[derive(Clone, Copy)]
struct MemItem {
    ptr: *mut c_void,
    size: usize,
}

struct MemZone {
    items: Vec<MemItem>,
    total: usize,
    highwater: usize,
    limit: usize,
    not_lifo: usize,
    rogue: usize,
}

unsafe extern "C" fn mem_alloc(mem: *mut c_void, count: u32, size: u32) -> *mut c_void {
    let count = count as usize;
    let size = size as usize;
    let len = count * size;

    // induced allocation failure
    if mem.is_null() {
        return std::ptr::null_mut();
    }

    let mut zone = ManuallyDrop::new(unsafe { Box::from_raw(mem as *mut MemZone) });

    // induced allocation failure
    if zone.limit != 0 && zone.total + len > zone.limit {
        return std::ptr::null_mut();
    }

    extern "C" {
        fn malloc(size: usize) -> *mut c_void;
    }

    // perform the allocation
    let ptr = unsafe { malloc(len) };
    if ptr.is_null() {
        return std::ptr::null_mut();
    }

    // fill the memory to make sure that the code does not depend on zeros
    unsafe { std::ptr::write_bytes(ptr, 0xa5, len) };

    // create a new item for the list
    let item = MemItem { ptr, size: len };

    // update the statistics
    zone.total += item.size;
    if zone.total > zone.highwater {
        zone.highwater = zone.total;
    }

    // insert item
    zone.items.push(item);

    // return the allocated memory
    ptr
}

unsafe extern "C" fn mem_free(mem: *mut c_void, ptr: *mut c_void) {
    extern "C" {
        fn free(p: *mut c_void);
    }

    if mem.is_null() {
        unsafe { free(ptr) };
        return;
    }

    let mut zone = ManuallyDrop::new(unsafe { Box::from_raw(mem as *mut MemZone) });

    let last = zone.items.pop();
    let mut found = None;

    if let Some(last) = last {
        if last.ptr == ptr {
            // this is it
            found = Some(last);
        } else {
            zone.items.push(last);

            let index = zone.items.iter().position(|item| item.ptr == ptr);

            if let Some(index) = index {
                let last = zone.items.remove(index);
                found = Some(last);
                zone.not_lifo += 1;
            }
        }
    }

    if let Some(item) = found {
        zone.total -= item.size;
    } else {
        zone.rogue += 1;
    }

    unsafe { free(ptr) }
}

fn mem_setup() -> libz_rs_sys::z_stream {
    let zone = MemZone {
        items: Vec::new(),
        total: 0,
        highwater: 0,
        limit: 0,
        not_lifo: 0,
        rogue: 0,
    };
    let zone = Box::new(zone);

    let stream = libz_rs_sys::z_stream {
        next_in: std::ptr::null_mut(),
        avail_in: 0,
        total_in: 0,
        next_out: std::ptr::null_mut(),
        avail_out: 0,
        total_out: 0,
        msg: std::ptr::null_mut(),
        state: std::ptr::null_mut(),
        zalloc: Some(mem_alloc),
        zfree: Some(mem_free),
        opaque: Box::leak(zone) as *mut _ as *mut c_void,
        data_type: 0,
        adler: 0,
        reserved: 0,
    };

    stream
}

fn mem_limit(stream: &mut libz_rs_sys::z_stream, limit: usize) {
    assert!(!stream.opaque.is_null());

    let mut zone = ManuallyDrop::new(unsafe { Box::from_raw(stream.opaque as *mut MemZone) });
    zone.limit = limit;
}

fn mem_done(stream: &mut libz_rs_sys::z_stream) {
    assert!(!stream.opaque.is_null());

    extern "C" {
        fn free(p: *mut c_void);
    }

    // zone not wrapped in ManuallyDrop so it is free'd
    let mut zone = unsafe { Box::from_raw(stream.opaque as *mut MemZone) };

    let count = zone.items.len();

    for item in zone.items.drain(..) {
        unsafe { free(item.ptr) };
    }

    assert_eq!(
        (count, zone.total),
        (0, 0),
        "{} bytes in {count} blocks not freed",
        zone.total
    );
    assert_eq!(zone.not_lifo, 0, "{} frees not LIFO", zone.not_lifo);
    assert_eq!(zone.rogue, 0, "{} frees not recognized", zone.rogue);

    stream.opaque = std::ptr::null_mut();
    stream.zalloc = None;
    stream.zfree = None;
}

#[test]
fn gzip_header_check() {
    use libz_rs_sys::*;

    let input: &[u8] = &[
        0x1f, 0x8b, 0x08, 0x1f, 0x44, 0x0a, 0x45, 0x65, 0x00, 0x03, 0x0e, 0x00, 0x54, 0x47, 0x0a,
        0x00, 0x45, 0x58, 0x54, 0x52, 0x41, 0x20, 0x44, 0x41, 0x54, 0x41, 0x74, 0x65, 0x73, 0x74,
        0x2e, 0x74, 0x78, 0x74, 0x00, 0x41, 0x20, 0x63, 0x6f, 0x6d, 0x6d, 0x65, 0x6e, 0x74, 0x00,
        0x3b, 0x92,
    ];

    let _what = "gzip header parsing";
    let step = 0;
    let len = 1;
    let err = Z_OK;

    let err = Some(err);
    let mut stream = mem_setup();

    let init_err = unsafe { inflateInit2_(&mut stream, 47, VERSION, STREAM_SIZE) };
    if init_err != Z_OK {
        mem_done(&mut stream);
        return;
    }

    let mut out = vec![0u8; len];
    let mut extra: [u8; 14] = [0; 14];
    let mut name: [u8; 9] = [0; 9];
    let mut comment: [u8; 10] = [0; 10];

    // Set header
    // See: https://www.zlib.net/manual.html
    let mut header = gz_header {
        text: 0,
        time: 0,
        xflags: 0,
        os: 0,
        extra: extra.as_mut_ptr(),
        extra_len: 0,
        extra_max: 14,
        name: name.as_mut_ptr(),
        name_max: 9, // How / where should this be set?
        comment: comment.as_mut_ptr(),
        comm_max: 10,
        hcrc: 0,
        done: 0,
    };

    let ret = unsafe { inflateGetHeader(&mut stream, &mut header) };
    assert_eq!(ReturnCode::from(ret), ReturnCode::Ok);

    let have = input.len();
    let step = if step == 0 || step > have { have } else { step };

    stream.avail_in = step as _;
    stream.next_in = input.as_ptr() as *mut _;

    stream.avail_out = len as _;
    stream.next_out = out.as_mut_ptr();

    let ret = unsafe { inflate(&mut stream, InflateFlush::NoFlush as _) };

    if let Some(err) = err {
        if err != 9 {
            assert_eq!(ret, err)
        }
    }

    assert_eq!(header.text, 1);
    assert_eq!(header.time, 1699023428);
    assert_eq!(header.os, 3);
    assert_eq!(header.hcrc, 1);
    assert_eq!(header.done, 1);

    // Check the header comment
    let comment_string = match std::str::from_utf8(&comment) {
        Ok(s) => s,
        Err(_) => panic!("Invalid string found in comment"),
    };
    assert_eq!("A comment\0", comment_string);

    // Check header original filename
    let name_string = match std::str::from_utf8(&name) {
        Ok(s) => s,
        Err(_) => panic!("Invalid string found in name"),
    };
    assert_eq!("test.txt\0", name_string);

    // Check header extra
    let extra_bytes = [84, 71, 10, 0, 69, 88, 84, 82, 65, 32, 68, 65, 84, 65];
    assert_eq!(extra_bytes, extra);

    let ret = unsafe { inflateReset2(&mut stream, -8) };
    assert_eq!(ret, Z_OK);

    let ret = unsafe { inflateEnd(&mut stream) };
    assert_eq!(ret, Z_OK);

    mem_done(&mut stream);
}

fn inf(input: &[u8], _what: &str, step: usize, win: i32, len: usize, err: c_int) {
    use libz_rs_sys::*;

    let mut err = Some(err);

    let mut stream = mem_setup();

    let init_err = unsafe { inflateInit2_(&mut stream, win, VERSION, STREAM_SIZE) };
    if init_err != Z_OK {
        mem_done(&mut stream);
        return;
    }

    let mut out = vec![0u8; len];

    let mut extra: [u8; 1024] = [0; 1024];
    let mut name: [u8; 64] = [0; 64];
    let mut comment: [u8; 64] = [0; 64];

    // Set header
    // See: https://www.zlib.net/manual.html
    let mut header = gz_header {
        text: 0,
        time: 0,
        xflags: 0,
        os: 0,
        extra: extra.as_mut_ptr(),
        extra_len: 0,
        extra_max: 1024,
        name: name.as_mut_ptr(),
        name_max: 64, // How / where should this be set?
        comment: comment.as_mut_ptr(),
        comm_max: 64,
        hcrc: 0,
        done: 0,
    };

    if win == 47 {
        let err = unsafe { inflateGetHeader(&mut stream, &mut header) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);
    }

    let mut have = input.len();
    let step = if step == 0 || step > have { have } else { step };

    stream.avail_in = step as _;
    have -= step;
    stream.next_in = input.as_ptr() as *mut _;

    loop {
        stream.avail_out = len as _;
        stream.next_out = out.as_mut_ptr();

        let ret = unsafe { inflate(&mut stream, InflateFlush::NoFlush as _) };

        if let Some(err) = err {
            assert_eq!(ret, err)
        }

        if !matches!(ret, Z_OK | Z_BUF_ERROR | Z_NEED_DICT) {
            break;
        }

        if matches!(ret, Z_NEED_DICT) {
            let ret = unsafe { inflateSetDictionary(&mut stream, input.as_ptr(), 1) };
            assert_eq!(ret, Z_DATA_ERROR);

            unsafe { set_mode_dict(&mut stream) }
            let ret = unsafe { inflateSetDictionary(&mut stream, out.as_ptr(), 0) };
            assert_eq!(ret, Z_OK);

            let ret = unsafe { inflate(&mut stream, InflateFlush::NoFlush as _) };
            assert_eq!(ret, Z_BUF_ERROR);
        }

        let mut copy = z_stream::default();
        let ret = unsafe { inflateCopy(&mut copy, &stream) };
        assert_eq!(ret, Z_OK);

        let ret = unsafe { inflateEnd(&mut copy) };
        assert_eq!(ret, Z_OK);

        // only care about this error on the first iteration
        err = None;

        have += stream.avail_in as usize;
        stream.avail_in = if step > have { have } else { step } as _;
        have -= stream.avail_in as usize;

        if stream.avail_in == 0 {
            break;
        }
    }

    let ret = unsafe { inflateReset2(&mut stream, -8) };
    assert_eq!(ret, Z_OK);

    let ret = unsafe { inflateEnd(&mut stream) };
    assert_eq!(ret, Z_OK);

    mem_done(&mut stream);
}

#[test]
fn support() {
    use libz_rs_sys::*;

    let mut stream = mem_setup();

    let ret = unsafe {
        inflateInit_(
            &mut stream,
            zlibVersion(),
            core::mem::size_of::<z_stream>() as i32,
        )
    };
    assert_eq!(ret, Z_OK);

    let ret = unsafe { inflateSetDictionary(&mut stream, std::ptr::null(), 0) };
    assert_eq!(ret, Z_STREAM_ERROR);

    let ret = unsafe { inflateEnd(&mut stream) };
    assert_eq!(ret, Z_OK);

    mem_done(&mut stream);
}

#[test]
fn force_window_allocation() {
    inf(&[0x63, 0x00], "force window allocation", 0, -15, 1, Z_OK);
}

#[test]
fn force_window_replacement() {
    inf(
        &[0x63, 0x18, 0x05],
        "force window replacement",
        0,
        -8,
        259,
        Z_OK,
    );
}

#[test]
fn force_split_window_update() {
    inf(
        &[0x63, 0x18, 0x68, 0x30, 0xd0, 0x0, 0x0],
        "force split window update",
        4,
        -8,
        259,
        Z_OK,
    );
}

#[test]
fn use_fixed_blocks() {
    inf(&[0x03, 0x00], "use fixed blocks", 0, -15, 1, Z_STREAM_END);
}

#[test]
fn bad_window_size() {
    inf(&[], "bad window size", 0, 1, 0, Z_STREAM_ERROR);
}

#[test]
fn cover_wrap() {
    use libz_rs_sys::*;

    assert_eq!(unsafe { inflate(std::ptr::null_mut(), 0) }, Z_STREAM_ERROR);
    assert_eq!(unsafe { inflateEnd(std::ptr::null_mut()) }, Z_STREAM_ERROR);

    let mut strm = mem_setup();
    strm.avail_in = 0;
    strm.next_in = std::ptr::null_mut();
    let mut ret = unsafe { inflateInit2_(&mut strm, -8, VERSION, STREAM_SIZE) };
    let mut input = [0x63u8, 0x00];
    strm.avail_in = input.len() as _;
    strm.next_in = input.as_mut_ptr().cast();
    strm.avail_out = 1;
    strm.next_out = (&mut ret) as *mut _ as *mut u8;

    let dict = [0u8; 257];
    ret = unsafe { inflateSetDictionary(&mut strm, dict.as_ptr(), 257) };
    assert_eq!(ret, Z_OK);

    // ------------------------------

    let size = 2 * INFLATE_STATE_SIZE + 256;
    mem_limit(&mut strm, size);

    ret = unsafe { inflatePrime(&mut strm, 16, 0) };
    assert_eq!(ret, Z_OK);

    let mut input = [0x80u8, 0x00];
    strm.avail_in = input.len() as _;
    strm.next_in = input.as_mut_ptr();
    ret = unsafe { inflateSync(&mut strm) };
    assert_eq!(ret, Z_DATA_ERROR);

    ret = unsafe { inflate(&mut strm, InflateFlush::NoFlush as _) };
    assert_eq!(ret, Z_STREAM_ERROR);

    let mut input = [0u8, 0, 0xFF, 0xFF];
    strm.avail_in = input.len() as _;
    strm.next_in = input.as_mut_ptr();
    ret = unsafe { inflateSync(&mut strm) };
    assert_eq!(ret, Z_OK);
    let _ = unsafe { inflateSyncPoint(&mut strm) };

    let mut copy = z_stream::default();
    ret = unsafe { inflateCopy(&mut copy, &strm) };
    assert_eq!(ret, Z_MEM_ERROR);

    mem_limit(&mut strm, 0);
    ret = unsafe { inflateUndermine(&mut strm, 1) };
    assert_eq!(ret, Z_OK);

    let _ = unsafe { inflateMark(&strm) };
    ret = unsafe { inflateEnd(&mut strm) };
    assert_eq!(ret, Z_OK);
    mem_done(&mut strm);
}

#[test]
fn bad_gzip_method() {
    inf(
        &[0x1f, 0x8b, 0x00, 0x00],
        "bad gzip method",
        0,
        31,
        0,
        Z_DATA_ERROR,
    )
}
#[test]
fn bad_gzip_flags() {
    inf(
        &[0x1f, 0x8b, 0x08, 0x80],
        "bad gzip flags",
        0,
        31,
        0,
        Z_DATA_ERROR,
    )
}
#[test]
fn bad_zlib_method() {
    inf(&[0x77, 0x85], "bad zlib method", 0, 15, 0, Z_DATA_ERROR)
}
#[test]
fn set_window_size_from_header() {
    inf(&[0x08, 0x99], "set window size from header", 0, 0, 0, Z_OK)
}
#[test]
fn bad_zlib_window_size() {
    inf(&[0x78, 0x9c], "bad zlib window size", 0, 8, 0, Z_DATA_ERROR)
}
#[test]
fn check_adler32() {
    inf(
        &[0x78, 0x9c, 0x63, 0x0, 0x0, 0x0, 0x1, 0x0, 0x1],
        "check adler32",
        0,
        15,
        1,
        Z_STREAM_END,
    )
}
#[test]
fn bad_header_crc() {
    inf(
        &[
            0x1f, 0x8b, 0x8, 0x1e, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x1, 0x0, 0x0, 0x0, 0x0, 0x0, 0x00,
        ],
        "bad header crc",
        0,
        47,
        1,
        Z_DATA_ERROR,
    )
}

#[test]
fn check_gzip_length() {
    inf(
        &[
            0x1f, 0x8b, 0x8, 0x2, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x1d, 0x26, 0x3, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
        ],
        "check gzip length",
        0,
        47,
        0,
        Z_STREAM_END,
    )
}

#[test]
fn bad_zlib_header_check() {
    inf(
        &[0x78, 0x90],
        "bad zlib header check",
        0,
        47,
        0,
        Z_DATA_ERROR,
    )
}

#[test]
fn good_zlib_header_check() {
    inf(
        &[
            0x1f, 0x8b, 0x08, 0x1f, 0x44, 0x0a, 0x45, 0x65, 0x00, 0x03, 0x0e, 0x00, 0x54, 0x47,
            0x0a, 0x00, 0x45, 0x58, 0x54, 0x52, 0x41, 0x20, 0x44, 0x41, 0x54, 0x41, 0x74, 0x65,
            0x73, 0x74, 0x2e, 0x74, 0x78, 0x74, 0x00, 0x41, 0x20, 0x63, 0x6f, 0x6d, 0x6d, 0x65,
            0x6e, 0x74, 0x00, 0x3b, 0x92,
        ],
        "good zlib header check",
        0,
        47,
        0,
        Z_OK,
    )
}

#[test]
fn need_dictionary() {
    inf(
        &[0x08, 0xb8, 0x0, 0x0, 0x0, 0x1],
        "need dictionary",
        0,
        8,
        0,
        Z_NEED_DICT,
    )
}

#[test]
fn compute_adler32() {
    inf(&[0x78, 0x9c, 0x63, 0x0], "compute adler32", 0, 15, 1, Z_OK)
}

/* do a raw inflate of data in hexadecimal with both inflate and inflateBack */
fn try_inflate(input: &[u8], expected_err: c_int) -> c_int {
    use libz_rs_sys::*;

    let len = input.len();

    /* allocate work areas */
    let size = len << 3;
    let mut out = vec![0; size];

    //    /* first with inflate */
    //    strcpy(prefix, id);
    //    strcat(prefix, "-late");

    let mut strm = mem_setup();
    strm.avail_in = 0;
    strm.next_in = std::ptr::null_mut();

    let mut ret = unsafe {
        inflateInit2_(
            &mut strm,
            if expected_err < 0 { 47 } else { -15 },
            VERSION,
            STREAM_SIZE,
        )
    };
    assert_eq!(ret, Z_OK);
    strm.avail_in = len as _;
    strm.next_in = input.as_ptr() as *mut u8;

    loop {
        strm.avail_out = size as _;
        strm.next_out = out.as_mut_ptr();

        ret = unsafe { inflate(&mut strm, InflateFlush::Trees as _) };
        assert!(!matches!(ret, Z_STREAM_ERROR | Z_MEM_ERROR));

        if matches!(ret, Z_DATA_ERROR | Z_NEED_DICT) {
            break;
        }

        if !(strm.avail_in > 0 || strm.avail_out == 0) {
            break;
        }
    }

    if expected_err != Z_OK {
        assert_eq!(ret, Z_DATA_ERROR);
    }

    unsafe { inflateEnd(&mut strm) };
    mem_done(&mut strm);

    /* then with inflateBack */
    if expected_err >= 0 {
        ret = try_inflate_back(input, expected_err);
    }

    ret
}

fn try_inflate_back(input: &[u8], expected_err: c_int) -> c_int {
    use libz_rs_sys::*;

    let len = input.len();

    /* allocate work areas */
    let mut win = vec![0; 32768];

    // re-setup the stream for inflateBack
    let mut strm = mem_setup();

    let mut ret =
        unsafe { inflateBackInit_(&mut strm, 15, win.as_mut_ptr(), VERSION, STREAM_SIZE) };
    assert_eq!(ret, Z_OK);

    strm.avail_in = len as _;
    strm.next_in = input.as_ptr() as *mut u8;

    // assumes you have `pull` and `push` extern "C" functions defined
    ret = unsafe {
        inflateBack(
            &mut strm,
            Some(pull),
            std::ptr::null_mut(),
            Some(push),
            std::ptr::null_mut(),
        )
    };
    assert_ne!(ret, Z_STREAM_ERROR);

    if expected_err != Z_OK && ret != Z_BUF_ERROR {
        assert_eq!(ret, Z_DATA_ERROR);
    }

    unsafe { inflateBackEnd(&mut strm) };
    mem_done(&mut strm);

    ret
}

#[test]
fn invalid_stored_block_length() {
    try_inflate(&[0, 0, 0, 0, 0], Z_STREAM_END);
}

#[test]
fn fixed() {
    try_inflate(&[3, 0], Z_OK);
}

#[test]
fn invalid_block_type() {
    try_inflate(&[6], Z_STREAM_END);
}
#[test]
fn stored() {
    try_inflate(&[0x1, 0x1, 0x0, 0xfe, 0xff, 0x0], Z_OK);
}

#[test]
fn too_many_length_or_distance_symbols() {
    try_inflate(&[0xfc, 0x0, 0x0], Z_STREAM_END);
}

#[test]
fn invalid_code_lengths_set() {
    try_inflate(&[0x4, 0x0, 0xfe, 0xff], Z_STREAM_END);
}
#[test]
fn invalid_bit_length_repeat_1() {
    try_inflate(&[0x4, 0x0, 0x24, 0x49, 0x0], Z_STREAM_END);
}
#[test]
fn invalid_bit_length_repeat_2() {
    try_inflate(&[0x4, 0x0, 0x24, 0xe9, 0xff, 0xff], Z_STREAM_END);
}
#[test]
fn invalid_code_missing_end_of_block() {
    try_inflate(&[0x4, 0x0, 0x24, 0xe9, 0xff, 0x6d], Z_STREAM_END);
}
#[test]
fn invalid_literal_lengths_set() {
    try_inflate(
        &[
            0x4, 0x80, 0x49, 0x92, 0x24, 0x49, 0x92, 0x24, 0x71, 0xff, 0xff, 0x93, 0x11, 0x0,
        ],
        Z_STREAM_END,
    );
}
#[test]
fn invalid_distances_set() {
    try_inflate(
        &[
            0x4, 0x80, 0x49, 0x92, 0x24, 0x49, 0x92, 0x24, 0xf, 0xb4, 0xff, 0xff, 0xc3, 0x84,
        ],
        Z_STREAM_END,
    );
}
#[test]
fn invalid_literal_length_code() {
    try_inflate(
        &[
            0x4, 0xc0, 0x81, 0x8, 0x0, 0x0, 0x0, 0x0, 0x20, 0x7f, 0xeb, 0xb, 0x0, 0x0,
        ],
        Z_STREAM_END,
    );
}
#[test]
fn invalid_distance_code() {
    try_inflate(&[0x2, 0x7e, 0xff, 0xff], Z_STREAM_END);
}
#[test]
fn invalid_distance_too_far_back() {
    try_inflate(
        &[
            0xc, 0xc0, 0x81, 0x0, 0x0, 0x0, 0x0, 0x0, 0x90, 0xff, 0x6b, 0x4, 0x0,
        ],
        Z_STREAM_END,
    );
}

#[test]
fn incorrect_data_check() {
    try_inflate(
        &[
            0x1f, 0x8b, 0x8, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x3, 0x0, 0x0, 0x0, 0x0, 0x1,
        ],
        Z_ERRNO,
    );
}
#[test]
fn incorrect_length_check() {
    try_inflate(
        &[
            0x1f, 0x8b, 0x8, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x3, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x1,
        ],
        Z_ERRNO,
    );
}

#[test]
fn zlib_ng_pull_17() {
    try_inflate(
        &[
            0x5, 0xc0, 0x21, 0xd, 0x0, 0x0, 0x0, 0x80, 0xb0, 0xfe, 0x6d, 0x2f, 0x91, 0x6c,
        ],
        Z_OK,
    );
}
#[test]
fn long_code() {
    try_inflate(
        &[
            0x5, 0xe0, 0x81, 0x91, 0x24, 0xcb, 0xb2, 0x2c, 0x49, 0xe2, 0xf, 0x2e, 0x8b, 0x9a, 0x47,
            0x56, 0x9f, 0xfb, 0xfe, 0xec, 0xd2, 0xff, 0x1f,
        ],
        Z_OK,
    );
}
#[test]
fn length_extra() {
    try_inflate(
        &[
            0xed, 0xc0, 0x1, 0x1, 0x0, 0x0, 0x0, 0x40, 0x20, 0xff, 0x57, 0x1b, 0x42, 0x2c, 0x4f,
        ],
        Z_OK,
    );
}
#[test]
fn long_distance_and_extra() {
    try_inflate(
        &[
            0xed, 0xcf, 0xc1, 0xb1, 0x2c, 0x47, 0x10, 0xc4, 0x30, 0xfa, 0x6f, 0x35, 0x1d, 0x1,
            0x82, 0x59, 0x3d, 0xfb, 0xbe, 0x2e, 0x2a, 0xfc, 0xf, 0xc,
        ],
        Z_OK,
    );
}
#[test]
fn window_end() {
    try_inflate(
        &[
            0xed, 0xc0, 0x81, 0x0, 0x0, 0x0, 0x0, 0x80, 0xa0, 0xfd, 0xa9, 0x17, 0xa9, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x6,
        ],
        Z_OK,
    );
}

#[test]
fn back_too_far() {
    try_inflate_back(
        &[75, 39, 108, 46, 8, 59, 81, 81, 0, 81, 81, 0, 0, 0, 10, 10],
        Z_DATA_ERROR,
    );
}

#[test]
fn inflate_fast_type_return() {
    inf(
        &[0x2, 0x8, 0x20, 0x80, 0x0, 0x3, 0x0],
        "inflate_fast TYPE return",
        0,
        -15,
        258,
        Z_STREAM_END,
    );
}
#[test]
fn window_wrap() {
    inf(
        &[0x63, 0x18, 0x5, 0x40, 0xc, 0x0],
        "window wrap",
        3,
        -8,
        300,
        Z_OK,
    );
}

#[test]
fn fast_length_extra_bits() {
    inf(
        &[
            0xe5, 0xe0, 0x81, 0xad, 0x6d, 0xcb, 0xb2, 0x2c, 0xc9, 0x01, 0x1e, 0x59, 0x63, 0xae,
            0x7d, 0xee, 0xfb, 0x4d, 0xfd, 0xb5, 0x35, 0x41, 0x68, 0xff, 0x7f, 0x0f, 0x0, 0x0, 0x0,
        ],
        "fast length extra bits",
        0,
        -8,
        258,
        Z_DATA_ERROR,
    );
}
#[test]
fn fast_distance_extra_bits() {
    inf(
        &[
            0x25, 0xfd, 0x81, 0xb5, 0x6d, 0x59, 0xb6, 0x6a, 0x49, 0xea, 0xaf, 0x35, 0x6, 0x34,
            0xeb, 0x8c, 0xb9, 0xf6, 0xb9, 0x1e, 0xef, 0x67, 0x49, 0x50, 0xfe, 0xff, 0xff, 0x3f,
            0x0, 0x0,
        ],
        "fast distance extra bits",
        0,
        -8,
        258,
        Z_DATA_ERROR,
    )
}
#[test]
fn fast_invalid_distance_code() {
    inf(
        &[0x3, 0x7e, 0x0, 0x0, 0x0, 0x0, 0x0],
        "fast invalid distance code",
        0,
        -8,
        258,
        Z_DATA_ERROR,
    );
}
#[test]
fn fast_invalid_literal_length_code() {
    inf(
        &[0x1b, 0x7, 0x0, 0x0, 0x0, 0x0, 0x0],
        "fast invalid literal/length code",
        0,
        -8,
        258,
        Z_DATA_ERROR,
    );
}
#[test]
fn fast_2nd_level_codes_and_too_far_back() {
    inf(
        &[
            0xd, 0xc7, 0x1, 0xae, 0xeb, 0x38, 0xc, 0x4, 0x41, 0xa0, 0x87, 0x72, 0xde, 0xdf, 0xfb,
            0x1f, 0xb8, 0x36, 0xb1, 0x38, 0x5d, 0xff, 0xff, 0x0,
        ],
        "fast 2nd level codes and too far back",
        0,
        -8,
        258,
        Z_DATA_ERROR,
    );
}
#[test]
fn very_common_case() {
    inf(
        &[0x63, 0x18, 0x5, 0x8c, 0x10, 0x8, 0x0, 0x0, 0x0, 0x0],
        "very common case",
        0,
        -8,
        259,
        Z_OK,
    );
}
#[test]
fn contiguous_and_wrap_around_window() {
    inf(
        &[
            0x63, 0x60, 0x60, 0x18, 0xc9, 0x0, 0x8, 0x18, 0x18, 0x18, 0x26, 0xc0, 0x28, 0x0, 0x29,
            0x0, 0x0, 0x0,
        ],
        "contiguous and wrap around window",
        6,
        -8,
        259,
        Z_OK,
    );
}
#[test]
fn copy_direct_from_output() {
    inf(
        &[0x63, 0x0, 0x3, 0x0, 0x0, 0x0, 0x0, 0x0],
        "copy direct from output",
        0,
        -8,
        259,
        Z_STREAM_END,
    );
}

#[test]
fn cover_cve_2022_37434() {
    inf(
        &[
            0x1f, 0x8b, 0x08, 0x04, 0x61, 0x62, 0x63, 0x64, 0x61, 0x62, 0x52, 0x51, 0x1f, 0x8b,
            0x08, 0x04, 0x61, 0x62, 0x63, 0x64, 0x61, 0x62, 0x52, 0x51, 0x1f, 0x8b, 0x08, 0x04,
            0x61, 0x62, 0x63, 0x64, 0x61, 0x62, 0x52, 0x51, 0x1f, 0x8b, 0x08, 0x04, 0x61, 0x62,
            0x63, 0x64, 0x61, 0x62, 0x52, 0x51,
        ],
        "wtf",
        13,
        47,
        12,
        Z_OK,
    );
}

fn uncompress_help(input: &[u8]) -> Vec<u8> {
    use libz_rs_sys::*;

    let mut dest_vec = vec![0u8; 1 << 16];

    let mut dest_len = dest_vec.len() as c_ulong;
    let dest = dest_vec.as_mut_ptr();

    let source = input.as_ptr();
    let source_len = input.len() as _;

    let err = unsafe { uncompress(dest, &mut dest_len, source, source_len) };

    if err != 0 {
        panic!("error {:?}", ReturnCode::from(err));
    }

    dest_vec.truncate(dest_len as usize);

    dest_vec
}

#[test]
fn hello_world_uncompressed() {
    // "Hello World!" compressed at level 0
    let deflated = [
        0x78, 0x01, 0x01, 0x0d, 0x00, 0xf2, 0xff, 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x20, 0x57, 0x6f,
        0x72, 0x6c, 0x64, 0x21, 0x0a, 0x20, 0x91, 0x04, 0x48,
    ];

    let output = uncompress_help(&deflated);
    assert_eq!(String::from_utf8(output).unwrap(), "Hello World!\n");
}

#[test]
fn copy_direct_from_next_in_to_next_out() {
    assert_eq_rs_ng!({
        let input = [120, 1, 1, 2, 0, 253, 255, 6, 10, 0, 24, 0, 17];
        let output = [6u8, 10];
        let mut dest_vec = vec![0u8; 1 << 16];

        let mut dest_len = dest_vec.len() as c_ulong;
        let dest = dest_vec.as_mut_ptr();

        let source = input.as_ptr();
        let mut source_len = input.len() as c_ulong;

        let err = unsafe { uncompress2(dest, &mut dest_len, source, &mut source_len) };

        if err != 0 {
            panic!("error {:?}", ReturnCode::from(err));
        }

        assert_eq!(dest_len as usize, output.len());
        assert_eq!(source_len as usize, input.len());

        dest_vec.truncate(dest_len as usize);

        assert_eq!(dest_vec, output);
    });
}

#[test]
fn hello_world_fixed() {
    // "Hello World!" compressed with `minideflate -k -f -9`
    let deflated = [
        0x78, 0x01, 0xf3, 0x48, 0xcd, 0xc9, 0xc9, 0x57, 0x08, 0xcf, 0x2f, 0xca, 0x49, 0x51, 0xe4,
        0x02, 0x00, 0x20, 0x91, 0x04, 0x48,
    ];
    let output = uncompress_help(&deflated);
    assert_eq!(String::from_utf8(output).unwrap(), "Hello World!\n");
}

#[test]
fn hello_world_dynamic() {
    // smallest input that the fuzzer found that triggers a dynamic block
    let input = "\n\0\0\0\0l\0\nl\0l\0l\u{1}llll\n";
    let deflated = [
        120, 156, 5, 193, 177, 1, 0, 0, 8, 195, 160, 184, 246, 86, 254, 159, 133, 85, 105, 146,
        131, 61, 24, 141, 3, 128,
    ];

    let output = uncompress_help(&deflated);
    assert_eq!(String::from_utf8(output).unwrap(), input);
}

#[test]
fn inflate_adler() {
    use libz_rs_sys::*;

    const ORIGINAL: &str = "The quick brown fox jumped over the lazy dog\0";

    const COMPRESSED: [u8; 52] = [
        0x78, 0x9c, 0x0b, 0xc9, 0x48, 0x55, 0x28, 0x2c, 0xcd, 0x4c, 0xce, 0x56, 0x48, 0x2a, 0xca,
        0x2f, 0xcf, 0x53, 0x48, 0xcb, 0xaf, 0x50, 0xc8, 0x2a, 0xcd, 0x2d, 0x48, 0x4d, 0x51, 0xc8,
        0x2f, 0x4b, 0x2d, 0x52, 0x28, 0xc9, 0x48, 0x55, 0xc8, 0x49, 0xac, 0xaa, 0x54, 0x48, 0xc9,
        0x4f, 0x07, 0x00, 0x6b, 0x93, 0x10, 0x30,
    ];

    let mut stream = z_stream {
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

    let err = unsafe { inflateInit2_(&mut stream, 32 + MAX_WBITS, VERSION, STREAM_SIZE) };
    assert_eq!(err, Z_OK);

    let mut uncompressed = [0u8; 1024];

    stream.next_in = COMPRESSED.as_ptr() as *mut u8;
    stream.avail_in = COMPRESSED.len() as _;
    stream.next_out = uncompressed.as_mut_ptr();
    stream.avail_out = uncompressed.len() as _;

    let err = unsafe { inflate(&mut stream, Z_NO_FLUSH) };
    assert_eq!(err, Z_STREAM_END);

    assert_eq!(stream.adler, 0x6b931030);

    let err = unsafe { inflateEnd(&mut stream) };
    assert_eq!(err, Z_OK);

    let length = Ord::min(stream.total_out as usize, ORIGINAL.len());
    assert_eq!(&uncompressed[..length], &ORIGINAL.as_bytes()[..length])
}

#[test]
fn inflate_get_header_non_gzip_stream() {
    use libz_rs_sys::*;

    let mut stream = mem_setup();

    let win = 15; // i.e. zlib compression (and not gzip)
    let init_err = unsafe { inflateInit2_(&mut stream, win, VERSION, STREAM_SIZE) };
    if init_err != Z_OK {
        mem_done(&mut stream);
        return;
    }

    let mut header = gz_header::default();

    assert_eq!(
        unsafe { inflateGetHeader(&mut stream, &mut header) },
        ReturnCode::StreamError as i32
    );

    let ret = unsafe { inflateEnd(&mut stream) };
    assert_eq!(ret, Z_OK);

    mem_done(&mut stream);
}

#[test]
fn inflate_window_bits_0_is_15() {
    let input = b"Hello World!\n";

    let mut compressed = [0; 64];
    let (compressed, err) = compress_slice(&mut compressed, input, DeflateConfig::new(6));
    assert_eq!(err, ReturnCode::Ok);

    let config = InflateConfig { window_bits: 15 };
    let mut output_15 = [0; 64];
    let (output_15, err) = uncompress_slice(&mut output_15, compressed, config);
    assert_eq!(err, ReturnCode::Ok);

    let config = InflateConfig { window_bits: 0 };
    let mut output_0 = [0; 64];
    let (output_0, err) = uncompress_slice(&mut output_0, compressed, config);
    assert_eq!(err, ReturnCode::Ok);

    // for window size 0, the default of 15 is picked
    // NOTE: the window size does not actually influence
    // the output for an input this small.
    assert_eq!(output_15, output_0);

    assert_eq!(output_15, input);
}

#[test]
fn uncompress_edge_cases() {
    let config = InflateConfig { window_bits: 15 };

    let (result, err) = uncompress_slice(&mut [], &[], config);
    assert_eq!(err, ReturnCode::DataError);
    assert!(result.is_empty());

    let mut output = [0; 1];
    let (result, err) = uncompress_slice(&mut output, &[], config);
    assert_eq!(err, ReturnCode::DataError);
    assert!(result.is_empty());

    let input = b"Hello World!\n";

    let mut compressed = [0; 64];
    let (compressed, err) = compress_slice(&mut compressed, input, DeflateConfig::new(6));
    assert_eq!(err, ReturnCode::Ok);

    let (result, err) = uncompress_slice(&mut [], compressed, config);
    assert_eq!(err, ReturnCode::DataError);
    assert!(result.is_empty());
}

#[test]
fn gzip_header_fields_insufficient_space() {
    let chunk_size = 16;

    let input = b"Hello World\n";

    let extra =
        "Scheduling and executing async tasks is a job handled by an async runtime, such as\0";
    let name =
        "tokio, async-std, and smol. You've probably used them at some point, either directly or\0";
    let comment =
        "indirectly. They, along with many frameworks that require async, do their best to hide\0";

    let config = DeflateConfig {
        window_bits: 31,
        ..Default::default()
    };

    let mut stream = MaybeUninit::<libz_rs_sys::z_stream>::zeroed();

    const VERSION: *const c_char = libz_rs_sys::zlibVersion();
    const STREAM_SIZE: c_int = core::mem::size_of::<libz_rs_sys::z_stream>() as c_int;

    let err = unsafe {
        libz_rs_sys::deflateInit2_(
            stream.as_mut_ptr(),
            config.level,
            config.method as i32,
            config.window_bits,
            config.mem_level,
            config.strategy as i32,
            VERSION,
            STREAM_SIZE,
        )
    };
    assert_eq!(err, 0);

    let stream = unsafe { stream.assume_init_mut() };

    let mut header = libz_rs_sys::gz_header {
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

    let err = unsafe { libz_rs_sys::deflateSetHeader(stream, &mut header) };
    assert_eq!(err, 0);

    let mut output_rs = [0u8; 512];
    stream.next_out = output_rs.as_mut_ptr();
    stream.avail_out = output_rs.len() as _;

    for chunk in input.chunks(chunk_size) {
        stream.next_in = chunk.as_ptr() as *mut u8;
        stream.avail_in = chunk.len() as _;

        let err = unsafe { libz_rs_sys::deflate(stream, InflateFlush::NoFlush as _) };

        assert_eq!(err, ReturnCode::Ok as i32, "{:?}", stream.msg);
    }

    let err = unsafe { libz_rs_sys::deflate(stream, InflateFlush::Finish as _) };
    assert_eq!(ReturnCode::from(err), ReturnCode::StreamEnd);

    let output_rs = &mut output_rs[..stream.total_out as usize];

    let err = unsafe { libz_rs_sys::deflateEnd(stream) };
    assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

    assert_eq_rs_ng!({
        let mut stream = MaybeUninit::<z_stream>::zeroed();

        const VERSION: *const c_char = libz_rs_sys::zlibVersion();
        const STREAM_SIZE: c_int = core::mem::size_of::<z_stream>() as c_int;

        let err = unsafe {
            inflateInit2_(
                stream.as_mut_ptr(),
                config.window_bits,
                VERSION,
                STREAM_SIZE,
            )
        };
        assert_eq!(err, 0);

        let stream = unsafe { stream.assume_init_mut() };

        let mut output = [0u8; 64];
        stream.next_out = output.as_mut_ptr();
        stream.avail_out = output.len() as _;

        let mut extra_buf = [0u8; 64];
        assert!(extra_buf.len() < extra.len());
        let mut name_buf = [0u8; 1];
        assert!(name_buf.len() < name.len());
        let mut comment_buf = [0u8; 64];
        assert!(comment_buf.len() < comment.len());

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

        for chunk in output_rs.chunks_mut(chunk_size) {
            stream.next_in = chunk.as_mut_ptr();
            stream.avail_in = chunk.len() as _;

            let err = unsafe { inflate(stream, InflateFlush::NoFlush as _) };

            if err == ReturnCode::StreamEnd as i32 {
                break;
            }

            assert_eq!(err, ReturnCode::Ok as i32);
        }

        let err = unsafe { inflateEnd(stream) };
        assert_eq!(err, ReturnCode::Ok as i32);

        assert!(!header.extra.is_null());
        assert_eq!(
            if extra_buf.last() != Some(&0) {
                std::str::from_utf8(&extra_buf).unwrap()
            } else {
                CStr::from_bytes_until_nul(&extra_buf)
                    .unwrap()
                    .to_str()
                    .unwrap()
            },
            &extra[..extra_buf.len()]
        );

        assert!(!header.name.is_null());
        assert_eq!(
            if name_buf.last() != Some(&0) {
                std::str::from_utf8(&name_buf).unwrap()
            } else {
                CStr::from_bytes_until_nul(&name_buf)
                    .unwrap()
                    .to_str()
                    .unwrap()
            },
            &name[..name_buf.len()]
        );

        assert!(!header.comment.is_null());
        assert_eq!(
            std::str::from_utf8(&comment_buf).unwrap(),
            &comment.trim_end_matches('\0')[..comment_buf.len()]
        );

        (extra_buf, name_buf, comment_buf)
    });
}

#[test]
fn gzip_chunked_1_byte() {
    gzip_chunked(1);
}

#[test]
fn gzip_chunked_2_bytes() {
    gzip_chunked(2);
}

#[test]
fn gzip_chunked_15_bytes() {
    gzip_chunked(15);
}

#[test]
fn gzip_chunked_16_bytes() {
    gzip_chunked(16);
}

#[test]
fn gzip_chunked_17_bytes() {
    gzip_chunked(17);
}

#[test]
fn gzip_chunked_32_bytes() {
    gzip_chunked(32);
}

#[test]
fn gzip_chunked_512_bytes() {
    gzip_chunked(512);
}

fn gzip_chunked(chunk_size: usize) {
    let input = b"Hello World\n";

    let extra =
        "Scheduling and executing async tasks is a job handled by an async runtime, such as\0";
    let name =
        "tokio, async-std, and smol. You’ve probably used them at some point, either directly or\0";
    let comment =
        "indirectly. They, along with many frameworks that require async, do their best to hide\0";

    let config = DeflateConfig {
        window_bits: 31,
        ..Default::default()
    };

    let mut stream = MaybeUninit::<libz_rs_sys::z_stream>::zeroed();

    const VERSION: *const c_char = libz_rs_sys::zlibVersion();
    const STREAM_SIZE: c_int = core::mem::size_of::<libz_rs_sys::z_stream>() as c_int;

    let err = unsafe {
        libz_rs_sys::deflateInit2_(
            stream.as_mut_ptr(),
            config.level,
            config.method as i32,
            config.window_bits,
            config.mem_level,
            config.strategy as i32,
            VERSION,
            STREAM_SIZE,
        )
    };
    assert_eq!(err, 0);

    let stream = unsafe { stream.assume_init_mut() };

    let mut header = libz_rs_sys::gz_header {
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

    let err = unsafe { libz_rs_sys::deflateSetHeader(stream, &mut header) };
    assert_eq!(err, 0);

    let mut output_rs = [0u8; 512];
    stream.next_out = output_rs.as_mut_ptr();
    stream.avail_out = output_rs.len() as _;

    for chunk in input.chunks(chunk_size) {
        stream.next_in = chunk.as_ptr() as *mut u8;
        stream.avail_in = chunk.len() as _;

        let err = unsafe { libz_rs_sys::deflate(stream, InflateFlush::NoFlush as _) };

        assert_eq!(err, ReturnCode::Ok as i32, "{:?}", stream.msg);
    }

    let err = unsafe { libz_rs_sys::deflate(stream, InflateFlush::Finish as _) };
    assert_eq!(ReturnCode::from(err), ReturnCode::StreamEnd);

    let output_rs = &mut output_rs[..stream.total_out as usize];

    let err = unsafe { libz_rs_sys::deflateEnd(stream) };
    assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

    {
        #[allow(unused_imports)] // to switch to libz_ng_sys easily
        use libz_rs_sys::{
            gz_header, inflate, inflateEnd, inflateGetHeader, inflateInit2_, z_stream,
        };

        let mut stream = MaybeUninit::<z_stream>::zeroed();

        const VERSION: *const c_char = libz_rs_sys::zlibVersion();
        const STREAM_SIZE: c_int = core::mem::size_of::<z_stream>() as c_int;

        let err = unsafe {
            inflateInit2_(
                stream.as_mut_ptr(),
                config.window_bits,
                VERSION,
                STREAM_SIZE,
            )
        };
        assert_eq!(err, 0);

        let stream = unsafe { stream.assume_init_mut() };

        let mut output = [0u8; 64];
        stream.next_out = output.as_mut_ptr();
        stream.avail_out = output.len() as _;

        let mut extra_buf = [0u8; 64];
        let mut name_buf = [0u8; 64];
        let mut comment_buf = [0u8; 256];

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

        for chunk in output_rs.chunks_mut(chunk_size) {
            stream.next_in = chunk.as_mut_ptr();
            stream.avail_in = chunk.len() as _;

            let err = unsafe { inflate(stream, InflateFlush::NoFlush as _) };

            if err == ReturnCode::StreamEnd as i32 {
                break;
            }

            assert_eq!(err, ReturnCode::Ok as i32);
        }

        let err = unsafe { inflateEnd(stream) };
        assert_eq!(err, ReturnCode::Ok as i32);

        assert!(!header.extra.is_null());
        assert_eq!(
            std::str::from_utf8(&extra_buf).unwrap(),
            &extra[..extra_buf.len()]
        );

        assert!(!header.name.is_null());
        assert_eq!(
            std::str::from_utf8(&name_buf).unwrap(),
            &name[..name_buf.len()]
        );

        assert!(!header.comment.is_null());
        assert_eq!(
            unsafe { CStr::from_ptr(comment_buf.as_ptr().cast()) }
                .to_str()
                .unwrap(),
            comment.trim_end_matches('\0')
        );
    }
}

#[test]
fn chunked_output_rs() {
    use libz_rs_sys::*;

    let input = [99u8, 96, 192, 11, 24, 25, 0];

    let mut stream = MaybeUninit::<z_stream>::zeroed();

    let err = unsafe { inflateInit2_(stream.as_mut_ptr(), -15, VERSION, STREAM_SIZE) };
    assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

    let stream = unsafe { stream.assume_init_mut() };

    stream.next_in = input.as_ptr() as *mut u8;
    stream.avail_in = input.len() as _;

    let mut output = [0; 33];

    stream.next_out = output.as_mut_ptr();
    stream.avail_out = 32;

    let err = unsafe { inflate(stream, InflateFlush::NoFlush as _) };
    assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

    assert_eq!(stream.avail_out, 0);
    assert_eq!(stream.total_out, 32);

    stream.avail_out = 1;

    let err = unsafe { inflate(stream, InflateFlush::Finish as _) };
    assert_eq!(ReturnCode::from(err), ReturnCode::StreamEnd);

    unsafe { inflateEnd(stream) };

    assert_eq!(stream.total_out, 33);
}

#[test]
fn version_error() {
    use libz_rs_sys::*;

    let mut stream = core::mem::MaybeUninit::zeroed();

    let ret = unsafe {
        inflateInit_(
            stream.as_mut_ptr(),
            zlibVersion(),
            core::mem::size_of::<z_stream>() as i32,
        )
    };
    assert_eq!(ret, Z_OK);

    let ret = unsafe { inflateEnd(stream.as_mut_ptr()) };
    assert_eq!(ret, Z_OK);

    // invalid stream size
    let ret = unsafe { inflateInit_(stream.as_mut_ptr(), zlibVersion(), 1) };
    assert_eq!(ret, Z_VERSION_ERROR);

    // version is null
    let ret = unsafe {
        inflateInit_(
            stream.as_mut_ptr(),
            core::ptr::null(),
            core::mem::size_of::<z_stream>() as i32,
        )
    };
    assert_eq!(ret, Z_VERSION_ERROR);

    // version is invalid
    let ret = unsafe {
        inflateInit_(
            stream.as_mut_ptr(),
            b"!\0".as_ptr() as *const c_char,
            core::mem::size_of::<z_stream>() as i32,
        )
    };
    assert_eq!(ret, Z_VERSION_ERROR);

    // version is invalid, inflateInit2_
    let ret = unsafe {
        inflateInit2_(
            stream.as_mut_ptr(),
            1,
            b"!\0".as_ptr() as *const c_char,
            core::mem::size_of::<z_stream>() as i32,
        )
    };
    assert_eq!(ret, Z_VERSION_ERROR);
}

#[test]
fn issue_109() {
    let input = &include_bytes!("test-data/issue-109.gz")[10..][..32758];

    let window_bits = -15;

    assert_eq_rs_ng!({
        let mut output: Vec<u8> = Vec::with_capacity(1 << 15);

        let mut buf = [0; 8192];

        let mut stream = MaybeUninit::<z_stream>::zeroed();

        let err = unsafe {
            inflateInit2_(
                stream.as_mut_ptr(),
                window_bits,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as c_int,
            )
        };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let stream = unsafe { stream.assume_init_mut() };

        stream.next_in = input.as_ptr() as *mut u8;
        stream.avail_in = input.len() as _;

        while stream.avail_in != 0 {
            stream.next_out = buf.as_mut_ptr();
            stream.avail_out = buf.len() as _;

            let err = unsafe { inflate(stream, InflateFlush::NoFlush as _) };

            if ReturnCode::from(err) == ReturnCode::BufError {
                output.extend(&buf[..stream.avail_out as usize]);
                stream.avail_out = buf.len() as _;
                continue;
            }
        }

        let err = inflateEnd(stream);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        output
    });
}

#[test]
fn window_match_bug() {
    // this hit some invalid logic in the inflate `match_` function where invalid bytes were copied
    // to the current output position.
    let input = &include_bytes!("test-data/window-match-bug.zraw");

    let window_bits = -10;

    assert_eq_rs_ng!({
        let mut output: Vec<u8> = Vec::with_capacity(1 << 15);
        let mut buf = [0; 402];

        let mut stream = MaybeUninit::<z_stream>::zeroed();

        let err = unsafe {
            inflateInit2_(
                stream.as_mut_ptr(),
                window_bits,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as c_int,
            )
        };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let stream = unsafe { stream.assume_init_mut() };

        stream.next_in = input.as_ptr() as *mut u8;
        stream.avail_in = input.len() as _;

        loop {
            stream.next_out = buf.as_mut_ptr();
            stream.avail_out = buf.len() as _;

            let err = unsafe { inflate(stream, InflateFlush::Finish as _) };

            output.extend(&buf[..buf.len() - stream.avail_out as usize]);

            match ReturnCode::from(err) {
                ReturnCode::BufError => {
                    assert_eq!(stream.avail_out, 0);
                    stream.avail_out = buf.len() as _;
                }
                ReturnCode::StreamEnd => break,
                other => panic!("unexpected {other:?}"),
            }
        }

        let err = inflateEnd(stream);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        output
    });
}

#[test]
fn op_len_edge_case() {
    let window_bits = -9;

    let input = &include_bytes!("test-data/op-len-edge-case.zraw");

    assert_eq_rs_ng!({
        let mut output: Vec<u8> = Vec::with_capacity(1 << 15);

        let mut buf = [0; 266];

        let mut stream = MaybeUninit::<z_stream>::zeroed();

        let err = unsafe {
            inflateInit2_(
                stream.as_mut_ptr(),
                window_bits,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as c_int,
            )
        };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let stream = unsafe { stream.assume_init_mut() };

        stream.next_in = input.as_ptr() as *mut u8;
        stream.avail_in = input.len() as _;

        while stream.avail_in != 0 {
            stream.next_out = buf.as_mut_ptr();
            stream.avail_out = buf.len() as _;

            let err = unsafe { inflate(stream, InflateFlush::NoFlush as _) };

            if ReturnCode::from(err) == ReturnCode::BufError {
                output.extend(&buf[..stream.avail_out as usize]);
                stream.avail_out = buf.len() as _;
                continue;
            }
        }

        let err = inflateEnd(stream);
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        output
    });
}

// Fills the provided buffer with pseudorandom bytes based on the given seed
// Duplicates bytes by `step` in a row
fn prng_bytes(seed: u64, bytes: &mut [u8], step: usize) {
    const M: u64 = 2u64.pow(32);
    const A: u64 = 1664525;
    const C: u64 = 1013904223;
    let mut state = seed;
    for chunk in bytes.chunks_mut(4 * step) {
        state = (A * state + C) % M;
        let rand_bytes = state.to_le_bytes();
        for (i, byte) in chunk.iter_mut().enumerate() {
            *byte = rand_bytes[i / step];
        }
    }
}

#[test]
#[cfg_attr(miri, ignore = "slow")]
fn test_inflate_flush_block() {
    let window_bits = -15; // Raw
    const CHUNK: usize = 16384;

    // Create a compressed vector of random data that's bigger then the zlib block size
    let mut data = vec![0u8; 160000];
    prng_bytes(314159, &mut data, 4);
    let config = DeflateConfig {
        window_bits,
        ..DeflateConfig::default()
    };
    let mut output = vec![0u8; 80000];
    // Compress the data
    let (compressed_data, return_code) = compress_slice(&mut output, &data, config);
    assert_eq!(return_code, ReturnCode::Ok);

    // Log the stream positions and data_type output from libz
    let mut zlib_log = Vec::new();
    {
        let mut stream = MaybeUninit::<libz_sys::z_stream>::zeroed();

        let ret = unsafe {
            libz_sys::inflateInit2_(
                stream.as_mut_ptr(),
                window_bits,
                libz_sys::zlibVersion(),
                core::mem::size_of::<libz_sys::z_stream>() as c_int,
            )
        };
        assert_eq!(ReturnCode::from(ret), ReturnCode::Ok);

        let mut output = vec![0u8; CHUNK * 2];
        let stream = unsafe { stream.assume_init_mut() };
        stream.next_in = compressed_data.as_ptr() as *mut u8;
        stream.avail_in = compressed_data.len() as _;
        loop {
            stream.next_out = output.as_mut_ptr();
            stream.avail_out = output.len() as _;

            let ret = unsafe { libz_sys::inflate(stream, InflateFlush::Block as i32) };

            let log = format!(
                "In:{} Out:{} DT:{}",
                stream.avail_in, stream.avail_out, stream.data_type
            );
            zlib_log.push(log);

            assert_eq!(ReturnCode::from(ret), ReturnCode::Ok);

            if stream.avail_in == 0 {
                break;
            }
        }
    }

    // Log the stream positions and data_type output from libz_rs and compare
    {
        use libz_rs_sys::*;

        let mut stream = MaybeUninit::<z_stream>::zeroed();

        let ret = unsafe {
            inflateInit2_(
                stream.as_mut_ptr(),
                window_bits,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as c_int,
            )
        };
        assert_eq!(ReturnCode::from(ret), ReturnCode::Ok);

        let mut output = vec![0u8; CHUNK * 2];
        let stream = unsafe { stream.assume_init_mut() };
        stream.next_in = compressed_data.as_ptr() as *mut u8;
        stream.avail_in = compressed_data.len() as _;
        loop {
            stream.next_out = output.as_mut_ptr();
            stream.avail_out = output.len() as _;

            let ret = unsafe { inflate(stream, InflateFlush::Block as i32) };

            let log = format!(
                "In:{} Out:{} DT:{}",
                stream.avail_in, stream.avail_out, stream.data_type
            );
            // Compare log entries
            assert_eq!(zlib_log.remove(0), log);

            assert_eq!(ReturnCode::from(ret), ReturnCode::Ok);

            if stream.avail_in == 0 {
                break;
            }
        }
    }
}

#[test]
fn issue_172() {
    const BUF: &[u8] = &[
        31, 139, 8, 0, 0, 0, 0, 0, 0, 3, 75, 173, 40, 72, 77, 46, 73, 77, 81, 200, 47, 45, 41, 40,
        45, 1, 0, 176, 1, 57, 179, 15, 0, 0, 0,
    ];

    assert_eq_rs_ng!({
        let mut exitcode = 0;
        for chunk in 1..BUF.len() {
            let mut ret;
            let mut out = [0u8; 32];

            let mut strm = MaybeUninit::zeroed();

            // first validate the config
            ret = inflateInit2_(strm.as_mut_ptr(), 31, VERSION, STREAM_SIZE);
            assert_eq!(ret, Z_OK);

            let strm = strm.assume_init_mut();

            strm.avail_out = out.len() as _;
            strm.next_in = BUF.as_ptr() as *mut u8;
            strm.next_out = out.as_mut_ptr();
            while ret == Z_OK && strm.total_in < BUF.len() as _ {
                strm.avail_in = if chunk as c_ulong > (BUF.len() as c_ulong - strm.total_in) {
                    (BUF.len() as c_ulong - strm.total_in) as c_uint
                } else {
                    chunk as c_uint
                };
                ret = inflate(strm, Z_NO_FLUSH);
            }
            if ret != Z_STREAM_END {
                eprintln!("Finished with {ret} at chunk size {chunk}\n");
                exitcode = 1;
            }

            if &out[..strm.total_out as usize] != b"expected output" {
                eprintln!("Output did not match at chunk size {chunk}\n");
                exitcode = 1;
            }

            let err = inflateEnd(strm);
            assert_eq!(ReturnCode::from(err), ReturnCode::Ok);
        }

        assert!(exitcode == 0);

        exitcode
    });
}

#[test]
fn header_configured_but_no_gzip() {
    let input: &[u8] = &[0x63, 0x18, 0x68, 0x30, 0xd0, 0x0, 0x0];

    assert_eq_rs_ng!({
        let mut stream = MaybeUninit::<z_stream>::zeroed();

        let init_err = unsafe { inflateInit2_(stream.as_mut_ptr(), 47, VERSION, STREAM_SIZE) };
        assert_eq!(init_err, Z_OK);

        let stream = stream.assume_init_mut();

        let mut header = gz_header {
            text: 0,
            time: 0,
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
            done: 123,
        };

        let ret = unsafe { inflateGetHeader(stream, &mut header) };
        assert_eq!(ReturnCode::from(ret), ReturnCode::Ok);

        stream.avail_in = input.len() as _;
        stream.next_in = input.as_ptr() as *mut _;

        let mut out = [0u8; 32];
        stream.avail_out = out.len() as _;
        stream.next_out = out.as_mut_ptr();

        let ret = unsafe { inflate(stream, InflateFlush::NoFlush as _) };
        assert_eq!(ReturnCode::from(ret), ReturnCode::DataError);

        let ret = unsafe { inflateEnd(stream) };
        assert_eq!(ret, Z_OK);

        // we did set a gzip header, but the data is not gzip.
        assert_eq!(header.done, -1);
    })
}

#[test]
fn issue_232() {
    use libz_rs_sys::*;

    const INPUT: &[u8] = &[
        0x18, 0x57, 0x0a, 0xa8, 0xa8, 0xa8, 0xa8, 0x7e, 0x18, 0x57, 0xa8, 0xa8, 0xa8, 0xa8, 0xa8,
        0xa8, 0x83, 0x83, 0xa8, 0x83, 0x83, 0x83, 0x83, 0x84, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83,
        0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83,
        0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x01, 0x04, 0xa8, 0xa8, 0x18, 0x57, 0x18, 0x0a,
        0x57, 0x4f, 0xa8, 0xa8, 0xa8, 0xa8, 0x11, 0x04, 0xa8, 0xa8, 0xe1, 0xe1, 0xe1, 0xe1, 0xe0,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xc2, 0x83,
        0x83, 0x84, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83,
        0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83,
        0x01, 0x04, 0xa8, 0xa8, 0x18, 0x57, 0x18, 0x0a, 0x57, 0x4f, 0xa8, 0xa8, 0xa8, 0xa8, 0x11,
        0x04, 0xe8, 0xa8, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0x3a, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xc2, 0x83, 0x83, 0x84, 0x83, 0x83, 0x83, 0x83, 0x83,
        0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83,
        0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83,
        0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83,
        0x83, 0x83, 0x83, 0x01, 0x04, 0xa8, 0xa8, 0xa8, 0xa8, 0xa8, 0xa8, 0x11, 0x04, 0xa8, 0xa8,
        0xa8, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x01, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83,
        0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x01, 0x04, 0xa8, 0xa8, 0xa8,
        0xa8, 0xa8, 0xa8, 0x11, 0x04, 0xa8, 0xa8, 0xa8, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x01,
        0x04, 0xa8, 0xa8, 0xa8, 0xa8, 0xa8, 0xa8, 0x11, 0x04, 0xa8, 0xa8, 0x04, 0xa8, 0xa8, 0xa8,
        0xa8, 0xa8, 0xa8, 0x11, 0x04, 0xa8, 0xa8, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83,
        0x83, 0x83, 0x01, 0x04, 0xa8, 0xa8, 0x18, 0x57, 0x18, 0x0a, 0x57, 0x4f, 0xa8, 0xa8, 0xa8,
        0xa8, 0x11, 0x04, 0xe8, 0xa8, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0x3a, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xc2, 0x83, 0x83, 0x84, 0x83, 0x83, 0x83,
        0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83,
        0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x01, 0x04, 0xa8, 0xa8, 0x18,
        0x57, 0x18, 0x0a, 0x57, 0x4f, 0xa8, 0xa8, 0xa8, 0xa8, 0x11, 0x04, 0xa8, 0xa8, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xc2, 0x83, 0x83, 0x84, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83,
        0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83,
        0x83, 0x83, 0x83, 0x01, 0x04, 0xa8, 0xa8, 0x18, 0x57, 0x18, 0x0a, 0x57, 0x4f, 0xa8, 0xa8,
        0x83, 0x83, 0x01, 0x04, 0xa8, 0xa8, 0x18, 0x57, 0x18, 0x0a, 0x57, 0x4f, 0xa8, 0xa8, 0xa8,
        0xa8, 0x11, 0x04, 0xa8, 0xa8, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xc2, 0x83, 0x83, 0x84, 0x83, 0x83, 0x83,
        0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83,
        0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x01, 0x04, 0xa8, 0xa8, 0x18,
        0x57, 0x18, 0x0a, 0x57, 0x4f, 0xa8, 0xa8, 0xa8, 0xa8, 0x11, 0x04, 0xe8, 0xa8, 0xe1, 0xe1,
        0xe1, 0xe1, 0xa8, 0xa8, 0x11, 0x04, 0xe8, 0xa8, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xc2, 0x83, 0x83,
        0x84, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83,
        0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x01,
        0x04, 0xa8, 0xa8, 0x18, 0x57, 0x18, 0x0a, 0x57, 0x4f, 0xa8, 0xa8, 0x83, 0x83, 0x01, 0x04,
        0xa8, 0xa8, 0x18, 0x57, 0x18, 0x0a, 0x57, 0x4f, 0xa8, 0xa8, 0xa8, 0xa8, 0x11, 0x04, 0xa8,
        0xa8, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xc2, 0x83, 0x83, 0x84, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83,
        0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83, 0x83,
        0x83, 0x83, 0x83, 0x83, 0x83, 0x7d, 0xfe, 0x04, 0xa8, 0xa8, 0x18, 0x57, 0x18, 0x0a, 0x57,
        0x4f, 0xa8, 0xa8, 0xa8, 0xa8, 0x11, 0x04, 0x68, 0xa8, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xa8, 0xa8, 0x11, 0x04, 0xe8, 0xa8, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1, 0xe1,
        0xe1, 0xe1, 0xe1, 0xa8, 0xa8, 0xa8, 0xa8, 0x83, 0x61, 0xa8, 0xa8, 0x7e, 0x18, 0x57, 0xa8,
        0xa8, 0xa8, 0xa8, 0xa8, 0xa8, 0x83, 0x83, 0x83, 0x01, 0x04, 0xa8, 0x83, 0xa8, 0xa8, 0xa8,
        0xa8, 0x0e,
    ];

    let mut stream = z_stream::default();

    let err = unsafe {
        inflateInit2_(
            &mut stream,
            15 + 32,
            zlibVersion(),
            core::mem::size_of::<z_stream>() as _,
        )
    };
    assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

    let mut output = vec![0; INPUT.len()];
    let input_len: u64 = INPUT.len().try_into().unwrap();
    stream.next_out = output.as_mut_ptr();
    stream.avail_out = output.len().try_into().unwrap();
    stream.next_in = INPUT.as_ptr();
    stream.avail_in = INPUT.len().try_into().unwrap();

    while input_len.checked_sub(stream.total_in as _).unwrap() > 0 {
        let err = unsafe { inflate(&mut stream, InflateFlush::Finish as _) };
        match ReturnCode::from(err) {
            ReturnCode::StreamEnd => {
                break;
            }
            ReturnCode::BufError | ReturnCode::Ok => {
                let add_space: u32 = Ord::max(1024, output.len().try_into().unwrap());
                output.resize(output.len() + add_space as usize, 0);

                // If extend() reallocates, it may have moved in memory.
                stream.next_out = output.as_mut_ptr();
                stream.avail_out += add_space;
            }
            err => {
                panic!("{err:?}");
            }
        }
    }

    unsafe { inflateEnd(&mut stream) };
}

#[test]
fn blow_up_the_stack_1() {
    // requires a sequence of states that would blow up the stack if inflate is not stack safe.

    const INPUT: &[u8] = include_bytes!("test-data/blow_up_the_stack_1.gz");

    let mut output_ng = vec![0; INPUT.len() * 128];
    let mut output_rs = vec![0; INPUT.len() * 128];

    let config = InflateConfig::default();

    let (_, err) = crate::helpers::uncompress_slice_ng(&mut output_ng, INPUT, config);
    assert_eq!(err, ReturnCode::DataError);

    let (_, err) = uncompress_slice(&mut output_rs, INPUT, config);
    assert_eq!(err, ReturnCode::DataError);
}

#[test]
#[cfg_attr(miri, ignore = "slow")]
fn blow_up_the_stack_2() {
    // requires a sequence of states that would blow up the stack if inflate is not stack safe.

    const INPUT: &[u8] = include_bytes!("test-data/blow_up_the_stack_2.gz");

    let mut output_ng = vec![0; INPUT.len() * 128];
    let mut output_rs = vec![0; INPUT.len() * 128];

    let config = InflateConfig::default();

    let (_, err) = crate::helpers::uncompress_slice_ng(&mut output_ng, INPUT, config);
    assert_eq!(err, ReturnCode::DataError);

    let (_, err) = uncompress_slice(&mut output_rs, INPUT, config);
    assert_eq!(err, ReturnCode::DataError);
}

#[test]
fn codes_used() {
    // -1 is returned on NULL
    assert_eq_rs_ng!({ inflateCodesUsed(core::ptr::null_mut()) });
    assert_eq!(
        unsafe { libz_rs_sys::inflateCodesUsed(core::ptr::null_mut()) },
        c_ulong::MAX
    );

    let inputs = &[
        include_bytes!("test-data/compression-corpus/The fastest WASM zlib.md.gzip-0.gz")
            .as_slice(),
        include_bytes!("test-data/compression-corpus/The fastest WASM zlib.md.gzip-9.gz")
            .as_slice(),
        include_bytes!("test-data/compression-corpus/The fastest WASM zlib.md.gzip-filtered-9.gz")
            .as_slice(),
        include_bytes!("test-data/compression-corpus/The fastest WASM zlib.md.gzip-fixed-9.gz")
            .as_slice(),
        include_bytes!("test-data/compression-corpus/The fastest WASM zlib.md.gzip-huffman-9.gz")
            .as_slice(),
        include_bytes!("test-data/compression-corpus/The fastest WASM zlib.md.gzip-rle-9.gz")
            .as_slice(),
    ];

    for input in inputs {
        assert_eq_rs_ng!({
            let mut output: Vec<u8> = vec![0u8; 1 << 15];
            let mut codes_used = Vec::new();

            let mut stream = MaybeUninit::<z_stream>::zeroed();

            let err = unsafe {
                inflateInit2_(
                    stream.as_mut_ptr(),
                    16 + 15,
                    zlibVersion(),
                    core::mem::size_of::<z_stream>() as c_int,
                )
            };
            assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

            let stream = unsafe { stream.assume_init_mut() };
            codes_used.push(inflateCodesUsed(stream));

            stream.next_out = output.as_mut_ptr();
            stream.avail_out = output.len() as _;

            for chunk in input.chunks(16) {
                stream.next_in = chunk.as_ptr().cast_mut();
                stream.avail_in = chunk.len() as _;

                let err = unsafe { inflate(stream, InflateFlush::NoFlush as _) };
                codes_used.push(inflateCodesUsed(stream));

                if err == ReturnCode::StreamEnd as i32 {
                    break;
                }

                assert_eq!(err, ReturnCode::Ok as i32);
            }

            let err = inflateEnd(stream);
            assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

            output.truncate(stream.total_out as usize);
            (output, codes_used)
        });
    }
}

#[test]
fn test_dict_inflate() {
    // Maximum dictionary size, according to inflateGetDictionary() description.
    const MAX_DICTIONARY_SIZE: usize = 32768;

    let mut dictionary = *b"hello\0";
    let mut hello = *b"hello\0";

    let (compr, dict_id) = assert_eq_rs_ng!({
        let mut compr = [0u8; 1024];

        let mut strm = MaybeUninit::zeroed();

        let ret = unsafe {
            deflateInit_(
                strm.as_mut_ptr(),
                Z_BEST_COMPRESSION,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            )
        };

        let c_stream = unsafe { strm.assume_init_mut() };

        assert_eq!(ReturnCode::from(ret), ReturnCode::Ok);

        let err =
            unsafe { deflateSetDictionary(c_stream, dictionary.as_ptr(), dictionary.len() as _) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let dict_id = c_stream.adler;
        c_stream.avail_out = compr.len() as _;
        c_stream.next_out = compr.as_mut_ptr();

        c_stream.avail_in = hello.len() as _;
        c_stream.next_in = hello.as_mut_ptr();

        let err = unsafe { deflate(c_stream, Z_FINISH) };
        assert_eq!(ReturnCode::from(err), ReturnCode::StreamEnd);

        if err != Z_STREAM_END {
            panic!("deflate should report Z_STREAM_END\n");
        }
        let err = unsafe { deflateEnd(c_stream) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        (compr, dict_id)
    });

    assert_eq_rs_ng!({
        let mut compr = compr;
        let mut uncompr = *b"garbage garbage garbage\0";

        let mut stream = MaybeUninit::<z_stream>::zeroed();

        let err = unsafe {
            inflateInit_(
                stream.as_mut_ptr(),
                zlibVersion(),
                core::mem::size_of::<z_stream>() as c_int,
            )
        };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let d_stream = stream.assume_init_mut();

        d_stream.adler = 0;
        d_stream.avail_in = compr.len() as _;
        d_stream.next_in = compr.as_mut_ptr();

        d_stream.avail_out = uncompr.len() as _;
        d_stream.next_out = uncompr.as_mut_ptr();

        let mut check_dictionary = [0i8; MAX_DICTIONARY_SIZE];

        loop {
            let mut err = unsafe { inflate(d_stream, Z_NO_FLUSH) };

            if err == Z_STREAM_END {
                break;
            }

            if err == Z_NEED_DICT {
                if d_stream.adler != dict_id {
                    panic!("unexpected dictionary");
                }
                err = unsafe {
                    inflateSetDictionary(d_stream, dictionary.as_mut_ptr(), dictionary.len() as _)
                };
            }

            assert_eq!(ReturnCode::from(err), ReturnCode::Ok);
        }

        let mut check_dictionary_len = 0;
        let err = unsafe {
            inflateGetDictionary(d_stream, core::ptr::null_mut(), &mut check_dictionary_len)
        };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        if (check_dictionary_len as usize) < dictionary.len() {
            panic!("bad dictionary length");
        }

        let err = unsafe {
            inflateGetDictionary(
                d_stream,
                check_dictionary.as_mut_ptr().cast(),
                &mut check_dictionary_len,
            )
        };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        assert_eq!(
            dictionary,
            &check_dictionary.map(|c| c as u8)[..dictionary.len()]
        );

        let err = unsafe { inflateEnd(d_stream) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        assert_eq!(uncompr[..hello.len()], hello);
    });
}

mod stable_api {
    use zlib_rs::{
        deflate::{compress_slice, DeflateConfig},
        InflateError, ReturnCode,
    };

    fn with_window_bits(n: i32) {
        let config = DeflateConfig {
            window_bits: n,
            ..DeflateConfig::default()
        };

        let mut output = [0u8; 64];
        let input = "Hello World!";
        let (compressed, ret) = compress_slice(&mut output, input.as_bytes(), config);
        assert_eq!(ret, ReturnCode::Ok);

        let mut decompressed = [0u8; 64];
        let mut inflate = zlib_rs::Inflate::new(n >= 0, n.unsigned_abs() as u8);
        inflate
            .decompress(compressed, &mut decompressed, zlib_rs::InflateFlush::Finish)
            .unwrap();

        assert_eq!(inflate.total_in() as usize, compressed.len());

        assert_eq!(
            &decompressed[..inflate.total_out() as usize],
            input.as_bytes()
        );
    }

    #[test]
    fn raw() {
        with_window_bits(-15);
    }

    #[test]
    fn zlib_header() {
        with_window_bits(15);
    }

    #[test]
    fn gz_header() {
        with_window_bits(16 + 15);
    }

    #[test]
    #[should_panic = "StreamError"]
    fn invalid_config() {
        zlib_rs::Inflate::new(true, 123);
    }

    #[test]
    fn invalid_data() {
        let mut inflate = zlib_rs::Inflate::new(true, 15);

        // Clearly invalid input.
        let compressed = [0xAA; 64];
        let mut decompressed = [0u8; 64];

        let ret = inflate.decompress(
            &compressed,
            &mut decompressed,
            zlib_rs::InflateFlush::Finish,
        );

        assert_eq!(ret, Err(InflateError::DataError));
    }

    #[test]
    fn need_dict() {
        let mut inflate = zlib_rs::Inflate::new(true, 15);

        let compressed = [0x08, 0xb8, 0x0, 0x0, 0x0, 0x1];
        let mut decompressed = [0u8; 64];

        let ret = inflate.decompress(
            &compressed,
            &mut decompressed,
            zlib_rs::InflateFlush::Finish,
        );

        assert_eq!(ret, Err(InflateError::NeedDict { dict_id: 1 }));
    }

    #[test]
    fn reset_reuse() {
        let input1 = "Hello World!";
        let input2 = "Goodbye World!";

        let mut output1 = [0u8; 64];
        let config1 = DeflateConfig {
            window_bits: 15,
            ..DeflateConfig::default()
        };
        let (compressed1, ret) = compress_slice(&mut output1, input1.as_bytes(), config1);
        assert_eq!(ret, ReturnCode::Ok);

        let mut output2 = [0u8; 64];
        let config2 = DeflateConfig {
            window_bits: -15,
            ..DeflateConfig::default()
        };
        let (compressed2, ret) = compress_slice(&mut output2, input2.as_bytes(), config2);
        assert_eq!(ret, ReturnCode::Ok);

        // Start with header enabled.
        let zlib_header_first = true;
        let mut inflate = zlib_rs::Inflate::new(zlib_header_first, 15);

        let mut decompressed1 = [0u8; 64];
        inflate
            .decompress(
                compressed1,
                &mut decompressed1,
                zlib_rs::InflateFlush::Finish,
            )
            .unwrap();

        assert_eq!(inflate.total_in() as usize, compressed1.len());
        assert_eq!(
            &decompressed1[..inflate.total_out() as usize],
            input1.as_bytes()
        );

        // Reset for a *raw* stream: swap the zlib_header flag.
        inflate.reset(!zlib_header_first);

        let mut decompressed2 = [0u8; 64];
        inflate
            .decompress(
                compressed2,
                &mut decompressed2,
                zlib_rs::InflateFlush::Finish,
            )
            .unwrap();

        assert_eq!(inflate.total_in() as usize, compressed2.len());
        assert_eq!(
            &decompressed2[..inflate.total_out() as usize],
            input2.as_bytes()
        );
    }

    #[test]
    #[cfg_attr(
        not(target_pointer_width = "64"),
        ignore = "test only works on 64-bit systems"
    )]
    #[cfg_attr(miri, ignore = "slow")]
    fn buffer_overflows_c_uint() {
        // "Hello World!" compressed at level 0
        let deflated = [
            0x78u8, 0x01, 0x01, 0x0d, 0x00, 0xf2, 0xff, 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x20, 0x57,
            0x6f, 0x72, 0x6c, 0x64, 0x21, 0x0a, 0x20, 0x91, 0x04, 0x48,
        ];

        let mut v = vec![0u8; u32::MAX as usize + deflated.len() / 2];
        v[..deflated.len()].copy_from_slice(&deflated);

        assert!(((v.len() as u32) as usize) < deflated.len());

        let mut decompressed = [0u8; 64];
        let mut inflate = zlib_rs::Inflate::new(true, 15);
        let status = inflate
            .decompress(&v, &mut decompressed, zlib_rs::InflateFlush::Finish)
            .unwrap();

        assert_eq!(status, zlib_rs::Status::StreamEnd);
        assert_eq!(b"Hello World!", &decompressed[..12]);
    }
}

extern "C" fn push(desc: *mut c_void, _buf: *mut c_uchar, _len: c_uint) -> c_int {
    (!desc.is_null()) as _
}

unsafe extern "C" fn pull(desc: *mut c_void, buf: *mut *const c_uchar) -> c_uint {
    static mut NEXT: c_uint = 0;
    static DAT: [c_uchar; 4] = [0x63 /* 99 */, 0, 2, 0];

    let strm = desc.cast::<libz_rs_sys::z_stream>();
    if strm.is_null() {
        NEXT = 0;
        return 0; // no input (already provided at next_in)
    };

    // In `infback` this is an unreachable state, triggering a StreamError.
    zlib_rs::inflate::set_mode_sync(strm);

    if (NEXT as usize) < DAT.len() {
        *buf = DAT.as_ptr().add(NEXT as usize);
        NEXT += 1;

        1
    } else {
        0
    }
}

/// Cover `inflateBack` edge cases.
#[test]
fn cover_back() {
    let mut window = [0u8; 32768];

    let ret = assert_eq_rs_ng!({
        inflateBackInit_(
            core::ptr::null_mut(),
            0,
            window.as_mut_ptr(),
            core::ptr::null(),
            0,
        )
    });
    assert_eq!(ret, Z_VERSION_ERROR);

    let ret = assert_eq_rs_ng!({
        inflateBackInit_(
            core::ptr::null_mut(),
            0,
            window.as_mut_ptr(),
            VERSION,
            STREAM_SIZE,
        )
    });
    assert_eq!(ret, Z_STREAM_ERROR);

    let ret = unsafe {
        libz_rs_sys::inflateBack(
            core::ptr::null_mut(),
            None,
            core::ptr::null_mut(),
            None,
            core::ptr::null_mut(),
        )
    };
    assert_eq!(ret, Z_STREAM_ERROR);

    let ret = assert_eq_rs_ng!({ inflateBackEnd(core::ptr::null_mut(),) });
    assert_eq!(ret, Z_STREAM_ERROR);

    unsafe {
        use libz_rs_sys::*;

        let mut stream = mem_setup();
        let ret = inflateBackInit_(&mut stream, 15, window.as_mut_ptr(), VERSION, STREAM_SIZE);
        assert_eq!(ret, Z_OK);

        let mut input = *b"\x03\0";
        stream.avail_in = input.len() as _;
        stream.next_in = input.as_mut_ptr();

        let ret = libz_rs_sys::inflateBack(
            &mut stream,
            Some(pull),
            core::ptr::null_mut(),
            Some(push),
            core::ptr::null_mut(),
        );
        assert_eq!(ret, Z_STREAM_END);

        // Force output error.
        let mut input = *b"\x63\0\0";
        stream.avail_in = input.len() as _;
        stream.next_in = input.as_mut_ptr();
        let ret = {
            let ptr = &mut stream as *mut z_stream;
            libz_rs_sys::inflateBack(
                ptr,
                Some(pull),
                core::ptr::null_mut(),
                Some(push),
                ptr.cast(), // Cursed!
            )
        };
        assert_eq!(ret, Z_BUF_ERROR);

        // Force mode error by mucking with state. We pass a pointer to the stream state twice, so
        // this test is actually UB (violating aliasing), which miri will detect.
        #[cfg(not(miri))]
        {
            let ret = {
                let ptr = { &mut stream as *mut z_stream };
                libz_rs_sys::inflateBack(
                    ptr,
                    Some(pull),
                    ptr.cast(), // Cursed!
                    Some(push),
                    core::ptr::null_mut(),
                )
            };
            assert_eq!(ret, Z_STREAM_ERROR);
        }

        let ret = { inflateBackEnd(&mut stream) };
        assert_eq!(ret, Z_OK);
        mem_done(&mut stream);

        let ret = inflateBackInit_(&mut stream, 15, window.as_mut_ptr(), VERSION, STREAM_SIZE);
        assert_eq!(ret, Z_OK);
        let ret = { inflateBackEnd(&mut stream) };
        assert_eq!(ret, Z_OK);
    }
}

#[test]
fn done_state_returns_stream_end() {
    use std::mem::size_of;

    let input = [
        31u8, 139, 8, 0, 0, 0, 0, 0, 0, 3, 203, 72, 205, 201, 201, 87, 40, 207, 47, 202, 73, 81,
        200, 0, 179, 33, 36, 68, 4, 89, 28, 137, 13, 0, 181, 147, 9, 162, 53, 0, 0, 0,
    ];

    assert_eq_rs_ng!({
        let mut out: [u8; 64] = [0; 64];

        let mut stream = MaybeUninit::<z_stream>::zeroed();

        let ret = unsafe {
            inflateInit2_(
                stream.as_mut_ptr(),
                15 + 16,
                zlibVersion(),
                size_of::<z_stream>() as _,
            )
        };
        assert_eq!(ret, Z_OK);

        let stream = stream.assume_init_mut();

        // The first call returns Z_STREAM_END
        stream.avail_in = input.len() as _;
        stream.next_in = input.as_ptr().cast_mut();
        stream.avail_out = out.len() as _;
        stream.next_out = out.as_mut_ptr();

        // The mode will be Mode::Done at this point.
        assert_eq!(unsafe { inflate(stream, Z_NO_FLUSH) }, Z_STREAM_END);

        // A second call to inflate returns StreamEnd
        stream.avail_in = input.len() as _;
        stream.next_in = input.as_ptr().cast_mut();
        stream.avail_out = out.len() as _;
        stream.next_out = out.as_mut_ptr();

        assert_eq!(unsafe { inflate(stream, Z_FINISH) }, Z_STREAM_END);
    });
}

#[test]
fn inflate_copy_after_half_input() {
    let input = include_bytes!("test-data/compression-corpus/The fastest WASM zlib.md.gzip-9.gz");

    let _ = assert_eq_rs_ng!({
        let mut stream = MaybeUninit::<z_stream>::zeroed();
        let ret = unsafe {
            inflateInit2_(
                stream.as_mut_ptr(),
                16 + 15,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as i32,
            )
        };
        let stream = stream.assume_init_mut();
        assert_eq!(ret, Z_OK);

        let mut out = vec![0u8; 16 * 1024];

        // First, decompress only the first half of the compressed input.
        let half = input.len() / 2;

        stream.next_in = input.as_ptr() as *mut _;
        stream.avail_in = half as _;
        stream.next_out = out.as_mut_ptr();
        stream.avail_out = out.len() as _;

        loop {
            let ret = unsafe { inflate(stream, InflateFlush::NoFlush as _) };

            assert!(
                matches!(ret, Z_OK | Z_BUF_ERROR | Z_STREAM_END),
                "unexpected inflate return: {}",
                ret
            );

            if matches!(ret, Z_STREAM_END) {
                unreachable!("we only provide half of the input")
            }

            if stream.avail_in == 0 {
                break;
            }

            if stream.avail_out == 0 {
                unreachable!("there is enough output space")
            }
        }

        // At this point, we’ve decompressed part (or possibly all) of the stream.
        let prefix_len = stream.total_out as usize;

        // Make a copy of the inflate state *after* half the input has been processed.
        let mut copy = MaybeUninit::<z_stream>::zeroed();
        let ret = unsafe { inflateCopy(copy.as_mut_ptr(), stream) };
        let copy = copy.assume_init_mut();
        assert_eq!(ret, Z_OK);

        // Prepare a separate output buffer for the copy, and copy the already-produced prefix
        let mut out_copy = vec![0u8; 16 * 1024];
        out_copy[..prefix_len].copy_from_slice(&out[..prefix_len]);

        // The original stream already has next_out pointing at out[prefix_len].
        // For the copy, we need to point it at the corresponding location in out_copy.
        copy.next_out = unsafe { out_copy.as_mut_ptr().add(prefix_len) };
        copy.avail_out = (out_copy.len() - prefix_len) as _;

        // Now feed the *remainder* of the compressed input to both streams.
        let remaining = input.len() - half;
        if remaining > 0 {
            stream.next_in = unsafe { input.as_ptr().add(half) as *mut _ };
            stream.avail_in = remaining as _;

            copy.next_in = stream.next_in;
            copy.avail_in = stream.avail_in;
        }

        // Decompress the remainder in lockstep on both the original and the copy.
        loop {
            let ret1 = unsafe { inflate(stream, InflateFlush::NoFlush as _) };
            let ret2 = unsafe { inflate(copy, InflateFlush::NoFlush as _) };

            // Both streams should behave identically
            assert_eq!(ret1, ret2);
            assert!(
                matches!(ret1, Z_OK | Z_BUF_ERROR | Z_STREAM_END),
                "unexpected inflate return: {ret}",
            );

            // Their accounting should remain in sync at all times
            assert_eq!(stream.total_out, copy.total_out);
            assert_eq!(stream.total_in, copy.total_in);

            if matches!(ret1, Z_STREAM_END) {
                break;
            }

            // If we somehow run out of input or output space, bail
            if stream.avail_in == 0 && copy.avail_in == 0 {
                break;
            }
            if stream.avail_out == 0 || copy.avail_out == 0 {
                break;
            }
        }

        assert_eq!(unsafe { inflateEnd(stream) }, Z_OK);
        assert_eq!(unsafe { inflateEnd(copy) }, Z_OK);

        let total = stream.total_out as usize;
        assert_eq!(total, copy.total_out as usize);

        assert_eq!(&out[..total], &out_copy[..total]);

        out[..total].to_vec()
    });
}

#[test]
fn inflate_validate_toggles_checksum_update() {
    let input = include_bytes!("test-data/compression-corpus/The fastest WASM zlib.md.gzip-9.gz");

    // Check that `inflateValidate` toggles checksum updating.
    //
    // - decompress normally, the checksum should update
    // - toggle, the checksum should not update
    // - toggle again, the checksum should update
    assert_eq_rs_ng!({
        let mut stream = MaybeUninit::<z_stream>::zeroed();
        let ret = inflateInit2_(
            stream.as_mut_ptr(),
            16 + 15, // gzip + max window
            zlibVersion(),
            core::mem::size_of::<z_stream>() as c_int,
        );
        assert_eq!(ret, Z_OK);

        let stream = stream.assume_init_mut();

        let mut out1 = vec![0u8; 16 * 1024];
        let mut out2 = vec![0u8; 16 * 1024];
        let mut out3 = vec![0u8; 16 * 1024];

        let check1 = {
            let ret = inflateValidate(stream, 1);
            assert_eq!(ret, Z_OK);

            stream.next_in = input.as_ptr() as *mut _;
            stream.avail_in = input.len() as _;
            stream.next_out = out1.as_mut_ptr();
            stream.avail_out = out1.len() as _;

            let ret = loop {
                let ret = inflate(stream, InflateFlush::NoFlush as _);

                assert!(
                    matches!(ret, Z_OK | Z_BUF_ERROR | Z_STREAM_END),
                    "unexpected inflate return (run 1): {}",
                    ret
                );

                if matches!(ret, Z_STREAM_END) {
                    break ret;
                }

                if stream.avail_in == 0 {
                    break ret;
                }

                if stream.avail_out == 0 {
                    unreachable!("run 1: not enough output space");
                }
            };

            assert_eq!(ret, Z_STREAM_END);
            out1.truncate(stream.total_out as usize);
            stream.adler
        };

        assert_eq!(inflateReset(stream), Z_OK);

        let check2 = {
            let ret = inflateValidate(stream, 0);
            assert_eq!(ret, Z_OK);

            stream.next_in = input.as_ptr() as *mut _;
            stream.avail_in = input.len() as _;
            stream.next_out = out2.as_mut_ptr();
            stream.avail_out = out2.len() as _;

            let ret = loop {
                let ret = inflate(stream, InflateFlush::NoFlush as _);

                assert!(
                    matches!(ret, Z_OK | Z_BUF_ERROR | Z_STREAM_END),
                    "unexpected inflate return (run 2): {}",
                    ret
                );

                if matches!(ret, Z_STREAM_END) {
                    break ret;
                }

                if stream.avail_in == 0 {
                    break ret;
                }

                if stream.avail_out == 0 {
                    unreachable!("run 2: not enough output space");
                }
            };

            assert_eq!(ret, Z_STREAM_END);
            out2.truncate(stream.total_out as usize);
            stream.adler
        };

        // Output must be identical, regardless of validation.
        assert_eq!(out1, out2);

        assert_ne!(
            check1, check2,
            "checksum with validation disabled unexpectedly matches the validated checksum",
        );

        assert_eq!(inflateReset(stream), Z_OK);

        let check3 = {
            let ret = inflateValidate(stream, 0);
            assert_eq!(ret, Z_OK);
            let ret = inflateValidate(stream, 1);
            assert_eq!(ret, Z_OK);

            stream.next_in = input.as_ptr() as *mut _;
            stream.avail_in = input.len() as _;
            stream.next_out = out3.as_mut_ptr();
            stream.avail_out = out3.len() as _;

            let ret = loop {
                let ret = inflate(stream, InflateFlush::NoFlush as _);

                assert!(
                    matches!(ret, Z_OK | Z_BUF_ERROR | Z_STREAM_END),
                    "unexpected inflate return (run 3): {}",
                    ret
                );

                if matches!(ret, Z_STREAM_END) {
                    break ret;
                }

                if stream.avail_in == 0 {
                    break ret;
                }

                if stream.avail_out == 0 {
                    unreachable!("run 3: not enough output space");
                }
            };

            assert_eq!(ret, Z_STREAM_END);
            out3.truncate(stream.total_out as usize);
            stream.adler
        };

        assert_eq!(out1, out3);

        // With validation back on, checksum should again match the first run
        assert_eq!(
            check1, check3,
            "checksum with validation re-enabled does not match validated run"
        );

        assert_eq!(inflateEnd(stream), Z_OK);
    });
}
