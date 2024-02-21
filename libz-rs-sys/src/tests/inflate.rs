use std::mem::ManuallyDrop;

use crate as libz_rs_sys;

use std::ffi::{c_char, c_int, c_void};

use libz_rs_sys::*;
use zlib_rs::deflate::compress_slice;
use zlib_rs::inflate::{uncompress_slice, INFLATE_STATE_SIZE};
use zlib_rs::{Flush, MAX_WBITS};

const VERSION: *const c_char = "2.3.0\0".as_ptr() as *const c_char;
const STREAM_SIZE: c_int = std::mem::size_of::<libz_rs_sys::z_stream>() as c_int;

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

    // perform the allocation
    let ptr = unsafe { libc::malloc(len) };
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
    if mem.is_null() {
        unsafe { libc::free(ptr) };
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

    unsafe { libc::free(ptr) }
}

fn mem_setup() -> z_stream {
    let zone = MemZone {
        items: Vec::new(),
        total: 0,
        highwater: 0,
        limit: 0,
        not_lifo: 0,
        rogue: 0,
    };
    let zone = Box::new(zone);

    let stream = z_stream {
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

fn mem_limit(stream: &mut z_stream, limit: usize) {
    assert!(!stream.opaque.is_null());

    let mut zone = ManuallyDrop::new(unsafe { Box::from_raw(stream.opaque as *mut MemZone) });
    zone.limit = limit;
}

fn mem_done(stream: &mut z_stream) {
    assert!(!stream.opaque.is_null());

    // zone not wrapped in ManuallyDrop so it is free'd
    let mut zone = unsafe { Box::from_raw(stream.opaque as *mut MemZone) };

    let count = zone.items.len();

    for item in zone.items.drain(..) {
        unsafe { libc::free(item.ptr) };
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
    let input = &[
        0x1f, 0x8b, 0x08, 0x1f, 0x44, 0x0a, 0x45, 0x65, 0x00, 0x03, 0x0e, 0x00, 0x54, 0x47, 0x0a,
        0x00, 0x45, 0x58, 0x54, 0x52, 0x41, 0x20, 0x44, 0x41, 0x54, 0x41, 0x74, 0x65, 0x73, 0x74,
        0x2e, 0x74, 0x78, 0x74, 0x00, 0x41, 0x20, 0x63, 0x6f, 0x6d, 0x6d, 0x65, 0x6e, 0x74, 0x00,
        0x3b, 0x92, 0x2b, 0x49, 0x2d, 0x2e, 0xe1, 0x02, 0x00, 0xc6, 0x35, 0xb9, 0x3b, 0x05, 0x00,
        0x00, 0x00,
    ];
    let _what = "gzip header parsing";
    let step = 0;
    let win = 47;
    let len = 1;
    let err = Z_DATA_ERROR;

    let mut err = Some(err);
    let mut stream = mem_setup();

    let init_err = unsafe { inflateInit2_(&mut stream, win, VERSION, STREAM_SIZE) };
    if init_err != Z_OK {
        mem_done(&mut stream);
        return;
    }

    let mut out = vec![0u8; len];

    let extra: [u8; 64] = [0; 64];
    let name: [u8; 64] = [0; 64];
    let comment: [u8; 64] = [0; 64];

    // Set header
    // See: https://www.zlib.net/manual.html
    let mut header = gz_header {
        text: 0,
        time: 0,
        xflags: 0,
        os: 0,
        extra: extra.as_ptr() as *mut u8,
        extra_len: 0,
        extra_max: 1024,
        name: name.as_ptr() as *mut u8,
        name_max: 64, // How / where should this be set?
        comment: comment.as_ptr() as *mut u8,
        comm_max: 64,
        hcrc: 0,
        done: 0,
    };

    let err = unsafe { inflateGetHeader(&mut stream, &mut header) };
    assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

    let mut have = input.len();
    let step = if step == 0 || step > have { have } else { step };

    stream.avail_in = step as _;
    have -= step;
    stream.next_in = input.as_ptr() as *mut _;

    loop {
        stream.avail_out = len as _;
        stream.next_out = out.as_mut_ptr();

        let ret = unsafe { inflate(&mut stream, Flush::NoFlush as _) };
        if let Some(err) = err {
            assert_eq!(ret, err)
        }

        println!("{:?}", header);
        assert_eq!(header.text, 1);

        if !matches!(ret, Z_OK | Z_BUF_ERROR | Z_NEED_DICT) {
            break;
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

    /*
    unsafe {
    //    println!("name {:?}", *header.name);
          assert_eq!(*header.name, "test.txt");
    }
    */
    //assert_eq!(true, false);

    let ret = unsafe { inflateReset2(&mut stream, -8) };
    assert_eq!(ret, Z_OK);

    let ret = unsafe { inflateEnd(&mut stream) };
    assert_eq!(ret, Z_OK);

    mem_done(&mut stream);
}

fn inf(input: &[u8], _what: &str, step: usize, win: i32, len: usize, err: c_int) {
    let mut err = Some(err);

    let mut stream = mem_setup();

    let init_err = unsafe { inflateInit2_(&mut stream, win, VERSION, STREAM_SIZE) };
    if init_err != Z_OK {
        mem_done(&mut stream);
        return;
    }

    let mut out = vec![0u8; len];

    let extra: [u8; 1024] = [0; 1024];
    let name: [u8; 64] = [0; 64];
    let comment: [u8; 64] = [0; 64];

    // Set header
    // See: https://www.zlib.net/manual.html
    let mut header = gz_header {
        text: 0,
        time: 0,
        xflags: 0,
        os: 0,
        extra: extra.as_ptr() as *mut u8,
        extra_len: 0,
        extra_max: 1024,
        name: name.as_ptr() as *mut u8,
        name_max: 64, // How / where should this be set?
        comment: comment.as_ptr() as *mut u8,
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

        let ret = unsafe { inflate(&mut stream, Flush::NoFlush as _) };

        if let Some(err) = err {
            if err != 9 {
                assert_eq!(ret, err)
            }
        }

        if !matches!(ret, Z_OK | Z_BUF_ERROR | Z_NEED_DICT) {
            break;
        }

        if matches!(ret, Z_NEED_DICT) {
            todo!("need dict");
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
    let mut stream = mem_setup();

    let ret = unsafe { inflateInit_(&mut stream, std::ptr::null(), 0) };
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
    assert_eq!(unsafe { inflate(std::ptr::null_mut(), 0) }, Z_STREAM_ERROR);
    assert_eq!(unsafe { inflateEnd(std::ptr::null_mut()) }, Z_STREAM_ERROR);

    let mut strm = mem_setup();
    strm.avail_in = 0;
    strm.next_in = std::ptr::null_mut();
    let mut ret = unsafe { inflateInit2_(&mut strm, -8, VERSION, STREAM_SIZE) };
    let mut input = [0x63, 0x00];
    strm.avail_in = input.len() as _;
    strm.next_in = input.as_mut_ptr().cast();
    strm.avail_out = 1;
    strm.next_out = (&mut ret) as *mut _ as *mut u8;
    mem_limit(&mut strm, 1);
    ret = unsafe { inflate(&mut strm, Flush::NoFlush as _) };
    assert_eq!(ret, Z_MEM_ERROR);
    ret = unsafe { inflate(&mut strm, Flush::NoFlush as _) };
    assert_eq!(ret, Z_MEM_ERROR);
    mem_limit(&mut strm, 0);

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

    ret = unsafe { inflate(&mut strm, Flush::NoFlush as _) };
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
#[ignore = "gzip"]
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
fn try_inflate(input: &[u8], err: c_int) -> c_int {
    let len = input.len();

    /* allocate work areas */
    let size = len << 3;
    let mut out = vec![0; size];
    // let mut win = vec![0; 32768];

    //    /* first with inflate */
    //    strcpy(prefix, id);
    //    strcat(prefix, "-late");

    let mut strm = mem_setup();
    strm.avail_in = 0;
    strm.next_in = std::ptr::null_mut();

    let mut ret = unsafe {
        inflateInit2_(
            &mut strm,
            if err < 0 { 47 } else { -15 },
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

        ret = unsafe { inflate(&mut strm, Flush::Trees as _) };
        assert!(!matches!(ret, Z_STREAM_ERROR | Z_MEM_ERROR));

        if matches!(ret, Z_DATA_ERROR | Z_NEED_DICT) {
            break;
        }

        if !(strm.avail_in > 0 || strm.avail_out == 0) {
            break;
        }
    }

    if err != Z_OK {
        assert_eq!(ret, Z_DATA_ERROR);
    }

    unsafe { inflateEnd(&mut strm) };
    mem_done(&mut strm);

    //    /* then with inflateBack */
    //    if (err >= 0) {
    //        strcpy(prefix, id);
    //        strcat(prefix, "-back");
    //        mem_setup(&strm);
    //        ret = PREFIX(inflateBackInit)(&strm, 15, win);
    //        assert(ret == Z_OK);
    //        strm.avail_in = len;
    //        strm.next_in = in;
    //        ret = PREFIX(inflateBack)(&strm, pull, NULL, push, NULL);
    //        assert(ret != Z_STREAM_ERROR);
    //        if (err && ret != Z_BUF_ERROR) {
    //            assert(ret == Z_DATA_ERROR);
    //            assert(strcmp(id, strm.msg) == 0);
    //        }
    //        PREFIX(inflateBackEnd)(&strm);
    //        mem_done(&strm, prefix);
    //    }

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
    let mut dest_vec = vec![0u8; 1 << 16];

    let mut dest_len = dest_vec.len() as c_ulong;
    let dest = dest_vec.as_mut_ptr();

    let source = input.as_ptr();
    let source_len = input.len() as _;

    let err = unsafe { uncompress(dest, &mut dest_len, source, source_len) };

    if err != 0 {
        panic!("error {:?}", libz_rs_sys::ReturnCode::from(err));
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
    let deflated = [120, 1, 1, 2, 0, 253, 255, 6, 10, 0, 24, 0, 17];

    let output = uncompress_help(&deflated);
    assert_eq!(String::from_utf8(output).unwrap(), "\u{6}\n");
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
fn small_window_ours() {
    const N: usize = 128;

    const PLAIN: [u8; N] = {
        let mut i = 0;
        let mut plain = [0; N];
        while i < plain.len() {
            plain[i] = i as u8;
            i += 1;
        }
        plain
    };

    let dictionary1 = [b'a'; (1 << 9) - N];

    let mut stream = libz_ng_sys::z_stream {
        next_in: std::ptr::null_mut(),
        avail_in: 0,
        total_in: 0,
        next_out: std::ptr::null_mut(),
        avail_out: 0,
        total_out: 0,
        msg: std::ptr::null_mut(),
        state: std::ptr::null_mut(),
        zalloc: zlib_rs::allocate::zcalloc,
        zfree: zlib_rs::allocate::zcfree,
        opaque: std::ptr::null_mut(),
        data_type: 0,
        adler: 0,
        reserved: 0,
    };

    unsafe {
        let err = libz_ng_sys::deflateInit2_(
            &mut stream,
            libz_ng_sys::Z_BEST_COMPRESSION,
            libz_ng_sys::Z_DEFLATED,
            -9,
            8,
            libz_ng_sys::Z_DEFAULT_STRATEGY,
            b"1.3.0\0".as_ptr() as *const std::ffi::c_char,
            std::mem::size_of::<libz_ng_sys::z_stream>() as i32,
        );

        assert_eq!(err, Z_OK);
    };

    unsafe {
        let err = libz_ng_sys::deflateSetDictionary(
            &mut stream,
            dictionary1.as_ptr(),
            std::mem::size_of_val(&dictionary1) as u32,
        );

        assert_eq!(err, Z_OK);
    };

    unsafe {
        let err = libz_ng_sys::deflateSetDictionary(
            &mut stream,
            PLAIN.as_ptr(),
            std::mem::size_of_val(&PLAIN) as u32,
        );

        assert_eq!(err, Z_OK);
    };

    let mut plain = PLAIN;
    stream.next_in = plain.as_mut_ptr();
    stream.avail_in = plain.len() as _;

    let mut compr = [0; N];
    stream.next_out = compr.as_mut_ptr();
    stream.avail_out = compr.len() as _;

    let err = unsafe { libz_ng_sys::deflate(&mut stream, libz_ng_sys::Z_FINISH) };
    assert_eq!(err, Z_STREAM_END);

    let err = unsafe { libz_ng_sys::deflateEnd(&mut stream) };
    assert_eq!(err, Z_OK,);

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

    let err = unsafe { inflateInit2_(&mut stream, -9, VERSION, STREAM_SIZE) };
    assert_eq!(err, Z_OK);

    let err = unsafe {
        inflateSetDictionary(&mut stream, dictionary1.as_ptr(), dictionary1.len() as u32)
    };
    assert_eq!(err, Z_OK);

    let err = unsafe { inflateSetDictionary(&mut stream, plain.as_ptr(), plain.len() as u32) };
    assert_eq!(err, Z_OK);

    stream.avail_in = compr.len() as _;
    stream.next_in = compr.as_mut_ptr();

    let mut plain_again = [0; N];
    stream.avail_out = plain_again.len() as _;
    stream.next_out = plain_again.as_mut_ptr();

    let err = unsafe { inflate(&mut stream, Flush::NoFlush as _) };
    assert_eq!(err, Z_STREAM_END);

    let err = unsafe { inflateEnd(&mut stream) };
    assert_eq!(err, Z_OK);

    assert_eq!(plain, plain_again);
}

#[test]
fn inflate_adler() {
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

    let err = unsafe { inflateInit2(&mut stream, 32 + MAX_WBITS) };
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
