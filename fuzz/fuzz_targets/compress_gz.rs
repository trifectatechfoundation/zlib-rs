#![cfg_attr(not(miri), no_main)]

use std::{
    ffi::{c_int, c_uint, c_ulong, CString},
    mem::{size_of, MaybeUninit},
};

use libfuzzer_sys::{
    arbitrary::{self, Arbitrary},
    fuzz_target,
};

use zlib_rs::{deflate::DeflateConfig, DeflateFlush, ReturnCode};

#[derive(Debug, Arbitrary, Clone)]
struct GzHeaderData {
    text: i32,
    time: c_ulong,
    os: i32,
    extra: Vec<u8>,
    name: CString,
    comment: CString,
    hcrc: i32,
}

impl GzHeaderData {
    fn as_gz_header(&mut self) -> libz_rs_sys::gz_header {
        libz_rs_sys::gz_header {
            text: self.text,
            time: self.time,
            xflags: 0,
            os: self.os,
            extra: self.extra.as_mut_ptr(),
            extra_len: self.extra.len().try_into().unwrap(),
            extra_max: 0,                              // doesn't matter for writing.
            name: self.name.as_ptr() as *mut u8, // hack: UB if written to, but we shouldn't write during deflate.
            name_max: 0,                         // doesn't matter for writing.
            comment: self.comment.as_ptr() as *mut u8, // hack: UB if written to, but we shouldn't write during deflate.
            comm_max: 0,                               // doesn't matter for writing.
            hcrc: self.hcrc,
            done: 0, // doesn't matter for writing.
        }
    }
}

#[derive(Debug, Arbitrary)]
struct Input {
    source: String,
    config: DeflateConfig,
    flush: DeflateFlush,
    header: GzHeaderData,
}

fuzz_target!(|input: Input| {
    let ng = compress_gz_ng(&input);
    let rs = compress_gz_rs(&input);

    assert_eq!(ng, rs)
});

fn compress_gz_rs(input: &Input) -> Option<Vec<u8>> {
    use libz_rs_sys::*;

    let Input {
        ref source,
        config,
        flush,
        ref header,
    } = input;

    let mut header = header.clone();

    // Initialize stream.
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
            size_of::<z_stream>() as c_int,
        )
    };

    if err != ReturnCode::Ok as i32 {
        return None;
    }

    let streamp = unsafe { stream.assume_init_mut() };

    // Create header.
    let mut header = header.as_gz_header();
    let err = unsafe { deflateSetHeader(streamp, &mut header as gz_headerp) };
    if err != ReturnCode::Ok as i32 {
        // Deallocate, so that we don't trigger ASAN leak detector.
        let err = unsafe { deflateEnd(streamp) };
        assert_eq!(err, ReturnCode::Ok as i32);
        return None;
    }

    let bound = unsafe { deflateBound(streamp, source.len() as _) };
    let buf_size = match flush {
        DeflateFlush::NoFlush | DeflateFlush::Finish => bound,
        // Other flush options might require more than `bound` bytes, so add a safety margin. We
        // _could_ catch Z_BUF_ERROR to allocate more memory, but that's not interesting right now.
        // (In fact, it's more interesting if NoFlush/Finish triggers UB write if we're
        // miscalculating)
        DeflateFlush::PartialFlush
        | DeflateFlush::SyncFlush
        | DeflateFlush::FullFlush
        | DeflateFlush::Block => bound * 2,
    };

    // We deliberately use uninitialized memory as the input buffer here, to simulate how these
    // functions will likely be used from C, and to catch us ever reading uninitialized memory.
    let mut dest: Vec<u8> = Vec::with_capacity(buf_size as usize);

    let max = c_uint::MAX as usize;

    let mut left = dest.capacity();
    let mut source_len = source.len();

    streamp.next_in = source.as_ptr().cast_mut().cast();
    streamp.next_out = dest.as_mut_ptr().cast();

    loop {
        if streamp.avail_out == 0 {
            streamp.avail_out = Ord::min(left, max) as _;
            left -= streamp.avail_out as usize;
        }

        if streamp.avail_in == 0 {
            streamp.avail_in = Ord::min(source_len, max) as _;
            source_len -= streamp.avail_in as usize;
        }

        let flush = if source_len > 0 {
            *flush
        } else {
            DeflateFlush::Finish
        };

        let err = unsafe { deflate(streamp, flush as i32) };
        if ReturnCode::from(err) != ReturnCode::Ok {
            break;
        }
    }

    unsafe { dest.set_len(streamp.total_out as usize) }

    let err = unsafe { deflateEnd(streamp) };
    assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

    Some(dest)
}

fn compress_gz_ng(input: &Input) -> Option<Vec<u8>> {
    use libz_ng_sys::*;

    let Input {
        ref source,
        config,
        flush,
        ref header,
    } = input;

    let mut header = header.clone();

    // Initialize stream.
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
            size_of::<z_stream>() as c_int,
        )
    };

    if err != ReturnCode::Ok as i32 {
        return None;
    }

    let streamp = unsafe { stream.assume_init_mut() };

    // Create header. The layout is the same between zlib-rs and zlib-ng. 
    let mut header = unsafe { core::mem::transmute::<libz_rs_sys::gz_header, gz_header>(header.as_gz_header()) };

    let err = unsafe { deflateSetHeader(streamp, &mut header as gz_headerp) };
    if err != ReturnCode::Ok as i32 {
        // Deallocate, so that we don't trigger ASAN leak detector.
        let err = unsafe { deflateEnd(streamp) };
        assert_eq!(err, ReturnCode::Ok as i32);
        return None;
    }

    let bound = unsafe { deflateBound(streamp, source.len() as _) };
    let buf_size = match flush {
        DeflateFlush::NoFlush | DeflateFlush::Finish => bound,
        // Other flush options might require more than `bound` bytes, so add a safety margin. We
        // _could_ catch Z_BUF_ERROR to allocate more memory, but that's not interesting right now.
        // (In fact, it's more interesting if NoFlush/Finish triggers UB write if we're
        // miscalculating)
        DeflateFlush::PartialFlush
        | DeflateFlush::SyncFlush
        | DeflateFlush::FullFlush
        | DeflateFlush::Block => bound * 2,
    };

    // We deliberately use uninitialized memory as the input buffer here, to simulate how these
    // functions will likely be used from C, and to catch us ever reading uninitialized memory.
    let mut dest: Vec<u8> = Vec::with_capacity(buf_size as usize);

    let max = c_uint::MAX as usize;

    let mut left = dest.capacity();
    let mut source_len = source.len();

    streamp.next_in = source.as_ptr().cast_mut().cast();
    streamp.next_out = dest.as_mut_ptr().cast();

    loop {
        if streamp.avail_out == 0 {
            streamp.avail_out = Ord::min(left, max) as _;
            left -= streamp.avail_out as usize;
        }

        if streamp.avail_in == 0 {
            streamp.avail_in = Ord::min(source_len, max) as _;
            source_len -= streamp.avail_in as usize;
        }

        let flush = if source_len > 0 {
            *flush
        } else {
            DeflateFlush::Finish
        };

        let err = unsafe { deflate(streamp, flush as i32) };
        if ReturnCode::from(err) != ReturnCode::Ok {
            break;
        }
    }

    unsafe { dest.set_len(streamp.total_out as _) }

    let err = unsafe { deflateEnd(streamp) };
    assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

    Some(dest)
}
