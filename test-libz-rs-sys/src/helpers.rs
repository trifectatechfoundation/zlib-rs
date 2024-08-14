use zlib_rs::{
    deflate::DeflateConfig, inflate::InflateConfig, DeflateFlush, InflateFlush, ReturnCode,
};

// we use the libz_sys but configure zlib-ng in zlib compat mode
use libz_sys as libz_ng_sys;

use core::ffi::{c_int, c_uint};

pub fn compress_slice_ng<'a>(
    output: &'a mut [u8],
    input: &[u8],
    config: DeflateConfig,
) -> (&'a mut [u8], ReturnCode) {
    compress_slice_with_flush_ng(output, input, config, DeflateFlush::Finish)
}

pub fn compress_slice_with_flush_ng<'a>(
    output: &'a mut [u8],
    input: &[u8],
    config: DeflateConfig,
    final_flush: DeflateFlush,
) -> (&'a mut [u8], ReturnCode) {
    let mut stream = libz_ng_sys::z_stream {
        next_in: input.as_ptr() as *mut u8,
        avail_in: 0, // for special logic in the first  iteration
        total_in: 0,
        next_out: output.as_mut_ptr(),
        avail_out: 0, // for special logic on the first iteration
        total_out: 0,
        msg: core::ptr::null_mut(),
        state: core::ptr::null_mut(),
        zalloc: zlib_rs::allocate::Allocator::C.zalloc,
        zfree: zlib_rs::allocate::Allocator::C.zfree,
        opaque: core::ptr::null_mut(),
        data_type: 0,
        adler: 0,
        reserved: 0,
    };

    let err = unsafe {
        libz_ng_sys::deflateInit2_(
            &mut stream,
            config.level,
            config.method as i32,
            config.window_bits,
            config.mem_level,
            config.strategy as i32,
            libz_ng_sys::zlibVersion(),
            core::mem::size_of::<libz_ng_sys::z_stream>() as c_int,
        )
    };

    if err != libz_ng_sys::Z_OK {
        return (&mut [], ReturnCode::from(err));
    }

    let max = c_uint::MAX as usize;

    let mut left = output.len();
    let mut source_len = input.len();

    loop {
        if stream.avail_out == 0 {
            stream.avail_out = Ord::min(left, max) as _;
            left -= stream.avail_out as usize;
        }

        if stream.avail_in == 0 {
            stream.avail_in = Ord::min(source_len, max) as _;
            source_len -= stream.avail_in as usize;
        }

        let flush = if source_len > 0 {
            DeflateFlush::NoFlush
        } else {
            final_flush
        };

        let err = unsafe { libz_ng_sys::deflate(&mut stream, flush as i32) };

        if err != libz_ng_sys::Z_OK {
            break;
        }
    }

    // may DataError if there was insufficient output space
    let err = unsafe { libz_ng_sys::deflateEnd(&mut stream) };
    let return_code: ReturnCode = ReturnCode::from(err);

    (&mut output[..stream.total_out as usize], return_code)
}

pub fn uncompress_slice_ng<'a>(
    output: &'a mut [u8],
    input: &[u8],
    config: InflateConfig,
) -> (&'a mut [u8], ReturnCode) {
    let mut stream = libz_ng_sys::z_stream {
        next_in: input.as_ptr() as *mut u8,
        avail_in: input.len() as _,
        total_in: 0,
        next_out: output.as_mut_ptr(),
        avail_out: output.len() as _,
        total_out: 0,
        msg: std::ptr::null_mut(),
        state: std::ptr::null_mut(),
        zalloc: zlib_rs::allocate::Allocator::C.zalloc,
        zfree: zlib_rs::allocate::Allocator::C.zfree,
        opaque: std::ptr::null_mut(),
        data_type: 0,
        adler: 0,
        reserved: 0,
    };

    let dest_len = output.len();
    let mut dest_len_ptr = 0;

    // z_uintmax_t len, left;
    let mut left;
    let dest;
    let buf: &mut [u8] = &mut [1]; /* for detection of incomplete stream when *destLen == 0 */

    let mut len = input.len() as u64;
    if dest_len != 0 {
        left = dest_len as u64;
        dest_len_ptr = 0;
        dest = output.as_mut_ptr();
    } else {
        left = 1;
        dest = buf.as_mut_ptr();
    }

    let err = unsafe {
        libz_ng_sys::inflateInit2_(
            &mut stream,
            config.window_bits,
            libz_ng_sys::zlibVersion(),
            std::mem::size_of::<libz_ng_sys::z_stream>() as i32,
        )
    };
    if err != ReturnCode::Ok as _ {
        return (&mut [], ReturnCode::from(err));
    }

    stream.next_out = dest;
    stream.avail_out = 0;

    let err = loop {
        if stream.avail_out == 0 {
            stream.avail_out = Ord::min(left, u32::MAX as u64) as u32;
            left -= stream.avail_out as u64;
        }

        if stream.avail_out == 0 {
            stream.avail_in = Ord::min(len, u32::MAX as u64) as u32;
            len -= stream.avail_in as u64;
        }

        let err = unsafe { libz_ng_sys::inflate(&mut stream, InflateFlush::NoFlush as _) };
        let err = ReturnCode::from(err);

        if err != ReturnCode::Ok as _ {
            break err;
        }
    };

    if dest_len != 0 {
        dest_len_ptr = stream.total_out;
    } else if stream.total_out != 0 && err == ReturnCode::BufError as _ {
        left = 1;
    }

    unsafe { libz_ng_sys::inflateEnd(&mut stream) };

    let ret = match err {
        ReturnCode::StreamEnd => ReturnCode::Ok,
        ReturnCode::NeedDict => ReturnCode::DataError,
        ReturnCode::BufError if (left + stream.avail_out as u64) != 0 => ReturnCode::DataError,
        _ => err,
    };

    // SAFETY: we have now initialized these bytes
    let output_slice =
        unsafe { std::slice::from_raw_parts_mut(output.as_mut_ptr(), dest_len_ptr as usize) };

    (output_slice, ret)
}
