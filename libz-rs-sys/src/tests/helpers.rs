use zlib_rs::{deflate::DeflateConfig, DeflateFlush, ReturnCode};

// we use the libz_sys but configure zlib-ng in zlib compat mode
use libz_sys as libz_ng_sys;

use core::ffi::{c_char, c_int, c_uint};

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
        zalloc: zlib_rs::allocate::zalloc_c,
        zfree: zlib_rs::allocate::zfree_c,
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
