//! This fuzzer is intended to find memory safety bugs and undefined behavior. The input entire
//! input is processed, allowing for analysis of files of arbitrary size. It also speeds up
//! coverage by disabling checksum validation and disregarding correctness of the results.
//!
//! This test must be run with `--features disable-checksum`. It's also suggested to initialize
//! fuzzing with a corpus of real zlib/gzip files. Place the corpus in directory
//! `corpus/uncompress` (the default corpus location).
//!
//! Then, the fuzzer can be run like:
//!
//! ```
//! cargo fuzz run uncompress --features disable-checksum -j$(nproc)
//! ```
//!
//! If not starting with an initial corpus, consider using the `-- -max_len=1048576` argument to
//! test larger inputs.
//!
//! libfuzzer uses LLVM sanitizers to detect some classes of bugs and UB. For detecting
//! Rust-specific UB, use Miri. Once a corpus with suitable coverage has been built, you can run
//! Miri against the corpus by executing:
//! ```
//! MIRIFLAGS=-Zmiri-disable-isolation cargo miri nextest run --bin uncompress --features disable-checksum
//! ```
//! This assumes the corpus is located in the default directory of `corpus/uncompress`. If it
//! isn't, specify the corpus directory with the `ZLIB_RS_CORPUS_DIR` environment variable.
#![cfg_attr(not(any(miri, test)), no_main)]

use libfuzzer_sys::{fuzz_target, Corpus};
use libz_rs_sys::{
    gz_header, inflate, inflateEnd, inflateGetHeader, inflateInit2_, z_stream, zlibVersion,
};
use zlib_rs::{InflateFlush, ReturnCode};

fuzz_target!(|input: &[u8]| -> Corpus { run(input) });

fn run(input: &[u8]) -> Corpus {
    if input.is_empty() {
        return Corpus::Reject;
    }

    let mut stream = z_stream::default();

    let err = unsafe {
        inflateInit2_(
            &mut stream,
            15 + 32, // Support both zlib and gzip files.
            zlibVersion(),
            size_of::<z_stream>() as _,
        )
    };
    assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

    let mut extra = vec![0; 64];
    let mut name = vec![0; 64];
    let mut comment = vec![0; 64];
    let mut header = gz_header {
        text: 0,
        time: 0,
        xflags: 0,
        os: 0,
        extra: extra.as_mut_ptr(),
        extra_len: 0,
        extra_max: 64,
        name: name.as_mut_ptr(),
        name_max: 64,
        comment: comment.as_mut_ptr(),
        comm_max: 64,
        hcrc: 0,
        done: 0,
    };

    let err = unsafe { inflateGetHeader(&mut stream, &mut header) };
    assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

    let mut output = vec![0; input.len()];
    let input_len: u64 = input.len().try_into().unwrap();
    stream.next_out = output.as_mut_ptr();
    stream.avail_out = output.len().try_into().unwrap();

    // Small enough to hit interesting cases, but large enough to hit the fast path
    let chunk_size = 64;

    // For code coverage (on CI), we want to keep inputs that triggered the error
    // branches, to get an accurate picture of what error paths we actually hit.
    //
    // It helps that on CI we start with a corpus of valid files: a mutation of such an
    // input is not a sequence of random bytes, but rather quite close to correct and
    // hence likely to hit interesting error conditions.
    let invalid_input = if cfg!(feature = "keep-invalid-in-corpus") {
        Corpus::Keep
    } else {
        Corpus::Reject
    };

    for chunk in input.chunks(chunk_size) {
        stream.next_in = chunk.as_ptr() as *mut u8;
        stream.avail_in = chunk.len() as _;

        let err = unsafe { inflate(&mut stream, InflateFlush::NoFlush as _) };
        match ReturnCode::from(err) {
            ReturnCode::StreamEnd => {
                break;
            }
            ReturnCode::Ok => {
                continue;
            }
            ReturnCode::BufError => {
                let add_space: u32 = Ord::max(1024, output.len().try_into().unwrap());
                output.resize(output.len() + add_space as usize, 0);

                // If resize() reallocates, it may have moved in memory.
                stream.next_out = output.as_mut_ptr();
                stream.avail_out += add_space;
            }
            _ => {
                unsafe { inflateEnd(&mut stream) };
                return invalid_input;
            }
        }
    }

    let err = unsafe { inflateEnd(&mut stream) };
    match ReturnCode::from(err) {
        ReturnCode::Ok => Corpus::Keep,
        _ => invalid_input,
    }
}

libfuzzer_sys::fuzz_mutator!(|data: &mut [u8], size: usize, max_size: usize, seed: u32| {
    if seed.is_multiple_of(4) {
        libfuzzer_sys::fuzzer_mutate(data, size, max_size)
    } else {
        // Decompress the input data. If that fails, use a dummy value.
        let mut buf = vec![0xAAu8; 4 * max_size];
        let config = zlib_rs::inflate::InflateConfig::default();
        let (decompressed, ret) = helpers::uncompress_slice_ng(&mut buf, &data[..size], config);

        // Mutate the decompressed data with `libFuzzer`'s default mutator. Make
        // the `decompressed` vec's extra capacity available for insertion
        // mutations via `resize`.
        let len = decompressed.len();
        let decompressed = &mut buf;
        let cap = decompressed.capacity();

        decompressed.resize(cap, 0);
        let new_decompressed_size = libfuzzer_sys::fuzzer_mutate(decompressed, len, cap);

        // Recompress the mutated data.
        let (compressed, ret) = helpers::compress_slice_ng(
            data,
            &decompressed[..new_decompressed_size],
            Default::default(),
        );

        // Copy the recompressed mutated data into `data` and return the new size.
        let new_size = std::cmp::min(max_size, compressed.len());
        new_size
    }
});

mod helpers {
    use core::ffi::{c_int, c_uint};
    use zlib_rs::{
        deflate::DeflateConfig, inflate::InflateConfig, DeflateFlush, InflateFlush, ReturnCode,
    };

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
}

#[cfg(test)]
mod tests {
    #[cfg(miri)]
    use {
        crate::run,
        rstest::rstest,
        std::{fs::File, io::Read, path::PathBuf},
    };

    #[rstest]
    #[cfg(miri)]
    fn miri_corpus(#[files("${ZLIB_RS_CORPUS_DIR:-corpus/uncompress}/*")] path: PathBuf) {
        let mut input = File::open(path).unwrap();
        let mut buf = Vec::new();
        input.read_to_end(&mut buf).unwrap();

        run(&buf);
    }
}
