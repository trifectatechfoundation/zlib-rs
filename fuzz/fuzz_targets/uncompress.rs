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
