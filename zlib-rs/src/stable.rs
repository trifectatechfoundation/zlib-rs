use core::{ffi::c_uint, mem::MaybeUninit};

use crate::deflate::DeflateConfig;
use crate::inflate::InflateConfig;
use crate::ReturnCode;
pub use crate::{DeflateFlush, InflateFlush};

/// Possible status results of compressing some data or successfully
/// decompressing a block of data.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Status {
    /// Indicates success.
    ///
    /// Means that more input may be needed but isn't available
    /// and/or there's more output to be written but the output buffer is full.
    Ok,

    /// Indicates that forward progress is not possible due to input or output
    /// buffers being empty.
    ///
    /// For compression it means the input buffer needs some more data or the
    /// output buffer needs to be freed up before trying again.
    ///
    /// For decompression this means that more input is needed to continue or
    /// the output buffer isn't large enough to contain the result. The function
    /// can be called again after fixing both.
    BufError,

    /// Indicates that all input has been consumed and all output bytes have
    /// been written. Decompression/compression should not be called again.
    ///
    /// For decompression with zlib streams the adler-32 of the decompressed
    /// data has also been verified.
    StreamEnd,
}

/// Errors that can occur when decompressing.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[repr(i32)]
pub enum InflateError {
    /// Decompressing this input requires a dictionary.
    NeedDict { dict_id: u32 } = 2,
    /// The [`Inflate`] is in an inconsistent state, most likely
    /// due to an invalid configuration parameter.
    StreamError = -2,
    /// The input is not a valid deflate stream.
    DataError = -3,
    /// A memory allocation failed.
    MemError = -4,
}

impl From<InflateError> for ReturnCode {
    fn from(value: InflateError) -> Self {
        match value {
            InflateError::NeedDict { .. } => ReturnCode::NeedDict,
            InflateError::StreamError => ReturnCode::StreamError,
            InflateError::DataError => ReturnCode::DataError,
            InflateError::MemError => ReturnCode::MemError,
        }
    }
}

impl InflateError {
    pub fn as_str(self) -> &'static str {
        ReturnCode::from(self).error_message_str()
    }
}

/// The state that is used to decompress an input.
pub struct Inflate {
    // A `z_stream` rather than an `InflateStream`, because the latter holds a `&mut State`. That
    // reference is protected while an `Inflate` is passed by value, which makes the deallocation
    // in `inflate::end` undefined behavior. A raw pointer is not retagged, so no protector exists.
    inner: crate::c_api::z_stream,
    total_in: u64,
    total_out: u64,
}

impl Inflate {
    fn stream(&mut self) -> &mut crate::inflate::InflateStream<'static> {
        // SAFETY: `Inflate::new` initialized `self.inner` with `inflate::init`.
        unsafe { crate::inflate::InflateStream::from_stream_mut(&mut self.inner) }.unwrap()
    }

    /// The amount of bytes consumed from the input so far.
    pub fn total_in(&self) -> u64 {
        self.total_in
    }

    /// The amount of decompressed bytes that have been written to the output thus far.
    pub fn total_out(&self) -> u64 {
        self.total_out
    }

    /// The error message if the previous operation failed.
    pub fn error_message(&self) -> Option<&'static str> {
        if self.inner.msg.is_null() {
            None
        } else {
            unsafe { core::ffi::CStr::from_ptr(self.inner.msg).to_str() }.ok()
        }
    }

    /// Create a new instance. This function allocates, and so it is recommended to re-use this
    /// state when possible, using [`Inflate::reset`] as needed.
    ///
    /// This function will:
    ///
    /// - decode a raw deflate stream when `expect_header = false` and `window_bits` is in the
    ///   range `8..=15`
    /// - decode a zlib header followed by a deflate stream when `expect_header = true` and
    ///   `window_bits` is in the range `8..=15`
    /// - decode a gzip header followed by a deflate stream when `expect_header = true` and
    ///   `window_bits` is in the range `16 + 8..=16 + 15`
    /// - decode either a zlib or a gzip header, followed by a deflate stream when
    ///   `expect_header = true` and `window_bits` is in the range `32 + 8..=32 + 15`
    ///
    /// `window_bits` can also be 0 to request that inflate use the window size in the
    /// zlib header of the compressed stream when using zlib.
    ///
    /// Note that when deflating a value of `window_bits = 8` is silently converted to
    /// `window_bits = 9` in most zlib implementations, and hence should be inflated using
    /// `window_bits = 9`.
    ///
    /// # Panics
    ///
    /// This function may panic when the `window_bits` and `expect_header` have values not listed above.
    pub fn new(expect_header: bool, window_bits: u8) -> Self {
        let config = InflateConfig {
            window_bits: if expect_header {
                i32::from(window_bits)
            } else {
                -i32::from(window_bits)
            },
        };

        let mut inner = crate::c_api::z_stream::default();
        let ret = crate::inflate::init(&mut inner, config);
        assert_eq!(ret, ReturnCode::Ok);

        Self {
            inner,
            total_in: 0,
            total_out: 0,
        }
    }

    /// Reset the state to allow handling a new stream.
    pub fn reset(&mut self, zlib_header: bool) {
        let mut config = InflateConfig::default();

        if !zlib_header {
            config.window_bits = -config.window_bits;
        }

        self.total_in = 0;
        self.total_out = 0;

        crate::inflate::reset_with_config(self.stream(), config);
    }

    /// Decompress `input` and write all decompressed bytes into `output`,
    /// with `flush` defining some details about this.
    pub fn decompress(
        &mut self,
        input: &[u8],
        output: &mut [u8],
        flush: InflateFlush,
    ) -> Result<Status, InflateError> {
        self.decompress_uninit(
            input,
            unsafe { &mut *(output as *mut _ as *mut [MaybeUninit<u8>]) },
            flush,
        )
    }

    /// Decompress `input` and write all decompressed bytes into a potentially uninitialized `output`,
    /// with `flush` defining some details about this.
    pub fn decompress_uninit(
        &mut self,
        input: &[u8],
        output: &mut [MaybeUninit<u8>],
        flush: InflateFlush,
    ) -> Result<Status, InflateError> {
        // Limit the length of the input and output to the maximum value of a c_uint. For larger
        // inputs, this will either complete or signal that more input and output is needed. The
        // caller should be able to handle this regardless.
        self.inner.avail_in = Ord::min(input.len(), c_uint::MAX as usize) as c_uint;
        self.inner.avail_out = Ord::min(output.len(), c_uint::MAX as usize) as c_uint;

        // This cast_mut is unfortunate, that is just how the types are.
        self.inner.next_in = input.as_ptr().cast_mut();
        self.inner.next_out = output.as_mut_ptr().cast();

        let start_in = self.inner.next_in;
        let start_out = self.inner.next_out;

        // SAFETY: the inflate state was properly initialized.
        let ret = unsafe { crate::inflate::inflate(self.stream(), flush) };

        self.total_in += (self.inner.next_in as usize - start_in as usize) as u64;
        self.total_out += (self.inner.next_out as usize - start_out as usize) as u64;

        match ret {
            ReturnCode::Ok => Ok(Status::Ok),
            ReturnCode::StreamEnd => Ok(Status::StreamEnd),
            ReturnCode::NeedDict => Err(InflateError::NeedDict {
                dict_id: self.inner.adler as u32,
            }),
            ReturnCode::ErrNo => unreachable!("the rust API does not use files"),
            ReturnCode::StreamError => Err(InflateError::StreamError),
            ReturnCode::DataError => Err(InflateError::DataError),
            ReturnCode::MemError => Err(InflateError::MemError),
            ReturnCode::BufError => Ok(Status::BufError),
            ReturnCode::VersionError => unreachable!("the rust API does not use the version"),
        }
    }

    pub fn set_dictionary(&mut self, dictionary: &[u8]) -> Result<u32, InflateError> {
        match crate::inflate::set_dictionary(self.stream(), dictionary) {
            ReturnCode::Ok => Ok(self.inner.adler as u32),
            ReturnCode::StreamError => Err(InflateError::StreamError),
            ReturnCode::DataError => Err(InflateError::DataError),
            other => unreachable!("set_dictionary does not return {other:?}"),
        }
    }
}

impl Drop for Inflate {
    fn drop(&mut self) {
        let _ = crate::inflate::end(self.stream());
    }
}

/// Errors that can occur when compressing.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum DeflateError {
    /// The [`Deflate`] is in an inconsistent state, most likely
    /// due to an invalid configuration parameter.
    StreamError = -2,
    /// The input is not a valid deflate stream.
    DataError = -3,
    /// A memory allocation failed.
    MemError = -4,
}

impl From<DeflateError> for ReturnCode {
    fn from(value: DeflateError) -> Self {
        match value {
            DeflateError::StreamError => ReturnCode::StreamError,
            DeflateError::DataError => ReturnCode::DataError,
            DeflateError::MemError => ReturnCode::MemError,
        }
    }
}

impl DeflateError {
    pub fn as_str(self) -> &'static str {
        ReturnCode::from(self).error_message_str()
    }
}

impl From<ReturnCode> for Result<Status, DeflateError> {
    fn from(value: ReturnCode) -> Self {
        match value {
            ReturnCode::Ok => Ok(Status::Ok),
            ReturnCode::StreamEnd => Ok(Status::StreamEnd),
            ReturnCode::NeedDict => unreachable!("compression does not use dictionary"),
            ReturnCode::ErrNo => unreachable!("the rust API does not use files"),
            ReturnCode::StreamError => Err(DeflateError::StreamError),
            ReturnCode::DataError => Err(DeflateError::DataError),
            ReturnCode::MemError => Err(DeflateError::MemError),
            ReturnCode::BufError => Ok(Status::BufError),
            ReturnCode::VersionError => unreachable!("the rust API does not use the version"),
        }
    }
}

/// The state that is used to compress an input.
pub struct Deflate {
    // See the note on `Inflate::inner`.
    inner: crate::c_api::z_stream,
    total_in: u64,
    total_out: u64,
}

impl Deflate {
    fn stream(&mut self) -> &mut crate::deflate::DeflateStream<'static> {
        // SAFETY: `Deflate::new` initialized `self.inner` with `deflate::init`.
        unsafe { crate::deflate::DeflateStream::from_stream_mut(&mut self.inner) }.unwrap()
    }

    /// The number of bytes that were read from the input.
    pub fn total_in(&self) -> u64 {
        self.total_in
    }

    /// The number of compressed bytes that were written to the output.
    pub fn total_out(&self) -> u64 {
        self.total_out
    }

    /// The error message if the previous operation failed.
    pub fn error_message(&self) -> Option<&'static str> {
        if self.inner.msg.is_null() {
            None
        } else {
            unsafe { core::ffi::CStr::from_ptr(self.inner.msg).to_str() }.ok()
        }
    }

    /// Create a new instance - this allocates so should be done with care.
    ///
    /// The `window_bits` must be in the range `8..=15`, with `15` being most common.
    pub fn new(level: i32, zlib_header: bool, window_bits: u8) -> Self {
        let config = DeflateConfig {
            window_bits: if zlib_header {
                i32::from(window_bits)
            } else {
                -i32::from(window_bits)
            },
            level,
            ..DeflateConfig::default()
        };

        let mut inner = crate::c_api::z_stream::default();
        let ret = crate::deflate::init(&mut inner, config);
        assert_eq!(ret, ReturnCode::Ok);

        Self {
            inner,
            total_in: 0,
            total_out: 0,
        }
    }

    /// Prepare the instance for a new stream.
    pub fn reset(&mut self) {
        self.total_in = 0;
        self.total_out = 0;

        crate::deflate::reset(self.stream());
    }

    /// Compress `input` and write compressed bytes to `output`,
    /// with `flush` controlling additional characteristics.
    pub fn compress(
        &mut self,
        input: &[u8],
        output: &mut [u8],
        flush: DeflateFlush,
    ) -> Result<Status, DeflateError> {
        self.compress_uninit(
            input,
            unsafe { &mut *(output as *mut _ as *mut [MaybeUninit<u8>]) },
            flush,
        )
    }

    /// Compress `input` and write compressed bytes to a potentially uninitialized `output`,
    /// with `flush` controlling additional characteristics.
    pub fn compress_uninit(
        &mut self,
        input: &[u8],
        output: &mut [MaybeUninit<u8>],
        flush: DeflateFlush,
    ) -> Result<Status, DeflateError> {
        // Limit the length of the input and output to the maximum value of a c_uint. For larger
        // inputs, this will either complete or signal that more input and output is needed. The
        // caller should be able to handle this regardless.
        self.inner.avail_in = Ord::min(input.len(), c_uint::MAX as usize) as c_uint;
        self.inner.avail_out = Ord::min(output.len(), c_uint::MAX as usize) as c_uint;

        // This cast_mut is unfortunate, that is just how the types are.
        self.inner.next_in = input.as_ptr().cast_mut();
        self.inner.next_out = output.as_mut_ptr().cast();

        let start_in = self.inner.next_in;
        let start_out = self.inner.next_out;

        let ret = crate::deflate::deflate(self.stream(), flush).into();

        self.total_in += (self.inner.next_in as usize - start_in as usize) as u64;
        self.total_out += (self.inner.next_out as usize - start_out as usize) as u64;

        // Clear these pointers so there can be no use after free.
        self.inner.next_in = core::ptr::null_mut();
        self.inner.next_out = core::ptr::null_mut();

        self.inner.avail_in = 0;
        self.inner.avail_out = 0;

        ret
    }

    /// Specifies the compression dictionary to use.
    ///
    /// Returns the Adler-32 checksum of the dictionary.
    pub fn set_dictionary(&mut self, dictionary: &[u8]) -> Result<u32, DeflateError> {
        match crate::deflate::set_dictionary(self.stream(), dictionary) {
            ReturnCode::Ok => Ok(self.inner.adler as u32),
            ReturnCode::StreamError => Err(DeflateError::StreamError),
            other => unreachable!("set_dictionary does not return {other:?}"),
        }
    }

    /// Dynamically updates the compression level.
    ///
    /// This can be used to switch between compression levels for different
    /// kinds of data, or it can be used in conjunction with a call to [`Deflate::reset`]
    /// to reuse the compressor.
    ///
    /// This may return an error if there wasn't enough output space to complete
    /// the compression of the available input data before changing the
    /// compression level. Flushing the stream before calling this method
    /// ensures that the function will succeed on the first call.
    pub fn set_level(&mut self, level: i32) -> Result<Status, DeflateError> {
        // Clear these pointers so there can be no use after free.
        self.inner.next_in = core::ptr::null_mut();
        self.inner.next_out = core::ptr::null_mut();

        self.inner.avail_in = 0;
        self.inner.avail_out = 0;

        match crate::deflate::params(self.stream(), level, Default::default()) {
            ReturnCode::Ok => Ok(Status::Ok),
            ReturnCode::StreamError => Err(DeflateError::StreamError),
            ReturnCode::BufError => Ok(Status::BufError),
            other => unreachable!("set_level does not return {other:?}"),
        }
    }
}

impl Drop for Deflate {
    fn drop(&mut self) {
        let _ = crate::deflate::end(self.stream());
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &[u8] = b"scatter scatter scatter, gather gather gather, scatter gather";

    fn compress(input: &[u8]) -> Vec<u8> {
        let mut deflate = Deflate::new(6, true, 15);
        let mut output = vec![0u8; 256];

        let status = deflate
            .compress(input, &mut output, DeflateFlush::Finish)
            .unwrap();

        assert_eq!(status, Status::StreamEnd);
        output.truncate(deflate.total_out() as usize);

        output
    }

    fn decompress(input: &[u8]) -> Vec<u8> {
        let mut inflate = Inflate::new(true, 15);
        let mut output = vec![0u8; 256];

        let status = inflate
            .decompress(input, &mut output, InflateFlush::Finish)
            .unwrap();

        assert_eq!(status, Status::StreamEnd);
        output.truncate(inflate.total_out() as usize);

        output
    }

    #[test]
    fn round_trip() {
        assert_eq!(decompress(&compress(INPUT)), INPUT);
    }

    #[test]
    fn reuse_after_reset() {
        let compressed = compress(INPUT);

        let mut inflate = Inflate::new(true, 15);
        let mut output = vec![0u8; 256];

        for _ in 0..2 {
            inflate.reset(true);

            let status = inflate
                .decompress(&compressed, &mut output, InflateFlush::Finish)
                .unwrap();

            assert_eq!(status, Status::StreamEnd);
            assert_eq!(&output[..inflate.total_out() as usize], INPUT);
        }
    }

    #[test]
    fn reuse_after_error() {
        let mut compressed = compress(INPUT);
        let last = compressed.len() - 1;
        compressed[last] ^= 0xff;

        let mut inflate = Inflate::new(true, 15);
        let mut output = vec![0u8; 256];

        assert!(inflate
            .decompress(&compressed, &mut output, InflateFlush::Finish)
            .is_err());

        inflate.reset(true);

        let compressed = compress(INPUT);
        let status = inflate
            .decompress(&compressed, &mut output, InflateFlush::Finish)
            .unwrap();

        assert_eq!(status, Status::StreamEnd);
        assert_eq!(&output[..inflate.total_out() as usize], INPUT);
    }

    /// The state now lives behind a raw pointer, so moving the value must not invalidate it.
    #[test]
    fn usable_after_being_moved() {
        fn make() -> Inflate {
            Inflate::new(true, 15)
        }

        let mut boxed = Box::new(make());
        let mut vec = vec![make()];
        let mut output = vec![0u8; 256];

        for inflate in [&mut *boxed, &mut vec[0]] {
            let compressed = compress(INPUT);
            let status = inflate
                .decompress(&compressed, &mut output, InflateFlush::Finish)
                .unwrap();

            assert_eq!(status, Status::StreamEnd);
            assert_eq!(&output[..inflate.total_out() as usize], INPUT);
        }
    }

    /// Passing a value to a function by value retags the references reachable from it and
    /// protects them for the duration of the call. Storing the state as a `&mut State` meant the
    /// drop glue then freed the allocation that protected reference points into, which miri
    /// rejects under both Stacked Borrows and Tree Borrows.
    #[test]
    fn drop_by_value() {
        drop(Inflate::new(true, 15));
        drop(Deflate::new(6, true, 15));
    }

    /// The retag is recursive, so burying the value in a field does not avoid the protector.
    #[test]
    fn drop_nested_by_value() {
        struct Wrapper {
            _inflate: Option<Inflate>,
            _deflate: Box<Deflate>,
        }

        drop(Wrapper {
            _inflate: Some(Inflate::new(true, 15)),
            _deflate: Box::new(Deflate::new(6, true, 15)),
        });
    }

    /// A value that has been used still has to survive the same drop.
    #[test]
    fn drop_by_value_after_use() {
        let mut inflate = Inflate::new(true, 15);
        let mut output = vec![0u8; 256];

        inflate
            .decompress(&compress(INPUT), &mut output, InflateFlush::Finish)
            .unwrap();

        drop(inflate);
    }

    /// The other drop shapes never created a protector, and must stay that way.
    #[test]
    fn drop_without_being_passed_by_value() {
        let _scope_end = Inflate::new(true, 15);

        let mut vec = vec![Inflate::new(true, 15)];
        vec.clear();

        let mut option = Some(Deflate::new(6, true, 15));
        let _ = option.take();
    }
}
