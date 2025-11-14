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
pub enum DecompressError {
    /// Decompressing this input requires a dictionary.
    NeedDict { dict_id: u32 } = 2,
    /// The [`Decompress`] is in an inconsistent state, most likely
    /// due to an invalid configuration parameter.
    StreamError = -2,
    /// The input is not a valid deflate stream.
    DataError = -3,
    /// A memory allocation failed.
    MemError = -4,
}

impl From<DecompressError> for ReturnCode {
    fn from(value: DecompressError) -> Self {
        match value {
            DecompressError::NeedDict { .. } => ReturnCode::NeedDict,
            DecompressError::StreamError => ReturnCode::StreamError,
            DecompressError::DataError => ReturnCode::DataError,
            DecompressError::MemError => ReturnCode::MemError,
        }
    }
}

impl DecompressError {
    pub fn as_str(self) -> &'static str {
        ReturnCode::from(self).error_message_str()
    }
}

/// The state that is used to decompress an input.
pub struct Decompress(crate::inflate::InflateStream<'static>);

unsafe impl Sync for Decompress {}
unsafe impl Send for Decompress {}

impl Decompress {
    /// The amount of bytes consumed from the input so far.
    pub fn total_in(&self) -> u64 {
        #[allow(clippy::useless_conversion)]
        u64::from(self.0.total_in)
    }

    /// The amount of decompressed bytes that have been written to the output thus far.
    pub fn total_out(&self) -> u64 {
        #[allow(clippy::useless_conversion)]
        u64::from(self.0.total_out)
    }

    /// Create a new instance. Note that it allocates in various ways and thus should be re-used.
    pub fn new(zlib_header: bool, window_bits: u8) -> Self {
        Self(crate::inflate::InflateStream::new(zlib_header, window_bits))
    }

    /// Reset the state to allow handling a new stream.
    pub fn reset(&mut self, zlib_header: bool) {
        let mut config = InflateConfig::default();

        if !zlib_header {
            config.window_bits = -config.window_bits;
        }

        crate::inflate::reset_with_config(&mut self.0, config);
    }

    /// Decompress `input` and write all decompressed bytes into `output`, with `flush` defining some details about this.
    pub fn decompress(
        &mut self,
        input: &[u8],
        output: &mut [u8],
        flush: InflateFlush,
    ) -> Result<Status, DecompressError> {
        self.0.avail_in = input.len() as _;
        self.0.avail_out = output.len() as _;

        // This cast_mut is unfortunate, that is just how the types are.
        self.0.next_in = input.as_ptr().cast_mut();
        self.0.next_out = output.as_mut_ptr();

        match unsafe { crate::inflate::inflate(&mut self.0, flush) } {
            ReturnCode::Ok => Ok(Status::Ok),
            ReturnCode::StreamEnd => Ok(Status::StreamEnd),
            ReturnCode::NeedDict => Err(DecompressError::NeedDict {
                dict_id: self.0.adler as u32,
            }),
            ReturnCode::ErrNo => unreachable!("the rust API does not use files"),
            ReturnCode::StreamError => Err(DecompressError::StreamError),
            ReturnCode::DataError => Err(DecompressError::DataError),
            ReturnCode::MemError => Err(DecompressError::MemError),
            ReturnCode::BufError => Ok(Status::BufError),
            ReturnCode::VersionError => unreachable!("the rust API does not use the version"),
        }
    }
}

impl Drop for Decompress {
    fn drop(&mut self) {
        let _ = crate::inflate::end(&mut self.0);
    }
}

/// Errors that can occur when compressing.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum CompressError {
    /// The [`Compress`] is in an inconsistent state, most likely
    /// due to an invalid configuration parameter.
    StreamError = -2,
    /// The input is not a valid deflate stream.
    DataError = -3,
    /// A memory allocation failed.
    MemError = -4,
}

impl From<CompressError> for ReturnCode {
    fn from(value: CompressError) -> Self {
        match value {
            CompressError::StreamError => ReturnCode::StreamError,
            CompressError::DataError => ReturnCode::DataError,
            CompressError::MemError => ReturnCode::MemError,
        }
    }
}

impl CompressError {
    pub fn as_str(self) -> &'static str {
        ReturnCode::from(self).error_message_str()
    }
}

impl From<ReturnCode> for Result<Status, CompressError> {
    fn from(value: ReturnCode) -> Self {
        match value {
            ReturnCode::Ok => Ok(Status::Ok),
            ReturnCode::StreamEnd => Ok(Status::StreamEnd),
            ReturnCode::NeedDict => unreachable!("compression does not use dictionary"),
            ReturnCode::ErrNo => unreachable!("the rust API does not use files"),
            ReturnCode::StreamError => Err(CompressError::StreamError),
            ReturnCode::DataError => Err(CompressError::DataError),
            ReturnCode::MemError => Err(CompressError::MemError),
            ReturnCode::BufError => Ok(Status::BufError),
            ReturnCode::VersionError => unreachable!("the rust API does not use the version"),
        }
    }
}

/// The state that is used to compress an input.
pub struct Compress(crate::deflate::DeflateStream<'static>);

unsafe impl Sync for Compress {}
unsafe impl Send for Compress {}

impl Compress {
    /// The number of bytes that were read from the input.
    pub fn total_in(&self) -> u64 {
        #[allow(clippy::useless_conversion)]
        u64::from(self.0.total_in)
    }

    /// The number of compressed bytes that were written to the output.
    pub fn total_out(&self) -> u64 {
        #[allow(clippy::useless_conversion)]
        u64::from(self.0.total_out)
    }

    /// Create a new instance - this allocates so should be done with care.
    pub fn new(level: i32, zlib_header: bool, window_bits: u8) -> Self {
        Self(crate::deflate::DeflateStream::new(
            level,
            zlib_header,
            window_bits,
        ))
    }

    /// Prepare the instance for a new stream.
    pub fn reset(&mut self) {
        crate::deflate::reset(&mut self.0);
    }

    /// Compress `input` and write compressed bytes to `output`, with `flush` controlling additional characteristics.
    pub fn compress(
        &mut self,
        input: &[u8],
        output: &mut [u8],
        flush: DeflateFlush,
    ) -> Result<Status, CompressError> {
        self.0.avail_in = input.len() as _;
        self.0.avail_out = output.len() as _;

        // This cast_mut is unfortunate, that is just how the types are.
        self.0.next_in = input.as_ptr().cast_mut();
        self.0.next_out = output.as_mut_ptr();

        crate::deflate::deflate(&mut self.0, flush).into()
    }
}

impl Drop for Compress {
    fn drop(&mut self) {
        let _ = crate::deflate::end(&mut self.0);
    }
}
