#![allow(non_snake_case)] // TODO ultimately remove this
#![allow(clippy::missing_safety_doc)] // obviously needs to be fixed long-term

use std::ffi::{c_char, c_int, c_long, c_ulong, c_void};
use std::{alloc::Layout, mem::MaybeUninit};
use core::slice;

mod bitreader;
mod inffixed_tbl;
mod inftrees;
mod window;

use crate::{
    adler32::adler32, allocate, c_api::{gz_header, Z_DEFLATED, z_stream}, read_buf::ReadBuf, Code, Flush, ReturnCode,
    DEF_WBITS, MAX_WBITS, MIN_WBITS,
};

use crate::crc32;

use self::{
    bitreader::BitReader,
    inftrees::{inflate_table, CodeType, InflateTable},
    window::Window,
};

// TODO should only be used by tests; only export when running tests
#[repr(C)]
pub struct InflateStream<'a> {
    pub(crate) next_in: *mut crate::c_api::Bytef,
    pub(crate) avail_in: crate::c_api::uInt,
    pub(crate) total_in: crate::c_api::z_size,
    pub(crate) next_out: *mut crate::c_api::Bytef,
    pub(crate) avail_out: crate::c_api::uInt,
    pub(crate) total_out: crate::c_api::z_size,
    pub(crate) msg: *mut c_char,
    pub(crate) state: &'a mut State<'a>,
    pub(crate) zalloc: crate::c_api::alloc_func,
    pub(crate) zfree: crate::c_api::free_func,
    pub(crate) opaque: crate::c_api::voidpf,
    pub(crate) data_type: c_int,
    pub(crate) adler: crate::c_api::z_checksum,
    pub(crate) reserved: crate::c_api::uLong,
}

// TODO should only be used by tests; only export when running tests
pub const INFLATE_STATE_SIZE: usize = core::mem::size_of::<crate::inflate::State>();

impl<'a> InflateStream<'a> {
    const _S: () = assert!(core::mem::size_of::<z_stream>() == core::mem::size_of::<Self>());
    const _A: () = assert!(core::mem::align_of::<z_stream>() == core::mem::align_of::<Self>());

    /// # Safety
    ///
    /// The `strm` pointer must be either `NULL` or a correctly initalized `z_stream`. Here
    /// correctly initalized does not just mean that the pointer is valid and well-aligned, but
    /// also that it has been initialized by that `inflateInit_` or `inflateInit2_`.
    #[inline(always)]
    pub unsafe fn from_stream_ref(strm: *const z_stream) -> Option<&'a Self> {
        if strm.is_null() {
            return None;
        }

        // safety: ptr points to a valid value of type z_stream (if non-null)
        let stream = unsafe { &*strm };

        if stream.zalloc.is_none() || stream.zfree.is_none() {
            return None;
        }

        if stream.state.is_null() {
            return None;
        }

        // safety: InflateStream has the same layout as z_stream
        let stream = unsafe { &*(strm as *const InflateStream) };

        Some(stream)
    }

    /// # Safety
    ///
    /// The `strm` pointer must be either `NULL` or a correctly initalized `z_stream`. Here
    /// correctly initalized does not just mean that the pointer is valid and well-aligned, but
    /// also that it has been initialized by that `inflateInit_` or `inflateInit2_`.
    #[inline(always)]
    pub unsafe fn from_stream_mut(strm: *mut z_stream) -> Option<&'a mut Self> {
        if strm.is_null() {
            return None;
        }

        // safety: ptr points to a valid value of type z_stream (if non-null)
        let stream = unsafe { &mut *strm };

        if stream.zalloc.is_none() || stream.zfree.is_none() {
            return None;
        }

        if stream.state.is_null() {
            return None;
        }

        // safety: InflateStream has the same layout as z_stream
        let stream = unsafe { &mut *(strm as *mut InflateStream) };

        Some(stream)
    }

    unsafe fn alloc_layout(&self, layout: std::alloc::Layout) -> *mut c_void {
        (self.zalloc)(self.opaque, 1, layout.size() as u32)
    }

    unsafe fn dealloc<T>(&self, ptr: *mut T) {
        (self.zfree)(self.opaque, ptr.cast())
    }
}

const MAX_BITS: u8 = 15; // maximum number of bits in a code
const MAX_DIST_EXTRA_BITS: u8 = 13; // maximum number of extra distance bits
                                    //
pub fn uncompress_slice<'a>(
    output: &'a mut [u8],
    input: &[u8],
    config: InflateConfig,
) -> (&'a mut [u8], ReturnCode) {
    let output_uninit = unsafe {
        std::slice::from_raw_parts_mut(output.as_mut_ptr() as *mut MaybeUninit<u8>, output.len())
    };

    uncompress(output_uninit, input, config)
}

/// Inflates `source` into `dest`, and writes the final inflated size into `dest_len`.
pub fn uncompress<'a>(
    output: &'a mut [MaybeUninit<u8>],
    input: &[u8],
    config: InflateConfig,
) -> (&'a mut [u8], ReturnCode) {
    let mut dest_len_ptr = output.len() as u64;
    let dest_len = output.len() as u64;

    // for detection of incomplete stream when *destLen == 0
    let mut buf = [0u8];

    let mut left;
    let mut len = input.len() as u64;

    let dest;
    if dest_len != 0 {
        left = dest_len;
        dest_len_ptr = 0;
        dest = output.as_mut_ptr() as *mut u8;
    } else {
        left = 1;
        dest = buf.as_mut_ptr();
    }

    let mut stream = z_stream {
        next_in: input.as_ptr() as *mut u8,
        avail_in: input.len() as _,
        total_in: 0,
        next_out: dest,
        avail_out: dest_len as _,
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

    let err = init(&mut stream, config);
    if err != ReturnCode::Ok {
        return (&mut [], err);
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

        let err = if let Some(stream) = unsafe { InflateStream::from_stream_mut(&mut stream) } {
            unsafe { inflate(stream, Flush::NoFlush) }
        } else {
            ReturnCode::StreamError
        };

        if err != ReturnCode::Ok as _ {
            break err;
        }
    };

    if dest_len != 0 {
        dest_len_ptr = stream.total_out;
    } else if stream.total_out != 0 && err == ReturnCode::BufError as _ {
        left = 1;
    }

    unsafe { end(&mut stream) };

    let ret = match err {
        ReturnCode::StreamEnd => ReturnCode::Ok,
        ReturnCode::NeedDict => ReturnCode::DataError,
        ReturnCode::BufError if (left + stream.avail_out as u64) != 0 => ReturnCode::DataError,
        _ => err,
    };

    // SAFETY: we have now initialized these bytes
    let output_slice = unsafe {
        std::slice::from_raw_parts_mut(output.as_mut_ptr() as *mut u8, dest_len_ptr as usize)
    };

    (output_slice, ret)
}

#[derive(Debug, Clone, Copy)]
pub enum Mode {
    Head,
    Flags,
    Time,
    Os,
    ExLen,
    Extra,
    Name,
    Comment,
    HCrc,
    Sync,
    Mem,
    Length,
    Type,
    TypeDo,
    Stored,
    CopyBlock,
    Check,
    Len,
    LenExt,
    Dist,
    DistExt,
    Match,
    Table,
    LenLens,
    CodeLens,
    DictId,
    Dict,
    Done,
    Bad,
}

#[derive(Clone, Copy)]
#[allow(clippy::enum_variant_names)]
enum Codes {
    Fixed(&'static [Code]),
    Codes,
    Len,
    Dist,
}

impl Default for Codes {
    fn default() -> Self {
        Codes::Fixed(&[])
    }
}

#[derive(Default, Clone, Copy)]
struct Table {
    codes: Codes,
    bits: usize,
}

pub(crate) struct State<'a> {
    /// Current inflate mode
    mode: Mode,
    /// true if processing the last block
    last: bool,
    /// bitflag
    ///
    /// - bit 0 true if zlib
    /// - bit 1 true if gzip
    /// - bit 2 true to validate check value
    wrap: usize,

    /// table for length/literal codes
    len_table: Table,

    /// table for dist codes
    dist_table: Table,

    /// log base 2 of requested window size
    wbits: usize,
    // allocated window if needed (capacity == 0 if unused)
    window: Window<'a>,

    /// place to store gzip header if needed
    head: Option<&'a mut gz_header>,

    //
    /// number of code length code lengths
    ncode: usize,
    /// number of length code lengths
    nlen: usize,
    /// number of distance code lengths
    ndist: usize,
    /// number of code lengths in lens[]
    have: usize,
    /// next available space in codes[]
    next: usize, // represented as an index, don't want a self-referential structure here

    // IO
    bit_reader: BitReader<'a>,
    writer: ReadBuf<'a>,

    /// length of a block to copy
    length: usize,
    /// distance back to copy the string from
    offset: usize,

    /// extra bits needed
    extra: usize,

    /// if false, allow invalid distance too far
    sane: bool,
    /// bits back of last unprocessed length/lit
    back: usize,

    /// initial length of match
    was: usize,

    /// size of memory copying chunk
    chunksize: usize,

    in_available: usize,
    out_available: usize,

    /// temporary storage space for code lengths
    lens: [u16; 320],
    /// work area for code table building
    work: [u16; 288],

    error_message: Option<&'static str>,
    flush: Flush,

    checksum: u32,
    havedict: bool,
    dmax: usize,
    flags: i32,

    codes_codes: [Code; crate::ENOUGH_LENS],
    len_codes: [Code; crate::ENOUGH_LENS],
    dist_codes: [Code; crate::ENOUGH_DISTS],
}

impl<'a> State<'a> {
    fn new(reader: &'a [u8], writer: ReadBuf<'a>) -> Self {
        let in_available = reader.len();
        let out_available = writer.capacity();

        Self {
            flush: Flush::NoFlush,

            last: false,
            wrap: 0,
            mode: Mode::Head,
            length: 0,

            len_table: Table::default(),
            dist_table: Table::default(),

            wbits: 0,
            offset: 0,
            extra: 0,
            sane: true,
            back: 0,
            was: 0,
            chunksize: 0,
            in_available,
            out_available,

            bit_reader: BitReader::new(reader),
            writer,
            window: Window::empty(),
            head: None,

            lens: [0u16; 320],
            work: [0u16; 288],

            ncode: 0,
            nlen: 0,
            ndist: 0,
            have: 0,
            next: 0,

            error_message: None,

            checksum: 0,
            havedict: false,
            dmax: 0,
            flags: 0,

            codes_codes: [Code::default(); crate::ENOUGH_LENS],
            len_codes: [Code::default(); crate::ENOUGH_LENS],
            dist_codes: [Code::default(); crate::ENOUGH_DISTS],
        }
    }

    fn len_table_ref(&self) -> &[Code] {
        match self.len_table.codes {
            Codes::Fixed(fixed) => fixed,
            Codes::Codes => &self.codes_codes,
            Codes::Len => &self.len_codes,
            Codes::Dist => &self.dist_codes,
        }
    }

    fn dist_table_ref(&self) -> &[Code] {
        match self.dist_table.codes {
            Codes::Fixed(fixed) => fixed,
            Codes::Codes => &self.codes_codes,
            Codes::Len => &self.len_codes,
            Codes::Dist => &self.dist_codes,
        }
    }

    fn len_table_get(&self, index: usize) -> Code {
        self.len_table_ref()[index]
    }

    fn dist_table_get(&self, index: usize) -> Code {
        self.dist_table_ref()[index]
    }
}

macro_rules! pull_byte {
    ($self:expr) => {
        match $self.bit_reader.pull_byte() {
            Err(return_code) => return $self.inflate_leave(return_code),
            Ok(_) => (),
        }
    };
}

macro_rules! need_bits {
    ($self:expr, $n:expr) => {
        match $self.bit_reader.need_bits($n) {
            Err(return_code) => return $self.inflate_leave(return_code),
            Ok(v) => v,
        }
    };
}

// swaps endianness
const fn zswap32(q: u32) -> u32 {
    u32::from_be(q.to_le())
}

const INFLATE_FAST_MIN_HAVE: usize = 15;
const INFLATE_FAST_MIN_LEFT: usize = 260;

impl<'a> State<'a> {
    fn dispatch(&mut self) -> ReturnCode {
        match self.mode {
            Mode::Head => self.head(),
            Mode::Flags => self.flags(),
            Mode::Time => self.time(),
            Mode::Os => self.os(),
            Mode::ExLen => self.ex_len(),
            Mode::Extra => self.extra(),
            Mode::Name => self.name(),
            Mode::Comment => self.comment(),
            Mode::HCrc => self.hcrc(),
            Mode::Sync => self.sync(),
            Mode::Type => self.type_(),
            Mode::TypeDo => self.type_do(),
            Mode::Stored => self.stored(),
            Mode::CopyBlock => self.copy_block(),
            Mode::Check => self.check(),
            Mode::Len => self.len(),
            Mode::LenExt => self.len_ext(),
            Mode::Dist => self.dist(),
            Mode::DistExt => self.dist_ext(),
            Mode::Match => self.match_(),
            Mode::Done => todo!(),
            Mode::Table => self.table(),
            Mode::LenLens => self.len_lens(),
            Mode::CodeLens => self.code_lens(),
            Mode::Dict => self.dict(),
            Mode::DictId => self.dict_id(),
            Mode::Bad => todo!(),
            Mode::Mem => self.mem(),
            Mode::Length => todo!(),
        }
    }

    // ----------------

    /// Initial state
    #[inline(never)]
    fn head(&mut self) -> ReturnCode {
        if self.wrap == 0 {
            self.mode = Mode::TypeDo;
            return self.type_do();
        }

        need_bits!(self, 16);

        // Gzip
        if (self.wrap & 2) != 0 && self.bit_reader.hold() == 0x8b1f {
            if self.wbits == 0 {
                self.wbits = 15;
            }

            let b0 = self.bit_reader.bits(8) as u8;
            let b1 = (self.bit_reader.hold() >> 8) as u8;
            self.checksum = crc32(&[b0, b1], crate::CRC32_INITIAL_VALUE);
            self.bit_reader.init_bits();

            self.mode = Mode::Flags;
            return self.flags();
        }

        if ((self.bit_reader.bits(8) << 8) + (self.bit_reader.hold() >> 8)) % 31 != 0 {
            self.mode = Mode::Bad;
            return self.bad("incorrect header check\0");
        }

        if self.bit_reader.bits(4) != Z_DEFLATED as u64 {
            self.mode = Mode::Bad;
            return self.bad("unknown compression method\0");
        }

        self.bit_reader.drop_bits(4);
        let len = self.bit_reader.bits(4) as usize + 8;

        if self.wbits == 0 {
            self.wbits = len;
        }

        if len > MAX_WBITS as usize || len > self.wbits {
            self.mode = Mode::Bad;
            return self.bad("invalid window size\0");
        }

        self.dmax = 1 << len;
        self.flags = 0; // indicate zlib header
        self.checksum = crate::ADLER32_INITIAL_VALUE as _;

        if self.bit_reader.hold() & 0x200 != 0 {
            self.bit_reader.init_bits();

            self.mode = Mode::DictId;
            self.dict_id()
        } else {
            self.bit_reader.init_bits();

            self.mode = Mode::Type;
            self.type_()
        }
    }

    fn flags(&mut self) -> ReturnCode {
        need_bits!(self, 16);
        self.flags = self.bit_reader.hold() as i32;

        // Z_DEFLATED = 8 is the only supported method
        if self.flags & 0xff != Z_DEFLATED {
            self.mode = Mode::Bad;
            return self.bad("unknown compression method\0");
        }

        if self.flags & 0xe000 != 0 {
            self.mode = Mode::Bad;
            return self.bad("unknown header flags set\0");
        }

        if let Some(head) = self.head.as_mut()  {
            head.text = ((self.bit_reader.hold() >> 8) & 1) as i32;
        }

        if (self.flags & 0x0200) != 0 && (self.wrap & 4) != 0 {
            let b0 = self.bit_reader.bits(8) as u8;
            let b1 = (self.bit_reader.hold() >> 8) as u8;
            self.checksum = crc32(&[b0, b1], self.checksum);
        }

        self.bit_reader.init_bits();
        self.mode = Mode::Time;
        self.time()
    }

    fn time(&mut self) -> ReturnCode {
        need_bits!(self, 32);
        if let Some(head) = self.head.as_mut()  {
            head.time = self.bit_reader.hold();
        }

        if (self.flags & 0x0200) != 0 && (self.wrap & 4) != 0 {
            self.checksum = crc32(&(self.bit_reader.hold() as u32).to_ne_bytes(), self.checksum);
        }

        self.bit_reader.init_bits();
        self.mode = Mode::Os;
        self.os()
    }

    fn os(&mut self) -> ReturnCode {
        need_bits!(self, 16);
        if let Some(head) = self.head.as_mut()  {
            head.xflags = (self.bit_reader.hold() & 0xff) as i32;
            head.os = (self.bit_reader.hold() >> 8) as i32;
        }

        if (self.flags & 0x0200) != 0 && (self.wrap & 4) != 0 {
            self.checksum = crc32(&(self.bit_reader.hold() as u16).to_ne_bytes(), self.checksum);
        }

        self.bit_reader.init_bits();
        self.mode = Mode::ExLen;
        self.ex_len()
    }

    fn ex_len(&mut self) -> ReturnCode {
        if (self.flags & 0x0400) != 0 {
            need_bits!(self, 16);

            self.length = self.bit_reader.hold() as usize;
            if let Some(head) = self.head.as_mut()  {
                head.extra_len = self.bit_reader.hold() as u32;
            }

            if (self.flags & 0x0200) != 0 && (self.wrap & 4) != 0 {
                self.checksum = crc32(&(self.bit_reader.hold() as u16).to_ne_bytes(), self.checksum);
            }

        } else if let Some(head) = self.head.as_mut() {
            head.extra = std::ptr::null_mut();
        }

        self.bit_reader.init_bits();

        self.mode = Mode::Extra;
        self.extra()
    }

    fn extra(&mut self) -> ReturnCode {
        if (self.flags & 0x0400) != 0 {

            let copy = Ord::min(self.length, self.in_available);
            if copy != 0 {
                if let Some(head) = self.head.as_mut() {

                    // If extra is not empty, and extra_len and extra_max are set
                    if !head.extra.is_null() && head.extra_len != 0 && head.extra_max != 0 {

                        let len = head.extra_len - self.length as u32;
                        if len < head.extra_max {

                            let copy_length = unsafe { head.extra.add(len as usize) };
                            let dest = if len + (copy as u32) > head.extra_max {
                                head.extra_max - len
                            } else {
                                copy as u32
                            };

                            unsafe {
                                std::ptr::copy_nonoverlapping(
                                    self.bit_reader.as_ptr(),
                                    copy_length,
                                    dest as usize
                                );
                            }
                        }
                    }
                }

                // Checksum
                if (self.flags & 0x0200) != 0 && (self.wrap & 4) != 0 {
                    let bytes = unsafe { slice::from_raw_parts(self.bit_reader.as_ptr(), copy) };
                    self.checksum = crc32(bytes, self.checksum)
                }

                self.in_available -= copy;
                self.bit_reader.advance(copy);
                self.length -= copy;
            }

            // Checks for errors occur after returning
            if self.length != 0 {
                return self.inflate_leave(ReturnCode::Ok);
            }
        }

        self.length = 0;
        self.mode = Mode::Name;
        self.name()
    }

    fn name(&mut self) -> ReturnCode {
        if (self.flags & 0x0800) != 0 {
            if self.in_available == 0 {
                return self.inflate_leave(ReturnCode::Ok);
            }

            let mut copy = 0;
            loop {

                need_bits!(self, 8);
                let len = self.bit_reader.hold() as u8;
                self.bit_reader.init_bits();

                copy += 1;
                if let Some(head) = self.head.as_mut() {
                    if !head.name.is_null() && self.length < (head.name_max as usize) {
                        unsafe { *head.name = len; }
                        head.name = unsafe { head.name.add(1) };
                        self.length += 1;
                    }
                }

                if len == 0 || copy >= self.bit_reader.bytes_remaining() {
                    break;
                }
            }

            if let Some(head) = self.head.as_mut() {
                if (self.flags & 0x0200) != 0 && (self.wrap & 4) != 0 {
                    let bytes = unsafe { slice::from_raw_parts(head.name.sub(copy), copy) };
                    self.checksum = crc32(bytes, self.checksum)
                }
            }

            if self.bit_reader.bytes_remaining() == 0 {
                return self.inflate_leave(ReturnCode::Ok);
            }

        } else if let Some(head) = self.head.as_mut() {
            head.name = std::ptr::null_mut();
        }

        self.length = 0;
        self.mode = Mode::Comment;
        self.comment()
    }

    fn comment(&mut self) -> ReturnCode {
        assert!(self.length == 0);

        if (self.flags & 0x0100) != 0 {

            if self.in_available == 0 {
                return self.inflate_leave(ReturnCode::Ok);
            }

            let mut copy = 0;
            loop {

                // Take a byte
                need_bits!(self, 8);
                let len = self.bit_reader.hold() as u8;
                self.bit_reader.init_bits();

                copy += 1;
                if let Some(head) = self.head.as_mut() {
                    if !head.comment.is_null() && self.length < (head.comment_max as usize) {
                        unsafe { *head.comment = len; }
                        head.comment = unsafe { head.comment.add(1) };
                        self.length += 1;
                    }
                }

                if len == 0 || copy >= self.bit_reader.bytes_remaining() {
                    break;
                }
            }

            if let Some(head) = self.head.as_mut() {
                if (self.flags & 0x0200) != 0 && (self.wrap & 4) != 0 {
                    let bytes = unsafe { slice::from_raw_parts(head.comment.sub(copy), copy) };
                    self.checksum = crc32(bytes, self.checksum)
                }
            }

            if self.bit_reader.bytes_remaining() == 0 {
                return self.inflate_leave(ReturnCode::Ok);
            }

        } else if let Some(head) = self.head.as_mut() {
            head.comment = std::ptr::null_mut();
        }

        self.mode = Mode::HCrc;
        self.hcrc()
    }

    fn hcrc(&mut self) -> ReturnCode {
        if (self.flags & 0x0200) != 0 {
            need_bits!(self, 16);

            if (self.wrap & 4) != 0 && self.bit_reader.hold() as u32 != (self.checksum & 0xffff) {
                self.mode = Mode::Bad;
                return self.bad("header crc mismatch\0");
            }

            self.bit_reader.init_bits();
        }

        if let Some(head) = self.head.as_mut() {
            head.hcrc = (self.flags >> 9) & 1;
            head.done = 1;
        }

        self.checksum = crate::CRC32_INITIAL_VALUE;
        self.mode = Mode::Type;
        self.type_()
    }

    fn sync(&mut self) -> ReturnCode {
        ReturnCode::StreamError
    }

    fn check(&mut self) -> ReturnCode {
        if self.wrap != 0 {
            need_bits!(self, 32);

            if self.wrap & 4 != 0 && !self.writer.filled().is_empty() {
                if self.flags != 0 {
                    self.checksum = crc32(self.writer.filled(), self.checksum);
                } else {
                    self.checksum = adler32(self.checksum, self.writer.filled());
                }
            }

            let given_checksum = if self.flags != 0 {
                self.bit_reader.hold() as u32
            } else {
                zswap32(self.bit_reader.hold() as u32)
            };

            if self.wrap & 4 != 0 && given_checksum != self.checksum {
                self.mode = Mode::Bad;
                return self.bad("incorrect data check\0");
            }

            self.bit_reader.init_bits();
        }

        // for gzip, last bytes contain LENGTH
        if self.wrap != 0 && self.flags != 0 {
            need_bits!(self, 32);
            if (self.wrap & 4) != 0 && self.bit_reader.hold() != (self.writer.len() & 0xffffffff) as u64 {
                self.mode = Mode::Bad;
                return self.bad("incorrect length check\0");
            }

            self.bit_reader.init_bits();
        }

        // inflate stream terminated properly
        self.inflate_leave(ReturnCode::StreamEnd)
    }

    fn type_(&mut self) -> ReturnCode {
        use Flush::*;

        match self.flush {
            Block | Trees => self.inflate_leave(ReturnCode::Ok),
            NoFlush | PartialFlush | SyncFlush | FullFlush | Finish => self.type_do(),
        }
    }

    fn type_do(&mut self) -> ReturnCode {
        if self.last {
            self.bit_reader.next_byte_boundary();
            self.mode = Mode::Check;
            return self.check();
        }

        need_bits!(self, 3);
        self.last = self.bit_reader.bits(1) != 0;
        self.bit_reader.drop_bits(1);

        match self.bit_reader.bits(2) {
            0 => {
                // eprintln!("inflate:     stored block{last}");

                self.bit_reader.drop_bits(2);

                self.mode = Mode::Stored;
                self.stored()
            }
            1 => {
                // eprintln!("inflate:     fixed codes block{last}");

                self.len_table = Table {
                    codes: Codes::Fixed(&self::inffixed_tbl::LENFIX),
                    bits: 9,
                };

                self.dist_table = Table {
                    codes: Codes::Fixed(&self::inffixed_tbl::DISTFIX),
                    bits: 5,
                };

                self.bit_reader.drop_bits(2);

                if let Flush::Trees = self.flush {
                    self.inflate_leave(ReturnCode::Ok)
                } else {
                    self.mode = Mode::Len;
                    self.len()
                }
            }
            2 => {
                // eprintln!("inflate:     dynamic codes block{last}");

                self.bit_reader.drop_bits(2);

                self.mode = Mode::Table;
                self.table()
            }
            3 => {
                eprintln!("inflate:     invalid block type");

                self.bit_reader.drop_bits(2);

                self.mode = Mode::Bad;
                self.bad("invalid block type\0")
            }
            _ => unsafe { std::hint::unreachable_unchecked() },
        }
    }

    fn stored(&mut self) -> ReturnCode {
        self.bit_reader.next_byte_boundary();

        need_bits!(self, 32);

        let hold = self.bit_reader.bits(32) as u32;

        // eprintln!("hold {hold:#x}");

        if hold as u16 != !((hold >> 16) as u16) {
            self.mode = Mode::Bad;
            return self.bad("invalid stored block lengths\0");
        }

        self.length = hold as usize & 0xFFFF;
        // eprintln!("inflate:     stored length {}", state.length);

        self.bit_reader.init_bits();

        if let Flush::Trees = self.flush {
            self.inflate_leave(ReturnCode::Ok)
        } else {
            self.mode = Mode::CopyBlock;
            self.copy_block()
        }
    }

    fn copy_block(&mut self) -> ReturnCode {
        loop {
            let mut copy = self.length;

            if copy == 0 {
                break;
            }

            copy = Ord::min(copy, self.writer.remaining());
            copy = Ord::min(copy, self.bit_reader.bytes_remaining());

            if copy == 0 {
                return self.inflate_leave(ReturnCode::Ok);
            }

            self.writer.extend(&self.bit_reader.as_slice()[..copy]);
            self.bit_reader.advance(copy);

            self.length -= copy;
        }

        self.mode = Mode::Type;
        self.type_()
    }

    fn len(&mut self) -> ReturnCode {
        let avail_in = self.bit_reader.bytes_remaining();
        let avail_out = self.writer.remaining();

        // INFLATE_FAST_MIN_LEFT is important. It makes sure there is at least 32 bytes of free
        // space available. This means for many SIMD operations we don't need to process a
        // remainder; we just copy blindly, and a later operation will overwrite the extra copied
        // bytes
        if avail_in >= INFLATE_FAST_MIN_HAVE && avail_out >= INFLATE_FAST_MIN_LEFT {
            return inflate_fast_help(self, 0);
        }

        self.back = 0;

        // get a literal, length, or end-of-block code
        let mut here;
        loop {
            let bits = self.bit_reader.bits(self.len_table.bits);
            here = self.len_table_get(bits as usize);

            if here.bits <= self.bit_reader.bits_in_buffer() {
                break;
            }

            pull_byte!(self);
        }

        if here.op != 0 && here.op & 0xf0 == 0 {
            let last = here;
            loop {
                let bits = self.bit_reader.bits((last.bits + last.op) as usize) as u16;
                here = self.len_table_get((last.val + (bits >> last.bits)) as usize);
                if last.bits + here.bits <= self.bit_reader.bits_in_buffer() {
                    break;
                }

                pull_byte!(self);
            }

            self.bit_reader.drop_bits(last.bits as usize);
            self.back += last.bits as usize;
        }

        self.bit_reader.drop_bits(here.bits as usize);
        self.back += here.bits as usize;
        self.length = here.val as usize;

        if here.op == 0 {
            if self.writer.remaining() == 0 {
                eprintln!("Ok: read_buf is full ({} bytes)", self.writer.capacity());
                return self.inflate_leave(ReturnCode::Ok);
            }

            self.writer.push(self.length as u8);

            self.mode = Mode::Len;

            self.len()
        } else if here.op & 32 != 0 {
            // end of block

            // eprintln!("inflate:         end of block");

            self.back = usize::MAX;
            self.mode = Mode::Type;
            self.type_()
        } else if here.op & 64 != 0 {
            self.mode = Mode::Bad;
            self.bad("invalid literal/length code\0")
        } else {
            // length code
            self.extra = (here.op & MAX_BITS) as usize;
            self.mode = Mode::LenExt;
            self.len_ext()
        }
    }

    fn len_ext(&mut self) -> ReturnCode {
        let extra = self.extra;

        // get extra bits, if any
        if extra != 0 {
            need_bits!(self, extra);
            self.length += self.bit_reader.bits(extra) as usize;
            self.bit_reader.drop_bits(extra);
            self.back += extra;
        }

        // eprintln!("inflate: length {}", state.length);

        self.was = self.length;
        self.mode = Mode::Dist;
        self.dist()
    }

    fn dist(&mut self) -> ReturnCode {
        // get distance code
        let mut here;
        loop {
            let bits = self.bit_reader.bits(self.dist_table.bits) as usize;
            here = self.dist_table_get(bits);
            if here.bits <= self.bit_reader.bits_in_buffer() {
                break;
            }

            pull_byte!(self);
        }

        if here.op & 0xf0 == 0 {
            let last = here;

            loop {
                let bits = self.bit_reader.bits((last.bits + last.op) as usize);
                here = self.dist_table_get(last.val as usize + ((bits as usize) >> last.bits));

                if last.bits + here.bits <= self.bit_reader.bits_in_buffer() {
                    break;
                }

                pull_byte!(self);
            }

            self.bit_reader.drop_bits(last.bits as usize);
            self.back += last.bits as usize;
        }

        self.bit_reader.drop_bits(here.bits as usize);

        if here.op & 64 != 0 {
            self.mode = Mode::Bad;
            return self.bad("invalid distance code\0");
        }

        self.offset = here.val as usize;

        self.extra = (here.op & MAX_BITS) as usize;
        self.mode = Mode::DistExt;
        self.dist_ext()
    }

    fn dist_ext(&mut self) -> ReturnCode {
        let extra = self.extra;

        if extra > 0 {
            need_bits!(self, extra);
            self.offset += self.bit_reader.bits(extra) as usize;
            self.bit_reader.drop_bits(extra);
            self.back += extra;
        }

        if self.offset > self.dmax {
            self.mode = Mode::Bad;
            return self.bad("invalid distance code too far back\0");
        }

        // eprintln!("inflate: distance {}", state.offset);

        self.mode = Mode::Match;
        self.match_()
    }

    /// copy match from window to output

    fn match_(&mut self) -> ReturnCode {
        if self.writer.remaining() == 0 {
            eprintln!(
                "BufError: read_buf is full ({} bytes)",
                self.writer.capacity()
            );
            return self.inflate_leave(ReturnCode::BufError);
        }

        // this is not quite right. not sure when that matters
        let out = self.writer.remaining() + self.writer.len();
        let left = self.writer.remaining();

        let copy = out - left;

        let copy = if self.offset > copy {
            // copy from window to output

            let mut copy = self.offset - copy;

            if copy > self.window.have() {
                if self.sane {
                    self.mode = Mode::Bad;
                    return self.bad("invalid distance too far back\0");
                }

                // TODO INFLATE_ALLOW_INVALID_DISTANCE_TOOFAR_ARRR
                panic!("INFLATE_ALLOW_INVALID_DISTANCE_TOOFAR_ARRR")
            }

            let slice = self.window.copy(copy);
            copy = Ord::min(slice.len(), self.length);
            copy = Ord::min(copy, left);

            self.writer.extend(&slice[..copy]);

            copy
        } else {
            let copy = Ord::min(self.length, self.writer.remaining());
            self.writer.copy_match(self.offset, copy);

            copy
        };

        self.length -= copy;

        if self.length == 0 {
            self.mode = Mode::Len;
            self.len()
        } else {
            // otherwise it seems to recurse?
            self.match_()
        }
    }

    /// get dynamic table entries descriptor

    fn table(&mut self) -> ReturnCode {
        need_bits!(self, 14);
        self.nlen = self.bit_reader.bits(5) as usize + 257;
        self.bit_reader.drop_bits(5);
        self.ndist = self.bit_reader.bits(5) as usize + 1;
        self.bit_reader.drop_bits(5);
        self.ncode = self.bit_reader.bits(4) as usize + 4;
        self.bit_reader.drop_bits(4);

        // TODO pkzit_bug_workaround
        if self.nlen > 286 || self.ndist > 30 {
            self.mode = Mode::Bad;
            return self.bad("too many length or distance symbols\0");
        }

        self.have = 0;
        self.mode = Mode::LenLens;
        self.len_lens()
    }

    /// get code length code lengths (not a typo)

    fn len_lens(&mut self) -> ReturnCode {
        // permutation of code lengths ;
        const ORDER: [u16; 19] = [
            16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15,
        ];

        while self.have < self.ncode {
            need_bits!(self, 3);
            self.lens[ORDER[self.have] as usize] = self.bit_reader.bits(3) as u16;
            self.have += 1;
            self.bit_reader.drop_bits(3);
        }

        while self.have < 19 {
            self.lens[ORDER[self.have] as usize] = 0;
            self.have += 1;
        }

        self.len_table.bits = 7;

        let InflateTable::Success(root) = inflate_table(
            CodeType::Codes,
            &self.lens,
            19,
            &mut self.codes_codes,
            self.len_table.bits,
            &mut self.work,
        ) else {
            self.mode = Mode::Bad;
            return self.bad("invalid code lengths set\0");
        };

        self.len_table.codes = Codes::Codes;
        self.len_table.bits = root;

        self.have = 0;
        self.mode = Mode::CodeLens;
        self.code_lens()
    }

    /// get length and distance code code lengths

    fn code_lens(&mut self) -> ReturnCode {
        while self.have < self.nlen + self.ndist {
            let here = loop {
                let bits = self.bit_reader.bits(self.len_table.bits);
                let here = self.len_table_get(bits as usize);
                if here.bits <= self.bit_reader.bits_in_buffer() {
                    break here;
                }

                pull_byte!(self);
            };

            let here_bits = here.bits as usize;

            match here.val {
                0..=15 => {
                    self.bit_reader.drop_bits(here_bits);
                    self.lens[self.have] = here.val;
                    self.have += 1;
                }
                16 => {
                    need_bits!(self, here_bits + 2);
                    self.bit_reader.drop_bits(here_bits);
                    if self.have == 0 {
                        self.mode = Mode::Bad;
                        return self.bad("invalid bit length repeat\0");
                    }

                    let len = self.lens[self.have - 1];
                    let copy = 3 + self.bit_reader.bits(2) as usize;
                    self.bit_reader.drop_bits(2);

                    if self.have + copy > self.nlen + self.ndist {
                        self.mode = Mode::Bad;
                        return self.bad("invalid bit length repeat\0");
                    }

                    for _ in 0..copy {
                        self.lens[self.have] = len;
                        self.have += 1;
                    }
                }
                17 => {
                    need_bits!(self, here_bits + 3);
                    self.bit_reader.drop_bits(here_bits);
                    let len = 0;
                    let copy = 3 + self.bit_reader.bits(3) as usize;
                    self.bit_reader.drop_bits(3);

                    if self.have + copy > self.nlen + self.ndist {
                        self.mode = Mode::Bad;
                        return self.bad("invalid bit length repeat\0");
                    }

                    for _ in 0..copy {
                        self.lens[self.have] = len as u16;
                        self.have += 1;
                    }
                }
                18.. => {
                    need_bits!(self, here_bits + 7);
                    self.bit_reader.drop_bits(here_bits);
                    let len = 0;
                    let copy = 11 + self.bit_reader.bits(7) as usize;
                    self.bit_reader.drop_bits(7);

                    if self.have + copy > self.nlen + self.ndist {
                        self.mode = Mode::Bad;
                        return self.bad("invalid bit length repeat\0");
                    }

                    for _ in 0..copy {
                        self.lens[self.have] = len as u16;
                        self.have += 1;
                    }
                }
            }
        }

        // check for end-of-block code (better have one)
        if self.lens[256] == 0 {
            self.mode = Mode::Bad;
            return self.bad("invalid code -- missing end-of-block\0");
        }

        // build code tables

        self.len_table.bits = 10;

        let InflateTable::Success(root) = inflate_table(
            CodeType::Lens,
            &self.lens,
            self.nlen,
            &mut self.len_codes,
            self.len_table.bits,
            &mut self.work,
        ) else {
            self.mode = Mode::Bad;
            return self.bad("invalid literal/lengths set\0");
        };

        self.len_table.codes = Codes::Len;
        self.len_table.bits = root;

        self.dist_table.bits = 9;

        let InflateTable::Success(root) = inflate_table(
            CodeType::Dists,
            &self.lens[self.nlen..],
            self.ndist,
            &mut self.dist_codes,
            self.dist_table.bits,
            &mut self.work,
        ) else {
            self.mode = Mode::Bad;
            return self.bad("invalid distances set\0");
        };

        self.dist_table.bits = root;
        self.dist_table.codes = Codes::Dist;

        self.mode = Mode::Len;

        if matches!(self.flush, Flush::Trees) {
            return self.inflate_leave(ReturnCode::Ok);
        }

        self.len()
    }

    fn dict_id(&mut self) -> ReturnCode {
        need_bits!(self, 32);

        self.checksum = zswap32(self.bit_reader.hold() as u32);

        self.bit_reader.init_bits();

        self.mode = Mode::Dict;
        self.dict()
    }

    fn dict(&mut self) -> ReturnCode {
        if !self.havedict {
            return self.inflate_leave(ReturnCode::NeedDict);
        }

        self.checksum = crate::ADLER32_INITIAL_VALUE as _;

        self.mode = Mode::Type;
        self.type_()
    }

    fn mem(&mut self) -> ReturnCode {
        self.inflate_leave(ReturnCode::MemError)
    }

    fn bad(&mut self, msg: &'static str) -> ReturnCode {
        dbg!(msg);
        self.error_message = Some(msg);
        self.inflate_leave(ReturnCode::DataError)
    }

    // NOTE: it is crucial for the internal bookkeeping that this is the only route for actually
    // leaving the inflate function call chain
    fn inflate_leave(&mut self, return_code: ReturnCode) -> ReturnCode {
        // actual logic is in `inflate` itself
        return_code
    }
}

fn inflate_fast_help(state: &mut State, _start: usize) -> ReturnCode {
    let mut bit_reader = BitReader::new(&[]);
    std::mem::swap(&mut bit_reader, &mut state.bit_reader);

    let mut writer = ReadBuf::new(&mut []);
    std::mem::swap(&mut writer, &mut state.writer);

    let lcode = state.len_table_ref();
    let dcode = state.dist_table_ref();

    // IDEA: use const generics for the bits here?
    let lmask = (1u64 << state.len_table.bits) - 1;
    let dmask = (1u64 << state.dist_table.bits) - 1;

    // TODO verify if this is relevant for us
    let extra_safe = false;

    let mut bad = None;

    'outer: loop {
        bit_reader.refill();

        let mut here = lcode[(bit_reader.hold() & lmask) as usize];

        if here.op == 0 {
            writer.push(here.val as u8);
            bit_reader.drop_bits(here.bits as usize);
            here = lcode[(bit_reader.hold() & lmask) as usize];

            if here.op == 0 {
                writer.push(here.val as u8);
                bit_reader.drop_bits(here.bits as usize);
                here = lcode[(bit_reader.hold() & lmask) as usize];
            }
        }

        'dolen: loop {
            bit_reader.drop_bits(here.bits as usize);
            let op = here.op;

            if op == 0 {
                writer.push(here.val as u8);
            } else if op & 16 != 0 {
                let op = op & MAX_BITS;
                let len = here.val + bit_reader.bits(op as usize) as u16;
                bit_reader.drop_bits(op as usize);

                here = dcode[(bit_reader.hold() & dmask) as usize];
                if bit_reader.bits_in_buffer() < MAX_BITS + MAX_DIST_EXTRA_BITS {
                    bit_reader.refill();
                }

                'dodist: loop {
                    bit_reader.drop_bits(here.bits as usize);
                    let op = here.op;

                    if op & 16 != 0 {
                        let op = op & MAX_BITS;
                        let dist = here.val + bit_reader.bits(op as usize) as u16;

                        if dist as usize > state.dmax {
                            bad = Some("invalid distance too far back\0");
                            state.mode = Mode::Bad;
                            break 'outer;
                        }

                        bit_reader.drop_bits(op as usize);

                        // max distance in output
                        let written = writer.len();

                        if dist as usize > written {
                            // copy fropm the window
                            if (dist as usize - written) > state.window.have() {
                                if state.sane {
                                    bad = Some("invalid distance too far back\0");
                                    state.mode = Mode::Bad;
                                    break 'outer;
                                }

                                panic!("INFLATE_ALLOW_INVALID_DISTANCE_TOOFAR_ARRR")
                            }

                            // horrible reuse of this variable; find a good name
                            let op = dist as usize - written;

                            if state.window.next() == 0 {
                                // very common case (TODO: why?)
                                todo!()
                            } else if state.window.next() >= op {
                                let slice = &state.window.copy(op)[..Ord::min(op, len as usize)];
                                writer.extend(slice);
                            } else {
                                // wrap around in window
                                todo!()
                            }

                            // may need some bytes from the output
                            if op < len as usize {
                                writer.copy_match(dist as usize, len as usize - op);
                            }
                        } else if extra_safe {
                            todo!()
                        } else {
                            writer.copy_match(dist as usize, len as usize)
                        }
                    } else if (op & 64) == 0 {
                        // 2nd level distance code
                        here = dcode[(here.val + bit_reader.bits(op as usize) as u16) as usize];
                        continue 'dodist;
                    } else {
                        bad = Some("invalid distance code\0");
                        state.mode = Mode::Bad;
                        break 'outer;
                    }

                    break 'dodist;
                }
            } else if (op & 64) == 0 {
                // 2nd level length code
                here = lcode[(here.val + bit_reader.bits(op as usize) as u16) as usize];
                continue 'dolen;
            } else if op & 32 != 0 {
                // end of block
                state.mode = Mode::Type;
                break 'outer;
            } else {
                bad = Some("invalid literal/length code\0");
                state.mode = Mode::Bad;
                break 'outer;
            }

            break 'dolen;
        }

        let remaining = bit_reader.bytes_remaining();
        if remaining.saturating_sub(INFLATE_FAST_MIN_LEFT - 1) > 0
            && writer.remaining() > INFLATE_FAST_MIN_LEFT
        {
            continue;
        }

        break 'outer;
    }

    // return unused bytes (on entry, bits < 8, so in won't go too far back)
    bit_reader.return_unused_bytes();

    state.bit_reader = bit_reader;
    state.writer = writer;

    match state.mode {
        Mode::Type => state.type_(),
        Mode::Len => state.len(),
        Mode::Bad => state.bad(bad.unwrap()),
        _ => unreachable!(),
    }
}

pub fn prime(stream: &mut InflateStream, bits: i32, value: i32) -> ReturnCode {
    if bits == 0 {
        /* fall through */
    } else if bits < 0 {
        stream.state.bit_reader.init_bits();
    } else if bits > 16 || stream.state.bit_reader.bits_in_buffer() + bits as u8 > 32 {
        return ReturnCode::StreamError;
    } else {
        stream.state.bit_reader.prime(bits as u8, value as u64);
    }

    ReturnCode::Ok
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct InflateConfig {
    pub window_bits: i32,
}

impl Default for InflateConfig {
    fn default() -> Self {
        Self {
            window_bits: DEF_WBITS,
        }
    }
}

/// Initialize the stream in an inflate state
pub fn init(stream: &mut z_stream, config: InflateConfig) -> ReturnCode {
    stream.msg = std::ptr::null_mut();

    if stream.zalloc.is_none() {
        stream.zalloc = Some(allocate::zcalloc);
        stream.opaque = std::ptr::null_mut();
    }

    if stream.zfree.is_none() {
        stream.zfree = Some(allocate::zcfree);
    }

    let mut state = State::new(&[], ReadBuf::new(&mut []));

    // state.window = None;
    // state.mode = Mode::Head;

    // TODO this can change depending on the used/supported SIMD instructions
    state.chunksize = 32;

    // SAFETY: we assume allocation does not cause UB
    stream.state = unsafe { stream.alloc_value(state).cast() };

    if stream.state.is_null() {
        return ReturnCode::MemError as _;
    }

    // SAFETY: we've correctly initialized the stream to be an InflateStream
    let ret = if let Some(stream) = unsafe { InflateStream::from_stream_mut(stream) } {
        reset_with_config(stream, config)
    } else {
        ReturnCode::StreamError
    };

    if ret != ReturnCode::Ok as _ {
        let ptr = stream.state;
        stream.state = std::ptr::null_mut();
        // SAFETY: we assume deallocation does not cause UB
        unsafe { stream.dealloc(ptr) };
    }

    ret
}

pub fn reset_with_config(stream: &mut InflateStream, config: InflateConfig) -> ReturnCode {
    let mut window_bits = config.window_bits;
    let wrap;

    if window_bits < 0 {
        wrap = 0;

        if window_bits < -MAX_WBITS {
            return ReturnCode::StreamError;
        }

        window_bits = -window_bits;
    } else {
        wrap = (window_bits >> 4) + 5; // TODO wth?

        if window_bits < 48 {
            window_bits &= MAX_WBITS;
        }
    }

    if window_bits != 0 && !(MIN_WBITS..=MAX_WBITS).contains(&window_bits) {
        eprintln!("invalid windowBits");
        return ReturnCode::StreamError;
    }

    if stream.state.window.size() != 0 && stream.state.wbits != window_bits as usize {
        let mut window = Window::empty();
        std::mem::swap(&mut window, &mut stream.state.window);

        if window.size() != 0 {
            unsafe { stream.dealloc(window.as_mut_slice().as_mut_ptr() as *mut u8) };
        }
    }

    stream.state.wrap = wrap as usize;
    stream.state.wbits = window_bits as _;

    reset(stream)
}

pub fn reset(stream: &mut InflateStream) -> ReturnCode {
    // reset the state of the window
    stream.state.window.clear();

    stream.state.error_message = None;

    reset_keep(stream)
}

pub fn reset_keep(stream: &mut InflateStream) -> ReturnCode {
    stream.total_in = 0;
    stream.total_out = 0;
    // stream.state.total = 0;

    stream.msg = std::ptr::null_mut();

    let state = &mut stream.state;

    if state.wrap != 0 {
        //  to support ill-conceived Java test suite
        // stream.adler = state.wrap & 1;
    }

    state.mode = Mode::Head;
    state.checksum = crate::ADLER32_INITIAL_VALUE as u32;

    state.last = false;
    state.havedict = false;
    state.flags = -1;
    state.dmax = 32768;
    //    state.head = NULL;
    state.bit_reader = BitReader::new(&[]);

    state.next = 0;
    state.len_table = Table::default();
    state.dist_table = Table::default();

    state.sane = true;
    state.back = usize::MAX;

    ReturnCode::Ok
}

pub unsafe fn inflate(stream: &mut InflateStream, flush: Flush) -> ReturnCode {
    if stream.next_out.is_null() || (stream.next_in.is_null() && stream.avail_in != 0) {
        return ReturnCode::StreamError as _;
    }

    let source_slice = std::slice::from_raw_parts(stream.next_in, stream.avail_in as usize);
    let dest_slice = std::slice::from_raw_parts_mut(stream.next_out, stream.avail_out as usize);

    let state = &mut stream.state;

    state.bit_reader.update_slice(source_slice);
    state.writer = ReadBuf::new(dest_slice);

    state.in_available = stream.avail_in as _;
    state.out_available = stream.avail_out as _;

    let mut err = state.dispatch();

    let in_read = state.bit_reader.as_ptr() as usize - stream.next_in as usize;
    let out_written = state.writer.as_mut_ptr() as usize - stream.next_out as usize;

    stream.total_out += out_written as u64;
    stream.total_in += in_read as u64;

    stream.avail_in = state.bit_reader.bytes_remaining() as u32;
    stream.next_in = state.bit_reader.as_ptr() as *mut u8;

    stream.avail_out = (state.writer.capacity() - state.writer.len()) as u32;
    stream.next_out = state.writer.as_mut_ptr() as *mut u8;

    stream.adler = state.checksum as u64;

    let valid_mode = |mode| !matches!(mode, Mode::Bad | Mode::Mem | Mode::Sync);
    let not_done = |mode| {
        !matches!(
            mode,
            Mode::Check | Mode::Length | Mode::Bad | Mode::Mem | Mode::Sync
        )
    };

    let must_update_window = state.window.size() != 0
        || (out_written != 0
            && valid_mode(state.mode)
            && (not_done(state.mode) || !matches!(state.flush, Flush::Finish)));

    if must_update_window && update_window(stream, out_written) != ReturnCode::Ok {
        stream.state.mode = Mode::Mem;
        err = ReturnCode::MemError;
    }

    if let Some(msg) = stream.state.error_message {
        assert!(msg.ends_with(|c| c == '\0'));
        stream.msg = msg.as_ptr() as *mut u8 as *mut i8;
    }

    if ((in_read == 0 && out_written == 0) || flush == Flush::Finish as _)
        && err == (ReturnCode::Ok as _)
    {
        ReturnCode::BufError as _
    } else {
        err as _
    }
}

fn syncsearch(mut got: usize, buf: &[u8]) -> (usize, usize) {
    let len = buf.len();
    let mut next = 0;

    while next < len && got < 4 {
        if buf[next] == if got < 2 { 0 } else { 0xff } {
            got += 1;
        } else if buf[next] != 0 {
            got = 0;
        } else {
            got = 4 - got;
        }
        next += 1;
    }

    (got, next)
}

pub fn sync(stream: &mut InflateStream) -> ReturnCode {
    let state = &mut stream.state;

    if stream.avail_in == 0 && state.bit_reader.bits_in_buffer() < 8 {
        return ReturnCode::BufError as _;
    }
    /* if first time, start search in bit buffer */
    if !matches!(state.mode, Mode::Sync) {
        state.mode = Mode::Sync;

        let (buf, len) = state.bit_reader.start_sync_search();

        (state.have, _) = syncsearch(0, &buf[..len]);
    }

    // search available input
    let slice = unsafe { std::slice::from_raw_parts(stream.next_in, stream.avail_in as usize) };

    let len;
    (state.have, len) = syncsearch(state.have, slice);
    stream.next_in = unsafe { stream.next_in.add(len) };
    stream.avail_in -= len as u32;
    stream.total_in += len as u64;

    /* return no joy or set up to restart inflate() on a new block */
    if state.have != 4 {
        return ReturnCode::DataError as _;
    }

    if state.flags == -1 {
        state.wrap = 0; /* if no header yet, treat as raw */
    } else {
        state.wrap &= !4; /* no point in computing a check value now */
    }

    let flags = state.flags;
    let total_in = stream.total_in;
    let total_out = stream.total_out;

    reset(stream);

    stream.total_in = total_in;
    stream.total_out = total_out;

    stream.state.flags = flags;
    stream.state.mode = Mode::Type;

    ReturnCode::Ok as _
}

/*
  Returns true if inflate is currently at the end of a block generated by
  Z_SYNC_FLUSH or Z_FULL_FLUSH. This function is used by one PPP
  implementation to provide an additional safety check. PPP uses
  Z_SYNC_FLUSH but removes the length bytes of the resulting empty stored
  block. When decompressing, PPP checks that at the end of input packet,
  inflate is waiting for these length bytes.
*/
pub fn sync_point(stream: &mut InflateStream) -> bool {
    matches!(stream.state.mode, Mode::Stored) && stream.state.bit_reader.bits_in_buffer() == 0
}

pub unsafe fn copy(dest: *mut z_stream, source: &InflateStream) -> ReturnCode {
    let stream = source;

    if stream.next_out.is_null() || (stream.next_in.is_null() && stream.avail_in != 0) {
        return ReturnCode::StreamError;
    }

    let layout = std::alloc::Layout::array::<State>(1).unwrap();

    let destination = z_stream {
        next_in: stream.next_in,
        avail_in: stream.avail_in,
        total_in: stream.total_in,
        next_out: stream.next_out,
        avail_out: stream.avail_out,
        total_out: stream.total_out,
        msg: stream.msg,
        state: stream.alloc_layout(layout) as *mut crate::c_api::internal_state,
        zalloc: Some(stream.zalloc),
        zfree: Some(stream.zfree),
        opaque: stream.opaque,
        data_type: stream.data_type,
        adler: stream.adler,
        reserved: stream.reserved,
    };

    if destination.state.is_null() {
        return ReturnCode::MemError;
    }

    let state = &stream.state;

    let writer: MaybeUninit<ReadBuf> =
        unsafe { std::ptr::read(&state.writer as *const _ as *const MaybeUninit<ReadBuf>) };

    let mut copy = State {
        mode: state.mode,
        last: state.last,
        wrap: state.wrap,
        len_table: state.len_table,
        dist_table: state.dist_table,
        wbits: state.wbits,
        window: Window::empty(),
        head: None,
        ncode: state.ncode,
        nlen: state.nlen,
        ndist: state.ndist,
        have: state.have,
        next: state.next,
        bit_reader: state.bit_reader,
        writer: ReadBuf::new(&mut []),
        length: state.length,
        offset: state.offset,
        extra: state.extra,
        sane: state.sane,
        back: state.back,
        was: state.was,
        chunksize: state.chunksize,
        in_available: state.in_available,
        out_available: state.out_available,
        lens: state.lens,
        work: state.work,
        error_message: state.error_message,
        flush: state.flush,
        checksum: state.checksum,
        havedict: state.havedict,
        dmax: state.dmax,
        flags: state.flags,
        codes_codes: state.codes_codes,
        len_codes: state.len_codes,
        dist_codes: state.dist_codes,
    };

    // TODO make sure the codes point to the right thing

    if !state.window.is_empty() {
        let source = state.window.as_slice();

        let layout = std::alloc::Layout::array::<MaybeUninit<u8>>(source.len()).unwrap();
        let dst = stream.alloc_layout(layout) as *mut MaybeUninit<u8>;

        if dst.is_null() {
            stream.dealloc(destination.state);
            return ReturnCode::MemError;
        }

        unsafe { std::ptr::copy_nonoverlapping(source.as_ptr(), dst, source.len()) }

        copy.window = Window::from_raw_parts(dst, source.len())
    }

    unsafe { std::ptr::write(destination.state.cast(), copy) };

    // update the writer; it cannot be cloned so we need to use some shennanigans
    let field_ptr = unsafe { std::ptr::addr_of_mut!((*(destination.state as *mut State)).writer) };
    unsafe { std::ptr::copy(writer.as_ptr(), field_ptr, 1) };

    // TODO similarly update the gzip header

    unsafe { std::ptr::write(dest, destination) };

    ReturnCode::Ok
}

pub fn undermine(stream: &mut InflateStream, subvert: i32) -> ReturnCode {
    stream.state.sane = (!subvert) != 0;

    ReturnCode::Ok
}

pub fn mark(stream: &InflateStream) -> c_long {
    if stream.next_out.is_null() || (stream.next_in.is_null() && stream.avail_in != 0) {
        return c_long::MIN;
    }

    let state = &stream.state;

    let length = match state.mode {
        Mode::CopyBlock => state.length,
        Mode::Match => state.was - state.length,
        _ => 0,
    };

    (((state.back as c_long) as c_ulong) << 16) as c_long + length as c_long
}

pub fn set_dictionary(stream: &mut InflateStream, dictionary: &[u8]) -> ReturnCode {
    if stream.state.wrap != 0 && !matches!(stream.state.mode, Mode::Dict) {
        return ReturnCode::StreamError;
    }

    // check for correct dictionary identifier
    if matches!(stream.state.mode, Mode::Dict) {
        let dictid = adler32(1, dictionary);

        if dictid != stream.state.checksum {
            return ReturnCode::StreamError;
        }
    }

    let err = 'blk: {
        // initialize the window if needed
        if stream.state.window.size() == 0 {
            match init_window(
                |layout| unsafe { stream.alloc_layout(layout) },
                stream.state.wbits,
                stream.state.chunksize,
            ) {
                Err(e) => break 'blk e,
                Ok(window) => stream.state.window = window,
            }
        }

        stream.state.checksum = stream
            .state
            .window
            .extend(dictionary, stream.state.checksum);

        ReturnCode::Ok
    };

    if err != ReturnCode::Ok {
        stream.state.mode = Mode::Mem;
        return ReturnCode::MemError;
    }

    stream.state.havedict = true;

    ReturnCode::Ok
}

/// # Safety
///
/// The `strm` must be either NULL or a valid mutable reference to a z_stream value where the state
/// has been initialized with `inflateInit_` or `inflateInit2_`.
pub unsafe extern "C" fn end(strm: *mut z_stream) -> i32 {
    let Some(stream) = InflateStream::from_stream_mut(strm) else {
        return ReturnCode::StreamError as _;
    };

    let mut state = State::new(&[], ReadBuf::new(&mut []));
    std::mem::swap(&mut state, stream.state);

    let mut window = Window::empty();
    std::mem::swap(&mut window, &mut state.window);

    if window.size() != 0 {
        stream.dealloc(window.as_mut_slice().as_mut_ptr() as *mut u8);
    }

    // safety: a valid &mut InflateStream is also a valid &mut z_stream
    let stream = unsafe { &mut *strm };

    let state_ptr = std::mem::replace(&mut stream.state, std::ptr::null_mut());
    stream.dealloc(state_ptr);

    ReturnCode::Ok as _
}

fn update_window(stream: &mut InflateStream, bytes_written: usize) -> ReturnCode {
    // initialize the window if needed
    if stream.state.window.size() == 0 {
        match init_window(
            |layout| unsafe { stream.alloc_layout(layout) },
            stream.state.wbits,
            stream.state.chunksize,
        ) {
            Err(e) => return e,
            Ok(window) => stream.state.window = window,
        }
    }

    stream.state.checksum = stream.state.window.extend(
        &stream.state.writer.filled()[..bytes_written],
        stream.state.checksum,
    );

    ReturnCode::Ok
}

fn init_window<'a>(
    alloc_layout: impl FnOnce(Layout) -> *mut c_void,
    wbits: usize,
    chunk_size: usize,
) -> Result<Window<'a>, ReturnCode> {
    // TODO check whether not including the chunk_size bytes in Window causes UB
    let wsize = (1 << wbits) as usize;
    let layout = Layout::from_size_align(wsize + chunk_size, 1).unwrap();
    let ptr = alloc_layout(layout) as *mut u8;

    if ptr.is_null() {
        return Err(ReturnCode::MemError);
    }

    let window = unsafe { Window::from_raw_parts(ptr as *mut MaybeUninit<u8>, wsize) };

    Ok(window)
}

pub fn get_header<'a>(stream: &'a mut InflateStream<'a>, head: &'a mut gz_header) -> ReturnCode {
    if (stream.state.wrap & 2) == 0 {
        return ReturnCode::StreamError;
    }

    head.done = 0;
    stream.state.head = Some(head);
    ReturnCode::Ok
}
