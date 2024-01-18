use std::mem::MaybeUninit;

use crate::{
    adler32::adler32, deflateEnd, trace, z_stream, Flush, ReturnCode, ADLER32_INITIAL_VALUE,
    MAX_WBITS, MIN_WBITS, Z_DEFLATED, Z_UNKNOWN,
};

use self::{
    algorithm::CONFIGURATION_TABLE,
    hash_calc::{Crc32HashCalc, HashCalc, RollHashCalc, StandardHashCalc, ZlibHashMap},
    pending::Pending,
    window::Window,
};

mod algorithm;
mod compare256;
mod hash_calc;
mod longest_match;
mod pending;
mod slide_hash;
mod trees_tbl;
mod window;

#[repr(C)]
pub(crate) struct DeflateStream<'a> {
    pub next_in: *mut crate::c_api::Bytef,
    pub avail_in: crate::c_api::uInt,
    pub total_in: crate::c_api::z_size,
    pub next_out: *mut crate::c_api::Bytef,
    pub avail_out: crate::c_api::uInt,
    pub total_out: crate::c_api::z_size,
    pub msg: *const libc::c_char,
    pub state: &'a mut State<'a>,
    pub zalloc: crate::c_api::alloc_func,
    pub zfree: crate::c_api::free_func,
    pub opaque: crate::c_api::voidpf,
    pub data_type: libc::c_int,
    pub adler: crate::c_api::z_checksum,
    pub reserved: crate::c_api::uLong,
}

impl<'a> DeflateStream<'a> {
    const _S: () = assert!(core::mem::size_of::<z_stream>() == core::mem::size_of::<Self>());
    const _A: () = assert!(core::mem::align_of::<z_stream>() == core::mem::align_of::<Self>());

    /// # Safety
    ///
    /// The `strm` pointer must be either `NULL` or a correctly initalized `z_stream`. Here
    /// correctly initalized does not just mean that the pointer is valid and well-aligned, but
    /// also that it has been initialized by that `deflateInit_` or `deflateInit2_`.
    #[inline(always)]
    pub(crate) unsafe fn from_stream_mut(strm: *mut z_stream) -> Option<&'a mut Self> {
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

        // safety: DeflateStream has the same layout as z_stream
        let stream = unsafe { &mut *(strm as *mut DeflateStream) };

        Some(stream)
    }
}

/// number of elements in hash table
pub(crate) const HASH_SIZE: usize = 65536;

/// Maximum value for memLevel in deflateInit2
const MAX_MEM_LEVEL: i32 = 9;
const DEF_MEM_LEVEL: i32 = if MAX_MEM_LEVEL > 8 { 8 } else { MAX_MEM_LEVEL };

pub fn init(strm: *mut z_stream, level: i32) -> ReturnCode {
    init2(
        strm,
        level,
        crate::c_api::Z_DEFLATED,
        crate::MAX_WBITS,
        DEF_MEM_LEVEL,
        crate::c_api::Z_DEFAULT_STRATEGY,
    )
}

pub fn init2(
    strm: *mut z_stream,
    mut level: i32,
    method: i32,
    mut window_bits: i32,
    mem_level: i32,
    strategy: i32,
) -> ReturnCode {
    /* Todo: ignore strm->next_in if we use it as window */
    let window_padding = 0;
    let mut wrap = 1;

    if strm.is_null() {
        return ReturnCode::StreamError;
    }

    let stream = unsafe { &mut *strm };

    stream.msg = std::ptr::null_mut();

    if stream.zalloc.is_none() {
        stream.zalloc = Some(crate::allocate::zcalloc);
        stream.opaque = std::ptr::null_mut();
    }

    if stream.zfree.is_none() {
        stream.zfree = Some(crate::allocate::zcfree);
    }

    if level == crate::c_api::Z_DEFAULT_COMPRESSION {
        level = 6;
    }

    if window_bits < 0 {
        wrap = 0;
        if window_bits < -MAX_WBITS {
            return ReturnCode::StreamError;
        }
        window_bits = -window_bits;
    } else if window_bits > MAX_WBITS {
        wrap = 2;
        window_bits -= 16;
    }

    let Ok(strategy) = Strategy::try_from(strategy) else {
        return ReturnCode::StreamError;
    };

    if (!(1..=MAX_MEM_LEVEL).contains(&mem_level))
        || method != Z_DEFLATED
        || !(MIN_WBITS..=MAX_WBITS).contains(&window_bits)
        || !(0..=9).contains(&level)
        || (window_bits == 8 && wrap != 1)
    {
        return ReturnCode::StreamError;
    }

    if window_bits == 8 {
        window_bits = 9; /* until 256-byte window bug fixed */
    }

    // allocated here to have the same order as zlib
    let state_ptr = unsafe { stream.alloc_layout(std::alloc::Layout::new::<State>()) };

    if state_ptr.is_null() {
        return ReturnCode::MemError;
    }

    let w_size = 1 << window_bits;
    let window_layout = std::alloc::Layout::array::<u16>(2 * (w_size + window_padding));
    let window_ptr = unsafe { stream.alloc_layout(window_layout.unwrap()) } as *mut MaybeUninit<u8>;

    let prev_layout = std::alloc::Layout::array::<u16>(w_size);
    let prev_ptr = unsafe { stream.alloc_layout(prev_layout.unwrap()) } as *mut u16;

    let head_layout = std::alloc::Layout::array::<u16>(HASH_SIZE);
    let head_ptr = unsafe { stream.alloc_layout(head_layout.unwrap()) } as *mut [u16; HASH_SIZE];

    let lit_bufsize = 1 << (mem_level + 6); // 16K elements by default
    let pending_buf_layout = std::alloc::Layout::array::<u32>(lit_bufsize);
    let pending_buf = unsafe { stream.alloc_layout(pending_buf_layout.unwrap()) } as *mut u8;

    if window_ptr.is_null() || prev_ptr.is_null() || head_ptr.is_null() || pending_buf.is_null() {
        todo!("mem error");
        // return ReturnCode::MemError;
    }

    let window =
        unsafe { Window::from_raw_parts(window_ptr, 2 * (w_size + window_padding), window_bits) };

    unsafe { std::ptr::write_bytes(prev_ptr, 0, w_size) }; // initialize!
    let prev = unsafe { std::slice::from_raw_parts_mut(prev_ptr, w_size) };

    let head = unsafe { &mut *head_ptr };

    let hash_map = ZlibHashMap::new(prev, head);

    let pending = unsafe { Pending::from_raw_parts(pending_buf, lit_bufsize) };

    let pending_end = pending_buf.wrapping_add(lit_bufsize);
    unsafe { std::ptr::write_bytes(pending_end, 0, 3 * lit_bufsize) }; // initialize!
    let sym_buf = unsafe { std::slice::from_raw_parts_mut(pending_end, 3 * lit_bufsize) };

    let state = State {
        status: Status::Init,

        // window
        w_bits: window_bits as usize,
        w_size,
        w_mask: w_size - 1,

        // allocated values
        window,
        pending,
        hash_map,

        //
        lit_bufsize,

        //
        sym_buf,
        sym_end: (lit_bufsize - 1) * 3,

        //
        level: level as i8, // set to zero again for testing?
        strategy,

        // these fields are not set explicitly at this point
        last_flush: 0,
        bi_buf: 0,
        bi_valid: 0,
        wrap,
        strstart: 0,
        block_start: 0,
        block_open: 0,
        window_size: 0,
        insert: 0,
        matches: 0,
        opt_len: 0,
        static_len: 0,
        lookahead: 0,
        sym_next: 0,
        ins_h: 0,
        max_chain_length: 0,
        max_lazy_match: 0,
        good_match: 0,
        nice_match: 0,

        //
        l_desc: TreeDesc::EMPTY,
        d_desc: TreeDesc::EMPTY,
        bl_desc: TreeDesc::EMPTY,

        bl_count: [0u16; MAX_BITS + 1],

        //
        heap: Heap::new(),

        //
        match_start: 0,
        match_length: 0,
        prev_match: 0,
        match_available: 0,
        prev_length: 0,

        // just provide a valid default; gets set properly later
        update_hash: StandardHashCalc::update_hash,
        insert_string: StandardHashCalc::insert_string,
        quick_insert_string: StandardHashCalc::quick_insert_string,
    };

    unsafe { *(state_ptr as *mut State) = state };
    stream.state = state_ptr.cast();

    let Some(stream) = (unsafe { DeflateStream::from_stream_mut(strm) }) else {
        return ReturnCode::StreamError;
    };

    reset(stream)
}

pub unsafe fn end(strm: *mut z_stream) -> i32 {
    let Some(stream) = DeflateStream::from_stream_mut(strm) else {
        return ReturnCode::StreamError as _;
    };

    let status = stream.state.status;

    let pending = stream.state.pending.as_mut_ptr();
    let head = stream.state.hash_map.head.as_mut_ptr();
    let prev = stream.state.hash_map.prev.as_mut_ptr();
    let window = stream.state.window.as_mut_ptr();
    let state = stream.state as *mut State;

    let opaque = stream.opaque;
    let free = stream.zfree;

    // safety: a valid &mut DeflateStream is also a valid &mut z_stream
    let stream = unsafe { &mut *strm };
    stream.state = std::ptr::null_mut();

    // deallocate in reverse order of allocations
    unsafe { free(opaque, pending.cast()) };
    unsafe { free(opaque, head.cast()) };
    unsafe { free(opaque, prev.cast()) };
    unsafe { free(opaque, window.cast()) };

    unsafe { free(opaque, state.cast()) };

    match status {
        Status::Busy => ReturnCode::DataError as i32,
        _ => ReturnCode::Ok as i32,
    }
}

fn reset(stream: &mut DeflateStream) -> ReturnCode {
    let ret = reset_keep(stream);

    if ret == ReturnCode::Ok {
        lm_init(stream.state);
    }

    ret
}

fn reset_keep(stream: &mut DeflateStream) -> ReturnCode {
    stream.total_in = 0;
    stream.total_out = 0;
    stream.msg = std::ptr::null_mut();
    stream.data_type = crate::c_api::Z_UNKNOWN;

    let state = &mut stream.state;

    state.pending.reset_keep();

    if state.wrap < 0 {
        // was made negative by deflate(..., Z_FINISH);
        state.wrap = -state.wrap;
    }

    // TODO gzip
    state.status = Status::Init;

    // TODO gzip
    stream.adler = ADLER32_INITIAL_VALUE as _;
    state.last_flush = -2;

    state.zng_tr_init();

    ReturnCode::Ok
}

fn lm_init(state: &mut State) {
    state.window_size = 2 * state.w_size;

    // zlib uses CLEAR_HASH here
    state.hash_map.clear();

    // Set the default configuration parameters:
    lm_set_level(state, state.level);

    state.strstart = 0;
    state.block_start = 0;
    state.lookahead = 0;
    state.insert = 0;
    state.prev_length = 0;
    state.match_available = 0;
    state.match_start = 0;
    state.ins_h = 0;
}

fn lm_set_level(state: &mut State, level: i8) {
    state.max_lazy_match = CONFIGURATION_TABLE[level as usize].max_lazy as usize;
    state.good_match = CONFIGURATION_TABLE[level as usize].good_length as usize;
    state.nice_match = CONFIGURATION_TABLE[level as usize].nice_length as usize;
    state.max_chain_length = CONFIGURATION_TABLE[level as usize].max_chain as usize;

    // Use rolling hash for deflate_slow algorithm with level 9. It allows us to
    // properly lookup different hash chains to speed up longest_match search. Since hashing
    // method changes depending on the level we cannot put this into functable. */
    if state.max_chain_length > 1024 {
        state.update_hash = RollHashCalc::update_hash;
        state.insert_string = RollHashCalc::insert_string;
        state.quick_insert_string = RollHashCalc::quick_insert_string;
    } else {
        // NOTE: this is hardcoded currently
        state.update_hash = Crc32HashCalc::update_hash;
        state.insert_string = Crc32HashCalc::insert_string;
        state.quick_insert_string = Crc32HashCalc::quick_insert_string;
    }

    state.level = level;
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Value {
    a: u16,
    b: u16,
}

impl Value {
    pub(crate) const fn new(a: u16, b: u16) -> Self {
        Self { a, b }
    }

    pub(crate) fn freq_mut(&mut self) -> &mut u16 {
        &mut self.a
    }

    pub(crate) fn code_mut(&mut self) -> &mut u16 {
        &mut self.a
    }

    pub(crate) fn dad_mut(&mut self) -> &mut u16 {
        &mut self.b
    }

    pub(crate) fn len_mut(&mut self) -> &mut u16 {
        &mut self.b
    }

    #[inline(always)]
    pub(crate) const fn freq(self) -> u16 {
        self.a
    }

    pub(crate) fn code(self) -> u16 {
        self.a
    }

    pub(crate) fn dad(self) -> u16 {
        self.b
    }

    pub(crate) fn len(self) -> u16 {
        self.b
    }
}

/// number of length codes, not counting the special END_BLOCK code
pub(crate) const LENGTH_CODES: usize = 29;

/// number of literal bytes 0..255
const LITERALS: usize = 256;

/// number of Literal or Length codes, including the END_BLOCK code
pub(crate) const L_CODES: usize = LITERALS + 1 + LENGTH_CODES;

/// number of distance codes
pub(crate) const D_CODES: usize = 30;

/// number of codes used to transfer the bit lengths
const BL_CODES: usize = 19;

/// maximum heap size
const HEAP_SIZE: usize = 2 * L_CODES + 1;

/// all codes must not exceed MAX_BITS bits
const MAX_BITS: usize = 15;

/// Bit length codes must not exceed MAX_BL_BITS bits
const MAX_BL_BITS: usize = 7;

pub(crate) const DIST_CODE_LEN: usize = 512;

#[allow(unused)]
#[repr(C)]
pub(crate) struct State<'a> {
    status: Status,

    pub(crate) pending: Pending<'a>, // output still pending

    last_flush: i32, /* value of flush param for previous deflate call */

    bi_buf: u64,
    pub(crate) bi_valid: u8,

    pub(crate) wrap: i8, /* bit 0 true for zlib, bit 1 true for gzip */

    pub(crate) strategy: Strategy,
    pub(crate) level: i8,

    /// Use a faster search when the previous match is longer than this
    pub(crate) good_match: usize,

    /// Stop searching when current match exceeds this
    pub(crate) nice_match: usize,

    // part of the fields below
    //    dyn_ltree: [Value; ],
    //    dyn_dtree: [Value; ],
    //    bl_tree: [Value; ],
    l_desc: TreeDesc<HEAP_SIZE>,             /* literal and length tree */
    d_desc: TreeDesc<{ 2 * D_CODES + 1 }>,   /* distance tree */
    bl_desc: TreeDesc<{ 2 * BL_CODES + 1 }>, /* Huffman tree for bit lengths */

    pub(crate) bl_count: [u16; MAX_BITS + 1],

    pub(crate) match_length: usize,    /* length of best match */
    pub(crate) prev_match: u16,        /* previous match */
    pub(crate) match_available: isize, /* set if previous match exists */
    pub(crate) strstart: usize,        /* start of string to insert */
    pub(crate) match_start: usize,     /* start of matching string */

    /// Length of the best match at previous step. Matches not greater than this
    /// are discarded. This is used in the lazy match evaluation.
    pub(crate) prev_length: usize,

    /// To speed up deflation, hash chains are never searched beyond this length.
    /// A higher limit improves compression ratio but degrades the speed.
    pub(crate) max_chain_length: usize,

    // TODO untangle this mess! zlib uses the same field differently based on compression level
    // we should just have 2 fields for clarity!
    //
    // Insert new strings in the hash table only if the match length is not
    // greater than this length. This saves time but degrades compression.
    // max_insert_length is used only for compression levels <= 3.
    // define max_insert_length  max_lazy_match
    /// Attempt to find a better match only when the current match is strictly smaller
    /// than this value. This mechanism is used only for compression levels >= 4.
    pub(crate) max_lazy_match: usize,

    /// Window position at the beginning of the current output block. Gets
    /// negative when the window is moved backwards.
    pub(crate) block_start: isize,

    /// Whether or not a block is currently open for the QUICK deflation scheme.
    /// true if there is an active block, or false if the block was just closed
    pub(crate) block_open: u8,

    pub(crate) window: Window<'a>,

    pub(crate) sym_buf: &'a mut [u8],
    pub(crate) sym_end: usize,
    pub(crate) sym_next: usize,

    /// Size of match buffer for literals/lengths.  There are 4 reasons for
    /// limiting lit_bufsize to 64K:
    ///   - frequencies can be kept in 16 bit counters
    ///   - if compression is not successful for the first block, all input
    ///     data is still in the window so we can still emit a stored block even
    ///     when input comes from standard input.  (This can also be done for
    ///     all blocks if lit_bufsize is not greater than 32K.)
    ///   - if compression is not successful for a file smaller than 64K, we can
    ///     even emit a stored file instead of a stored block (saving 5 bytes).
    ///     This is applicable only for zip (not gzip or zlib).
    ///   - creating new Huffman trees less frequently may not provide fast
    ///     adaptation to changes in the input data statistics. (Take for
    ///     example a binary file with poorly compressible code followed by
    ///     a highly compressible string table.) Smaller buffer sizes give
    ///     fast adaptation but have of course the overhead of transmitting
    ///     trees more frequently.
    ///   - I can't count above 4
    lit_bufsize: usize,

    /// Actual size of window: 2*wSize, except when the user input buffer is directly used as sliding window.
    pub(crate) window_size: usize,

    /// number of string matches in current block
    pub(crate) matches: usize,

    /// bit length of current block with optimal trees
    opt_len: usize,
    /// bit length of current block with static trees
    static_len: usize,

    /// bytes at end of window left to insert
    pub(crate) insert: usize,

    pub(crate) w_size: usize,    /* LZ77 window size (32K by default) */
    pub(crate) w_bits: usize,    /* log2(w_size)  (8..16) */
    pub(crate) w_mask: usize,    /* w_size - 1 */
    pub(crate) lookahead: usize, /* number of valid bytes ahead in window */

    // pub(crate) prev: &'a mut [u16],
    // pub(crate) head: &'a mut [u16; HASH_SIZE],
    pub(crate) hash_map: ZlibHashMap<'a>,

    ///  hash index of string to be inserted
    pub(crate) ins_h: usize,

    heap: Heap,

    pub(crate) update_hash: fn(h: u32, val: u32) -> u32,
    pub(crate) insert_string: fn(state: &mut State, string: usize, count: usize),
    pub(crate) quick_insert_string: fn(state: &mut State, string: usize) -> u16,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Strategy {
    Default = 0,
    Filtered = 1,
    HuffmanOnly = 2,
    Rle = 3,
    Fixed = 4,
}

impl TryFrom<i32> for Strategy {
    type Error = ();

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Strategy::Default),
            1 => Ok(Strategy::Filtered),
            2 => Ok(Strategy::HuffmanOnly),
            3 => Ok(Strategy::Rle),
            4 => Ok(Strategy::Fixed),
            _ => Err(()),
        }
    }
}

#[allow(unused)]
enum DataType {
    Binary = 0,
    Text = 1,
    Unknown = 2,
}

impl<'a> State<'a> {
    pub(crate) const BIT_BUF_SIZE: u8 = 64;

    pub(crate) fn max_dist(&self) -> usize {
        self.w_size - MIN_LOOKAHEAD
    }

    // TODO untangle this mess! zlib uses the same field differently based on compression level
    // we should just have 2 fields for clarity!
    pub(crate) fn max_insert_length(&self) -> usize {
        self.max_lazy_match
    }

    /// Total size of the pending buf. But because `pending` shares memory with `sym_buf`, this is
    /// not the number of bytes that are actually in `pending`!
    pub(crate) fn pending_buf_size(&self) -> usize {
        self.lit_bufsize * 4
    }

    pub(crate) fn tally_lit(&mut self, unmatched: u8) -> bool {
        self.sym_buf[self.sym_next] = 0;
        self.sym_next += 1;

        self.sym_buf[self.sym_next] = 0;
        self.sym_next += 1;

        self.sym_buf[self.sym_next] = unmatched;
        self.sym_next += 1;

        *self.l_desc.dyn_tree[unmatched as usize].freq_mut() += 1;

        assert!(
            unmatched as usize <= STD_MAX_MATCH - STD_MIN_MATCH,
            "zng_tr_tally: bad literal"
        );

        self.sym_next == self.sym_end
    }

    pub(crate) fn tally_dist(&mut self, mut dist: usize, len: usize) -> bool {
        self.sym_buf[self.sym_next] = dist as u8;
        self.sym_next += 1;

        self.sym_buf[self.sym_next] = (dist >> 8) as u8;
        self.sym_next += 1;

        self.sym_buf[self.sym_next] = len as u8;
        self.sym_next += 1;

        self.matches += 1;
        dist -= 1;

        assert!(
            dist < self.max_dist() && Self::d_code(dist) < D_CODES as u8,
            "tally_dist: bad match"
        );

        let index = self::trees_tbl::LENGTH_CODE[len] as usize + LITERALS + 1;
        *self.l_desc.dyn_tree[index].freq_mut() += 1;

        *self.d_desc.dyn_tree[Self::d_code(dist) as usize].freq_mut() += 1;

        self.sym_next == self.sym_end
    }

    fn detect_data_type(&mut self) -> DataType {
        // set bits 0..6, 14..25, and 28..31
        // 0xf3ffc07f = binary 11110011111111111100000001111111
        const NON_TEXT: u64 = 0xf3ffc07f;
        let mut mask = NON_TEXT;

        /* Check for non-textual bytes. */
        for n in 0..32 {
            if (mask & 1) != 0 && self.l_desc.dyn_tree[n].freq() != 0 {
                return DataType::Binary;
            }

            mask >>= 1;
        }

        /* Check for textual bytes. */
        if self.l_desc.dyn_tree[9].freq() != 0
            || self.l_desc.dyn_tree[10].freq() != 0
            || self.l_desc.dyn_tree[13].freq() != 0
        {
            return DataType::Text;
        }

        for n in 32..LITERALS {
            if self.l_desc.dyn_tree[n].freq() != 0 {
                return DataType::Text;
            }
        }

        // there are no explicit text or non-text bytes. The stream is either empty or has only
        // tolerated bytes
        DataType::Binary
    }

    fn flush_bits(&mut self) {
        debug_assert!(self.bi_valid <= 64);
        let removed = self.bi_valid.saturating_sub(7).next_multiple_of(8);
        let keep_bytes = self.bi_valid / 8; // can never divide by zero

        let src = &self.bi_buf.to_le_bytes();
        self.pending.extend(&src[..keep_bytes as usize]);

        self.bi_valid -= removed;
        self.bi_buf = self.bi_buf.checked_shr(removed as u32).unwrap_or(0);
    }

    fn flush_and_align_bits(&mut self) {
        debug_assert!(self.bi_valid <= 64);
        let keep_bytes = self.bi_valid.div_ceil(8);
        let src = &self.bi_buf.to_le_bytes();
        self.pending.extend(&src[..keep_bytes as usize]);

        self.bi_valid = 0;
        self.bi_buf = 0;
    }

    fn compress_block(&mut self, ltree: &[Value], dtree: &[Value]) {
        let mut sx = 0;

        if self.sym_next != 0 {
            loop {
                let mut dist = self.sym_buf[sx] as usize;
                sx += 1;
                dist += (self.sym_buf[sx] as usize) << 8;
                sx += 1;
                let lc = self.sym_buf[sx];
                sx += 1;

                if dist == 0 {
                    self.emit_lit(ltree, lc);
                } else {
                    self.emit_dist(ltree, dtree, lc, dist);
                }

                /* Check that the overlay between pending_buf and sym_buf is ok: */
                assert!(
                    self.pending.pending < self.lit_bufsize + sx,
                    "pending_buf overflow"
                );

                if sx >= self.sym_next {
                    break;
                }
            }
        }

        self.emit_end_block(ltree, false)
    }

    pub(crate) fn emit_end_block_and_align(&mut self, ltree: &[Value], is_last_block: bool) {
        self.emit_end_block(ltree, is_last_block);

        if is_last_block {
            self.flush_and_align_bits();
        }
    }

    fn emit_end_block(&mut self, ltree: &[Value], _is_last_block: bool) {
        const END_BLOCK: usize = 256;
        self.send_code(END_BLOCK, ltree);
    }

    pub(crate) fn emit_lit(&mut self, ltree: &[Value], c: u8) -> u16 {
        self.send_code(c as usize, ltree);

        trace!(
            "'{}' ",
            match char::from_u32(c as u32) {
                None => ' ',
                Some(c) => match c.is_ascii() && !c.is_whitespace() {
                    true => c,
                    false => ' ',
                },
            }
        );

        ltree[c as usize].len()
    }

    const fn d_code(dist: usize) -> u8 {
        if dist < 256 {
            self::trees_tbl::DIST_CODE[dist]
        } else {
            self::trees_tbl::DIST_CODE[256 + (dist >> 7)]
        }
    }

    pub(crate) fn emit_dist(
        &mut self,
        ltree: &[Value],
        dtree: &[Value],
        lc: u8,
        mut dist: usize,
    ) -> usize {
        let mut lc = lc as usize;

        /* Send the length code, len is the match length - STD_MIN_MATCH */
        let mut code = self::trees_tbl::LENGTH_CODE[lc] as usize;
        let c = code + LITERALS + 1;
        assert!(c < L_CODES, "bad l_code");
        // send_code_trace(s, c);

        let mut match_bits = ltree[c].code() as usize;
        let mut match_bits_len = ltree[c].len() as usize;
        let mut extra = StaticTreeDesc::EXTRA_LBITS[code] as usize;
        if extra != 0 {
            lc -= self::trees_tbl::BASE_LENGTH[code] as usize;
            match_bits |= lc << match_bits_len;
            match_bits_len += extra;
        }

        dist -= 1; /* dist is now the match distance - 1 */
        code = Self::d_code(dist) as usize;
        assert!(code < D_CODES, "bad d_code");
        // send_code_trace(s, code);

        /* Send the distance code */
        match_bits |= (dtree[code].code() as usize) << match_bits_len;
        match_bits_len += dtree[code].len() as usize;
        extra = StaticTreeDesc::EXTRA_DBITS[code] as usize;
        if extra != 0 {
            dist -= self::trees_tbl::BASE_DIST[code] as usize;
            match_bits |= dist << match_bits_len;
            match_bits_len += extra;
        }

        self.send_bits(match_bits as u64, match_bits_len as u8);

        match_bits_len
    }

    fn send_code(&mut self, code: usize, tree: &[Value]) {
        let node = tree[code];
        self.send_bits(node.code() as u64, node.len() as u8)
    }

    pub(crate) fn emit_tree(&mut self, block_type: BlockType, is_last_block: bool) {
        let header_bits = (block_type as u64) << 1 | (is_last_block as u64);
        self.send_bits(header_bits, 3);
    }

    fn send_bits(&mut self, val: u64, len: u8) {
        debug_assert!(len <= 64);
        debug_assert!(self.bi_valid <= 64);

        let total_bits = len + self.bi_valid;

        // send_bits_trace(s, val, len);\
        // sent_bits_add(s, len);\

        if total_bits < Self::BIT_BUF_SIZE {
            self.bi_buf |= val << self.bi_valid;
            self.bi_valid = total_bits;
        } else if self.bi_valid == Self::BIT_BUF_SIZE {
            self.pending.extend(&self.bi_buf.to_le_bytes());
            self.bi_buf = val;
            self.bi_valid = len;
        } else {
            self.bi_buf |= val << self.bi_valid;
            self.pending.extend(&self.bi_buf.to_le_bytes());
            self.bi_buf = val >> (Self::BIT_BUF_SIZE - self.bi_valid);
            self.bi_valid = total_bits - Self::BIT_BUF_SIZE;
        }
    }

    fn header(&self) -> u16 {
        // preset dictionary flag in zlib header
        const PRESET_DICT: u16 = 0x20;

        // The deflate compression method (the only one supported in this version)
        const Z_DEFLATED: u16 = 8;

        let dict = match self.strstart {
            0 => 0,
            _ => PRESET_DICT,
        };

        let h =
            (Z_DEFLATED + ((self.w_bits as u16 - 8) << 4)) << 8 | (self.level_flags() << 6) | dict;

        h + 31 - (h % 31)
    }

    fn level_flags(&self) -> u16 {
        if self.strategy >= Strategy::HuffmanOnly || self.level < 2 {
            0
        } else if self.level < 6 {
            1
        } else if self.level == 6 {
            2
        } else {
            3
        }
    }

    fn zng_tr_init(&mut self) {
        self.l_desc.stat_desc = &StaticTreeDesc::L;

        self.d_desc.stat_desc = &StaticTreeDesc::D;

        self.bl_desc.stat_desc = &StaticTreeDesc::BL;

        self.bi_buf = 0;
        self.bi_valid = 0;

        // Initialize the first block of the first file:
        self.init_block();
    }

    /// initializes a new block
    fn init_block(&mut self) {
        // Initialize the trees.
        // TODO would a memset also work here?

        for value in &mut self.l_desc.dyn_tree[..L_CODES] {
            *value.freq_mut() = 0;
        }

        for value in &mut self.d_desc.dyn_tree[..D_CODES] {
            *value.freq_mut() = 0;
        }

        for value in &mut self.bl_desc.dyn_tree[..BL_CODES] {
            *value.freq_mut() = 0;
        }

        // end of block literal code
        const END_BLOCK: usize = 256;

        *self.l_desc.dyn_tree[END_BLOCK].freq_mut() = 1;
        self.opt_len = 0;
        self.static_len = 0;
        self.sym_next = 0;
        self.matches = 0;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Status {
    Init,
    Busy,
    Finish,
}

const fn error_message(return_code: ReturnCode) -> *const i8 {
    const TABLE: [&str; 10] = [
        "need dictionary\0",      /* Z_NEED_DICT       2  */
        "stream end\0",           /* Z_STREAM_END      1  */
        "\0",                     /* Z_OK              0  */
        "file error\0",           /* Z_ERRNO         (-1) */
        "stream error\0",         /* Z_STREAM_ERROR  (-2) */
        "data error\0",           /* Z_DATA_ERROR    (-3) */
        "insufficient memory\0",  /* Z_MEM_ERROR     (-4) */
        "buffer error\0",         /* Z_BUF_ERROR     (-5) */
        "incompatible version\0", /* Z_VERSION_ERROR (-6) */
        "\0",
    ];

    let index = (ReturnCode::NeedDict as i32 - return_code as i32) as usize;

    TABLE[index].as_ptr().cast()
}

const fn rank_flush(f: i32) -> i32 {
    // rank Z_BLOCK between Z_NO_FLUSH and Z_PARTIAL_FLUSH
    ((f) * 2) - (if (f) > 4 { 9 } else { 0 })
}

#[derive(Debug)]
pub(crate) enum BlockState {
    /// block not completed, need more input or more output
    NeedMore = 0,
    /// block flush performed
    BlockDone = 1,
    /// finish started, need only more output at next deflate
    FinishStarted = 2,
    /// finish done, accept no more input or output
    FinishDone = 3,
}

// Maximum stored block length in deflate format (not including header).
pub(crate) const MAX_STORED: usize = 65535; // so u16::max

pub(crate) fn read_buf_window(stream: &mut DeflateStream, offset: usize, size: usize) -> usize {
    let len = Ord::min(stream.avail_in as usize, size);

    if len == 0 {
        return 0;
    }

    stream.avail_in -= len as u32;

    // TODO gzip
    if stream.state.wrap == 1 {
        // we likely cannot fuse the adler32 and the copy here because the input can be changed by
        // a concurrent thread. Therefore it cannot be converted into a slice!
        let window = &mut stream.state.window;
        unsafe { window.copy_and_initialize(offset..offset + len, stream.next_in) };

        let data = &stream.state.window.filled()[offset..][..len];
        stream.adler = adler32(stream.adler as u32, data) as _;
    } else {
        let window = &mut stream.state.window;
        unsafe { window.copy_and_initialize(offset..offset + len, stream.next_in) };
    }

    stream.next_in = stream.next_in.wrapping_add(len);
    stream.total_in += len as crate::c_api::z_size;

    len
}

pub(crate) enum BlockType {
    StoredBlock = 0,
    StaticTrees = 1,
    DynamicTrees = 2,
}

pub(crate) fn zng_tr_stored_block(
    state: &mut State,
    window_range: std::ops::Range<usize>,
    is_last: bool,
) {
    // send block type
    state.emit_tree(BlockType::StoredBlock, is_last);

    // align on byte boundary
    state.flush_and_align_bits();

    // cmpr_bits_align(s);

    let input_block: &[u8] = &state.window.filled()[window_range];
    let stored_len = input_block.len() as u16;

    state.pending.extend(&stored_len.to_le_bytes());
    state.pending.extend(&(!stored_len).to_le_bytes());

    // cmpr_bits_add(s, 32);
    // sent_bits_add(s, 32);
    if stored_len > 0 {
        state.pending.extend(input_block);
        // cmpr_bits_add(s, stored_len << 3);
        // sent_bits_add(s, stored_len << 3);
    }
}

/// The minimum match length mandated by the deflate standard
pub(crate) const STD_MIN_MATCH: usize = 3;
/// The maximum match length mandated by the deflate standard
pub(crate) const STD_MAX_MATCH: usize = 258;

/// The minimum wanted match length, affects deflate_quick, deflate_fast, deflate_medium and deflate_slow
pub(crate) const WANT_MIN_MATCH: usize = 4;

pub(crate) const MIN_LOOKAHEAD: usize = STD_MAX_MATCH + STD_MIN_MATCH + 1;

pub(crate) fn fill_window(stream: &mut DeflateStream) {
    debug_assert!(stream.state.lookahead < MIN_LOOKAHEAD);

    let wsize = stream.state.w_size;

    loop {
        let state = &mut stream.state;
        let mut more = state.window_size - state.lookahead - state.strstart;

        // If the window is almost full and there is insufficient lookahead,
        // move the upper half to the lower one to make room in the upper half.
        if state.strstart >= wsize + state.max_dist() {
            state.window.filled_mut().copy_within(wsize..2 * wsize, 0);

            if state.match_start >= wsize {
                state.match_start -= wsize;
            } else {
                state.match_start = 0;
                state.prev_length = 0;
            }
            state.strstart -= wsize; /* we now have strstart >= MAX_DIST */
            state.block_start -= wsize as isize;
            if state.insert > state.strstart {
                state.insert = state.strstart;
            }

            self::slide_hash::slide_hash(state);

            more += wsize;
        }

        if stream.avail_in == 0 {
            break;
        }

        // If there was no sliding:
        //    strstart <= WSIZE+MAX_DIST-1 && lookahead <= MIN_LOOKAHEAD - 1 &&
        //    more == window_size - lookahead - strstart
        // => more >= window_size - (MIN_LOOKAHEAD-1 + WSIZE + MAX_DIST-1)
        // => more >= window_size - 2*WSIZE + 2
        // In the BIG_MEM or MMAP case (not yet supported),
        //   window_size == input_size + MIN_LOOKAHEAD  &&
        //   strstart + s->lookahead <= input_size => more >= MIN_LOOKAHEAD.
        // Otherwise, window_size == 2*WSIZE so more >= 2.
        // If there was sliding, more >= WSIZE. So in all cases, more >= 2.
        assert!(more >= 2, "more < 2");

        let n = read_buf_window(stream, stream.state.strstart + stream.state.lookahead, more);

        let state = &mut stream.state;
        state.lookahead += n;

        // Initialize the hash value now that we have some input:
        if state.lookahead + state.insert >= STD_MIN_MATCH {
            let string = state.strstart - state.insert;
            if state.max_chain_length > 1024 {
                let v0 = state.window.filled()[string] as u32;
                let v1 = state.window.filled()[string + 1] as u32;
                state.ins_h = (state.update_hash)(v0, v1) as usize;
            } else if string >= 1 {
                (state.quick_insert_string)(state, string + 2 - STD_MIN_MATCH);
            }
            let mut count = state.insert;
            if state.lookahead == 1 {
                count -= 1;
            }
            if count > 0 {
                (state.insert_string)(state, string, count);
                state.insert -= count;
            }
        }

        // If the whole input has less than STD_MIN_MATCH bytes, ins_h is garbage,
        // but this is not important since only literal bytes will be emitted.

        if !(stream.state.lookahead < MIN_LOOKAHEAD && stream.avail_in != 0) {
            break;
        }
    }

    // initialize some memory at the end of the (filled) window, so SIMD operations can go "out of
    // bounds" without violating any requirements. The window allocation is already slightly bigger
    // to allow for this.
    stream.state.window.initialize_out_of_bounds();

    assert!(
        stream.state.strstart <= stream.state.window_size - MIN_LOOKAHEAD,
        "not enough room for search"
    );
}

pub(crate) struct StaticTreeDesc {
    /// static tree or NULL
    pub(crate) static_tree: &'static [Value],
    /// extra bits for each code or NULL
    extra_bits: &'static [u8],
    /// base index for extra_bits
    extra_base: usize,
    /// max number of elements in the tree
    elems: usize,
    /// max bit length for the codes
    max_length: u16,
}

impl StaticTreeDesc {
    const EMPTY: Self = Self {
        static_tree: &[],
        extra_bits: &[],
        extra_base: 0,
        elems: 0,
        max_length: 0,
    };

    /// extra bits for each length code
    const EXTRA_LBITS: [u8; LENGTH_CODES] = [
        0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0,
    ];

    /// extra bits for each distance code
    const EXTRA_DBITS: [u8; D_CODES] = [
        0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12,
        13, 13,
    ];

    /// extra bits for each bit length code
    const EXTRA_BLBITS: [u8; BL_CODES] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 3, 7];

    /// The lengths of the bit length codes are sent in order of decreasing
    /// probability, to avoid transmitting the lengths for unused bit length codes.
    const BL_ORDER: [u8; BL_CODES] = [
        16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15,
    ];

    pub(crate) const L: Self = Self {
        static_tree: &self::trees_tbl::STATIC_LTREE,
        extra_bits: &Self::EXTRA_LBITS,
        extra_base: LITERALS + 1,
        elems: L_CODES,
        max_length: MAX_BITS as u16,
    };

    pub(crate) const D: Self = Self {
        static_tree: &self::trees_tbl::STATIC_DTREE,
        extra_bits: &Self::EXTRA_DBITS,
        extra_base: 0,
        elems: D_CODES,
        max_length: MAX_BITS as u16,
    };

    pub(crate) const BL: Self = Self {
        static_tree: &[],
        extra_bits: &Self::EXTRA_BLBITS,
        extra_base: 0,
        elems: BL_CODES,
        max_length: MAX_BL_BITS as u16,
    };
}

struct TreeDesc<const N: usize> {
    dyn_tree: [Value; N],
    max_code: usize,
    stat_desc: &'static StaticTreeDesc,
}

impl<const N: usize> TreeDesc<N> {
    const EMPTY: Self = Self {
        dyn_tree: [Value::new(0, 0); N],
        max_code: 0,
        stat_desc: &StaticTreeDesc::EMPTY,
    };
}

fn build_tree<const N: usize>(state: &mut State, desc: &mut TreeDesc<N>) {
    let tree = &mut desc.dyn_tree;
    let stree = desc.stat_desc.static_tree;
    let elements = desc.stat_desc.elems;

    let mut max_code = state.heap.initialize(&mut tree[..elements]);

    // The pkzip format requires that at least one distance code exists,
    // and that at least one bit should be sent even if there is only one
    // possible code. So to avoid special checks later on we force at least
    // two codes of non zero frequency.
    while state.heap.heap_len < 2 {
        state.heap.heap_len += 1;
        let node = if max_code < 2 {
            max_code += 1;
            max_code
        } else {
            0
        };

        debug_assert!(node >= 0);
        let node = node as usize;

        state.heap.heap[state.heap.heap_len] = node as u32;
        *tree[node].freq_mut() = 1;
        state.heap.depth[node] = 0;
        state.opt_len -= 1;
        if !stree.is_empty() {
            state.static_len -= stree[node].len() as usize;
        }
        /* node is 0 or 1 so it does not have extra bits */
    }

    debug_assert!(max_code >= 0);
    let max_code = max_code as usize;
    desc.max_code = max_code;

    // The elements heap[heap_len/2+1 .. heap_len] are leaves of the tree,
    // establish sub-heaps of increasing lengths:
    let mut n = state.heap.heap_len / 2;
    while n >= 1 {
        state.heap.pqdownheap(tree, n);
        n -= 1;
    }

    state.heap.construct_huffman_tree(tree, elements);

    // At this point, the fields freq and dad are set. We can now
    // generate the bit lengths.
    gen_bitlen(state, desc);

    // The field len is now set, we can generate the bit codes
    gen_codes(&mut desc.dyn_tree, max_code, &state.bl_count);
}

fn gen_bitlen<const N: usize>(state: &mut State, desc: &mut TreeDesc<N>) {
    let heap = &mut state.heap;

    let tree = &mut desc.dyn_tree;
    let max_code = desc.max_code;
    let stree = desc.stat_desc.static_tree;
    let extra = desc.stat_desc.extra_bits;
    let base = desc.stat_desc.extra_base;
    let max_length = desc.stat_desc.max_length;

    state.bl_count.fill(0);

    // In a first pass, compute the optimal bit lengths (which may
    // overflow in the case of the bit length tree).
    *tree[heap.heap[heap.heap_max] as usize].len_mut() = 0; /* root of the heap */

    // number of elements with bit length too large
    let mut overflow: i32 = 0;

    for h in heap.heap_max + 1..HEAP_SIZE {
        let n = heap.heap[h] as usize;
        let mut bits = tree[tree[n].dad() as usize].len() + 1;

        if bits > max_length {
            bits = max_length;
            overflow += 1;
        }

        // We overwrite tree[n].Dad which is no longer needed
        *tree[n].len_mut() = bits;

        // not a leaf node
        if n > max_code {
            continue;
        }

        state.bl_count[bits as usize] += 1;
        let mut xbits = 0;
        if n >= base {
            xbits = extra[n - base] as usize;
        }

        let f = tree[n].freq() as usize;
        state.opt_len += f * (bits as usize + xbits);

        if !stree.is_empty() {
            state.static_len += f * (stree[n].len() as usize + xbits);
        }
    }

    if overflow == 0 {
        return;
    }

    /* Find the first bit length which could increase: */
    loop {
        let mut bits = max_length as usize - 1;
        while state.bl_count[bits] == 0 {
            bits -= 1;
        }
        state.bl_count[bits] -= 1; /* move one leaf down the tree */
        state.bl_count[bits + 1] += 2; /* move one overflow item as its brother */
        state.bl_count[max_length as usize] -= 1;
        /* The brother of the overflow item also moves one step up,
         * but this does not affect bl_count[max_length]
         */
        overflow -= 2;

        if overflow <= 0 {
            break;
        }
    }

    // Now recompute all bit lengths, scanning in increasing frequency.
    // h is still equal to HEAP_SIZE. (It is simpler to reconstruct all
    // lengths instead of fixing only the wrong ones. This idea is taken
    // from 'ar' written by Haruhiko Okumura.)
    let mut h = HEAP_SIZE;
    for bits in (1..=max_length).rev() {
        let mut n = state.bl_count[bits as usize];
        while n != 0 {
            h -= 1;
            let m = heap.heap[h] as usize;
            if m > max_code {
                continue;
            }

            if tree[m].len() != bits {
                // Tracev((stderr, "code %d bits %d->%u\n", m, tree[m].Len, bits));
                state.opt_len += (bits * tree[m].freq()) as usize;
                state.opt_len -= (tree[m].len() * tree[m].freq()) as usize;
                *tree[m].len_mut() = bits;
            }

            n -= 1;
        }
    }
}

fn gen_codes(tree: &mut [Value], max_code: usize, bl_count: &[u16]) {
    /* tree: the tree to decorate */
    /* max_code: largest code with non zero frequency */
    /* bl_count: number of codes at each bit length */
    let mut next_code = [0; MAX_BITS + 1]; /* next code value for each bit length */
    let mut code = 0; /* running code value */

    /* The distribution counts are first used to generate the code values
     * without bit reversal.
     */
    for bits in 1..=MAX_BITS {
        code = (code + bl_count[bits - 1]) << 1;
        next_code[bits] = code;
    }

    /* Check that the bit counts in bl_count are consistent. The last code
     * must be all ones.
     */
    assert!(
        code + bl_count[MAX_BITS] - 1 == (1 << MAX_BITS) - 1,
        "inconsistent bit counts"
    );

    trace!("\ngen_codes: max_code {max_code} ");

    for n in 0..=max_code {
        let len = tree[n].len();
        if len == 0 {
            continue;
        }

        /* Now reverse the bits */
        assert!((1..=15).contains(&len), "code length must be 1-15");
        *tree[n].code_mut() = next_code[len as usize].reverse_bits() >> (16 - len);
        next_code[len as usize] += 1;

        if tree != self::trees_tbl::STATIC_LTREE.as_slice() {
            trace!(
                "\nn {:>3} {} l {:>2} c {:>4x} ({:x}) ",
                n,
                //                match char::from_u32(n as u32) {
                //                    None => ' ',
                //                    Some(c) => match c.is_ascii() && !c.is_whitespace() {
                //                        true => c,
                //                        false => ' ',
                //                    },
                //                },
                if unsafe { libc::isgraph(n as i32) } > 0 {
                    char::from_u32(n as u32).unwrap()
                } else {
                    ' '
                },
                len,
                tree[n].code(),
                next_code[len as usize] - 1
            );
        }

        //        Tracecv(tree != static_ltree, (stderr, "\nn %3d %c l %2d c %4x (%x) ",
        //             n, (isgraph(n & 0xff) ? n : ' '), len, tree[n].Code, next_code[len]-1));
    }
}

/// repeat previous bit length 3-6 times (2 bits of repeat count)
const REP_3_6: usize = 16;

/// repeat a zero length 3-10 times  (3 bits of repeat count)
const REPZ_3_10: usize = 17;

/// repeat a zero length 11-138 times  (7 bits of repeat count)
const REPZ_11_138: usize = 18;

fn scan_tree(bl_desc: &mut TreeDesc<{ 2 * BL_CODES + 1 }>, tree: &mut [Value], max_code: usize) {
    /* tree: the tree to be scanned */
    /* max_code: and its largest code of non zero frequency */
    let mut prevlen = -1isize; /* last emitted length */
    let mut curlen: isize; /* length of current code */
    let mut nextlen = tree[0].len(); /* length of next code */
    let mut count = 0; /* repeat count of the current code */
    let mut max_count = 7; /* max repeat count */
    let mut min_count = 4; /* min repeat count */

    if nextlen == 0 {
        max_count = 138;
        min_count = 3;
    }

    *tree[max_code + 1].len_mut() = 0xffff; /* guard */

    let bl_tree = &mut bl_desc.dyn_tree;

    for n in 0..=max_code {
        curlen = nextlen as isize;
        nextlen = tree[n + 1].len();
        count += 1;
        if count < max_count && curlen == nextlen as isize {
            continue;
        } else if count < min_count {
            *bl_tree[curlen as usize].freq_mut() += count;
        } else if curlen != 0 {
            if curlen != prevlen {
                *bl_tree[curlen as usize].freq_mut() += 1;
            }
            *bl_tree[REP_3_6].freq_mut() += 1;
        } else if count <= 10 {
            *bl_tree[REPZ_3_10].freq_mut() += 1;
        } else {
            *bl_tree[REPZ_11_138].freq_mut() += 1;
        }

        count = 0;
        prevlen = curlen;

        if nextlen == 0 {
            max_count = 138;
            min_count = 3;
        } else if curlen == nextlen as isize {
            max_count = 6;
            min_count = 3;
        } else {
            max_count = 7;
            min_count = 4;
        }
    }
}

fn send_all_trees(state: &mut State, lcodes: usize, dcodes: usize, blcodes: usize) {
    assert!(
        lcodes >= 257 && dcodes >= 1 && blcodes >= 4,
        "not enough codes"
    );
    assert!(
        lcodes <= L_CODES && dcodes <= D_CODES && blcodes <= BL_CODES,
        "too many codes"
    );

    trace!("\nbl counts: ");
    state.send_bits(lcodes as u64 - 257, 5); /* not +255 as stated in appnote.txt */
    state.send_bits(dcodes as u64 - 1, 5);
    state.send_bits(blcodes as u64 - 4, 4); /* not -3 as stated in appnote.txt */

    for rank in 0..blcodes {
        trace!("\nbl code {:>2} ", StaticTreeDesc::BL_ORDER[rank]);
        state.send_bits(
            state.bl_desc.dyn_tree[StaticTreeDesc::BL_ORDER[rank] as usize].len() as u64,
            3,
        );
    }
    // trace!("\nbl tree: sent {}", state.bits_sent);

    let mut tmp1 = TreeDesc::EMPTY;
    let mut tmp2 = TreeDesc::EMPTY;
    std::mem::swap(&mut tmp1, &mut state.l_desc);
    std::mem::swap(&mut tmp2, &mut state.d_desc);

    send_tree(state, &tmp1.dyn_tree, lcodes - 1); /* literal tree */
    // trace!("\nlit tree: sent {}", state.bits_sent);

    send_tree(state, &tmp2.dyn_tree, dcodes - 1); /* distance tree */
    // trace!("\ndist tree: sent {}", state.bits_sent);

    std::mem::swap(&mut tmp1, &mut state.l_desc);
    std::mem::swap(&mut tmp2, &mut state.d_desc);
}

fn send_tree(state: &mut State, tree: &[Value], max_code: usize) {
    /* tree: the tree to be scanned */
    /* max_code and its largest code of non zero frequency */
    let mut prevlen: isize = -1; /* last emitted length */
    let mut curlen; /* length of current code */
    let mut nextlen = tree[0].len(); /* length of next code */
    let mut count = 0; /* repeat count of the current code */
    let mut max_count = 7; /* max repeat count */
    let mut min_count = 4; /* min repeat count */

    /* tree[max_code+1].Len = -1; */
    /* guard already set */
    if nextlen == 0 {
        max_count = 138;
        min_count = 3;
    }

    let mut bl_desc = TreeDesc::EMPTY;
    std::mem::swap(&mut bl_desc, &mut state.bl_desc);
    let bl_tree = &bl_desc.dyn_tree;

    for n in 0..=max_code {
        curlen = nextlen;
        nextlen = tree[n + 1].len();
        count += 1;
        if count < max_count && curlen == nextlen {
            continue;
        } else if count < min_count {
            loop {
                state.send_code(curlen as usize, bl_tree);

                count -= 1;
                if count == 0 {
                    break;
                }
            }
        } else if curlen != 0 {
            if curlen as isize != prevlen {
                state.send_code(curlen as usize, bl_tree);
                count -= 1;
            }
            assert!((3..=6).contains(&count), " 3_6?");
            state.send_code(REP_3_6, bl_tree);
            state.send_bits(count - 3, 2);
        } else if count <= 10 {
            state.send_code(REPZ_3_10, bl_tree);
            state.send_bits(count - 3, 3);
        } else {
            state.send_code(REPZ_11_138, bl_tree);
            state.send_bits(count - 11, 7);
        }

        count = 0;
        prevlen = curlen as isize;

        if nextlen == 0 {
            max_count = 138;
            min_count = 3;
        } else if curlen == nextlen {
            max_count = 6;
            min_count = 3;
        } else {
            max_count = 7;
            min_count = 4;
        }
    }

    std::mem::swap(&mut bl_desc, &mut state.bl_desc);
}

/// Construct the Huffman tree for the bit lengths and return the index in
/// bl_order of the last bit length code to send.
fn build_bl_tree(state: &mut State) -> usize {
    /* Determine the bit length frequencies for literal and distance trees */

    scan_tree(
        &mut state.bl_desc,
        &mut state.l_desc.dyn_tree,
        state.l_desc.max_code,
    );

    scan_tree(
        &mut state.bl_desc,
        &mut state.d_desc.dyn_tree,
        state.d_desc.max_code,
    );

    /* Build the bit length tree: */
    {
        let mut tmp = TreeDesc::EMPTY;
        std::mem::swap(&mut tmp, &mut state.bl_desc);
        build_tree(state, &mut tmp);
        std::mem::swap(&mut tmp, &mut state.bl_desc);
    }

    /* opt_len now includes the length of the tree representations, except
     * the lengths of the bit lengths codes and the 5+5+4 bits for the counts.
     */

    /* Determine the number of bit length codes to send. The pkzip format
     * requires that at least 4 bit length codes be sent. (appnote.txt says
     * 3 but the actual value used is 4.)
     */
    let mut max_blindex = BL_CODES - 1;
    while max_blindex >= 3 {
        let index = StaticTreeDesc::BL_ORDER[max_blindex] as usize;
        if state.bl_desc.dyn_tree[index].len() != 0 {
            break;
        }

        max_blindex -= 1;
    }

    /* Update opt_len to include the bit length tree and counts */
    state.opt_len += 3 * (max_blindex + 1) + 5 + 5 + 4;
    // Tracev((stderr, "\ndyn trees: dyn %lu, stat %lu", s->opt_len, s->static_len));

    max_blindex
}

fn zng_tr_flush_block(
    stream: &mut DeflateStream,
    window_offset: Option<usize>,
    stored_len: u32,
    last: bool,
) {
    /* window_offset: offset of the input block into the window */
    /* stored_len: length of input block */
    /* last: one if this is the last block for a file */

    let mut opt_lenb;
    let static_lenb;
    let mut max_blindex = 0;

    let state = &mut stream.state;

    if state.sym_next == 0 {
        opt_lenb = 0;
        static_lenb = 0;
        state.static_len = 7;
    } else if state.level > 0 {
        if stream.data_type == Z_UNKNOWN {
            stream.data_type = state.detect_data_type() as _;
        }

        {
            let mut tmp = TreeDesc::EMPTY;
            std::mem::swap(&mut tmp, &mut state.l_desc);

            build_tree(state, &mut tmp);
            std::mem::swap(&mut tmp, &mut state.l_desc);

            trace!(
                "\nlit data: dyn {}, stat {}",
                state.opt_len,
                state.static_len
            );
        }

        {
            let mut tmp = TreeDesc::EMPTY;
            std::mem::swap(&mut tmp, &mut state.d_desc);
            build_tree(state, &mut tmp);
            std::mem::swap(&mut tmp, &mut state.d_desc);

            trace!(
                "\nlit data: dyn {}, stat {}",
                state.opt_len,
                state.static_len
            );
        }

        // Build the bit length tree for the above two trees, and get the index
        // in bl_order of the last bit length code to send.
        max_blindex = build_bl_tree(state);

        // Determine the best encoding. Compute the block lengths in bytes.
        opt_lenb = (state.opt_len + 3 + 7) >> 3;
        static_lenb = (state.static_len + 3 + 7) >> 3;

        //        Tracev((stderr, "\nopt %lu(%lu) stat %lu(%lu) stored %u lit %u ",
        //                opt_lenb, state.opt_len, static_lenb, state.static_len, stored_len,
        //                state.sym_next / 3));

        if static_lenb <= opt_lenb || state.strategy == Strategy::Fixed {
            opt_lenb = static_lenb;
        }
    } else {
        assert!(window_offset.is_some(), "lost buf");
        /* force a stored block */
        opt_lenb = stored_len as usize + 5;
        static_lenb = stored_len as usize + 5;
    }

    if stored_len as usize + 4 <= opt_lenb && window_offset.is_some() {
        /* 4: two words for the lengths
         * The test buf != NULL is only necessary if LIT_BUFSIZE > WSIZE.
         * Otherwise we can't have processed more than WSIZE input bytes since
         * the last block flush, because compression would have been
         * successful. If LIT_BUFSIZE <= WSIZE, it is never too late to
         * transform a block into a stored block.
         */
        let window_offset = window_offset.unwrap();
        let range = window_offset..window_offset + stored_len as usize;
        zng_tr_stored_block(state, range, last);
    } else if static_lenb == opt_lenb {
        state.emit_tree(BlockType::StaticTrees, last);
        state.compress_block(
            &self::trees_tbl::STATIC_LTREE,
            &self::trees_tbl::STATIC_DTREE,
        );
    // cmpr_bits_add(s, s.static_len);
    } else {
        state.emit_tree(BlockType::DynamicTrees, last);
        send_all_trees(
            state,
            state.l_desc.max_code + 1,
            state.d_desc.max_code + 1,
            max_blindex + 1,
        );
        {
            let mut tmp1 = TreeDesc::EMPTY;
            let mut tmp2 = TreeDesc::EMPTY;
            std::mem::swap(&mut tmp1, &mut state.l_desc);
            std::mem::swap(&mut tmp2, &mut state.d_desc);
            state.compress_block(&tmp1.dyn_tree, &tmp2.dyn_tree);
            std::mem::swap(&mut tmp1, &mut state.l_desc);
            std::mem::swap(&mut tmp2, &mut state.d_desc);
        }
    }

    // TODO
    // This check is made mod 2^32, for files larger than 512 MB and unsigned long implemented on 32 bits.
    // assert_eq!(state.compressed_len, state.bits_sent, "bad compressed size");

    state.init_block();
    if last {
        state.flush_and_align_bits();
    }

    // Tracev((stderr, "\ncomprlen %lu(%lu) ", s->compressed_len>>3, s->compressed_len-7*last));
}

pub(crate) fn flush_block_only(stream: &mut DeflateStream, is_last: bool) {
    zng_tr_flush_block(
        stream,
        (stream.state.block_start >= 0).then_some(stream.state.block_start as usize),
        (stream.state.strstart as isize - stream.state.block_start) as u32,
        is_last,
    );

    stream.state.block_start = stream.state.strstart as isize;
    flush_pending(stream)
}

pub(crate) fn deflate(stream: &mut DeflateStream, flush: Flush) -> ReturnCode {
    if stream.next_out.is_null()
        || (stream.avail_in != 0 && stream.next_in.is_null())
        || (stream.state.status == Status::Finish && flush != Flush::Finish)
    {
        let err = ReturnCode::StreamError;
        stream.msg = error_message(err);
        return err;
    }

    if stream.avail_out == 0 {
        let err = ReturnCode::BufError;
        stream.msg = error_message(err);
        return err;
    }

    let old_flush = stream.state.last_flush;
    stream.state.last_flush = flush as i32;

    /* Flush as much pending output as possible */
    if !stream.state.pending.pending().is_empty() {
        flush_pending(stream);
        if stream.avail_out == 0 {
            /* Since avail_out is 0, deflate will be called again with
             * more output space, but possibly with both pending and
             * avail_in equal to zero. There won't be anything to do,
             * but this is not an error situation so make sure we
             * return OK instead of BUF_ERROR at next call of deflate:
             */
            stream.state.last_flush = -1;
            return ReturnCode::Ok;
        }

        /* Make sure there is something to do and avoid duplicate consecutive
         * flushes. For repeated and useless calls with Z_FINISH, we keep
         * returning Z_STREAM_END instead of Z_BUF_ERROR.
         */
    } else if stream.avail_in == 0
        && rank_flush(flush as i32) <= rank_flush(old_flush)
        && flush != Flush::Finish
    {
        let err = ReturnCode::BufError;
        stream.msg = error_message(err);
        return err;
    }

    /* User must not provide more input after the first FINISH: */
    if stream.state.status == Status::Finish && stream.avail_in != 0 {
        let err = ReturnCode::BufError;
        stream.msg = error_message(err);
        return err;
    }

    /* Write the header */
    if stream.state.status == Status::Init && stream.state.wrap == 0 {
        stream.state.status = Status::Busy;
    }

    if stream.state.status == Status::Init {
        let header = stream.state.header();
        stream.state.pending.extend(&header.to_be_bytes());

        /* Save the adler32 of the preset dictionary: */
        if stream.state.strstart != 0 {
            let adler = stream.adler as u32;
            stream.state.pending.extend(&adler.to_be_bytes());
        }

        stream.adler = ADLER32_INITIAL_VALUE as _;
        stream.state.status = Status::Busy;

        // compression must start with an empty pending buffer
        flush_pending(stream);

        if !stream.state.pending.pending().is_empty() {
            stream.state.last_flush = -1;

            return ReturnCode::Ok;
        }
    }

    // Start a new block or continue the current one.
    let state = &mut stream.state;
    if stream.avail_in != 0
        || state.lookahead != 0
        || (flush != Flush::NoFlush && state.status != Status::Finish)
    {
        let bstate = self::algorithm::run(stream, flush);

        let state = &mut stream.state;

        if matches!(bstate, BlockState::FinishStarted | BlockState::FinishDone) {
            state.status = Status::Finish;
        }

        match bstate {
            BlockState::NeedMore | BlockState::FinishStarted => {
                if stream.avail_out == 0 {
                    state.last_flush = -1; /* avoid BUF_ERROR next call, see above */
                }
                return ReturnCode::Ok;
                /* If flush != Z_NO_FLUSH && avail_out == 0, the next call
                 * of deflate should use the same flush parameter to make sure
                 * that the flush is complete. So we don't have to output an
                 * empty block here, this will be done at next call. This also
                 * ensures that for a very small output buffer, we emit at most
                 * one empty block.
                 */
            }
            BlockState::BlockDone => {
                if flush == Flush::PartialFlush {
                    // zng_tr_align(s);
                    todo!()
                } else if flush != Flush::Block {
                    /* FULL_FLUSH or SYNC_FLUSH */
                    // zng_tr_stored_block(s, (char*)0, 0L, 0);
                    /* For a full flush, this empty block will be recognized
                     * as a special marker by inflate_sync().
                     */
                    todo!()
                    //                    if flush == Flush::FullFlush {
                    //                        CLEAR_HASH(state); /* forget history */
                    //                        if (state.lookahead == 0) {
                    //                            state.strstart = 0;
                    //                            state.block_start = 0;
                    //                            state.insert = 0;
                    //                        }
                    //                    }
                }

                flush_pending(stream);

                if stream.avail_out == 0 {
                    stream.state.last_flush = -1; /* avoid BUF_ERROR at next call, see above */
                    return ReturnCode::Ok;
                }
            }
            BlockState::FinishDone => { /* do nothing */ }
        }
    }

    if flush != Flush::Finish {
        return ReturnCode::Ok;
    }

    // TODO gzip

    {
        if stream.state.wrap == 1 {
            let adler = stream.adler as u32;
            stream.state.pending.extend(&adler.to_be_bytes());
        }
    }

    flush_pending(stream);

    // If avail_out is zero, the application will call deflate again to flush the rest.
    if stream.state.wrap > 0 {
        stream.state.wrap = -stream.state.wrap; /* write the trailer only once! */
    }

    if stream.state.pending.pending().is_empty() {
        assert_eq!(stream.state.bi_valid, 0, "bi_buf not flushed");
        return ReturnCode::StreamEnd;
    }
    ReturnCode::Ok
}

pub(crate) fn flush_pending(stream: &mut DeflateStream) {
    let state = &mut stream.state;

    state.flush_bits();

    let pending = state.pending.pending();
    let len = Ord::min(pending.len(), stream.avail_out as usize);

    if len == 0 {
        return;
    }

    trace!("\n[FLUSH {len} bytes]");
    unsafe { std::ptr::copy_nonoverlapping(pending.as_ptr(), stream.next_out, len) };

    stream.next_out = stream.next_out.wrapping_add(len);
    stream.total_out += len as crate::c_api::z_size;
    stream.avail_out -= len as crate::c_api::uInt;

    state.pending.advance(len);
}

pub(crate) fn compress<'a>(output: &'a mut [u8], input: &[u8]) -> (&'a mut [u8], ReturnCode) {
    // TODO this is normally CompressionLevel::DefaultCompression but that does not work yet
    compress2(output, input, CompressionLevel::NoCompression as i32)
}

#[repr(i32)]
#[allow(unused)]
enum CompressionLevel {
    NoCompression = 0,
    BestSpeed = 1,
    BestCompression = 9,
    DefaultCompression = -1,
}

pub(crate) fn compress2<'a>(
    output: &'a mut [u8],
    input: &[u8],
    level: i32,
) -> (&'a mut [u8], ReturnCode) {
    compress3(
        output,
        input,
        level,
        crate::c_api::Z_DEFLATED,
        crate::MAX_WBITS,
        DEF_MEM_LEVEL,
        crate::c_api::Z_DEFAULT_STRATEGY,
    )
}

pub(crate) fn compress3<'a>(
    output: &'a mut [u8],
    input: &[u8],
    level: i32,
    method: i32,
    window_bits: i32,
    mem_level: i32,
    strategy: i32,
) -> (&'a mut [u8], ReturnCode) {
    let mut stream = z_stream {
        next_in: input.as_ptr() as *mut u8,
        avail_in: 0, // for special logic in the first  iteration
        total_in: 0,
        next_out: output.as_mut_ptr(),
        avail_out: 0, // for special logic on the first iteration
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

    let err = {
        let strm: *mut z_stream = &mut stream;
        init2(strm, level, method, window_bits, mem_level, strategy)
    };

    if err != ReturnCode::Ok as _ {
        return (output, err);
    }

    let max = libc::c_uint::MAX as usize;

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
            Flush::NoFlush
        } else {
            Flush::Finish
        };

        let err = if let Some(stream) = unsafe { DeflateStream::from_stream_mut(&mut stream) } {
            deflate(stream, flush)
        } else {
            ReturnCode::StreamError
        };

        if err != ReturnCode::Ok {
            break;
        }
    }

    let output = &mut output[..stream.total_out as usize];

    unsafe { deflateEnd(&mut stream) };

    (output, ReturnCode::Ok)
}

///  heap used to build the Huffman trees

/// The sons of heap[n] are heap[2*n] and heap[2*n+1]. heap[0] is not used.
/// The same heap array is used to build all trees.
struct Heap {
    heap: [u32; 2 * L_CODES + 1],

    /// number of elements in the heap
    heap_len: usize,

    /// element of the largest frequency
    heap_max: usize,

    depth: [u8; 2 * L_CODES + 1],
}

impl Heap {
    // an empty heap
    fn new() -> Self {
        Self {
            heap: [0; 2 * L_CODES + 1],
            heap_len: 0,
            heap_max: 0,
            depth: [0; 2 * L_CODES + 1],
        }
    }

    /// Construct the initial heap, with least frequent element in
    /// heap[SMALLEST]. The sons of heap[n] are heap[2*n] and heap[2*n+1]. heap[0] is not used.
    fn initialize(&mut self, tree: &mut [Value]) -> isize {
        let mut max_code = -1;

        self.heap_len = 0;
        self.heap_max = HEAP_SIZE;

        for (n, node) in tree.iter_mut().enumerate() {
            if node.freq() > 0 {
                self.heap_len += 1;
                self.heap[self.heap_len] = n as u32;
                max_code = n as isize;
                self.depth[n] = 0;
            } else {
                *node.len_mut() = 0;
            }
        }

        max_code
    }

    /// Index within the heap array of least frequent node in the Huffman tree
    const SMALLEST: usize = 1;

    fn smaller(tree: &[Value], n: u32, m: u32, depth: &[u8]) -> bool {
        let (n, m) = (n as usize, m as usize);

        match Ord::cmp(&tree[n].freq(), &tree[m].freq()) {
            std::cmp::Ordering::Less => true,
            std::cmp::Ordering::Equal => depth[n] <= depth[m],
            std::cmp::Ordering::Greater => false,
        }
    }

    fn pqdownheap(&mut self, tree: &[Value], mut k: usize) {
        /* tree: the tree to restore */
        /* k: node to move down */

        let v = self.heap[k];
        let mut j = k << 1; /* left son of k */

        while j <= self.heap_len {
            /* Set j to the smallest of the two sons: */
            if j < self.heap_len {
                let cond = Self::smaller(tree, self.heap[j + 1], self.heap[j], &self.depth);
                if cond {
                    j += 1;
                }
            }

            /* Exit if v is smaller than both sons */
            if Self::smaller(tree, v, self.heap[j], &self.depth) {
                break;
            }

            /* Exchange v with the smallest son */
            self.heap[k] = self.heap[j];
            k = j;

            /* And continue down the tree, setting j to the left son of k */
            j <<= 1;
        }

        self.heap[k] = v;
    }

    /// Remove the smallest element from the heap and recreate the heap with
    /// one less element. Updates heap and heap_len.
    fn pqremove(&mut self, tree: &[Value]) -> u32 {
        let top = self.heap[Self::SMALLEST];
        self.heap[Self::SMALLEST] = self.heap[self.heap_len];
        self.heap_len -= 1;

        self.pqdownheap(tree, Self::SMALLEST);

        top
    }

    /// Construct the Huffman tree by repeatedly combining the least two frequent nodes.
    fn construct_huffman_tree(&mut self, tree: &mut [Value], mut node: usize) {
        loop {
            let n = self.pqremove(tree) as usize; /* n = node of least frequency */
            let m = self.heap[Heap::SMALLEST] as usize; /* m = node of next least frequency */

            self.heap_max -= 1;
            self.heap[self.heap_max] = n as u32; /* keep the nodes sorted by frequency */
            self.heap_max -= 1;
            self.heap[self.heap_max] = m as u32;

            /* Create a new node father of n and m */
            *tree[node].freq_mut() = tree[n].freq() + tree[m].freq();
            self.depth[node] = Ord::max(self.depth[n], self.depth[m]) + 1;

            *tree[n].dad_mut() = node as u16;
            *tree[m].dad_mut() = node as u16;

            /* and insert the new node in the heap */
            self.heap[Heap::SMALLEST] = node as u32;
            node += 1;

            self.pqdownheap(tree, Heap::SMALLEST);

            if self.heap_len < 2 {
                break;
            }
        }

        self.heap_max -= 1;
        self.heap[self.heap_max] = self.heap[Heap::SMALLEST];
    }
}

#[cfg(test)]
mod test {

    use super::*;

    fn run_test_rs(data: &str) -> Vec<u8> {
        let length = 8 * 1024;
        let mut deflated = vec![0; length as usize];
        let mut length = length as u64;

        let error = unsafe {
            crate::c_api::compress(
                deflated.as_mut_ptr().cast(),
                &mut length,
                data.as_ptr().cast(),
                data.len() as _,
            )
        };

        assert_eq!(error, 0);

        deflated.truncate(length as usize);

        deflated
    }

    fn run_test_ng(data: &str) -> Vec<u8> {
        //        pub unsafe fn dynamic_compress(
        //            dest: *mut u8,
        //            dest_len: *mut libc::c_ulong,
        //            source: *const u8,
        //            source_len: libc::c_ulong,
        //        ) -> std::ffi::c_int {
        //            const LIBZ_NG_SO: &str = "/home/folkertdev/rust/zlib-ng/libz-ng.so";
        //
        //            let lib = libloading::Library::new(LIBZ_NG_SO).unwrap();
        //
        //            type Func = unsafe extern "C" fn(
        //                dest: *mut u8,
        //                destLen: *mut libc::c_ulong,
        //                source: *const u8,
        //                sourceLen: libc::c_ulong,
        //            ) -> std::ffi::c_int;
        //
        //            let f: libloading::Symbol<Func> = lib.get(b"zng_compress").unwrap();
        //
        //            f(dest, dest_len, source, source_len)
        //        }

        let length = 8 * 1024;
        let mut deflated = vec![0; length];
        let mut length = length;

        let error = unsafe {
            libz_ng_sys::compress(
                deflated.as_mut_ptr().cast(),
                &mut length,
                data.as_ptr().cast(),
                data.len() as _,
            )
        };

        assert_eq!(error, 0);

        deflated.truncate(length);

        deflated
    }

    #[test]
    #[ignore = "only for local testing (requires source changes to zlib"]
    fn compress_hello_world() {
        const EXPECTED: &[u8] = &[
            0x78, 0x01, 0x01, 0x0d, 0x00, 0xf2, 0xff, 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x20, 0x57,
            0x6f, 0x72, 0x6c, 0x64, 0x21, 0x0a, 0x20, 0x91, 0x04, 0x48,
        ];

        assert_eq!(run_test_ng("Hello World!\n"), EXPECTED);
        assert_eq!(run_test_rs("Hello World!\n"), EXPECTED);
    }

    #[test]
    #[ignore = "only for local testing (requires source changes to zlib"]
    fn compress_1025_character_string() {
        let input: String = "abcd".repeat(256) + "x";
        assert_eq!(run_test_ng(&input), run_test_rs(&input));
    }

    #[test]
    fn hello_world_huffman_only() {
        const EXPECTED: &[u8] = &[
            0x78, 0x01, 0xf3, 0x48, 0xcd, 0xc9, 0xc9, 0x57, 0x08, 0xcf, 0x2f, 0xca, 0x49, 0x51,
            0xe4, 0x02, 0x00, 0x20, 0x91, 0x04, 0x48,
        ];

        let input = "Hello World!\n";

        let mut output = vec![0; 128];

        let (output, err) = compress3(
            &mut output,
            input.as_bytes(),
            6,
            Z_DEFLATED,
            crate::MAX_WBITS,
            DEF_MEM_LEVEL,
            crate::c_api::Z_HUFFMAN_ONLY,
        );

        assert_eq!(err, ReturnCode::Ok);

        assert_eq!(output.len(), EXPECTED.len());

        assert_eq!(EXPECTED, output);
    }

    #[test]
    fn hello_world_quick() {
        const EXPECTED: &[u8] = &[
            0x78, 0x01, 0xf3, 0x48, 0xcd, 0xc9, 0xc9, 0x57, 0x08, 0xcf, 0x2f, 0xca, 0x49, 0x51,
            0xe4, 0x02, 0x00, 0x20, 0x91, 0x04, 0x48,
        ];

        let input = "Hello World!\n";

        let mut output = vec![0; 128];

        let (output, err) = compress3(
            &mut output,
            input.as_bytes(),
            1,
            Z_DEFLATED,
            crate::MAX_WBITS,
            DEF_MEM_LEVEL,
            crate::c_api::Z_DEFAULT_STRATEGY,
        );

        assert_eq!(err, ReturnCode::Ok);

        assert_eq!(output.len(), EXPECTED.len());

        assert_eq!(EXPECTED, output);
    }

    #[test]
    fn hello_world_quick_random() {
        const EXPECTED: &[u8] = &[
            0x78, 0x01, 0x53, 0xe1, 0x50, 0x51, 0xe1, 0x52, 0x51, 0x51, 0x01, 0x00, 0x03, 0xec,
            0x00, 0xeb,
        ];

        let input = "$\u{8}$$\n$$$";

        let mut output = vec![0; 128];

        let (output, err) = compress3(
            &mut output,
            input.as_bytes(),
            1,
            Z_DEFLATED,
            crate::MAX_WBITS,
            DEF_MEM_LEVEL,
            crate::c_api::Z_DEFAULT_STRATEGY,
        );

        assert_eq!(err, ReturnCode::Ok);

        assert_eq!(output.len(), EXPECTED.len());

        assert_eq!(EXPECTED, output);
    }
}
