use std::marker::PhantomData;

use crate::{
    adler32::adler32, z_stream, Flush, ReturnCode, ADLER32_INITIAL_VALUE, MAX_WBITS, MIN_WBITS,
    Z_DEFLATED,
};

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
    pub(crate) unsafe fn from_stream_ref(strm: *const z_stream) -> Option<&'a Self> {
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

        // safety: DeflateStream has the same layout as z_stream
        let stream = unsafe { &*(strm as *const DeflateStream) };

        Some(stream)
    }

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

    unsafe fn alloc_layout(&self, layout: std::alloc::Layout) -> *mut libc::c_void {
        (self.zalloc)(self.opaque, 1, layout.size() as u32)
    }

    unsafe fn dealloc<T>(&self, ptr: *mut T) {
        (self.zfree)(self.opaque, ptr.cast())
    }
}

/// number of elements in hash table
const HASH_SIZE: usize = 65536;

/// log2(HASH_SIZE)
const HASH_BITS: usize = 16;

const HASH_MASK: usize = HASH_SIZE - 1;

/// Maximum value for memLevel in deflateInit2
const MAX_MEM_LEVEL: i32 = 9;
const DEF_MEM_LEVEL: i32 = if MAX_MEM_LEVEL > 8 { 8 } else { MAX_MEM_LEVEL };

fn init(strm: *mut z_stream, level: i32) -> ReturnCode {
    init2(
        strm,
        level,
        crate::c_api::Z_DEFLATED,
        crate::MAX_WBITS,
        DEF_MEM_LEVEL,
        crate::c_api::Z_DEFAULT_STRATEGY,
    )
}

fn init2(
    strm: *mut z_stream,
    level: i32,
    method: i32,
    mut window_bits: i32,
    mem_level: i32,
    strategy: i32,
) -> ReturnCode {
    /* Todo: ignore strm->next_in if we use it as window */
    let window_padding = 0;
    let mut wrap = 1;

    if strm.is_null() {
        dbg!("here");
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
        wrap = 0;
        if window_bits < -MAX_WBITS {
            return ReturnCode::StreamError;
        }
    }

    let Ok(strategy) = Strategy::try_from(strategy) else {
        return ReturnCode::StreamError;
    };

    dbg!(method, mem_level, window_bits, level);

    if (!(1..=MAX_MEM_LEVEL).contains(&mem_level))
        || method != Z_DEFLATED
        || window_bits < MIN_WBITS
        || window_bits > MAX_WBITS
        || level < 0
        || level > 9
        || (window_bits == 8 && wrap != 1)
    {
        dbg!("here");
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
    let window_layout = std::alloc::Layout::array::<u16>(w_size + window_padding);
    let window_ptr = unsafe { stream.alloc_layout(window_layout.unwrap()) } as *mut u8;

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

    // TODO make window a slice (or use Window?)

    let prev = unsafe { std::slice::from_raw_parts_mut(prev_ptr, w_size) };
    prev.fill(0);

    let head = unsafe { &mut *head_ptr };

    let pending = Pending {
        buf: pending_buf,
        out: pending_buf,
        pending: 0,
        end: pending_buf.wrapping_add(4 * lit_bufsize),
        _marker: PhantomData,
    };

    let state = State {
        status: Status::Init,

        // window
        w_bits: window_bits as usize,
        w_size,
        w_mask: window_bits as usize - 1,

        // allocated values
        window: window_ptr,
        prev,
        head,
        pending,

        //
        high_water: 0,
        //
        // lit_bufsize

        //
        level: 0,
        strategy,

        // these fields are not set explicitly at this point
        last_flush: 0,
        bi_buf: 0,
        bi_valid: 0,
        wrap,
        strstart: 0,
        block_start: 0,
        window_size: 0,
        insert: 0,
        matches: 0,
        lookahead: 0,
    };

    unsafe { *(state_ptr as *mut State) = state };
    stream.state = state_ptr.cast();

    let Some(stream) = (unsafe { DeflateStream::from_stream_mut(strm) }) else {
        dbg!("here");
        return ReturnCode::StreamError;
    };

    reset(stream)
}

fn reset(stream: &mut DeflateStream) -> ReturnCode {
    let ret = reset_keep(stream);

    if ret == ReturnCode::Ok {
        lm_init(&mut stream.state);
    }

    ret
}

fn reset_keep(stream: &mut DeflateStream) -> ReturnCode {
    stream.total_in = 0;
    stream.total_out = 0;
    stream.msg = std::ptr::null_mut();
    stream.data_type = crate::c_api::Z_UNKNOWN;

    let state = &mut stream.state;

    state.pending = Pending {
        buf: state.pending.buf,
        out: state.pending.buf,
        pending: 0,
        end: state.pending.end,
        _marker: PhantomData,
    };

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
    state.head.fill(0);

    // Set the default configuration parameters:
    lm_set_level(state, state.level);

    state.strstart = 0;
    state.block_start = 0;
    state.lookahead = 0;
    state.insert = 0;
    //    state.prev_length = 0;
    //    state.match_available = 0;
    //    state.match_start = 0;
    //    state.ins_h = 0;
}

fn lm_set_level(state: &mut State, level: i8) {
    // s->max_lazy_match   = configuration_table[level].max_lazy;
    // s->good_match       = configuration_table[level].good_length;
    // s->nice_match       = configuration_table[level].nice_length;
    // s->max_chain_length = configuration_table[level].max_chain;

    // Use rolling hash for deflate_slow algorithm with level 9. It allows us to
    // properly lookup different hash chains to speed up longest_match search. Since hashing
    // method changes depending on the level we cannot put this into functable. */
    //    if (s->max_chain_length > 1024) {
    //        s->update_hash = &update_hash_roll;
    //        s->insert_string = &insert_string_roll;
    //        s->quick_insert_string = &quick_insert_string_roll;
    //    } else {
    //        s->update_hash = functable.update_hash;
    //        s->insert_string = functable.insert_string;
    //        s->quick_insert_string = functable.quick_insert_string;
    //    }

    state.level = level;
}

pub(crate) struct State<'a> {
    status: Status,

    pending: Pending<'a>, // output still pending

    last_flush: i32, /* value of flush param for previous deflate call */

    bi_buf: u64,
    bi_valid: u8,

    wrap: i8, /* bit 0 true for zlib, bit 1 true for gzip */

    strategy: Strategy,
    level: i8,

    strstart: usize, /* start of string to insert */

    /// Window position at the beginning of the current output block. Gets
    /// negative when the window is moved backwards.
    block_start: isize,

    window: *mut u8,

    /// High water mark offset in window for initialized bytes -- bytes above
    /// this are set to zero in order to avoid memory check warnings when
    /// longest match routines access bytes past the input.  This is then
    /// updated to the new high water mark.
    high_water: usize,

    /// Actual size of window: 2*wSize, except when the user input buffer is directly used as sliding window.
    window_size: usize,

    /// number of string matches in current block
    matches: usize,

    /// bytes at end of window left to insert
    insert: usize,

    w_size: usize,    /* LZ77 window size (32K by default) */
    w_bits: usize,    /* log2(w_size)  (8..16) */
    w_mask: usize,    /* w_size - 1 */
    lookahead: usize, /* number of valid bytes ahead in window */

    prev: &'a mut [u16],
    head: &'a mut [u16; HASH_SIZE],
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Strategy {
    Default = 0,
    Filtered = 1,
    HuffmanOnly = 2,
    RLE = 3,
    Fixed = 4,
}

impl TryFrom<i32> for Strategy {
    type Error = ();

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Strategy::Default),
            1 => Ok(Strategy::Filtered),
            2 => Ok(Strategy::HuffmanOnly),
            3 => Ok(Strategy::RLE),
            4 => Ok(Strategy::Fixed),
            _ => Err(()),
        }
    }
}

impl<'a> State<'a> {
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

    fn emit_tree(&mut self, block_type: BlockType, is_last_block: bool) {
        let header_bits = (block_type as u64) << 1 | (is_last_block as u64);
        self.send_bits(header_bits, 3);
    }

    fn send_bits(&mut self, val: u64, len: u8) {
        const BIT_BUF_SIZE: u8 = 64;

        debug_assert!(len <= 64);
        debug_assert!(self.bi_valid <= 64);

        let total_bits = len + self.bi_valid;

        // send_bits_trace(s, val, len);\
        // sent_bits_add(s, len);\

        if total_bits < BIT_BUF_SIZE {
            self.bi_buf |= val << self.bi_valid;
            self.bi_valid = total_bits;
        } else if self.bi_valid == BIT_BUF_SIZE {
            self.pending.extend(&self.bi_buf.to_le_bytes());
            self.bi_buf = val;
            self.bi_valid = len;
        } else {
            self.bi_buf |= val << self.bi_valid;
            self.pending.extend(&self.bi_buf.to_le_bytes());
            self.bi_buf = val >> (BIT_BUF_SIZE - self.bi_valid);
            self.bi_valid = total_bits - BIT_BUF_SIZE;
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

        let h = (Z_DEFLATED + ((self.w_bits as u16 - 8) << 4)) << 8 | self.level_flags() | dict;

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
        //    s->l_desc.dyn_tree = s->dyn_ltree;
        //    s->l_desc.stat_desc = &static_l_desc;
        //
        //    s->d_desc.dyn_tree = s->dyn_dtree;
        //    s->d_desc.stat_desc = &static_d_desc;
        //
        //    s->bl_desc.dyn_tree = s->bl_tree;
        //    s->bl_desc.stat_desc = &static_bl_desc;

        self.bi_buf = 0;
        self.bi_valid = 0;

        // Initialize the first block of the first file:
        self.init_block();
    }

    /// initializes a new block
    fn init_block(&mut self) {
        //    int n; /* iterates over tree elements */
        //
        //    /* Initialize the trees. */
        //    for (n = 0; n < L_CODES;  n++)
        //        s->dyn_ltree[n].Freq = 0;
        //    for (n = 0; n < D_CODES;  n++)
        //        s->dyn_dtree[n].Freq = 0;
        //    for (n = 0; n < BL_CODES; n++)
        //        s->bl_tree[n].Freq = 0;
        //
        //    s->dyn_ltree[END_BLOCK].Freq = 1;
        //    s->opt_len = s->static_len = 0L;
        //    s->sym_next = s->matches = 0;
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
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
enum BlockState {
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
const MAX_STORED: usize = 65535;

fn read_buf(stream: &mut DeflateStream, output: *mut u8, size: usize) -> usize {
    let len = Ord::min(stream.avail_in as usize, size);

    if len == 0 {
        return 0;
    }

    stream.avail_in -= len as u32;

    // TODO gzip and maybe DEFLATE_NEED_CHECKSUM too?
    if stream.state.wrap == 1 {
        // TODO fuse adler and memcpy
        let data = unsafe { std::slice::from_raw_parts(stream.next_in, len) };
        stream.adler = adler32(stream.adler as u32, data) as _;
        unsafe { std::ptr::copy_nonoverlapping(stream.next_in, output, len) }
    } else {
        unsafe { std::ptr::copy_nonoverlapping(stream.next_in, output, len) }
    }

    stream.next_in = stream.next_in.wrapping_add(len);
    stream.total_in += len as crate::c_api::z_size;

    len
}

enum BlockType {
    StoredBlock = 0,
    StaticTrees = 1,
    DynamicTrees = 2,
}

fn zng_tr_stored_block(state: &mut State, input_block: &[u8], is_last: bool) {
    // send block type
    state.emit_tree(BlockType::StoredBlock, is_last);

    // align on byte boundary
    state.flush_and_align_bits();

    // cmpr_bits_align(s);

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

fn deflate_stored(stream: &mut DeflateStream, flush: Flush) -> BlockState {
    // Smallest worthy block size when not flushing or finishing. By default
    // this is 32K. This can be as small as 507 bytes for memLevel == 1. For
    // large input and output buffers, the stored block size will be larger.
    let min_block = Ord::min(stream.state.pending.capacity() - 5, stream.state.w_size);

    // Copy as many min_block or larger stored blocks directly to next_out as
    // possible. If flushing, copy the remaining available input to next_out as
    // stored blocks, if there is enough space.

    // unsigned len, left, have, last = 0;
    let mut have;
    let mut last = false;
    let mut used = stream.avail_in;
    loop {
        // maximum deflate stored block length
        let mut len = MAX_STORED;

        // number of header bytes
        have = ((stream.state.bi_valid + 42) / 8) as usize;

        // we need room for at least the header
        if stream.avail_out < have as u32 {
            break;
        }

        let left = stream.state.strstart as isize - stream.state.block_start;
        let left = Ord::max(0, left) as usize;

        have = stream.avail_out as usize - have;

        if len > left as usize + stream.avail_in as usize {
            // limit len to the input
            len = left + stream.avail_in as usize;
        }

        len = Ord::min(len, have as usize);

        // If the stored block would be less than min_block in length, or if
        // unable to copy all of the available input when flushing, then try
        // copying to the window and the pending buffer instead. Also don't
        // write an empty block when flushing -- deflate() does that.
        if len < min_block
            && ((len == 0 && flush != Flush::Finish)
                || flush == Flush::NoFlush
                || len != left + stream.avail_in as usize)
        {
            break;
        }

        // Make a dummy stored block in pending to get the header bytes,
        // including any pending bits. This also updates the debugging counts.
        last = flush == Flush::Finish && len == left + stream.avail_in as usize;
        zng_tr_stored_block(&mut stream.state, &[], last);

        /* Replace the lengths in the dummy stored block with len. */
        stream.state.pending.rewind(4);
        stream.state.pending.extend(&(len as u16).to_le_bytes());
        stream.state.pending.extend(&(!len as u16).to_le_bytes());

        // Write the stored block header bytes.
        flush_pending(stream);

        // TODO debug counts?

        if left > 0 {
            let left = Ord::min(left, len);
            unsafe {
                std::ptr::copy_nonoverlapping(
                    stream.state.window.offset(stream.state.block_start),
                    stream.next_out,
                    left,
                );
            }

            stream.next_out = stream.next_out.wrapping_add(left);
            stream.avail_out = stream.avail_out.wrapping_sub(left as _);
            stream.total_out = stream.total_out.wrapping_add(left as _);
            stream.state.block_start += left as isize;
            len -= left;
        }

        // Copy uncompressed bytes directly from next_in to next_out, updating the check value.
        if len > 0 {
            read_buf(stream, stream.next_out, len);
            stream.next_out = stream.next_out.wrapping_add(len as _);
            stream.avail_out = stream.avail_out.wrapping_sub(len as _);
            stream.total_out = stream.total_out.wrapping_add(len as _);
        }

        if last {
            break;
        }
    }

    // Update the sliding window with the last s->w_size bytes of the copied
    // data, or append all of the copied data to the existing window if less
    // than s->w_size bytes were copied. Also update the number of bytes to
    // insert in the hash tables, in the event that deflateParams() switches to
    // a non-zero compression level.
    used -= stream.avail_in; /* number of input bytes directly copied */

    if used > 0 {
        let state = &mut stream.state;
        // If any input was used, then no unused input remains in the window, therefore s->block_start == s->strstart.
        if used as usize >= state.w_size {
            /* supplant the previous history */
            state.matches = 2; /* clear hash */

            unsafe {
                libc::memcpy(
                    state.window.cast(),
                    stream.next_in.wrapping_sub(state.w_size).cast(),
                    state.w_size,
                );
            }

            state.strstart = state.w_size;
            state.insert = state.strstart;
        } else {
            if state.window_size - state.strstart <= used as usize {
                /* Slide the window down. */
                state.strstart -= state.w_size;
                unsafe {
                    libc::memcpy(
                        state.window.cast(),
                        state.window.wrapping_add(state.w_size).cast(),
                        state.strstart,
                    );
                }
                if state.matches < 2 {
                    state.matches += 1; /* add a pending slide_hash() */
                }
                state.insert = Ord::min(state.insert, state.strstart);
            }
            unsafe {
                libc::memcpy(
                    state.window.wrapping_add(state.strstart).cast(),
                    stream.next_in.wrapping_sub(used as usize).cast(),
                    used as usize,
                );
            }

            state.strstart += used as usize;
            state.insert += Ord::min(used as usize, state.w_size - state.insert);
        }
        state.block_start = state.strstart as isize;
    }

    stream.state.high_water = Ord::max(stream.state.high_water, stream.state.strstart);

    if last {
        return BlockState::FinishDone;
    }

    // If flushing and all input has been consumed, then done.
    if flush != Flush::NoFlush
        && flush != Flush::Finish
        && stream.avail_in == 0
        && stream.state.strstart as isize == stream.state.block_start
    {
        return BlockState::BlockDone;
    }

    let have = stream.state.window_size - stream.state.strstart;
    if stream.avail_in as usize > have && stream.state.block_start >= stream.state.w_size as isize {
        todo!("fill window");
    }

    let have = Ord::min(have, stream.avail_in as usize);
    if have > 0 {
        read_buf(
            stream,
            stream.state.window.wrapping_add(stream.state.strstart),
            have,
        );

        let state = &mut stream.state;
        state.strstart += have;
        state.insert += Ord::min(have, state.w_size - state.insert);
    }

    let state = &mut stream.state;
    state.high_water = Ord::max(state.high_water, state.strstart);

    // There was not enough avail_out to write a complete worthy or flushed
    // stored block to next_out. Write a stored block to pending instead, if we
    // have enough input for a worthy block, or if flushing and there is enough
    // room for the remaining input as a stored block in the pending buffer.

    // number of header bytes
    let have = ((state.bi_valid + 42) >> 3) as usize;

    // maximum stored block length that will fit in pending:
    let have = Ord::min(state.pending.capacity() - have, MAX_STORED);
    let min_block = Ord::min(have, state.w_size);
    let left = state.strstart as isize - state.block_start;

    if left >= min_block as isize
        || ((left > 0 || flush == Flush::Finish)
            && flush != Flush::NoFlush
            && stream.avail_in == 0
            && left <= have as isize)
    {
        let len = Ord::min(left as usize, have); // TODO wrapping?
        last = flush == Flush::Finish && stream.avail_in == 0 && len == (left as usize);

        {
            // TODO hack remove
            let mut tmp = vec![0; len];

            unsafe {
                std::ptr::copy_nonoverlapping(
                    state.window.offset(state.block_start),
                    tmp.as_mut_ptr(),
                    len,
                )
            }

            zng_tr_stored_block(state, &tmp, last);
        }

        state.block_start += len as isize;
        flush_pending(stream);
    }

    // We've done all we can with the available input and output.
    if last {
        BlockState::FinishStarted
    } else {
        BlockState::NeedMore
    }
}

fn deflate_huff(state: &mut State, flush: Flush) -> BlockState {
    todo!()
    /*
    let mut bflush = false; /* set if current block must be flushed */

    loop {
        /* Make sure that we have a literal to write. */
        if state.lookahead == 0 {
            fill_window(state);
            if state.lookahead == 0 {
                if flush == Flush::NoFlush {
                    return BlockState::NeedMore;
                }

                break; /* flush the current block */
            }
        }

        /* Output a literal byte */
        bflush = zng_tr_tally_lit(state, state.window[state.strstart]);
        state.lookahead -= 1;
        state.strstart += 1;
        if bflush {
            FLUSH_BLOCK(state, 0);
        }
    }

    state.insert = 0;

    if flush == Flush::Finish {
        FLUSH_BLOCK(s, 1);
        return BlockState::FinishDone;
    }

    if state.sym_next {
        FLUSH_BLOCK(s, 0);
    }

    BlockState::BlockDone
    */
}

fn deflate_rle(state: &mut State, flush: Flush) -> BlockState {
    todo!()
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
        let bstate = match state.strategy {
            _ if state.level == 0 => deflate_stored(stream, flush),
            Strategy::HuffmanOnly => deflate_huff(state, flush),
            Strategy::RLE => deflate_rle(state, flush),
            Strategy::Default | Strategy::Filtered | Strategy::Fixed => {
                // (*(configuration_table[s->level].func))(s, flush);
                todo!()
            }
        };

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
        assert!(stream.state.bi_valid == 0, "bi_buf not flushed");
        return ReturnCode::StreamEnd;
    }
    return ReturnCode::Ok;
}

fn flush_pending(stream: &mut DeflateStream) {
    let state = &mut stream.state;

    state.flush_bits();

    let pending = state.pending.pending();
    let len = Ord::min(pending.len(), stream.avail_out as usize);

    if len == 0 {
        return;
    }

    unsafe { std::ptr::copy_nonoverlapping(pending.as_ptr(), stream.next_out, len) };

    stream.next_out = stream.next_out.wrapping_add(len);
    stream.total_out += len as crate::c_api::z_size;
    stream.avail_out -= len as crate::c_api::uInt;

    state.pending.advance(len);
}

struct Pending<'a> {
    buf: *mut u8,
    out: *mut u8,
    pending: usize,
    end: *mut u8,
    _marker: PhantomData<&'a mut [u8]>,
}

impl<'a> Pending<'a> {
    pub fn pending(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.out, self.pending) }
    }

    fn remaining(&self) -> usize {
        self.end as usize - self.out as usize
    }

    fn capacity(&self) -> usize {
        self.end as usize - self.buf as usize
    }

    #[inline(always)]
    #[track_caller]
    pub fn advance(&mut self, n: usize) {
        assert!(n <= self.remaining(), "advancing past then end");
        debug_assert!(self.pending >= n);

        self.out = self.out.wrapping_add(n);
        self.pending -= n;

        if self.pending == 0 {
            self.out = self.buf;
        }
    }

    #[inline(always)]
    #[track_caller]
    pub fn rewind(&mut self, n: usize) {
        assert!(n <= self.pending, "rewinding past then start");

        self.pending -= n;
    }

    #[inline(always)]
    #[track_caller]
    pub fn extend(&mut self, buf: &[u8]) {
        assert!(
            self.remaining() >= buf.len(),
            "buf.len() must fit in remaining()"
        );

        unsafe {
            std::ptr::copy_nonoverlapping(buf.as_ptr(), self.out.add(self.pending), buf.len());
        }

        self.pending += buf.len();
    }
}

pub(crate) fn compress<'a>(output: &'a mut [u8], input: &[u8]) -> (&'a mut [u8], ReturnCode) {
    // TODO this is normally CompressionLevel::DefaultCompression but that does not work yet
    compress2(output, input, CompressionLevel::NoCompression as i32)
}

#[repr(i32)]
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

    let err = init(&mut stream, level);
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

    (&mut output[..stream.total_out as usize], ReturnCode::Ok)
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
        pub unsafe fn dynamic_compress(
            dest: *mut u8,
            destLen: *mut libc::c_ulong,
            source: *const u8,
            sourceLen: libc::c_ulong,
        ) -> std::ffi::c_int {
            const LIBZ_NG_SO: &str = "/home/folkertdev/rust/zlib-ng/libz-ng.so";

            let lib = libloading::Library::new(LIBZ_NG_SO).unwrap();

            type Func = unsafe extern "C" fn(
                dest: *mut u8,
                destLen: *mut libc::c_ulong,
                source: *const u8,
                sourceLen: libc::c_ulong,
            ) -> std::ffi::c_int;

            let f: libloading::Symbol<Func> = lib.get(b"zng_compress").unwrap();

            f(dest, destLen, source, sourceLen)
        }

        let length = 8 * 1024;
        let mut deflated = vec![0; length as usize];
        let mut length = length as u64;

        let error = unsafe {
            dynamic_compress(
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

    #[test]
    fn compress_hello_world() {
        const EXPECTED: &[u8] = &[
            0x78, 0x01, 0x01, 0x0d, 0x00, 0xf2, 0xff, 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x20, 0x57,
            0x6f, 0x72, 0x6c, 0x64, 0x21, 0x0a, 0x20, 0x91, 0x04, 0x48,
        ];

        assert_eq!(run_test_ng("Hello World!\n"), EXPECTED);
        assert_eq!(run_test_rs("Hello World!\n"), EXPECTED);
    }

    #[test]
    fn compress_1025_character_string() {
        let input: String = "abcd".repeat(256) + "x";
        assert_eq!(run_test_ng(&input), run_test_rs(&input));
    }

    fn slide_hash_rust_chain(table: &mut [u16], wsize: u16) {
        for m in table.iter_mut() {
            *m = m.saturating_sub(wsize);
        }
    }

    fn slide_hash_rust(state: &mut State) {
        let wsize = state.w_size as u16;

        slide_hash_rust_chain(state.head, wsize);
        slide_hash_rust_chain(state.prev, wsize);
    }
}
