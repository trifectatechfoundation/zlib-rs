use std::marker::PhantomData;

use crate::{adler32::adler32, z_stream, Flush, ReturnCode, ADLER32_INITIAL_VALUE};

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

    /// bytes at end of window left to insert
    insert: usize,

    w_size: usize,    /* LZ77 window size (32K by default) */
    w_bits: usize,    /* log2(w_size)  (8..16) */
    w_mask: usize,    /* w_size - 1 */
    lookahead: usize, /* number of valid bytes ahead in window */

    _marker: PhantomData<&'a ()>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Strategy {
    Default = 0,
    Filtered = 1,
    HuffmanOnly = 2,
    RLE = 3,
    Fixed = 4,
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
        dbg!(self.bi_valid, keep_bytes);
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

    dbg!(state.pending.pending);

    // align on byte boundary
    state.flush_and_align_bits();

    dbg!(state.pending.pending);

    // cmpr_bits_align(s);

    let stored_len = input_block.len() as u16;
    dbg!(stored_len);

    state.pending.extend(&stored_len.to_le_bytes());
    state.pending.extend(&(!stored_len).to_le_bytes());

    // cmpr_bits_add(s, 32);
    // sent_bits_add(s, 32);
    if stored_len > 0 {
        state.pending.extend(input_block);
        // cmpr_bits_add(s, stored_len << 3);
        // sent_bits_add(s, stored_len << 3);
    }

    dbg!(state.pending.pending);
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
            dbg!("break 1");
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
            dbg!("break 2");
            break;
        }

        // Make a dummy stored block in pending to get the header bytes,
        // including any pending bits. This also updates the debugging counts.
        last = flush == Flush::Finish && len == left + stream.avail_in as usize;
        dbg!(stream.state.pending.pending);
        zng_tr_stored_block(&mut stream.state, &[], last);
        dbg!(stream.state.pending.pending);

        /* Replace the lengths in the dummy stored block with len. */
        stream.state.pending.rewind(4);
        stream.state.pending.extend(&(len as u16).to_le_bytes());
        stream.state.pending.extend(&(!len as u16).to_le_bytes());

        dbg!(len, stream.state.pending.pending);

        // Write the stored block header bytes.
        flush_pending(stream);

        // TODO debug counts?

        if left > 0 {
            let left = Ord::min(left, len);
            unsafe {
                dbg!(std::slice::from_raw_parts(
                    stream.state.window.offset(stream.state.block_start),
                    left
                ));

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
            stream.avail_out = stream.avail_out.wrapping_sub(len as _);
            dbg!("here");
            stream.total_out = stream.total_out.wrapping_add(len as _);
            stream.state.block_start += len as isize;
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
        todo!()
    }

    // TODO
    // s->high_water = MAX(s->high_water, s->strstart);

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

            dbg!(&tmp, len);

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
        panic!();
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
        dbg!(stream.total_out);
        let bstate = match state.strategy {
            _ if state.level == 0 => deflate_stored(stream, flush),
            Strategy::HuffmanOnly => deflate_huff(state, flush),
            Strategy::RLE => deflate_rle(state, flush),
            Strategy::Default | Strategy::Filtered | Strategy::Fixed => {
                // (*(configuration_table[s->level].func))(s, flush);
                todo!()
            }
        };
        dbg!(stream.total_out);

        dbg!(&bstate);

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
            dbg!(adler);
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

#[cfg(test)]
mod test {
    use std::thread::available_parallelism;

    use super::*;

    #[test]
    fn foobar() {
        let mut window = vec![0; 1 << 15];

        let mut pending_buf = vec![0; 1024];

        let pending = Pending {
            buf: pending_buf.as_mut_ptr(),
            out: pending_buf.as_mut_ptr(),
            pending: 0,
            end: pending_buf.as_mut_ptr().wrapping_add(pending_buf.len()),
            _marker: PhantomData,
        };

        let mut state = State {
            status: Status::Init,
            pending,
            last_flush: 0,
            bi_buf: 0,
            bi_valid: 0,
            wrap: 1,
            strategy: Strategy::Default,
            level: 0,
            strstart: 0,
            block_start: 0,
            window: window.as_mut_ptr(),
            window_size: 1024,
            high_water: 0,
            insert: 0,
            w_size: 1 << 15,
            w_bits: 15,
            w_mask: 0,
            lookahead: 0,
            _marker: PhantomData,
        };

        let mut input = *b"Hello World!\n";
        let mut output = vec![0; 32];

        let mut stream = DeflateStream {
            next_in: input.as_mut_ptr(),
            avail_in: input.len() as _,
            total_in: 0,
            next_out: output.as_mut_ptr(),
            avail_out: output.len() as _,
            total_out: 0,
            msg: std::ptr::null_mut(),
            state: &mut state,
            zalloc: crate::allocate::zcalloc,
            zfree: crate::allocate::zcfree,
            opaque: std::ptr::null_mut(),
            data_type: 0,
            adler: 0,
            reserved: 0,
        };

        // NoFlush is used by uncompress until the final call
        let x = deflate(&mut stream, Flush::NoFlush);
        dbg!(x, stream.total_out);

        println!("\n---------------\n");

        let x = deflate(&mut stream, Flush::Finish);
        dbg!(x, stream.total_out);
        output.truncate(stream.total_out as usize);

        println!("{:X?}", &output);
    }
}

pub(crate) fn compress<'a>(output: &mut [u8], input: &[u8]) -> (&'a mut [u8], ReturnCode) {
    compress2(output, input, CompressionLevel::DefaultCompression)
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
    compression_level: CompressionLevel,
) -> (&'a mut [u8], ReturnCode) {
    let mut window = vec![0; 1 << 15];

    let mut pending_buf = vec![0; 1024];

    let pending = Pending {
        buf: pending_buf.as_mut_ptr(),
        out: pending_buf.as_mut_ptr(),
        pending: 0,
        end: pending_buf.as_mut_ptr().wrapping_add(pending_buf.len()),
        _marker: PhantomData,
    };

    let mut state = State {
        status: Status::Init,
        pending,
        last_flush: 0,
        bi_buf: 0,
        bi_valid: 0,
        wrap: 1,
        strategy: Strategy::Default,
        level: 0,
        strstart: 0,
        block_start: 0,
        window: window.as_mut_ptr(),
        window_size: 1024,
        high_water: 0,
        insert: 0,
        w_size: 1 << 15,
        w_bits: 15,
        w_mask: 0,
        lookahead: 0,
        _marker: PhantomData,
    };

    let mut stream = DeflateStream {
        next_in: input.as_ptr() as *mut u8,
        avail_in: input.len() as _,
        total_in: 0,
        next_out: output.as_mut_ptr(),
        avail_out: output.len() as _,
        total_out: 0,
        msg: std::ptr::null_mut(),
        state: &mut state,
        zalloc: crate::allocate::zcalloc,
        zfree: crate::allocate::zcfree,
        opaque: std::ptr::null_mut(),
        data_type: 0,
        adler: 0,
        reserved: 0,
    };

    let max = libc::c_uint::MAX as usize;

    let mut left = output.len();
    let mut source_len = input.len();

    let mut err;
    loop {
        if stream.avail_out == 0 {
            stream.avail_out = Ord::min(left, max) as _;
            left -= stream.avail_out as usize;
        }
        if stream.avail_in == 0 {
            stream.avail_in = Ord::min(source_len, max) as _;
            source_len -= stream.avail_in as usize;
        }
        err = deflate(
            &mut stream,
            if source_len > 0 {
                Flush::NoFlush
            } else {
                Flush::Finish
            },
        );

        if err != ReturnCode::Ok {
            break;
        }
    }

    (&mut output[..stream.total_out as usize], ReturnCode::Ok)
}
