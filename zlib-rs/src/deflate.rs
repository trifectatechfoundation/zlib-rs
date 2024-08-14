use core::{ffi::CStr, marker::PhantomData, mem::MaybeUninit, ops::ControlFlow};

use crate::{
    adler32::adler32,
    allocate::Allocator,
    c_api::{gz_header, internal_state, z_checksum, z_stream},
    crc32::{crc32, Crc32Fold},
    read_buf::ReadBuf,
    trace, DeflateFlush, ReturnCode, ADLER32_INITIAL_VALUE, CRC32_INITIAL_VALUE, MAX_WBITS,
    MIN_WBITS,
};

use self::{
    algorithm::CONFIGURATION_TABLE,
    hash_calc::{Crc32HashCalc, HashCalc, HashCalcVariant, RollHashCalc, StandardHashCalc},
    pending::Pending,
    trees_tbl::STATIC_LTREE,
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
pub struct DeflateStream<'a> {
    pub(crate) next_in: *mut crate::c_api::Bytef,
    pub(crate) avail_in: crate::c_api::uInt,
    pub(crate) total_in: crate::c_api::z_size,
    pub(crate) next_out: *mut crate::c_api::Bytef,
    pub(crate) avail_out: crate::c_api::uInt,
    pub(crate) total_out: crate::c_api::z_size,
    pub(crate) msg: *const core::ffi::c_char,
    pub(crate) state: &'a mut State<'a>,
    pub(crate) alloc: Allocator<'a>,
    pub(crate) data_type: core::ffi::c_int,
    pub(crate) adler: crate::c_api::z_checksum,
    pub(crate) reserved: crate::c_api::uLong,
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

        // safety: DeflateStream has the same layout as z_stream
        let stream = unsafe { &mut *(strm as *mut DeflateStream) };

        Some(stream)
    }

    fn as_z_stream_mut(&mut self) -> &mut z_stream {
        // safety: a valid &mut DeflateStream is also a valid &mut z_stream
        unsafe { &mut *(self as *mut DeflateStream as *mut z_stream) }
    }

    pub fn pending(&self) -> (usize, u8) {
        (
            self.state.bit_writer.pending.pending,
            self.state.bit_writer.bits_used,
        )
    }
}

/// number of elements in hash table
pub(crate) const HASH_SIZE: usize = 65536;
/// log2(HASH_SIZE)
const HASH_BITS: usize = 16;

/// Maximum value for memLevel in deflateInit2
const MAX_MEM_LEVEL: i32 = 9;
const DEF_MEM_LEVEL: i32 = if MAX_MEM_LEVEL > 8 { 8 } else { MAX_MEM_LEVEL };

#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[cfg_attr(feature = "__internal-fuzz", derive(arbitrary::Arbitrary))]
pub enum Method {
    #[default]
    Deflated = 8,
}

impl TryFrom<i32> for Method {
    type Error = ();

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            8 => Ok(Self::Deflated),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "__internal-fuzz", derive(arbitrary::Arbitrary))]
pub struct DeflateConfig {
    pub level: i32,
    pub method: Method,
    pub window_bits: i32,
    pub mem_level: i32,
    pub strategy: Strategy,
}

#[cfg(any(test, feature = "__internal-test"))]
impl quickcheck::Arbitrary for DeflateConfig {
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        let mem_levels: Vec<_> = (1..=9).collect();
        let levels: Vec<_> = (0..=9).collect();

        let mut window_bits = Vec::new();
        window_bits.extend(9..=15); // zlib
        window_bits.extend(9 + 16..=15 + 16); // gzip
        window_bits.extend(-15..=-9); // raw

        Self {
            level: *g.choose(&levels).unwrap(),
            method: Method::Deflated,
            window_bits: *g.choose(&window_bits).unwrap(),
            mem_level: *g.choose(&mem_levels).unwrap(),
            strategy: *g
                .choose(&[
                    Strategy::Default,
                    Strategy::Filtered,
                    Strategy::HuffmanOnly,
                    Strategy::Rle,
                    Strategy::Fixed,
                ])
                .unwrap(),
        }
    }
}

impl DeflateConfig {
    pub fn new(level: i32) -> Self {
        Self {
            level,
            ..Self::default()
        }
    }
}

impl Default for DeflateConfig {
    fn default() -> Self {
        Self {
            level: crate::c_api::Z_DEFAULT_COMPRESSION,
            method: Method::Deflated,
            window_bits: MAX_WBITS,
            mem_level: DEF_MEM_LEVEL,
            strategy: Strategy::Default,
        }
    }
}

// TODO: This could use `MaybeUninit::slice_assume_init` when it is stable.
unsafe fn slice_assume_init_mut<T>(slice: &mut [MaybeUninit<T>]) -> &mut [T] {
    &mut *(slice as *mut [MaybeUninit<T>] as *mut [T])
}

// when stable, use MaybeUninit::write_slice
fn slice_to_uninit<T>(slice: &[T]) -> &[MaybeUninit<T>] {
    // safety: &[T] and &[MaybeUninit<T>] have the same layout
    unsafe { &*(slice as *const [T] as *const [MaybeUninit<T>]) }
}

pub fn init(stream: &mut z_stream, config: DeflateConfig) -> ReturnCode {
    let DeflateConfig {
        mut level,
        method: _,
        mut window_bits,
        mem_level,
        strategy,
    } = config;

    /* Todo: ignore strm->next_in if we use it as window */
    stream.msg = core::ptr::null_mut();

    // for safety we  must really make sure that alloc and free are consistent
    // this is a (slight) deviation from stock zlib. In this crate we pick the rust
    // allocator as the default, but `libz-rs-sys` always explicitly sets an allocator,
    // and can configure the C allocator
    #[cfg(feature = "rust-allocator")]
    if stream.zalloc.is_none() || stream.zfree.is_none() {
        stream.configure_default_rust_allocator()
    }

    #[cfg(feature = "c-allocator")]
    if stream.zalloc.is_none() || stream.zfree.is_none() {
        stream.configure_default_c_allocator()
    }

    if stream.zalloc.is_none() || stream.zfree.is_none() {
        return ReturnCode::StreamError;
    }

    if level == crate::c_api::Z_DEFAULT_COMPRESSION {
        level = 6;
    }

    let wrap = if window_bits < 0 {
        if window_bits < -MAX_WBITS {
            return ReturnCode::StreamError;
        }
        window_bits = -window_bits;

        0
    } else if window_bits > MAX_WBITS {
        window_bits -= 16;
        2
    } else {
        1
    };

    if (!(1..=MAX_MEM_LEVEL).contains(&mem_level))
        || !(MIN_WBITS..=MAX_WBITS).contains(&window_bits)
        || !(0..=9).contains(&level)
        || (window_bits == 8 && wrap != 1)
    {
        return ReturnCode::StreamError;
    }

    let window_bits = if window_bits == 8 {
        9 /* until 256-byte window bug fixed */
    } else {
        window_bits as usize
    };

    let alloc = Allocator {
        zalloc: stream.zalloc.unwrap(),
        zfree: stream.zfree.unwrap(),
        opaque: stream.opaque,
        _marker: PhantomData,
    };

    // allocated here to have the same order as zlib
    let Some(state_allocation) = alloc.allocate::<State>() else {
        return ReturnCode::MemError;
    };

    let w_size = 1 << window_bits;
    let window = Window::new_in(&alloc, window_bits);

    let prev = alloc.allocate_slice::<u16>(w_size);
    let head = alloc.allocate::<[u16; HASH_SIZE]>();

    let lit_bufsize = 1 << (mem_level + 6); // 16K elements by default
    let pending = Pending::new_in(&alloc, 4 * lit_bufsize);

    // zlib-ng overlays the pending_buf and sym_buf. We cannot really do that safely
    let sym_buf = ReadBuf::new_in(&alloc, 3 * lit_bufsize);

    // if any allocation failed, clean up allocations that did succeed
    let (window, prev, head, pending, sym_buf) = match (window, prev, head, pending, sym_buf) {
        (Some(window), Some(prev), Some(head), Some(pending), Some(sym_buf)) => {
            (window, prev, head, pending, sym_buf)
        }
        (window, prev, head, pending, sym_buf) => {
            unsafe {
                if let Some(mut sym_buf) = sym_buf {
                    alloc.deallocate(sym_buf.as_mut_ptr(), sym_buf.capacity())
                }
                if let Some(pending) = pending {
                    pending.drop_in(&alloc);
                }
                if let Some(head) = head {
                    alloc.deallocate(head.as_mut_ptr(), 1)
                }
                if let Some(prev) = prev {
                    alloc.deallocate(prev.as_mut_ptr(), prev.len())
                }
                if let Some(mut window) = window {
                    window.drop_in(&alloc);
                }

                alloc.deallocate(state_allocation.as_mut_ptr(), 1);
            }

            return ReturnCode::MemError;
        }
    };

    prev.fill(MaybeUninit::zeroed());
    let prev = unsafe { slice_assume_init_mut(prev) };

    *head = MaybeUninit::zeroed();
    let head = unsafe { head.assume_init_mut() };

    let state = State {
        status: Status::Init,

        // window
        w_bits: window_bits,
        w_size,
        w_mask: w_size - 1,

        // allocated values
        window,
        prev,
        head,
        bit_writer: BitWriter::from_pending(pending),

        //
        lit_bufsize,

        //
        sym_buf,

        //
        level: level as i8, // set to zero again for testing?
        strategy,

        // these fields are not set explicitly at this point
        last_flush: 0,
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
        crc_fold: Crc32Fold::new(),
        gzhead: None,
        gzindex: 0,

        //
        match_start: 0,
        match_length: 0,
        prev_match: 0,
        match_available: false,
        prev_length: 0,

        // just provide a valid default; gets set properly later
        hash_calc_variant: HashCalcVariant::Standard,
    };

    let state = state_allocation.write(state);
    stream.state = state as *mut _ as *mut internal_state;

    let Some(stream) = (unsafe { DeflateStream::from_stream_mut(stream) }) else {
        if cfg!(debug_assertions) {
            unreachable!("we should have initialized the stream properly");
        }
        return ReturnCode::StreamError;
    };

    reset(stream)
}

pub fn params(stream: &mut DeflateStream, level: i32, strategy: Strategy) -> ReturnCode {
    let level = if level == crate::c_api::Z_DEFAULT_COMPRESSION {
        6
    } else {
        level
    };

    if !(0..=9).contains(&level) {
        return ReturnCode::StreamError;
    }

    let level = level as i8;

    let func = CONFIGURATION_TABLE[stream.state.level as usize].func;

    let state = &mut stream.state;

    if (strategy != state.strategy || func != CONFIGURATION_TABLE[level as usize].func)
        && state.last_flush != -2
    {
        // Flush the last buffer.
        let err = deflate(stream, DeflateFlush::Block);
        if err == ReturnCode::StreamError {
            return err;
        }

        let state = &mut stream.state;

        if stream.avail_in != 0
            || ((state.strstart as isize - state.block_start) + state.lookahead as isize) != 0
        {
            return ReturnCode::BufError;
        }
    }

    let state = &mut stream.state;

    if state.level != level {
        if state.level == 0 && state.matches != 0 {
            if state.matches == 1 {
                self::slide_hash::slide_hash(state);
            } else {
                state.head.fill(0);
            }
            state.matches = 0;
        }

        lm_set_level(state, level);
    }

    state.strategy = strategy;

    ReturnCode::Ok
}

pub fn set_dictionary(stream: &mut DeflateStream, mut dictionary: &[u8]) -> ReturnCode {
    let state = &mut stream.state;

    let wrap = state.wrap;

    if wrap == 2 || (wrap == 1 && state.status != Status::Init) || state.lookahead != 0 {
        return ReturnCode::StreamError;
    }

    // when using zlib wrappers, compute Adler-32 for provided dictionary
    if wrap == 1 {
        stream.adler = adler32(stream.adler as u32, dictionary) as z_checksum;
    }

    // avoid computing Adler-32 in read_buf
    state.wrap = 0;

    // if dictionary would fill window, just replace the history
    if dictionary.len() >= state.window.capacity() {
        if wrap == 0 {
            // clear the hash table
            state.head.fill(0);

            state.strstart = 0;
            state.block_start = 0;
            state.insert = 0;
        } else {
            /* already empty otherwise */
        }

        // use the tail
        dictionary = &dictionary[dictionary.len() - state.w_size..];
    }

    // insert dictionary into window and hash
    let avail = stream.avail_in;
    let next = stream.next_in;
    stream.avail_in = dictionary.len() as _;
    stream.next_in = dictionary.as_ptr() as *mut u8;
    fill_window(stream);

    while stream.state.lookahead >= STD_MIN_MATCH {
        let str = stream.state.strstart;
        let n = stream.state.lookahead - (STD_MIN_MATCH - 1);
        stream.state.insert_string(str, n);
        stream.state.strstart = str + n;
        stream.state.lookahead = STD_MIN_MATCH - 1;
        fill_window(stream);
    }

    let state = &mut stream.state;

    state.strstart += state.lookahead;
    state.block_start = state.strstart as _;
    state.insert = state.lookahead;
    state.lookahead = 0;
    state.prev_length = 0;
    state.match_available = false;

    // restore the state
    stream.next_in = next;
    stream.avail_in = avail;
    state.wrap = wrap;

    ReturnCode::Ok
}

pub fn prime(stream: &mut DeflateStream, mut bits: i32, value: i32) -> ReturnCode {
    // our logic actually supports up to 32 bits.
    debug_assert!(bits <= 16, "zlib only supports up to 16 bits here");

    let mut value64 = value as u64;

    let state = &mut stream.state;

    if bits < 0
        || bits > BitWriter::BIT_BUF_SIZE as i32
        || bits > (core::mem::size_of_val(&value) << 3) as i32
    {
        return ReturnCode::BufError;
    }

    let mut put;

    loop {
        put = BitWriter::BIT_BUF_SIZE - state.bit_writer.bits_used;
        let put = Ord::min(put as i32, bits);

        if state.bit_writer.bits_used == 0 {
            state.bit_writer.bit_buffer = value64;
        } else {
            state.bit_writer.bit_buffer |=
                (value64 & ((1 << put) - 1)) << state.bit_writer.bits_used;
        }

        state.bit_writer.bits_used += put as u8;
        state.bit_writer.flush_bits();
        value64 >>= put;
        bits -= put;

        if bits == 0 {
            break;
        }
    }

    ReturnCode::Ok
}

pub fn copy<'a>(
    dest: &mut MaybeUninit<DeflateStream<'a>>,
    source: &mut DeflateStream<'a>,
) -> ReturnCode {
    // Safety: source and dest are both mutable references, so guaranteed not to overlap.
    // dest being a reference to maybe uninitialized memory makes a copy of 1 DeflateStream valid.
    unsafe {
        core::ptr::copy_nonoverlapping(source, dest.as_mut_ptr(), 1);
    }

    let alloc = &source.alloc;

    // allocated here to have the same order as zlib
    let Some(state_allocation) = alloc.allocate::<State>() else {
        return ReturnCode::MemError;
    };

    let source_state = &source.state;

    let window = source_state.window.clone_in(alloc);

    let prev = alloc.allocate_slice::<u16>(source_state.w_size);
    let head = alloc.allocate::<[u16; HASH_SIZE]>();

    let pending = source_state.bit_writer.pending.clone_in(alloc);
    let sym_buf = source_state.sym_buf.clone_in(alloc);

    // if any allocation failed, clean up allocations that did succeed
    let (window, prev, head, pending, sym_buf) = match (window, prev, head, pending, sym_buf) {
        (Some(window), Some(prev), Some(head), Some(pending), Some(sym_buf)) => {
            (window, prev, head, pending, sym_buf)
        }
        (window, prev, head, pending, sym_buf) => {
            // Safety: this access is in-bounds
            let field_ptr = unsafe { core::ptr::addr_of_mut!((*dest.as_mut_ptr()).state) };
            unsafe { core::ptr::write(field_ptr as *mut *mut State, core::ptr::null_mut()) };

            // Safety: it is an assumpion on DeflateStream that (de)allocation does not cause UB.
            unsafe {
                if let Some(mut sym_buf) = sym_buf {
                    alloc.deallocate(sym_buf.as_mut_ptr(), sym_buf.capacity())
                }
                if let Some(pending) = pending {
                    pending.drop_in(alloc);
                }
                if let Some(head) = head {
                    alloc.deallocate(head.as_mut_ptr(), HASH_SIZE)
                }
                if let Some(prev) = prev {
                    alloc.deallocate(prev.as_mut_ptr(), prev.len())
                }
                if let Some(mut window) = window {
                    window.drop_in(alloc);
                }

                alloc.deallocate(state_allocation.as_mut_ptr(), 1);
            }

            return ReturnCode::MemError;
        }
    };

    prev.copy_from_slice(slice_to_uninit(source_state.prev));
    let prev = unsafe { core::slice::from_raw_parts_mut(prev.as_mut_ptr().cast(), prev.len()) };
    let head = head.write(*source_state.head);

    let mut bit_writer = BitWriter::from_pending(pending);
    bit_writer.bits_used = source_state.bit_writer.bits_used;
    bit_writer.bit_buffer = source_state.bit_writer.bit_buffer;

    let dest_state = State {
        status: source_state.status,
        bit_writer,
        last_flush: source_state.last_flush,
        wrap: source_state.wrap,
        strategy: source_state.strategy,
        level: source_state.level,
        good_match: source_state.good_match,
        nice_match: source_state.nice_match,
        l_desc: source_state.l_desc.clone(),
        d_desc: source_state.d_desc.clone(),
        bl_desc: source_state.bl_desc.clone(),
        bl_count: source_state.bl_count,
        match_length: source_state.match_length,
        prev_match: source_state.prev_match,
        match_available: source_state.match_available,
        strstart: source_state.strstart,
        match_start: source_state.match_start,
        prev_length: source_state.prev_length,
        max_chain_length: source_state.max_chain_length,
        max_lazy_match: source_state.max_lazy_match,
        block_start: source_state.block_start,
        block_open: source_state.block_open,
        window,
        sym_buf,
        lit_bufsize: source_state.lit_bufsize,
        window_size: source_state.window_size,
        matches: source_state.matches,
        opt_len: source_state.opt_len,
        static_len: source_state.static_len,
        insert: source_state.insert,
        w_size: source_state.w_size,
        w_bits: source_state.w_bits,
        w_mask: source_state.w_mask,
        lookahead: source_state.lookahead,
        prev,
        head,
        ins_h: source_state.ins_h,
        heap: source_state.heap.clone(),
        hash_calc_variant: source_state.hash_calc_variant,
        crc_fold: source_state.crc_fold,
        gzhead: None,
        gzindex: source_state.gzindex,
    };

    // write the cloned state into state_ptr
    let state_ptr = state_allocation.write(dest_state);

    // insert the state_ptr into `dest`
    let field_ptr = unsafe { core::ptr::addr_of_mut!((*dest.as_mut_ptr()).state) };
    unsafe { core::ptr::write(field_ptr as *mut *mut State, state_ptr) };

    // update the gzhead field (it contains a mutable reference so we need to be careful
    let field_ptr = unsafe { core::ptr::addr_of_mut!((*dest.as_mut_ptr()).state.gzhead) };
    unsafe { core::ptr::copy(&source_state.gzhead, field_ptr, 1) };

    ReturnCode::Ok
}

/// # Returns
///
/// - Err when deflate is not done. A common cause is insufficient output space
/// - Ok otherwise
pub fn end<'a>(stream: &'a mut DeflateStream) -> Result<&'a mut z_stream, &'a mut z_stream> {
    let status = stream.state.status;

    let alloc = stream.alloc;

    // deallocate in reverse order of allocations
    unsafe {
        // safety: we make sure that these fields are not used (by invalidating the state pointer)
        stream.state.sym_buf.drop_in(&alloc);
        stream.state.bit_writer.pending.drop_in(&alloc);
        alloc.deallocate(stream.state.head, 1);
        if !stream.state.prev.is_empty() {
            alloc.deallocate(stream.state.prev.as_mut_ptr(), stream.state.prev.len());
        }
        stream.state.window.drop_in(&alloc);
    }

    let state = stream.state as *mut State;
    let stream = stream.as_z_stream_mut();
    stream.state = core::ptr::null_mut();

    // safety: `state` is not used later
    unsafe {
        alloc.deallocate(state, 1);
    }

    match status {
        Status::Busy => Err(stream),
        _ => Ok(stream),
    }
}

pub fn reset(stream: &mut DeflateStream) -> ReturnCode {
    let ret = reset_keep(stream);

    if ret == ReturnCode::Ok {
        lm_init(stream.state);
    }

    ret
}

fn reset_keep(stream: &mut DeflateStream) -> ReturnCode {
    stream.total_in = 0;
    stream.total_out = 0;
    stream.msg = core::ptr::null_mut();
    stream.data_type = crate::c_api::Z_UNKNOWN;

    let state = &mut stream.state;

    state.bit_writer.pending.reset_keep();

    // can be made negative by deflate(..., Z_FINISH);
    state.wrap = state.wrap.abs();

    state.status = match state.wrap {
        2 => Status::GZip,
        _ => Status::Init,
    };

    stream.adler = match state.wrap {
        2 => {
            state.crc_fold = Crc32Fold::new();
            CRC32_INITIAL_VALUE as _
        }
        _ => ADLER32_INITIAL_VALUE as _,
    };

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
    state.prev_length = 0;
    state.match_available = false;
    state.match_start = 0;
    state.ins_h = 0;
}

fn lm_set_level(state: &mut State, level: i8) {
    state.max_lazy_match = CONFIGURATION_TABLE[level as usize].max_lazy as usize;
    state.good_match = CONFIGURATION_TABLE[level as usize].good_length as usize;
    state.nice_match = CONFIGURATION_TABLE[level as usize].nice_length as usize;
    state.max_chain_length = CONFIGURATION_TABLE[level as usize].max_chain as usize;

    state.hash_calc_variant = HashCalcVariant::for_max_chain_length(state.max_chain_length);
    state.level = level;
}

pub fn tune(
    stream: &mut DeflateStream,
    good_length: usize,
    max_lazy: usize,
    nice_length: usize,
    max_chain: usize,
) -> ReturnCode {
    stream.state.good_match = good_length;
    stream.state.max_lazy_match = max_lazy;
    stream.state.nice_match = nice_length;
    stream.state.max_chain_length = max_chain;

    ReturnCode::Ok
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

struct BitWriter<'a> {
    pub(crate) pending: Pending<'a>, // output still pending
    pub(crate) bit_buffer: u64,
    pub(crate) bits_used: u8,
}

impl<'a> BitWriter<'a> {
    pub(crate) const BIT_BUF_SIZE: u8 = 64;

    fn from_pending(pending: Pending<'a>) -> Self {
        Self {
            pending,
            bit_buffer: 0,
            bits_used: 0,
        }
    }

    fn flush_bits(&mut self) {
        debug_assert!(self.bits_used <= 64);
        let removed = self.bits_used.saturating_sub(7).next_multiple_of(8);
        let keep_bytes = self.bits_used / 8; // can never divide by zero

        let src = &self.bit_buffer.to_le_bytes();
        self.pending.extend(&src[..keep_bytes as usize]);

        self.bits_used -= removed;
        self.bit_buffer = self.bit_buffer.checked_shr(removed as u32).unwrap_or(0);
    }

    fn flush_and_align_bits(&mut self) {
        debug_assert!(self.bits_used <= 64);
        let keep_bytes = self.bits_used.div_ceil(8);
        let src = &self.bit_buffer.to_le_bytes();
        self.pending.extend(&src[..keep_bytes as usize]);

        self.bits_used = 0;
        self.bit_buffer = 0;
    }

    fn send_bits(&mut self, val: u64, len: u8) {
        debug_assert!(len <= 64);
        debug_assert!(self.bits_used <= 64);

        let total_bits = len + self.bits_used;

        // send_bits_trace(s, val, len);\
        // sent_bits_add(s, len);\

        if total_bits < Self::BIT_BUF_SIZE {
            self.bit_buffer |= val << self.bits_used;
            self.bits_used = total_bits;
        } else if self.bits_used == Self::BIT_BUF_SIZE {
            // with how send_bits is called, this is unreachable in practice
            self.pending.extend(&self.bit_buffer.to_le_bytes());
            self.bit_buffer = val;
            self.bits_used = len;
        } else {
            self.bit_buffer |= val << self.bits_used;
            self.pending.extend(&self.bit_buffer.to_le_bytes());
            self.bit_buffer = val >> (Self::BIT_BUF_SIZE - self.bits_used);
            self.bits_used = total_bits - Self::BIT_BUF_SIZE;
        }
    }

    fn send_code(&mut self, code: usize, tree: &[Value]) {
        let node = tree[code];
        self.send_bits(node.code() as u64, node.len() as u8)
    }

    /// Send one empty static block to give enough lookahead for inflate.
    /// This takes 10 bits, of which 7 may remain in the bit buffer.
    pub fn align(&mut self) {
        self.emit_tree(BlockType::StaticTrees, false);
        self.emit_end_block(&STATIC_LTREE, false);
        self.flush_bits();
    }

    pub(crate) fn emit_tree(&mut self, block_type: BlockType, is_last_block: bool) {
        let header_bits = (block_type as u64) << 1 | (is_last_block as u64);
        self.send_bits(header_bits, 3);
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

        let lnode = ltree[c];
        let mut match_bits = lnode.code() as usize;
        let mut match_bits_len = lnode.len() as usize;
        let mut extra = StaticTreeDesc::EXTRA_LBITS[code] as usize;
        if extra != 0 {
            lc -= self::trees_tbl::BASE_LENGTH[code] as usize;
            match_bits |= lc << match_bits_len;
            match_bits_len += extra;
        }

        dist -= 1; /* dist is now the match distance - 1 */
        code = State::d_code(dist) as usize;
        assert!(code < D_CODES, "bad d_code");
        // send_code_trace(s, code);

        /* Send the distance code */
        let dnode = dtree[code];
        match_bits |= (dnode.code() as usize) << match_bits_len;
        match_bits_len += dnode.len() as usize;
        extra = StaticTreeDesc::EXTRA_DBITS[code] as usize;
        if extra != 0 {
            dist -= self::trees_tbl::BASE_DIST[code] as usize;
            match_bits |= dist << match_bits_len;
            match_bits_len += extra;
        }

        self.send_bits(match_bits as u64, match_bits_len as u8);

        match_bits_len
    }

    fn compress_block_help(&mut self, sym_buf: &[u8], ltree: &[Value], dtree: &[Value]) {
        for chunk in sym_buf.chunks_exact(3) {
            let [dist_low, dist_high, lc] = *chunk else {
                unreachable!("out of bound access on the symbol buffer");
            };

            match u16::from_be_bytes([dist_high, dist_low]) as usize {
                0 => self.emit_lit(ltree, lc) as usize,
                dist => self.emit_dist(ltree, dtree, lc, dist),
            };
        }

        self.emit_end_block(ltree, false)
    }

    fn send_tree(&mut self, tree: &[Value], bl_tree: &[Value], max_code: usize) {
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

        for n in 0..=max_code {
            curlen = nextlen;
            nextlen = tree[n + 1].len();
            count += 1;
            if count < max_count && curlen == nextlen {
                continue;
            } else if count < min_count {
                loop {
                    self.send_code(curlen as usize, bl_tree);

                    count -= 1;
                    if count == 0 {
                        break;
                    }
                }
            } else if curlen != 0 {
                if curlen as isize != prevlen {
                    self.send_code(curlen as usize, bl_tree);
                    count -= 1;
                }
                assert!((3..=6).contains(&count), " 3_6?");
                self.send_code(REP_3_6, bl_tree);
                self.send_bits(count - 3, 2);
            } else if count <= 10 {
                self.send_code(REPZ_3_10, bl_tree);
                self.send_bits(count - 3, 3);
            } else {
                self.send_code(REPZ_11_138, bl_tree);
                self.send_bits(count - 11, 7);
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
    }
}

#[allow(unused)]
#[repr(C)]
pub(crate) struct State<'a> {
    status: Status,

    last_flush: i32, /* value of flush param for previous deflate call */

    bit_writer: BitWriter<'a>,

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

    pub(crate) match_length: usize,   /* length of best match */
    pub(crate) prev_match: u16,       /* previous match */
    pub(crate) match_available: bool, /* set if previous match exists */
    pub(crate) strstart: usize,       /* start of string to insert */
    pub(crate) match_start: usize,    /* start of matching string */

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

    pub(crate) sym_buf: ReadBuf<'a>,

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

    pub(crate) prev: &'a mut [u16],
    pub(crate) head: &'a mut [u16; HASH_SIZE],

    ///  hash index of string to be inserted
    pub(crate) ins_h: usize,

    heap: Heap,

    pub(crate) hash_calc_variant: HashCalcVariant,

    crc_fold: crate::crc32::Crc32Fold,
    gzhead: Option<&'a mut gz_header>,
    gzindex: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
#[cfg_attr(feature = "__internal-fuzz", derive(arbitrary::Arbitrary))]
pub enum Strategy {
    #[default]
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

#[derive(Debug, PartialEq, Eq)]
enum DataType {
    Binary = 0,
    Text = 1,
    Unknown = 2,
}

impl<'a> State<'a> {
    pub const BIT_BUF_SIZE: u8 = BitWriter::BIT_BUF_SIZE;

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

    pub(crate) fn update_hash(&self, h: u32, val: u32) -> u32 {
        match self.hash_calc_variant {
            HashCalcVariant::Standard => StandardHashCalc::update_hash(h, val),
            HashCalcVariant::Crc32 => Crc32HashCalc::update_hash(h, val),
            HashCalcVariant::Roll => RollHashCalc::update_hash(h, val),
        }
    }

    pub(crate) fn quick_insert_string(&mut self, string: usize) -> u16 {
        match self.hash_calc_variant {
            HashCalcVariant::Standard => StandardHashCalc::quick_insert_string(self, string),
            HashCalcVariant::Crc32 => Crc32HashCalc::quick_insert_string(self, string),
            HashCalcVariant::Roll => RollHashCalc::quick_insert_string(self, string),
        }
    }

    pub(crate) fn insert_string(&mut self, string: usize, count: usize) {
        match self.hash_calc_variant {
            HashCalcVariant::Standard => StandardHashCalc::insert_string(self, string, count),
            HashCalcVariant::Crc32 => Crc32HashCalc::insert_string(self, string, count),
            HashCalcVariant::Roll => RollHashCalc::insert_string(self, string, count),
        }
    }

    pub(crate) fn tally_lit(&mut self, unmatched: u8) -> bool {
        self.sym_buf.push(0);
        self.sym_buf.push(0);
        self.sym_buf.push(unmatched);

        *self.l_desc.dyn_tree[unmatched as usize].freq_mut() += 1;

        assert!(
            unmatched as usize <= STD_MAX_MATCH - STD_MIN_MATCH,
            "zng_tr_tally: bad literal"
        );

        // signal that the current block should be flushed
        self.sym_buf.len() == self.sym_buf.capacity() - 3
    }

    const fn d_code(dist: usize) -> u8 {
        let index = if dist < 256 { dist } else { 256 + (dist >> 7) };
        self::trees_tbl::DIST_CODE[index]
    }

    pub(crate) fn tally_dist(&mut self, mut dist: usize, len: usize) -> bool {
        let symbols = [dist as u8, (dist >> 8) as u8, len as u8];
        self.sym_buf.extend(&symbols);

        self.matches += 1;
        dist -= 1;

        assert!(
            dist < self.max_dist() && Self::d_code(dist) < D_CODES as u8,
            "tally_dist: bad match"
        );

        let index = self::trees_tbl::LENGTH_CODE[len] as usize + LITERALS + 1;
        *self.l_desc.dyn_tree[index].freq_mut() += 1;

        *self.d_desc.dyn_tree[Self::d_code(dist) as usize].freq_mut() += 1;

        // signal that the current block should be flushed
        self.sym_buf.len() == self.sym_buf.capacity() - 3
    }

    fn detect_data_type(dyn_tree: &[Value]) -> DataType {
        // set bits 0..6, 14..25, and 28..31
        // 0xf3ffc07f = binary 11110011111111111100000001111111
        const NON_TEXT: u64 = 0xf3ffc07f;
        let mut mask = NON_TEXT;

        /* Check for non-textual bytes. */
        for value in &dyn_tree[0..32] {
            if (mask & 1) != 0 && value.freq() != 0 {
                return DataType::Binary;
            }

            mask >>= 1;
        }

        /* Check for textual bytes. */
        if dyn_tree[9].freq() != 0 || dyn_tree[10].freq() != 0 || dyn_tree[13].freq() != 0 {
            return DataType::Text;
        }

        if dyn_tree[32..LITERALS].iter().any(|v| v.freq() != 0) {
            return DataType::Text;
        }

        // there are no explicit text or non-text bytes. The stream is either empty or has only
        // tolerated bytes
        DataType::Binary
    }

    fn compress_block_static_trees(&mut self) {
        self.bit_writer.compress_block_help(
            self.sym_buf.filled(),
            self::trees_tbl::STATIC_LTREE.as_slice(),
            self::trees_tbl::STATIC_DTREE.as_slice(),
        )
    }

    fn compress_block_dynamic_trees(&mut self) {
        self.bit_writer.compress_block_help(
            self.sym_buf.filled(),
            &self.l_desc.dyn_tree,
            &self.d_desc.dyn_tree,
        );
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

        self.bit_writer.bit_buffer = 0;
        self.bit_writer.bits_used = 0;

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
        self.sym_buf.clear();
        self.matches = 0;
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Status {
    Init = 1,

    GZip = 4,
    Extra = 5,
    Name = 6,
    Comment = 7,
    Hcrc = 8,

    Busy = 2,
    Finish = 3,
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

    if stream.state.wrap == 2 {
        // we likely cannot fuse the crc32 and the copy here because the input can be changed by
        // a concurrent thread. Therefore it cannot be converted into a slice!
        let window = &mut stream.state.window;
        window.initialize_at_least(offset + len);
        unsafe { window.copy_and_initialize(offset..offset + len, stream.next_in) };

        let data = &stream.state.window.filled()[offset..][..len];
        stream.state.crc_fold.fold(data, CRC32_INITIAL_VALUE);
    } else if stream.state.wrap == 1 {
        // we likely cannot fuse the adler32 and the copy here because the input can be changed by
        // a concurrent thread. Therefore it cannot be converted into a slice!
        let window = &mut stream.state.window;
        window.initialize_at_least(offset + len);
        unsafe { window.copy_and_initialize(offset..offset + len, stream.next_in) };

        let data = &stream.state.window.filled()[offset..][..len];
        stream.adler = adler32(stream.adler as u32, data) as _;
    } else {
        let window = &mut stream.state.window;
        window.initialize_at_least(offset + len);
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
    window_range: core::ops::Range<usize>,
    is_last: bool,
) {
    // send block type
    state.bit_writer.emit_tree(BlockType::StoredBlock, is_last);

    // align on byte boundary
    state.bit_writer.flush_and_align_bits();

    // cmpr_bits_align(s);

    let input_block: &[u8] = &state.window.filled()[window_range];
    let stored_len = input_block.len() as u16;

    state.bit_writer.pending.extend(&stored_len.to_le_bytes());
    state
        .bit_writer
        .pending
        .extend(&(!stored_len).to_le_bytes());

    // cmpr_bits_add(s, 32);
    // sent_bits_add(s, 32);
    if stored_len > 0 {
        state.bit_writer.pending.extend(input_block);
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
            // in some cases zlib-ng copies uninitialized bytes here. We cannot have that, so
            // explicitly initialize them with zeros.
            //
            // see also the "fill_window_out_of_bounds" test.
            state.window.initialize_at_least(2 * wsize);
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
                state.ins_h = state.update_hash(v0, v1) as usize;
            } else if string >= 1 {
                state.quick_insert_string(string + 2 - STD_MIN_MATCH);
            }
            let mut count = state.insert;
            if state.lookahead == 1 {
                count -= 1;
            }
            if count > 0 {
                state.insert_string(string, count);
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

#[derive(Clone)]
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

/// Checks that symbol is a printing character (excluding space)
#[allow(unused)]
fn isgraph(c: u8) -> bool {
    (c > 0x20) && (c <= 0x7E)
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
                if isgraph(n as u8) {
                    char::from_u32(n as u32).unwrap()
                } else {
                    ' '
                },
                len,
                tree[n].code(),
                next_code[len as usize] - 1
            );
        }
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
    state.bit_writer.send_bits(lcodes as u64 - 257, 5); /* not +255 as stated in appnote.txt */
    state.bit_writer.send_bits(dcodes as u64 - 1, 5);
    state.bit_writer.send_bits(blcodes as u64 - 4, 4); /* not -3 as stated in appnote.txt */

    for rank in 0..blcodes {
        trace!("\nbl code {:>2} ", StaticTreeDesc::BL_ORDER[rank]);
        state.bit_writer.send_bits(
            state.bl_desc.dyn_tree[StaticTreeDesc::BL_ORDER[rank] as usize].len() as u64,
            3,
        );
    }
    // trace!("\nbl tree: sent {}", state.bits_sent);

    // literal tree
    state
        .bit_writer
        .send_tree(&state.l_desc.dyn_tree, &state.bl_desc.dyn_tree, lcodes - 1);
    // trace!("\nlit tree: sent {}", state.bits_sent);

    // distance tree
    state
        .bit_writer
        .send_tree(&state.d_desc.dyn_tree, &state.bl_desc.dyn_tree, dcodes - 1);
    // trace!("\ndist tree: sent {}", state.bits_sent);
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
        core::mem::swap(&mut tmp, &mut state.bl_desc);
        build_tree(state, &mut tmp);
        core::mem::swap(&mut tmp, &mut state.bl_desc);
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

    if state.sym_buf.is_empty() {
        opt_lenb = 0;
        static_lenb = 0;
        state.static_len = 7;
    } else if state.level > 0 {
        if stream.data_type == DataType::Unknown as i32 {
            stream.data_type = State::detect_data_type(&state.l_desc.dyn_tree) as i32;
        }

        {
            let mut tmp = TreeDesc::EMPTY;
            core::mem::swap(&mut tmp, &mut state.l_desc);

            build_tree(state, &mut tmp);
            core::mem::swap(&mut tmp, &mut state.l_desc);

            trace!(
                "\nlit data: dyn {}, stat {}",
                state.opt_len,
                state.static_len
            );
        }

        {
            let mut tmp = TreeDesc::EMPTY;
            core::mem::swap(&mut tmp, &mut state.d_desc);
            build_tree(state, &mut tmp);
            core::mem::swap(&mut tmp, &mut state.d_desc);

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
        state.bit_writer.emit_tree(BlockType::StaticTrees, last);
        state.compress_block_static_trees();
    // cmpr_bits_add(s, s.static_len);
    } else {
        state.bit_writer.emit_tree(BlockType::DynamicTrees, last);
        send_all_trees(
            state,
            state.l_desc.max_code + 1,
            state.d_desc.max_code + 1,
            max_blindex + 1,
        );

        state.compress_block_dynamic_trees();
    }

    // TODO
    // This check is made mod 2^32, for files larger than 512 MB and unsigned long implemented on 32 bits.
    // assert_eq!(state.compressed_len, state.bits_sent, "bad compressed size");

    state.init_block();
    if last {
        state.bit_writer.flush_and_align_bits();
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

#[must_use]
fn flush_bytes(stream: &mut DeflateStream, mut bytes: &[u8]) -> ControlFlow<ReturnCode> {
    let mut state = &mut stream.state;

    // we'll be using the pending buffer as temporary storage
    let mut beg = state.bit_writer.pending.pending().len(); /* start of bytes to update crc */

    while state.bit_writer.pending.remaining() < bytes.len() {
        let copy = state.bit_writer.pending.remaining();

        state.bit_writer.pending.extend(&bytes[..copy]);

        stream.adler = crc32(
            stream.adler as u32,
            &state.bit_writer.pending.pending()[beg..],
        ) as z_checksum;

        state.gzindex += copy;
        flush_pending(stream);
        state = &mut stream.state;

        // could not flush all the pending output
        if !state.bit_writer.pending.pending().is_empty() {
            state.last_flush = -1;
            return ControlFlow::Break(ReturnCode::Ok);
        }

        beg = 0;
        bytes = &bytes[copy..];
    }

    state.bit_writer.pending.extend(bytes);

    stream.adler = crc32(
        stream.adler as u32,
        &state.bit_writer.pending.pending()[beg..],
    ) as z_checksum;
    state.gzindex = 0;

    ControlFlow::Continue(())
}

pub fn deflate(stream: &mut DeflateStream, flush: DeflateFlush) -> ReturnCode {
    if stream.next_out.is_null()
        || (stream.avail_in != 0 && stream.next_in.is_null())
        || (stream.state.status == Status::Finish && flush != DeflateFlush::Finish)
    {
        let err = ReturnCode::StreamError;
        stream.msg = err.error_message();
        return err;
    }

    if stream.avail_out == 0 {
        let err = ReturnCode::BufError;
        stream.msg = err.error_message();
        return err;
    }

    let old_flush = stream.state.last_flush;
    stream.state.last_flush = flush as i32;

    /* Flush as much pending output as possible */
    if !stream.state.bit_writer.pending.pending().is_empty() {
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
        && flush != DeflateFlush::Finish
    {
        let err = ReturnCode::BufError;
        stream.msg = err.error_message();
        return err;
    }

    /* User must not provide more input after the first FINISH: */
    if stream.state.status == Status::Finish && stream.avail_in != 0 {
        let err = ReturnCode::BufError;
        stream.msg = err.error_message();
        return err;
    }

    /* Write the header */
    if stream.state.status == Status::Init && stream.state.wrap == 0 {
        stream.state.status = Status::Busy;
    }

    if stream.state.status == Status::Init {
        let header = stream.state.header();
        stream
            .state
            .bit_writer
            .pending
            .extend(&header.to_be_bytes());

        /* Save the adler32 of the preset dictionary: */
        if stream.state.strstart != 0 {
            let adler = stream.adler as u32;
            stream.state.bit_writer.pending.extend(&adler.to_be_bytes());
        }

        stream.adler = ADLER32_INITIAL_VALUE as _;
        stream.state.status = Status::Busy;

        // compression must start with an empty pending buffer
        flush_pending(stream);

        if !stream.state.bit_writer.pending.pending().is_empty() {
            stream.state.last_flush = -1;

            return ReturnCode::Ok;
        }
    }

    if stream.state.status == Status::GZip {
        /* gzip header */
        stream.state.crc_fold = Crc32Fold::new();

        stream.state.bit_writer.pending.extend(&[31, 139, 8]);

        let extra_flags = if stream.state.level == 9 {
            2
        } else if stream.state.strategy >= Strategy::HuffmanOnly || stream.state.level < 2 {
            4
        } else {
            0
        };

        match &stream.state.gzhead {
            None => {
                let bytes = [0, 0, 0, 0, 0, extra_flags, gz_header::OS_CODE];
                stream.state.bit_writer.pending.extend(&bytes);
                stream.state.status = Status::Busy;

                /* Compression must start with an empty pending buffer */
                flush_pending(stream);
                if !stream.state.bit_writer.pending.pending().is_empty() {
                    stream.state.last_flush = -1;
                    return ReturnCode::Ok;
                }
            }
            Some(gzhead) => {
                stream.state.bit_writer.pending.extend(&[gzhead.flags()]);
                let bytes = (gzhead.time as u32).to_le_bytes();
                stream.state.bit_writer.pending.extend(&bytes);
                stream
                    .state
                    .bit_writer
                    .pending
                    .extend(&[extra_flags, gzhead.os as u8]);

                if !gzhead.extra.is_null() {
                    let bytes = (gzhead.extra_len as u16).to_le_bytes();
                    stream.state.bit_writer.pending.extend(&bytes);
                }

                if gzhead.hcrc > 0 {
                    stream.adler = crc32(
                        stream.adler as u32,
                        stream.state.bit_writer.pending.pending(),
                    ) as z_checksum
                }

                stream.state.gzindex = 0;
                stream.state.status = Status::Extra;
            }
        }
    }

    if stream.state.status == Status::Extra {
        if let Some(gzhead) = stream.state.gzhead.as_ref() {
            if !gzhead.extra.is_null() {
                let gzhead_extra = gzhead.extra;

                let extra = unsafe {
                    core::slice::from_raw_parts(
                        gzhead_extra.add(stream.state.gzindex),
                        (gzhead.extra_len & 0xffff) as usize - stream.state.gzindex,
                    )
                };

                if let ControlFlow::Break(err) = flush_bytes(stream, extra) {
                    return err;
                }
            }
        }
        stream.state.status = Status::Name;
    }

    if stream.state.status == Status::Name {
        if let Some(gzhead) = stream.state.gzhead.as_ref() {
            if !gzhead.name.is_null() {
                let gzhead_name = unsafe { CStr::from_ptr(gzhead.name.cast()) };
                let bytes = gzhead_name.to_bytes_with_nul();
                if let ControlFlow::Break(err) = flush_bytes(stream, bytes) {
                    return err;
                }
            }
            stream.state.status = Status::Comment;
        }
    }

    if stream.state.status == Status::Comment {
        if let Some(gzhead) = stream.state.gzhead.as_ref() {
            if !gzhead.comment.is_null() {
                let gzhead_comment = unsafe { CStr::from_ptr(gzhead.comment.cast()) };
                let bytes = gzhead_comment.to_bytes_with_nul();
                if let ControlFlow::Break(err) = flush_bytes(stream, bytes) {
                    return err;
                }
            }
            stream.state.status = Status::Hcrc;
        }
    }

    if stream.state.status == Status::Hcrc {
        if let Some(gzhead) = stream.state.gzhead.as_ref() {
            if gzhead.hcrc != 0 {
                let bytes = (stream.adler as u16).to_le_bytes();
                if let ControlFlow::Break(err) = flush_bytes(stream, &bytes) {
                    return err;
                }
            }
        }

        stream.state.status = Status::Busy;

        // compression must start with an empty pending buffer
        flush_pending(stream);
        if !stream.state.bit_writer.pending.pending().is_empty() {
            stream.state.last_flush = -1;
            return ReturnCode::Ok;
        }
    }

    // Start a new block or continue the current one.
    let state = &mut stream.state;
    if stream.avail_in != 0
        || state.lookahead != 0
        || (flush != DeflateFlush::NoFlush && state.status != Status::Finish)
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
                match flush {
                    DeflateFlush::NoFlush => unreachable!("condition of inner surrounding if"),
                    DeflateFlush::PartialFlush => {
                        state.bit_writer.align();
                    }
                    DeflateFlush::SyncFlush => {
                        // add an empty stored block that is marked as not final. This is useful for
                        // parallel deflate where we want to make sure the intermediate blocks are not
                        // marked as "last block".
                        zng_tr_stored_block(state, 0..0, false);
                    }
                    DeflateFlush::FullFlush => {
                        // add an empty stored block that is marked as not final. This is useful for
                        // parallel deflate where we want to make sure the intermediate blocks are not
                        // marked as "last block".
                        zng_tr_stored_block(state, 0..0, false);

                        state.head.fill(0); // forget history

                        if state.lookahead == 0 {
                            state.strstart = 0;
                            state.block_start = 0;
                            state.insert = 0;
                        }
                    }
                    DeflateFlush::Block => { /* fall through */ }
                    DeflateFlush::Finish => unreachable!("condition of outer surrounding if"),
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

    if flush != DeflateFlush::Finish {
        return ReturnCode::Ok;
    }

    // write the trailer
    if stream.state.wrap == 2 {
        let crc_fold = core::mem::take(&mut stream.state.crc_fold);
        stream.adler = crc_fold.finish() as z_checksum;

        let adler = stream.adler as u32;
        stream.state.bit_writer.pending.extend(&adler.to_le_bytes());

        let total_in = stream.total_in as u32;
        stream
            .state
            .bit_writer
            .pending
            .extend(&total_in.to_le_bytes());
    } else if stream.state.wrap == 1 {
        let adler = stream.adler as u32;
        stream.state.bit_writer.pending.extend(&adler.to_be_bytes());
    }

    flush_pending(stream);

    // If avail_out is zero, the application will call deflate again to flush the rest.
    if stream.state.wrap > 0 {
        stream.state.wrap = -stream.state.wrap; /* write the trailer only once! */
    }

    if stream.state.bit_writer.pending.pending().is_empty() {
        assert_eq!(stream.state.bit_writer.bits_used, 0, "bi_buf not flushed");
        return ReturnCode::StreamEnd;
    }
    ReturnCode::Ok
}

pub(crate) fn flush_pending(stream: &mut DeflateStream) {
    let state = &mut stream.state;

    state.bit_writer.flush_bits();

    let pending = state.bit_writer.pending.pending();
    let len = Ord::min(pending.len(), stream.avail_out as usize);

    if len == 0 {
        return;
    }

    trace!("\n[FLUSH {len} bytes]");
    unsafe { core::ptr::copy_nonoverlapping(pending.as_ptr(), stream.next_out, len) };

    stream.next_out = stream.next_out.wrapping_add(len);
    stream.total_out += len as crate::c_api::z_size;
    stream.avail_out -= len as crate::c_api::uInt;

    state.bit_writer.pending.advance(len);
}

pub fn compress_slice<'a>(
    output: &'a mut [u8],
    input: &[u8],
    config: DeflateConfig,
) -> (&'a mut [u8], ReturnCode) {
    let output_uninit = unsafe {
        core::slice::from_raw_parts_mut(output.as_mut_ptr() as *mut MaybeUninit<u8>, output.len())
    };

    compress(output_uninit, input, config)
}

pub fn compress<'a>(
    output: &'a mut [MaybeUninit<u8>],
    input: &[u8],
    config: DeflateConfig,
) -> (&'a mut [u8], ReturnCode) {
    compress_with_flush(output, input, config, DeflateFlush::Finish)
}

pub fn compress_slice_with_flush<'a>(
    output: &'a mut [u8],
    input: &[u8],
    config: DeflateConfig,
    flush: DeflateFlush,
) -> (&'a mut [u8], ReturnCode) {
    let output_uninit = unsafe {
        core::slice::from_raw_parts_mut(output.as_mut_ptr() as *mut MaybeUninit<u8>, output.len())
    };

    compress_with_flush(output_uninit, input, config, flush)
}

pub fn compress_with_flush<'a>(
    output: &'a mut [MaybeUninit<u8>],
    input: &[u8],
    config: DeflateConfig,
    final_flush: DeflateFlush,
) -> (&'a mut [u8], ReturnCode) {
    let mut stream = z_stream {
        next_in: input.as_ptr() as *mut u8,
        avail_in: 0, // for special logic in the first  iteration
        total_in: 0,
        next_out: output.as_mut_ptr() as *mut u8,
        avail_out: 0, // for special logic on the first iteration
        total_out: 0,
        msg: core::ptr::null_mut(),
        state: core::ptr::null_mut(),
        zalloc: None,
        zfree: None,
        opaque: core::ptr::null_mut(),
        data_type: 0,
        adler: 0,
        reserved: 0,
    };

    let err = init(&mut stream, config);
    if err != ReturnCode::Ok {
        return (&mut [], err);
    }

    let max = core::ffi::c_uint::MAX as usize;

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

        let err = if let Some(stream) = unsafe { DeflateStream::from_stream_mut(&mut stream) } {
            deflate(stream, flush)
        } else {
            ReturnCode::StreamError
        };

        if err != ReturnCode::Ok {
            break;
        }
    }

    // SAFETY: we have now initialized these bytes
    let output_slice = unsafe {
        core::slice::from_raw_parts_mut(output.as_mut_ptr() as *mut u8, stream.total_out as usize)
    };

    // may DataError if insufficient output space
    let return_code = if let Some(stream) = unsafe { DeflateStream::from_stream_mut(&mut stream) } {
        match end(stream) {
            Ok(_) => ReturnCode::Ok,
            Err(_) => ReturnCode::DataError,
        }
    } else {
        ReturnCode::Ok
    };

    (output_slice, return_code)
}

pub const fn compress_bound(source_len: usize) -> usize {
    compress_bound_help(source_len, ZLIB_WRAPLEN)
}

const fn compress_bound_help(source_len: usize, wrap_len: usize) -> usize {
    source_len // The source size itself */
        // Always at least one byte for any input
        .wrapping_add(if source_len == 0 { 1 } else { 0 })
        // One extra byte for lengths less than 9
        .wrapping_add(if source_len < 9 { 1 } else { 0 })
        // Source encoding overhead, padded to next full byte
        .wrapping_add(deflate_quick_overhead(source_len))
        // Deflate block overhead bytes
        .wrapping_add(DEFLATE_BLOCK_OVERHEAD)
        // none, zlib or gzip wrapper
        .wrapping_add(wrap_len)
}

///  heap used to build the Huffman trees

/// The sons of heap[n] are heap[2*n] and heap[2*n+1]. heap[0] is not used.
/// The same heap array is used to build all trees.
#[derive(Clone)]
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
            core::cmp::Ordering::Less => true,
            core::cmp::Ordering::Equal => depth[n] <= depth[m],
            core::cmp::Ordering::Greater => false,
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

pub fn set_header<'a>(
    stream: &mut DeflateStream<'a>,
    head: Option<&'a mut gz_header>,
) -> ReturnCode {
    if stream.state.wrap != 2 {
        ReturnCode::StreamError as _
    } else {
        stream.state.gzhead = head;
        ReturnCode::Ok as _
    }
}

// zlib format overhead
const ZLIB_WRAPLEN: usize = 6;
// gzip format overhead
const GZIP_WRAPLEN: usize = 18;

const DEFLATE_HEADER_BITS: usize = 3;
const DEFLATE_EOBS_BITS: usize = 15;
const DEFLATE_PAD_BITS: usize = 6;
const DEFLATE_BLOCK_OVERHEAD: usize =
    (DEFLATE_HEADER_BITS + DEFLATE_EOBS_BITS + DEFLATE_PAD_BITS) >> 3;

const DEFLATE_QUICK_LIT_MAX_BITS: usize = 9;
const fn deflate_quick_overhead(x: usize) -> usize {
    let sum = x
        .wrapping_mul(DEFLATE_QUICK_LIT_MAX_BITS - 8)
        .wrapping_add(7);

    // imitate zlib-ng rounding behavior (on windows, c_ulong is 32 bits)
    (sum as core::ffi::c_ulong >> 3) as usize
}

/// For the default windowBits of 15 and memLevel of 8, this function returns
/// a close to exact, as well as small, upper bound on the compressed size.
/// They are coded as constants here for a reason--if the #define's are
/// changed, then this function needs to be changed as well.  The return
/// value for 15 and 8 only works for those exact settings.
///
/// For any setting other than those defaults for windowBits and memLevel,
/// the value returned is a conservative worst case for the maximum expansion
/// resulting from using fixed blocks instead of stored blocks, which deflate
/// can emit on compressed data for some combinations of the parameters.
///
/// This function could be more sophisticated to provide closer upper bounds for
/// every combination of windowBits and memLevel.  But even the conservative
/// upper bound of about 14% expansion does not seem onerous for output buffer
/// allocation.
pub fn bound(stream: Option<&mut DeflateStream>, source_len: usize) -> usize {
    // on windows, c_ulong is only a 32-bit integer
    let mask = core::ffi::c_ulong::MAX as usize;

    // conservative upper bound for compressed data
    let comp_len = source_len
        .wrapping_add((source_len.wrapping_add(7) & mask) >> 3)
        .wrapping_add((source_len.wrapping_add(63) & mask) >> 6)
        .wrapping_add(5);

    let Some(stream) = stream else {
        // return conservative bound plus zlib wrapper
        return comp_len.wrapping_add(6);
    };

    /* compute wrapper length */
    let wrap_len = match stream.state.wrap {
        0 => {
            // raw deflate
            0
        }
        1 => {
            // zlib wrapper
            if stream.state.strstart != 0 {
                ZLIB_WRAPLEN + 4
            } else {
                ZLIB_WRAPLEN
            }
        }
        2 => {
            // gzip wrapper
            let mut gz_wrap_len = GZIP_WRAPLEN;

            if let Some(header) = &stream.state.gzhead {
                if !header.extra.is_null() {
                    gz_wrap_len += 2 + header.extra_len as usize;
                }

                let mut c_string = header.name;
                if !c_string.is_null() {
                    loop {
                        gz_wrap_len += 1;
                        unsafe {
                            if *c_string == 0 {
                                break;
                            }
                            c_string = c_string.add(1);
                        }
                    }
                }

                let mut c_string = header.comment;
                if !c_string.is_null() {
                    loop {
                        gz_wrap_len += 1;
                        unsafe {
                            if *c_string == 0 {
                                break;
                            }
                            c_string = c_string.add(1);
                        }
                    }
                }

                if header.hcrc != 0 {
                    gz_wrap_len += 2;
                }
            }

            gz_wrap_len
        }
        _ => {
            // default
            ZLIB_WRAPLEN
        }
    };

    if stream.state.w_bits != MAX_WBITS as usize || HASH_BITS < 15 {
        if stream.state.level == 0 {
            /* upper bound for stored blocks with length 127 (memLevel == 1) ~4% overhead plus a small constant */
            source_len
                .wrapping_add(source_len >> 5)
                .wrapping_add(source_len >> 7)
                .wrapping_add(source_len >> 11)
                .wrapping_add(7)
                .wrapping_add(wrap_len)
        } else {
            comp_len.wrapping_add(wrap_len)
        }
    } else {
        compress_bound_help(source_len, wrap_len)
    }
}

#[cfg(test)]
mod test {
    use crate::{
        inflate::{uncompress_slice, InflateConfig, InflateStream},
        InflateFlush,
    };

    use super::*;

    use core::{ffi::CStr, sync::atomic::AtomicUsize};

    #[test]
    fn detect_data_type_basic() {
        let empty = || [Value::new(0, 0); LITERALS];

        assert_eq!(State::detect_data_type(&empty()), DataType::Binary);

        let mut binary = empty();
        binary[0] = Value::new(1, 0);
        assert_eq!(State::detect_data_type(&binary), DataType::Binary);

        let mut text = empty();
        text[b'\r' as usize] = Value::new(1, 0);
        assert_eq!(State::detect_data_type(&text), DataType::Text);

        let mut text = empty();
        text[b'a' as usize] = Value::new(1, 0);
        assert_eq!(State::detect_data_type(&text), DataType::Text);

        let mut non_text = empty();
        non_text[7] = Value::new(1, 0);
        assert_eq!(State::detect_data_type(&non_text), DataType::Binary);
    }

    #[test]
    fn from_stream_mut() {
        unsafe {
            assert!(DeflateStream::from_stream_mut(core::ptr::null_mut()).is_none());

            let mut stream = z_stream::default();
            assert!(DeflateStream::from_stream_mut(&mut stream).is_none());

            // state is still NULL
            assert!(DeflateStream::from_stream_mut(&mut stream).is_none());

            init(&mut stream, DeflateConfig::default());
            let stream = DeflateStream::from_stream_mut(&mut stream);
            assert!(stream.is_some());

            assert!(end(stream.unwrap()).is_ok());
        }
    }

    unsafe extern "C" fn fail_nth_allocation<const N: usize>(
        opaque: crate::c_api::voidpf,
        items: crate::c_api::uInt,
        size: crate::c_api::uInt,
    ) -> crate::c_api::voidpf {
        let count = unsafe { &*(opaque as *const AtomicUsize) };

        if count.fetch_add(1, core::sync::atomic::Ordering::Relaxed) != N {
            // must use the C allocator internally because (de)allocation is based on function
            // pointer values and because we don't use the rust allocator directly, the allocation
            // logic will store the pointer to the start at the start of the allocation.
            (crate::allocate::Allocator::C.zalloc)(opaque, items, size)
        } else {
            core::ptr::null_mut()
        }
    }

    #[test]
    fn init_invalid_allocator() {
        {
            let atomic = AtomicUsize::new(0);
            let mut stream = z_stream {
                zalloc: Some(fail_nth_allocation::<0>),
                zfree: Some(crate::allocate::Allocator::C.zfree),
                opaque: &atomic as *const _ as *const core::ffi::c_void as *mut _,
                ..z_stream::default()
            };
            assert_eq!(
                init(&mut stream, DeflateConfig::default()),
                ReturnCode::MemError
            );
        }

        {
            let atomic = AtomicUsize::new(0);
            let mut stream = z_stream {
                zalloc: Some(fail_nth_allocation::<3>),
                zfree: Some(crate::allocate::Allocator::C.zfree),
                opaque: &atomic as *const _ as *const core::ffi::c_void as *mut _,
                ..z_stream::default()
            };
            assert_eq!(
                init(&mut stream, DeflateConfig::default()),
                ReturnCode::MemError
            );
        }

        {
            let atomic = AtomicUsize::new(0);
            let mut stream = z_stream {
                zalloc: Some(fail_nth_allocation::<5>),
                zfree: Some(crate::allocate::Allocator::C.zfree),
                opaque: &atomic as *const _ as *const core::ffi::c_void as *mut _,
                ..z_stream::default()
            };
            assert_eq!(
                init(&mut stream, DeflateConfig::default()),
                ReturnCode::MemError
            );
        }
    }

    mod copy_invalid_allocator {
        use super::*;

        #[test]
        fn fail_0() {
            let mut stream = z_stream::default();

            let atomic = AtomicUsize::new(0);
            stream.opaque = &atomic as *const _ as *const core::ffi::c_void as *mut _;
            stream.zalloc = Some(fail_nth_allocation::<6>);
            stream.zfree = Some(crate::allocate::Allocator::C.zfree);

            // init performs 6 allocations; we don't want those to fail
            assert_eq!(init(&mut stream, DeflateConfig::default()), ReturnCode::Ok);

            let Some(stream) = (unsafe { DeflateStream::from_stream_mut(&mut stream) }) else {
                unreachable!()
            };

            let mut stream_copy = MaybeUninit::<DeflateStream>::zeroed();

            assert_eq!(copy(&mut stream_copy, stream), ReturnCode::MemError);

            assert!(end(stream).is_ok());
        }

        #[test]
        fn fail_3() {
            let mut stream = z_stream::default();

            let atomic = AtomicUsize::new(0);
            stream.zalloc = Some(fail_nth_allocation::<{ 6 + 3 }>);
            stream.zfree = Some(crate::allocate::Allocator::C.zfree);
            stream.opaque = &atomic as *const _ as *const core::ffi::c_void as *mut _;

            // init performs 6 allocations; we don't want those to fail
            assert_eq!(init(&mut stream, DeflateConfig::default()), ReturnCode::Ok);

            let Some(stream) = (unsafe { DeflateStream::from_stream_mut(&mut stream) }) else {
                unreachable!()
            };

            let mut stream_copy = MaybeUninit::<DeflateStream>::zeroed();

            assert_eq!(copy(&mut stream_copy, stream), ReturnCode::MemError);

            assert!(end(stream).is_ok());
        }

        #[test]
        fn fail_5() {
            let mut stream = z_stream::default();

            let atomic = AtomicUsize::new(0);
            stream.zalloc = Some(fail_nth_allocation::<{ 6 + 5 }>);
            stream.zfree = Some(crate::allocate::Allocator::C.zfree);
            stream.opaque = &atomic as *const _ as *const core::ffi::c_void as *mut _;

            // init performs 6 allocations; we don't want those to fail
            assert_eq!(init(&mut stream, DeflateConfig::default()), ReturnCode::Ok);

            let Some(stream) = (unsafe { DeflateStream::from_stream_mut(&mut stream) }) else {
                unreachable!()
            };

            let mut stream_copy = MaybeUninit::<DeflateStream>::zeroed();

            assert_eq!(copy(&mut stream_copy, stream), ReturnCode::MemError);

            assert!(end(stream).is_ok());
        }
    }

    mod invalid_deflate_config {
        use super::*;

        #[test]
        fn sanity_check() {
            let mut stream = z_stream::default();
            assert_eq!(init(&mut stream, DeflateConfig::default()), ReturnCode::Ok);

            assert!(stream.zalloc.is_some());
            assert!(stream.zfree.is_some());

            // this should be the default level
            let stream = unsafe { DeflateStream::from_stream_mut(&mut stream) }.unwrap();
            assert_eq!(stream.state.level, 6);

            assert!(end(stream).is_ok());
        }

        #[test]
        fn window_bits_correction() {
            // window_bits of 8 gets turned into 9 internally
            let mut stream = z_stream::default();
            let config = DeflateConfig {
                window_bits: 8,
                ..Default::default()
            };
            assert_eq!(init(&mut stream, config), ReturnCode::Ok);
            let stream = unsafe { DeflateStream::from_stream_mut(&mut stream) }.unwrap();
            assert_eq!(stream.state.w_bits, 9);

            assert!(end(stream).is_ok());
        }

        #[test]
        fn window_bits_too_low() {
            let mut stream = z_stream::default();
            let config = DeflateConfig {
                window_bits: -16,
                ..Default::default()
            };
            assert_eq!(init(&mut stream, config), ReturnCode::StreamError);
        }

        #[test]
        fn window_bits_too_high() {
            // window bits too high
            let mut stream = z_stream::default();
            let config = DeflateConfig {
                window_bits: 42,
                ..Default::default()
            };
            assert_eq!(init(&mut stream, config), ReturnCode::StreamError);
        }
    }

    #[test]
    fn end_data_error() {
        let mut stream = z_stream::default();
        assert_eq!(init(&mut stream, DeflateConfig::default()), ReturnCode::Ok);
        let stream = unsafe { DeflateStream::from_stream_mut(&mut stream) }.unwrap();

        // next deflate into too little space
        let input = b"Hello World\n";
        stream.next_in = input.as_ptr() as *mut u8;
        stream.avail_in = input.len() as _;
        let output = &mut [0, 0, 0];
        stream.next_out = output.as_mut_ptr();
        stream.avail_out = output.len() as _;

        // the deflate is fine
        assert_eq!(deflate(stream, DeflateFlush::NoFlush), ReturnCode::Ok);

        // but end is not
        assert!(end(stream).is_err());
    }

    #[test]
    fn test_reset_keep() {
        let mut stream = z_stream::default();
        assert_eq!(init(&mut stream, DeflateConfig::default()), ReturnCode::Ok);
        let stream = unsafe { DeflateStream::from_stream_mut(&mut stream) }.unwrap();

        // next deflate into too little space
        let input = b"Hello World\n";
        stream.next_in = input.as_ptr() as *mut u8;
        stream.avail_in = input.len() as _;

        let output = &mut [0; 1024];
        stream.next_out = output.as_mut_ptr();
        stream.avail_out = output.len() as _;
        assert_eq!(deflate(stream, DeflateFlush::Finish), ReturnCode::StreamEnd);

        assert_eq!(reset_keep(stream), ReturnCode::Ok);

        let output = &mut [0; 1024];
        stream.next_out = output.as_mut_ptr();
        stream.avail_out = output.len() as _;
        assert_eq!(deflate(stream, DeflateFlush::Finish), ReturnCode::StreamEnd);

        assert!(end(stream).is_ok());
    }

    #[test]
    fn hello_world_huffman_only() {
        const EXPECTED: &[u8] = &[
            0x78, 0x01, 0xf3, 0x48, 0xcd, 0xc9, 0xc9, 0x57, 0x08, 0xcf, 0x2f, 0xca, 0x49, 0x51,
            0xe4, 0x02, 0x00, 0x20, 0x91, 0x04, 0x48,
        ];

        let input = "Hello World!\n";

        let mut output = vec![0; 128];

        let config = DeflateConfig {
            level: 6,
            method: Method::Deflated,
            window_bits: crate::MAX_WBITS,
            mem_level: DEF_MEM_LEVEL,
            strategy: Strategy::HuffmanOnly,
        };

        let (output, err) = compress_slice(&mut output, input.as_bytes(), config);

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

        let config = DeflateConfig {
            level: 1,
            method: Method::Deflated,
            window_bits: crate::MAX_WBITS,
            mem_level: DEF_MEM_LEVEL,
            strategy: Strategy::Default,
        };

        let (output, err) = compress_slice(&mut output, input.as_bytes(), config);

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

        let config = DeflateConfig {
            level: 1,
            method: Method::Deflated,
            window_bits: crate::MAX_WBITS,
            mem_level: DEF_MEM_LEVEL,
            strategy: Strategy::Default,
        };

        let (output, err) = compress_slice(&mut output, input.as_bytes(), config);

        assert_eq!(err, ReturnCode::Ok);

        assert_eq!(output.len(), EXPECTED.len());

        assert_eq!(EXPECTED, output);
    }

    fn fuzz_based_test(input: &[u8], config: DeflateConfig, expected: &[u8]) {
        let mut output_rs = [0; 1 << 17];
        let (output_rs, err) = compress_slice(&mut output_rs, input, config);
        assert_eq!(err, ReturnCode::Ok);

        assert_eq!(output_rs, expected);
    }

    #[test]
    fn simple_rle() {
        fuzz_based_test(
            "\0\0\0\0\u{6}".as_bytes(),
            DeflateConfig {
                level: -1,
                method: Method::Deflated,
                window_bits: 11,
                mem_level: 4,
                strategy: Strategy::Rle,
            },
            &[56, 17, 99, 0, 2, 54, 0, 0, 11, 0, 7],
        )
    }

    #[test]
    fn fill_window_out_of_bounds() {
        const INPUT: &[u8] = &[
            0x71, 0x71, 0x71, 0x71, 0x71, 0x6a, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71,
            0x71, 0x71, 0x71, 0x71, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x1d, 0x1d, 0x1d, 0x1d, 0x63,
            0x63, 0x63, 0x63, 0x63, 0x1d, 0x1d, 0x1d, 0x1d, 0x1d, 0x1d, 0x1d, 0x1d, 0x1d, 0x1d,
            0x1d, 0x27, 0x0, 0x0, 0x0, 0x1d, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x71, 0x71, 0x71,
            0x71, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x31, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x1d, 0x1d, 0x0, 0x0, 0x0, 0x0, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50,
            0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x48, 0x50,
            0x50, 0x50, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x2c, 0x0, 0x0, 0x0, 0x0, 0x4a,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71,
            0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x70, 0x71, 0x71, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x71, 0x71, 0x71, 0x71, 0x6a, 0x0, 0x0, 0x0, 0x0,
            0x71, 0x0, 0x71, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x31, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71,
            0x71, 0x71, 0x0, 0x4a, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x71, 0x71,
            0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71,
            0x70, 0x71, 0x71, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x71, 0x71, 0x71, 0x71,
            0x6a, 0x0, 0x0, 0x0, 0x0, 0x71, 0x0, 0x71, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x31, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x1d, 0x1d, 0x0, 0x0, 0x0, 0x0,
            0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50,
            0x50, 0x50, 0x50, 0x50, 0x48, 0x50, 0x0, 0x0, 0x71, 0x71, 0x71, 0x71, 0x3b, 0x3f, 0x71,
            0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x50, 0x50, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x2c, 0x0, 0x0, 0x0, 0x0, 0x4a, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x71,
            0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71,
            0x71, 0x70, 0x71, 0x71, 0x0, 0x0, 0x0, 0x6, 0x0, 0x0, 0x0, 0x0, 0x0, 0x71, 0x71, 0x71,
            0x71, 0x71, 0x71, 0x70, 0x71, 0x71, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x71, 0x71, 0x71, 0x71, 0x3b, 0x3f, 0x71, 0x71, 0x71,
            0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71,
            0x71, 0x3b, 0x3f, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x20, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x71, 0x75, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71,
            0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x10, 0x0, 0x71, 0x71,
            0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x3b, 0x71, 0x71, 0x71, 0x71, 0x71,
            0x71, 0x76, 0x71, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x71, 0x71, 0x71, 0x71, 0x71,
            0x71, 0x71, 0x71, 0x10, 0x0, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71,
            0x71, 0x3b, 0x71, 0x71, 0x71, 0x71, 0x71, 0x71, 0x76, 0x71, 0x34, 0x34, 0x34, 0x34,
            0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34,
            0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x0, 0x0, 0x0, 0x0, 0x0, 0x34, 0x34, 0x34, 0x34,
            0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34,
            0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34,
            0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34,
            0x34, 0x34, 0x30, 0x34, 0x34, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x71, 0x0, 0x0, 0x0, 0x0, 0x6,
        ];

        fuzz_based_test(
            INPUT,
            DeflateConfig {
                level: -1,
                method: Method::Deflated,
                window_bits: 9,
                mem_level: 1,
                strategy: Strategy::HuffmanOnly,
            },
            &[
                0x18, 0x19, 0x4, 0xc1, 0x21, 0x1, 0xc4, 0x0, 0x10, 0x3, 0xb0, 0x18, 0x29, 0x1e,
                0x7e, 0x17, 0x83, 0xf5, 0x70, 0x6c, 0xac, 0xfe, 0xc9, 0x27, 0xdb, 0xb6, 0x6f, 0xdb,
                0xb6, 0x6d, 0xdb, 0x80, 0x24, 0xb9, 0xbb, 0xbb, 0x24, 0x49, 0x92, 0x24, 0xf, 0x2,
                0xd8, 0x36, 0x0, 0xf0, 0x3, 0x0, 0x0, 0x24, 0xd0, 0xb6, 0x6d, 0xdb, 0xb6, 0x6d,
                0xdb, 0xbe, 0x6d, 0xf9, 0x13, 0x4, 0xc7, 0x4, 0x0, 0x80, 0x30, 0x0, 0xc3, 0x22,
                0x68, 0xf, 0x36, 0x90, 0xc2, 0xb5, 0xfa, 0x7f, 0x48, 0x80, 0x81, 0xb, 0x40, 0x55,
                0x55, 0x55, 0xd5, 0x16, 0x80, 0xaa, 0x7, 0x9, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
                0xe, 0x7c, 0x82, 0xe0, 0x98, 0x0, 0x0, 0x0, 0x4, 0x60, 0x10, 0xf9, 0x8c, 0xe2,
                0xe5, 0xfa, 0x3f, 0x2, 0x54, 0x55, 0x55, 0x65, 0x0, 0xa8, 0xaa, 0xaa, 0xaa, 0xba,
                0x2, 0x50, 0xb5, 0x90, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x78, 0x82, 0xe0, 0xd0,
                0x8a, 0x41, 0x0, 0x0, 0xa2, 0x58, 0x54, 0xb7, 0x60, 0x83, 0x9a, 0x6a, 0x4, 0x96,
                0x87, 0xba, 0x51, 0xf8, 0xfb, 0x9b, 0x26, 0xfc, 0x0, 0x1c, 0x7, 0x6c, 0xdb, 0xb6,
                0x6d, 0xdb, 0xb6, 0x6d, 0xf7, 0xa8, 0x3a, 0xaf, 0xaa, 0x6a, 0x3, 0xf8, 0xc2, 0x3,
                0x40, 0x55, 0x55, 0x55, 0xd5, 0x5b, 0xf8, 0x80, 0xaa, 0x7a, 0xb, 0x0, 0x7f, 0x82,
                0xe0, 0x98, 0x0, 0x40, 0x18, 0x0, 0x82, 0xd8, 0x49, 0x40, 0x2, 0x22, 0x7e, 0xeb,
                0x80, 0xa6, 0xc, 0xa0, 0x9f, 0xa4, 0x2a, 0x38, 0xf, 0x0, 0x0, 0xe7, 0x1, 0xdc,
                0x55, 0x95, 0x17, 0x0, 0x0, 0xae, 0x0, 0x38, 0xc0, 0x67, 0xdb, 0x36, 0x80, 0x2b,
                0x0, 0xe, 0xf0, 0xd9, 0xf6, 0x13, 0x4, 0xc7, 0x4, 0x0, 0x0, 0x30, 0xc, 0x83, 0x22,
                0x69, 0x7, 0xc6, 0xea, 0xff, 0x19, 0x0, 0x0, 0x80, 0xaa, 0x0, 0x0, 0x0, 0x0, 0x0,
                0x0, 0x8e, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0x6a,
                0xf5, 0x63, 0x60, 0x60, 0x3, 0x0, 0xee, 0x8a, 0x88, 0x67,
            ],
        )
    }

    #[test]
    fn gzip_no_header() {
        let config = DeflateConfig {
            level: 9,
            method: Method::Deflated,
            window_bits: 31, // gzip
            ..Default::default()
        };

        let input = b"Hello World!";
        let os = gz_header::OS_CODE;

        fuzz_based_test(
            input,
            config,
            &[
                31, 139, 8, 0, 0, 0, 0, 0, 2, os, 243, 72, 205, 201, 201, 87, 8, 207, 47, 202, 73,
                81, 4, 0, 163, 28, 41, 28, 12, 0, 0, 0,
            ],
        )
    }

    #[test]
    #[rustfmt::skip]
    fn gzip_stored_block_checksum() {
        fuzz_based_test(
            &[
                27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 9, 0,
            ],
            DeflateConfig {
                level: 0,
                method: Method::Deflated,
                window_bits: 26,
                mem_level: 6,
                strategy: Strategy::Default,
            },
            &[
                31, 139, 8, 0, 0, 0, 0, 0, 4, gz_header::OS_CODE, 1, 18, 0, 237, 255, 27, 27, 27, 27, 27, 27, 27,
                27, 27, 27, 27, 27, 27, 27, 27, 27, 9, 0, 60, 101, 156, 55, 18, 0, 0, 0,
            ],
        )
    }

    #[test]
    fn gzip_header_pending_flush() {
        let extra = "aaaaaaaaaaaaaaaaaaaa\0";
        let name = "bbbbbbbbbbbbbbbbbbbb\0";
        let comment = "cccccccccccccccccccc\0";

        let mut header = gz_header {
            text: 0,
            time: 0,
            xflags: 0,
            os: 0,
            extra: extra.as_ptr() as *mut _,
            extra_len: extra.len() as _,
            extra_max: 0,
            name: name.as_ptr() as *mut _,
            name_max: 0,
            comment: comment.as_ptr() as *mut _,
            comm_max: 0,
            hcrc: 1,
            done: 0,
        };

        let config = DeflateConfig {
            window_bits: 31,
            mem_level: 1,
            ..Default::default()
        };

        let mut stream = z_stream::default();
        assert_eq!(init(&mut stream, config), ReturnCode::Ok);

        let Some(stream) = (unsafe { DeflateStream::from_stream_mut(&mut stream) }) else {
            unreachable!()
        };

        set_header(stream, Some(&mut header));

        let input = b"Hello World\n";
        stream.next_in = input.as_ptr() as *mut _;
        stream.avail_in = input.len() as _;

        let mut output = [0u8; 1024];
        stream.next_out = output.as_mut_ptr();
        stream.avail_out = 100;

        assert_eq!(stream.state.bit_writer.pending.capacity(), 512);

        // only 12 bytes remain, so to write the name the pending buffer must be flushed.
        // but there is insufficient output space to flush (only 100 bytes)
        stream.state.bit_writer.pending.extend(&[0; 500]);

        assert_eq!(deflate(stream, DeflateFlush::Finish), ReturnCode::Ok);

        // now try that again but with sufficient output space
        stream.avail_out = output.len() as _;
        assert_eq!(deflate(stream, DeflateFlush::Finish), ReturnCode::StreamEnd);

        let n = stream.total_out as usize;

        assert!(end(stream).is_ok());

        let output_rs = &mut output[..n];

        assert_eq!(output_rs.len(), 500 + 99);
    }

    #[test]
    fn gzip_with_header() {
        // this test is here mostly so we get some MIRI action on the gzip header. A test that
        // compares behavior with zlib-ng is in the libz-rs-sys test suite

        let extra = "some extra stuff\0";
        let name = "nomen est omen\0";
        let comment = "such comment\0";

        let mut header = gz_header {
            text: 0,
            time: 0,
            xflags: 0,
            os: 0,
            extra: extra.as_ptr() as *mut _,
            extra_len: extra.len() as _,
            extra_max: 0,
            name: name.as_ptr() as *mut _,
            name_max: 0,
            comment: comment.as_ptr() as *mut _,
            comm_max: 0,
            hcrc: 1,
            done: 0,
        };

        let config = DeflateConfig {
            window_bits: 31,
            ..Default::default()
        };

        let mut stream = z_stream::default();
        assert_eq!(init(&mut stream, config), ReturnCode::Ok);

        let Some(stream) = (unsafe { DeflateStream::from_stream_mut(&mut stream) }) else {
            unreachable!()
        };

        set_header(stream, Some(&mut header));

        let input = b"Hello World\n";
        stream.next_in = input.as_ptr() as *mut _;
        stream.avail_in = input.len() as _;

        let mut output = [0u8; 256];
        stream.next_out = output.as_mut_ptr();
        stream.avail_out = output.len() as _;

        assert_eq!(deflate(stream, DeflateFlush::Finish), ReturnCode::StreamEnd);

        let n = stream.total_out as usize;

        assert!(end(stream).is_ok());

        let output_rs = &mut output[..n];

        assert_eq!(output_rs.len(), 81);

        {
            let mut stream = z_stream::default();

            let config = InflateConfig {
                window_bits: config.window_bits,
            };

            assert_eq!(crate::inflate::init(&mut stream, config), ReturnCode::Ok);

            let Some(stream) = (unsafe { InflateStream::from_stream_mut(&mut stream) }) else {
                unreachable!();
            };

            stream.next_in = output_rs.as_mut_ptr() as _;
            stream.avail_in = output_rs.len() as _;

            let mut output = [0u8; 12];
            stream.next_out = output.as_mut_ptr();
            stream.avail_out = output.len() as _;

            let mut extra_buf = [0u8; 64];
            let mut name_buf = [0u8; 64];
            let mut comment_buf = [0u8; 64];

            let mut header = gz_header {
                text: 0,
                time: 0,
                xflags: 0,
                os: 0,
                extra: extra_buf.as_mut_ptr(),
                extra_len: 0,
                extra_max: extra_buf.len() as _,
                name: name_buf.as_mut_ptr(),
                name_max: name_buf.len() as _,
                comment: comment_buf.as_mut_ptr(),
                comm_max: comment_buf.len() as _,
                hcrc: 0,
                done: 0,
            };

            assert_eq!(
                crate::inflate::get_header(stream, Some(&mut header)),
                ReturnCode::Ok
            );

            assert_eq!(
                unsafe { crate::inflate::inflate(stream, InflateFlush::Finish) },
                ReturnCode::StreamEnd
            );

            crate::inflate::end(stream);

            assert!(!header.comment.is_null());
            assert_eq!(
                unsafe { CStr::from_ptr(header.comment.cast()) }
                    .to_str()
                    .unwrap(),
                comment.trim_end_matches('\0')
            );

            assert!(!header.name.is_null());
            assert_eq!(
                unsafe { CStr::from_ptr(header.name.cast()) }
                    .to_str()
                    .unwrap(),
                name.trim_end_matches('\0')
            );

            assert!(!header.extra.is_null());
            assert_eq!(
                unsafe { CStr::from_ptr(header.extra.cast()) }
                    .to_str()
                    .unwrap(),
                extra.trim_end_matches('\0')
            );
        }
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn insufficient_compress_space() {
        const DATA: &[u8] = include_bytes!("deflate/test-data/inflate_buf_error.dat");

        fn helper(deflate_buf: &mut [u8]) -> ReturnCode {
            let config = DeflateConfig {
                level: 0,
                method: Method::Deflated,
                window_bits: 10,
                mem_level: 6,
                strategy: Strategy::Default,
            };

            let (output, err) = compress_slice(deflate_buf, DATA, config);
            assert_eq!(err, ReturnCode::Ok);

            let config = InflateConfig {
                window_bits: config.window_bits,
            };

            let mut uncompr = [0; 1 << 17];
            let (uncompr, err) = uncompress_slice(&mut uncompr, output, config);

            if err == ReturnCode::Ok {
                assert_eq!(DATA, uncompr);
            }

            err
        }

        let mut output = [0; 1 << 17];

        // this is too little space
        assert_eq!(helper(&mut output[..1 << 16]), ReturnCode::DataError);

        // this is sufficient space
        assert_eq!(helper(&mut output), ReturnCode::Ok);
    }

    fn test_flush(flush: DeflateFlush, expected: &[u8]) {
        let input = b"Hello World!\n";

        let config = DeflateConfig {
            level: 6, // use gzip
            method: Method::Deflated,
            window_bits: 16 + crate::MAX_WBITS,
            mem_level: DEF_MEM_LEVEL,
            strategy: Strategy::Default,
        };

        let mut output_rs = vec![0; 128];

        // with the flush modes that we test here, the deflate process still has `Status::Busy`,
        // and the `deflateEnd` function will return `DataError`.
        let expected_err = ReturnCode::DataError;

        let (rs, err) = compress_slice_with_flush(&mut output_rs, input, config, flush);
        assert_eq!(expected_err, err);

        assert_eq!(rs, expected);
    }

    #[test]
    #[rustfmt::skip]
    fn sync_flush() {
        test_flush(
            DeflateFlush::SyncFlush,
            &[
                31, 139, 8, 0, 0, 0, 0, 0, 0, gz_header::OS_CODE, 242, 72, 205, 201, 201, 87, 8, 207, 47, 202, 73,
                81, 228, 2, 0, 0, 0, 255, 255,
            ],
        )
    }

    #[test]
    #[rustfmt::skip]
    fn partial_flush() {
        test_flush(
            DeflateFlush::PartialFlush,
            &[
                31, 139, 8, 0, 0, 0, 0, 0, 0, gz_header::OS_CODE, 242, 72, 205, 201, 201, 87, 8, 207, 47, 202, 73,
                81, 228, 2, 8,
            ],
        );
    }

    #[test]
    #[rustfmt::skip]
    fn full_flush() {
        test_flush(
            DeflateFlush::FullFlush,
            &[
                31, 139, 8, 0, 0, 0, 0, 0, 0, gz_header::OS_CODE, 242, 72, 205, 201, 201, 87, 8, 207, 47, 202, 73,
                81, 228, 2, 0, 0, 0, 255, 255,
            ],
        );
    }

    #[test]
    #[rustfmt::skip]
    fn block_flush() {
        test_flush(
            DeflateFlush::Block,
            &[
                31, 139, 8, 0, 0, 0, 0, 0, 0, gz_header::OS_CODE, 242, 72, 205, 201, 201, 87, 8, 207, 47, 202, 73,
                81, 228, 2,
            ],
        );
    }

    #[test]
    // splits the input into two, deflates them seperately and then joins the deflated byte streams
    // into something that can be correctly inflated again. This is the basic idea behind pigz, and
    // allows for parallel compression.
    fn split_deflate() {
        let input = "Hello World!\n";

        let (input1, input2) = input.split_at(6);

        let mut output1 = vec![0; 128];
        let mut output2 = vec![0; 128];

        let config = DeflateConfig {
            level: 6, // use gzip
            method: Method::Deflated,
            window_bits: 16 + crate::MAX_WBITS,
            mem_level: DEF_MEM_LEVEL,
            strategy: Strategy::Default,
        };

        // see also the docs on `SyncFlush`. it makes sure everything is flushed, ends on a byte
        // boundary, and that the final block does not have the "last block" bit set.
        let (prefix, err) = compress_slice_with_flush(
            &mut output1,
            input1.as_bytes(),
            config,
            DeflateFlush::SyncFlush,
        );
        assert_eq!(err, ReturnCode::DataError);

        let (output2, err) = compress_slice_with_flush(
            &mut output2,
            input2.as_bytes(),
            config,
            DeflateFlush::Finish,
        );
        assert_eq!(err, ReturnCode::Ok);

        let inflate_config = crate::inflate::InflateConfig {
            window_bits: 16 + 15,
        };

        // cuts off the length and crc
        let (suffix, end) = output2.split_at(output2.len() - 8);
        let (crc2, len2) = end.split_at(4);
        let crc2 = u32::from_le_bytes(crc2.try_into().unwrap());

        // cuts off the gzip header (10 bytes) from the front
        let suffix = &suffix[10..];

        let mut result: Vec<u8> = Vec::new();
        result.extend(prefix.iter());
        result.extend(suffix);

        // it would be more proper to use `stream.total_in` here, but the slice helpers hide the
        // stream so we're cheating a bit here
        let len1 = input1.len() as u32;
        let len2 = u32::from_le_bytes(len2.try_into().unwrap());
        assert_eq!(len2 as usize, input2.len());

        let crc1 = crate::crc32(0, input1.as_bytes());
        let crc = crate::crc32_combine(crc1, crc2, len2 as u64);

        // combined crc of the parts should be the crc of the whole
        let crc_cheating = crate::crc32(0, input.as_bytes());
        assert_eq!(crc, crc_cheating);

        // write the trailer
        result.extend(crc.to_le_bytes());
        result.extend((len1 + len2).to_le_bytes());

        let mut output = vec![0; 128];
        let (output, err) = crate::inflate::uncompress_slice(&mut output, &result, inflate_config);
        assert_eq!(err, ReturnCode::Ok);

        assert_eq!(output, input.as_bytes());
    }

    #[test]
    fn inflate_window_copy_slice() {
        let uncompressed = [
            9, 126, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 76, 33, 8, 2, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12,
            10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 76, 33, 8, 2, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 12, 10, 0, 0, 0, 0, 14, 0, 0, 0, 0, 0, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
            69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 14, 0, 0, 0, 0, 0, 69, 69, 69, 69, 69, 69, 69,
            69, 69, 69, 69, 69, 69, 69, 69, 9, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
            69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
            69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
            69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 12, 28, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 12, 10, 0, 0, 0, 0, 14, 0, 0, 0, 0, 0, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
            69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 14, 0, 0, 0, 0, 0, 69, 69, 69, 69, 69, 69, 69,
            69, 69, 69, 69, 69, 69, 69, 69, 9, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
            69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
            69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
            69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 12, 28, 0, 2, 0, 0, 0, 63, 1, 0, 12, 2,
            36, 0, 28, 0, 0, 0, 1, 0, 0, 63, 63, 13, 0, 0, 0, 0, 0, 0, 0, 63, 63, 63, 63, 0, 0, 0,
            0, 0, 0, 65, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 45, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 91, 0, 0, 0, 9, 0, 0, 0, 9, 0, 0, 12, 33, 2, 0, 0, 8,
            0, 4, 0, 0, 0, 12, 10, 41, 12, 10, 47,
        ];

        let compressed = &[
            31, 139, 8, 0, 0, 0, 0, 0, 4, 3, 181, 193, 49, 14, 194, 32, 24, 128, 209, 175, 192, 0,
            228, 151, 232, 206, 66, 226, 226, 96, 60, 2, 113, 96, 235, 13, 188, 139, 103, 23, 106,
            104, 108, 100, 49, 169, 239, 185, 39, 11, 199, 7, 51, 39, 171, 248, 118, 226, 63, 52,
            157, 120, 86, 102, 78, 86, 209, 104, 58, 241, 84, 129, 166, 12, 4, 154, 178, 229, 202,
            30, 36, 130, 166, 19, 79, 21, 104, 202, 64, 160, 41, 91, 174, 236, 65, 34, 10, 200, 19,
            162, 206, 68, 96, 130, 156, 15, 188, 229, 138, 197, 157, 161, 35, 3, 87, 126, 245, 0,
            28, 224, 64, 146, 2, 139, 1, 196, 95, 196, 223, 94, 10, 96, 92, 33, 86, 2, 0, 0,
        ];

        let config = InflateConfig { window_bits: 25 };

        let mut dest_vec_rs = vec![0u8; uncompressed.len()];
        let (output_rs, error) =
            crate::inflate::uncompress_slice(&mut dest_vec_rs, compressed, config);

        assert_eq!(ReturnCode::Ok, error);
        assert_eq!(output_rs, uncompressed);
    }

    #[test]
    fn hash_calc_difference() {
        let input = [
            0, 0, 0, 0, 0, 43, 0, 0, 0, 0, 0, 0, 43, 0, 0, 0, 0, 0, 0, 0, 0, 48, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 49, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 29, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 19, 0, 0, 0, 0, 0,
            0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 55, 0, 64, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 102, 102, 102, 102, 102,
            102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102,
            102, 102, 102, 102, 102, 102, 102, 102, 112, 102, 102, 102, 102, 102, 102, 102, 102,
            102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 0,
            0, 0, 0, 0, 0, 49, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 19, 0, 0, 0, 0, 0, 0, 0, 64, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 55, 0, 64, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 42, 0, 0, 0,
            50, 0,
        ];

        let config = DeflateConfig {
            level: 6,
            method: Method::Deflated,
            window_bits: 9,
            mem_level: 8,
            strategy: Strategy::Default,
        };

        let crc32 = [
            24, 149, 99, 96, 96, 96, 96, 208, 6, 17, 112, 138, 129, 193, 128, 1, 29, 24, 50, 208,
            1, 200, 146, 169, 79, 24, 74, 59, 96, 147, 52, 71, 22, 70, 246, 88, 26, 94, 80, 128,
            83, 6, 162, 219, 144, 76, 183, 210, 5, 8, 67, 105, 7, 108, 146, 230, 216, 133, 145,
            129, 22, 3, 3, 131, 17, 3, 0, 3, 228, 25, 128,
        ];

        let other = [
            24, 149, 99, 96, 96, 96, 96, 208, 6, 17, 112, 138, 129, 193, 128, 1, 29, 24, 50, 208,
            1, 200, 146, 169, 79, 24, 74, 59, 96, 147, 52, 71, 22, 70, 246, 88, 26, 94, 80, 128,
            83, 6, 162, 219, 144, 76, 183, 210, 5, 8, 67, 105, 36, 159, 35, 128, 57, 118, 97, 100,
            160, 197, 192, 192, 96, 196, 0, 0, 3, 228, 25, 128,
        ];

        // the output is slightly different based on what hashing algorithm is used
        match HashCalcVariant::for_compression_level(config.level as usize) {
            HashCalcVariant::Crc32 => {
                fuzz_based_test(&input, config, &crc32);
            }
            HashCalcVariant::Standard | HashCalcVariant::Roll => {
                fuzz_based_test(&input, config, &other);
            }
        }
    }
}
