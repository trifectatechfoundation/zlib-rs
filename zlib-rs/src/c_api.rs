#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use libc::{c_char, c_int, c_uchar, c_uint, c_ulong, c_void};

pub type alloc_func = unsafe extern "C" fn(voidpf, uInt, uInt) -> voidpf;
pub type Bytef = u8;
pub type free_func = unsafe extern "C" fn(voidpf, voidpf);
pub type in_func = unsafe extern "C" fn(*mut c_void, *mut *const c_uchar) -> c_uint;
pub type out_func = unsafe extern "C" fn(*mut c_void, *mut c_uchar, c_uint) -> c_int;
pub type uInt = c_uint;
pub type uLong = c_ulong;
pub type uLongf = c_ulong;
pub type voidp = *mut c_void;
pub type voidpc = *const c_void;
pub type voidpf = *mut c_void;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct z_stream {
    pub next_in: *mut Bytef,
    pub avail_in: uInt,
    pub total_in: z_size,
    pub next_out: *mut Bytef,
    pub avail_out: uInt,
    pub total_out: z_size,
    pub msg: *mut c_char,
    pub state: *mut internal_state,
    pub zalloc: Option<alloc_func>,
    pub zfree: Option<free_func>,
    pub opaque: voidpf,
    pub data_type: c_int,
    pub adler: z_checksum,
    pub reserved: uLong,
}
pub type z_streamp = *mut z_stream;

impl Default for z_stream {
    fn default() -> Self {
        Self {
            next_in: std::ptr::null_mut(),
            avail_in: 0,
            total_in: 0,
            next_out: std::ptr::null_mut(),
            avail_out: 0,
            total_out: 0,
            msg: std::ptr::null_mut(),
            state: std::ptr::null_mut(),
            zalloc: Some(crate::allocate::zcalloc),
            zfree: Some(crate::allocate::zcfree),
            opaque: std::ptr::null_mut(),
            data_type: 0,
            adler: 0,
            reserved: 0,
        }
    }
}

impl z_stream {
    pub(crate) unsafe fn alloc_layout(&self, layout: std::alloc::Layout) -> *mut c_void {
        match self.zalloc {
            None => unreachable!("zalloc no initialized"),
            Some(f) => f(self.opaque, 1, layout.size() as u32),
        }
    }

    pub(crate) unsafe fn alloc_value<T>(&self, value: T) -> *mut T {
        let ptr = self.alloc_layout(std::alloc::Layout::new::<T>()).cast();

        if ptr as usize != 0 {
            std::ptr::write(ptr, value);
        }

        ptr
    }

    pub(crate) unsafe fn dealloc<T>(&self, ptr: *mut T) {
        match self.zfree {
            None => unreachable!("zfree no initialized"),
            Some(f) => f(self.opaque, ptr.cast()),
        }
    }
}

// // zlib stores Adler-32 and CRC-32 checksums in unsigned long; zlib-ng uses uint32_t.
pub(crate) type z_size = c_ulong;
pub(crate) type z_checksum = c_ulong;

// opaque to the user
pub enum internal_state {}

pub const Z_NO_FLUSH: c_int = 0;
pub const Z_PARTIAL_FLUSH: c_int = 1;
pub const Z_SYNC_FLUSH: c_int = 2;
pub const Z_FULL_FLUSH: c_int = 3;
pub const Z_FINISH: c_int = 4;
pub const Z_BLOCK: c_int = 5;
pub const Z_TREES: c_int = 6;

pub const Z_OK: c_int = 0;
pub const Z_STREAM_END: c_int = 1;
pub const Z_NEED_DICT: c_int = 2;
pub const Z_ERRNO: c_int = -1;
pub const Z_STREAM_ERROR: c_int = -2;
pub const Z_DATA_ERROR: c_int = -3;
pub const Z_MEM_ERROR: c_int = -4;
pub const Z_BUF_ERROR: c_int = -5;
pub const Z_VERSION_ERROR: c_int = -6;

pub const Z_NO_COMPRESSION: c_int = 0;
pub const Z_BEST_SPEED: c_int = 1;
pub const Z_BEST_COMPRESSION: c_int = 9;
pub const Z_DEFAULT_COMPRESSION: c_int = -1;

pub const Z_DEFLATED: c_int = 8;

pub const Z_BINARY: c_int = 0;
pub const Z_TEXT: c_int = 1;
pub const Z_ASCII: c_int = Z_TEXT; /* for compatibility with 1.2.2 and earlier */
pub const Z_UNKNOWN: c_int = 2;

pub const Z_FILTERED: c_int = 1;
pub const Z_HUFFMAN_ONLY: c_int = 2;
pub const Z_RLE: c_int = 3;
pub const Z_FIXED: c_int = 4;
pub const Z_DEFAULT_STRATEGY: c_int = 0;
