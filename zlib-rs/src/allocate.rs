use std::{
    alloc::{GlobalAlloc, Layout},
    ffi::{c_uint, c_void},
    marker::PhantomData,
    mem::MaybeUninit,
};

use std::ffi::c_int;

#[allow(non_camel_case_types)]
type size_t = usize;

/// # Safety
///
/// The `ptr` must be allocated with the allocator that the `zfree` function belongs to.
pub unsafe fn free_aligned(zfree: crate::c_api::free_func, opaque: *mut c_void, ptr: *mut c_void) {
    if zfree as usize == zcfree as usize {
        // safety: only unsafe because of the public interface
        unsafe { zcfree(opaque, ptr) }
    } else if ptr.is_null() {
        /* do nothing */
    } else {
        /* Calculate offset to original memory allocation pointer */
        let sizeof_ptr = std::mem::size_of::<*mut c_void>();

        let original_ptr: *mut c_void = unsafe { ptr.offset(-8 * sizeof_ptr as isize) };
        let free_ptr: *mut c_void = unsafe { *(original_ptr.cast::<*mut c_void>()) };

        /* Free original memory allocation */
        unsafe { zfree(opaque, free_ptr) };
    }
}

/// # Safety
///
/// This function is safe, but must have this type signature to be used elsewhere in the library
pub unsafe extern "C" fn zcalloc(opaque: *mut c_void, items: c_uint, size: c_uint) -> *mut c_void {
    let _ = opaque;
    zng_alloc(items as size_t * size as size_t)
}

/// # Safety
///
/// The `ptr` must be allocated with the allocator that is used internally by `zcfree`
pub unsafe extern "C" fn zcfree(opaque: *mut c_void, ptr: *mut c_void) {
    let _ = opaque;
    zng_free(ptr)
}

#[cfg(unix)]
unsafe fn zng_alloc(size: size_t) -> *mut c_void {
    extern "C" {
        fn posix_memalign(memptr: *mut *mut c_void, align: size_t, size: size_t) -> c_int;
    }

    let mut ptr = std::ptr::null_mut();
    match posix_memalign(&mut ptr, 64, size) {
        0 => ptr,
        _ => std::ptr::null_mut(),
    }
}

#[cfg(windows)]
unsafe fn zng_alloc(size: size_t) -> *mut c_void {
    extern "C" {
        fn aligned_malloc(size: size_t, alignment: size_t) -> *mut c_void;

    }

    aligned_malloc(size, 64)
}

#[cfg(not(any(unix, windows)))]
unsafe fn zng_alloc(size: size_t) -> *mut c_void {
    extern "C" {
        fn memalign(align: size_t, size: size_t) -> *mut c_void;
    }

    // just sort of hope that this function exists
    memalign(64, size)
}

unsafe fn zng_free(ptr: *mut c_void) {
    extern "C" {
        fn free(p: *mut c_void);
    }

    unsafe { free(ptr) }
}

unsafe extern "C" fn zalloc_rust(_opaque: *mut c_void, count: c_uint, size: c_uint) -> *mut c_void {
    let align = 64;
    let size = count as usize * size as usize;

    // internally, we want to align allocations to 64 bytes (in part for SIMD reasons)
    let layout = Layout::from_size_align(size, align).unwrap();

    let ptr = std::alloc::System.alloc(layout);

    ptr as *mut c_void
}

/// # Safety
///
/// - `ptr` must be allocated with the rust `alloc::System` allocator
/// - `opaque` is a `&usize` that represents the size of the allocation
unsafe extern "C" fn zfree_rust(opaque: *mut c_void, ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }

    // we can't really do much else. Deallocating with an invalid layout is UB.
    debug_assert!(!opaque.is_null());
    if opaque.is_null() {
        return;
    }

    let size = *(opaque as *mut usize);
    let align = 64;

    let layout = Layout::from_size_align(size, align);
    let layout = layout.unwrap();

    std::alloc::System.dealloc(ptr.cast(), layout);
}

#[derive(Clone, Copy)]
#[repr(C)]
pub(crate) struct Allocator<'a> {
    pub(crate) zalloc: crate::c_api::alloc_func,
    pub(crate) zfree: crate::c_api::free_func,
    pub(crate) opaque: crate::c_api::voidpf,
    pub(crate) _marker: PhantomData<&'a ()>,
}

impl Allocator<'static> {
    pub const RUST: Self = Self {
        zalloc: zalloc_rust,
        zfree: zfree_rust,
        opaque: core::ptr::null_mut(),
        _marker: PhantomData,
    };

    pub const C: Self = Self {
        zalloc: zcalloc,
        zfree: zcfree,
        opaque: core::ptr::null_mut(),
        _marker: PhantomData,
    };
}

impl<'a> Allocator<'a> {
    pub fn allocate<T>(&self) -> Option<&'a mut MaybeUninit<T>> {
        let layout = Layout::new::<T>();
        let ptr = unsafe { (self.zalloc)(self.opaque, layout.size() as _, 1) };

        if ptr.is_null() {
            None
        } else {
            Some(unsafe { &mut *(ptr as *mut MaybeUninit<T>) })
        }
    }

    pub fn allocate_slice<T>(&self, len: usize) -> Option<&'a mut [MaybeUninit<T>]> {
        let layout = Layout::array::<T>(len).ok()?;
        let ptr = unsafe { (self.zalloc)(self.opaque, layout.size() as _, 1) };

        if ptr.is_null() {
            None
        } else {
            Some(unsafe { core::slice::from_raw_parts_mut(ptr.cast(), len) })
        }
    }

    /// # Panics
    ///
    /// - when `len` is 0
    ///
    /// # Safety
    ///
    /// - `ptr` must be allocated with this allocator
    /// - `len` must be the number of `T`s that are in this allocation
    pub unsafe fn deallocate<T>(&self, ptr: *mut T, len: usize) {
        if !ptr.is_null() {
            if self.zfree == Allocator::RUST.zfree {
                assert_ne!(len, 0, "invalid size for {:?}", ptr);
                let mut size = core::mem::size_of::<T>() * len;
                (Allocator::RUST.zfree)(&mut size as *mut usize as *mut c_void, ptr.cast())
            } else {
                (self.zfree)(self.opaque, ptr.cast())
            }
        }
    }
}
