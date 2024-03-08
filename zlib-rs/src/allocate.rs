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
    fn allocate_layout(&self, layout: Layout) -> *mut c_void {
        let ptr = if self.zalloc == Allocator::RUST.zalloc {
            unsafe { (Allocator::RUST.zalloc)(self.opaque, layout.size() as _, 1) }
        } else {
            // we cannot rely on the allocator giving properly aligned allocations and have to fix that ourselves

            let align = Ord::max(core::mem::size_of::<*mut c_void>(), layout.align());

            // we need at least
            //
            // - `align` space so that no matter what pointer we get, we can shift the start of our
            //      allocation by at most `align - 1` so that `ptr as usize % align == 0
            // - `size_of::<*mut _>` so that after aligning to `align`, there is `size_of::<*mut _>` space to store
            //      the pointer to the allocation. This pointer is then retrieved in `free`
            let extra_space = core::mem::size_of::<*mut c_void>() + align;

            // Safety: we assume allocating works correctly in the safety assumptions on
            // `DeflateStream` and `InflateStream`.
            let ptr = unsafe { (self.zalloc)(self.opaque, (layout.size() + extra_space) as _, 1) };

            if ptr.is_null() {
                return ptr;
            }

            // Calculate return pointer address with space enough to store original pointer
            let align_diff = (ptr as usize).next_multiple_of(layout.align()) - (ptr as usize);

            // Safety: offset is smaller than 64, and we allocated 64 extra bytes in the allocation
            let mut return_ptr = unsafe { ptr.cast::<u8>().add(align_diff) };

            // if there is not enough space to store a pointer we need to make more
            if align_diff < core::mem::size_of::<*mut c_void>() {
                // # Safety
                //
                // - `return_ptr` is well-aligned, therefore `return_ptr + align` is also well-aligned
                // - we reserve `size_of::<*mut _> + align` extra space in the allocation, so
                //      `ptr + align_diff + align` is still valid for (at least) `layout.size` bytes
                return_ptr = unsafe { return_ptr.add(align) };
            }

            // Store the original pointer for free()
            //
            // Safety: `align >= size_of::<*mut _>`, so there is now space for a pointer before `return_ptr`
            // in the allocation
            unsafe {
                let original_ptr = return_ptr.sub(core::mem::size_of::<*mut c_void>());
                std::ptr::write_unaligned(original_ptr.cast::<*mut c_void>(), ptr);
            };

            // Return properly aligned pointer in allocation
            return_ptr.cast::<c_void>()
        };

        assert_eq!(ptr as usize % layout.align(), 0);

        ptr
    }

    pub fn allocate<T>(&self) -> Option<&'a mut MaybeUninit<T>> {
        let ptr = self.allocate_layout(Layout::new::<T>());

        if ptr.is_null() {
            None
        } else {
            Some(unsafe { &mut *(ptr as *mut MaybeUninit<T>) })
        }
    }

    pub fn allocate_slice<T>(&self, len: usize) -> Option<&'a mut [MaybeUninit<T>]> {
        let ptr = self.allocate_layout(Layout::array::<T>(len).ok()?);

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
                let original_ptr = (ptr as *mut u8).sub(core::mem::size_of::<*const c_void>());
                let free_ptr = core::ptr::read_unaligned(original_ptr as *mut *mut c_void);

                (self.zfree)(self.opaque, free_ptr)
            }
        }
    }
}
