use std::ffi::{c_uint, c_void};

pub fn free_aligned(zfree: crate::c_api::free_func, opaque: *mut c_void, ptr: *mut c_void) {
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

pub unsafe extern "C" fn zcalloc(opaque: *mut c_void, items: c_uint, size: c_uint) -> *mut c_void {
    let _ = opaque;
    zng_alloc(items as libc::size_t * size as libc::size_t)
}

pub unsafe extern "C" fn zcfree(opaque: *mut c_void, ptr: *mut c_void) {
    let _ = opaque;
    zng_free(ptr)
}

#[cfg(unix)]
unsafe fn zng_alloc(size: libc::size_t) -> *mut c_void {
    let mut ptr = std::ptr::null_mut();
    match libc::posix_memalign(&mut ptr, 64, size) {
        0 => ptr,
        _ => std::ptr::null_mut(),
    }
}

#[cfg(windows)]
unsafe fn zng_alloc(size: libc::size_t) -> *mut c_void {
    libc::aligned_malloc(size, 64)
}

#[cfg(not(any(unix, windows)))]
unsafe fn zng_alloc(size: libc::size_t) -> *mut c_void {
    // just sort of hope that this function exists
    libc::memalign(64, size)
}

unsafe fn zng_free(ptr: *mut c_void) {
    // TODO based on the zlig-ng code this may need to use _aligned_free on (32-bit?) windows
    unsafe { libc::free(ptr) };
}
