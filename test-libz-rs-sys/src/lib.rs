#[cfg(test)]
mod deflate;
pub mod end_to_end;
mod helpers;
#[cfg(test)]
mod inflate;
#[cfg(test)]
mod zlib_ng_cve;

#[cfg(test)]
macro_rules! assert_eq_rs_ng {
    ($tt:tt) => {
        #[allow(unused_braces)]
        #[allow(unused_unsafe)]
        let rs = unsafe {
            use libz_rs_sys::*;

            $tt
        };

        #[allow(unused_braces)]
        #[allow(unused_unsafe)]
        let ng = unsafe {
            use libz_sys::*;

            $tt
        };

        assert_eq!(rs, ng);
    };
}

#[test]
fn adler_null() {
    assert_eq_rs_ng!({ adler32(0, core::ptr::null(), 0) });
    assert_eq_rs_ng!({ adler32(1, core::ptr::null(), 0) });
    assert_eq_rs_ng!({ adler32(2, core::ptr::null(), 32) });
}

#[test]
fn adler32_combine_negative() {
    assert_eq_rs_ng!({ adler32_combine(0, 123, 16) });
    assert_eq_rs_ng!({ adler32_combine(1, 123, 16) });
    assert_eq_rs_ng!({ adler32_combine(2, 123, -16) });
}

#[test]
fn crc32_null() {
    assert_eq_rs_ng!({ crc32(0, core::ptr::null(), 0) });
    assert_eq_rs_ng!({ crc32(1, core::ptr::null(), 0) });
    assert_eq_rs_ng!({ crc32(2, core::ptr::null(), 32) });
}

#[test]
fn crc32_combine_negative() {
    assert_eq_rs_ng!({ crc32_combine(0, 123, 16) });
    assert_eq_rs_ng!({ crc32_combine(1, 123, 16) });

    // interestingly, stock zlib just loops infinitely for this input
    // assert_eq_rs_ng!({ crc32_combine(2, 123, -16) });

    // so let's just lock in what we do
    assert_eq!(libz_rs_sys::crc32_combine(2, 123, -16), 2171032315);
}

#[test]
fn uncompress_null() {
    let mut dest = [0; 64];
    let source = [1, 2, 3];

    assert_eq_rs_ng!({
        uncompress(
            dest.as_mut_ptr(),
            &mut (dest.len() as _),
            source.as_ptr(),
            source.len() as _,
        )
    });

    // this makes stock zlib segfault
    assert_eq!(
        unsafe {
            libz_rs_sys::uncompress(
                dest.as_mut_ptr(),
                core::ptr::null_mut(),
                source.as_ptr(),
                source.len() as _,
            )
        },
        libz_rs_sys::Z_STREAM_ERROR
    );

    assert_eq_rs_ng!({
        uncompress(
            core::ptr::null_mut(),
            &mut (dest.len() as _),
            source.as_ptr(),
            source.len() as _,
        )
    });

    assert_eq_rs_ng!({
        uncompress(
            dest.as_mut_ptr(),
            &mut (dest.len() as _),
            core::ptr::null(),
            source.len() as _,
        )
    });

    assert_eq_rs_ng!({
        uncompress(
            core::ptr::null_mut(),
            &mut (dest.len() as _),
            core::ptr::null(),
            source.len() as _,
        )
    });
}

#[test]
fn compress_null() {
    let mut dest = [0; 64];
    let source = [1, 2, 3];

    assert_eq_rs_ng!({
        compress(
            dest.as_mut_ptr(),
            &mut (dest.len() as _),
            source.as_ptr(),
            source.len() as _,
        )
    });

    // this makes stock zlib segfault
    assert_eq!(
        unsafe {
            libz_rs_sys::compress(
                dest.as_mut_ptr(),
                core::ptr::null_mut(),
                source.as_ptr(),
                source.len() as _,
            )
        },
        libz_rs_sys::Z_STREAM_ERROR
    );

    assert_eq_rs_ng!({
        compress(
            core::ptr::null_mut(),
            &mut (dest.len() as _),
            source.as_ptr(),
            source.len() as _,
        )
    });

    assert_eq_rs_ng!({
        compress(
            dest.as_mut_ptr(),
            &mut (dest.len() as _),
            core::ptr::null(),
            source.len() as _,
        )
    });

    assert_eq_rs_ng!({
        compress(
            core::ptr::null_mut(),
            &mut (dest.len() as _),
            core::ptr::null(),
            source.len() as _,
        )
    });
}

#[test]
fn inflate_null() {
    use std::mem::MaybeUninit;

    assert_eq_rs_ng!({
        let mut strm = MaybeUninit::<z_stream>::zeroed();

        inflateInit_(
            core::ptr::null_mut(),
            zlibVersion(),
            core::mem::size_of::<z_stream>() as _,
        );

        inflateInit_(
            strm.as_mut_ptr(),
            core::ptr::null(),
            core::mem::size_of::<z_stream>() as _,
        );

        inflate(core::ptr::null_mut(), Z_NO_FLUSH);

        inflateEnd(core::ptr::null_mut());
    });
}
