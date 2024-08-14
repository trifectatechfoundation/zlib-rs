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
