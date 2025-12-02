#[cfg(test)]
mod deflate;
pub mod end_to_end;
#[cfg(test)]
#[cfg(feature = "gz")]
mod gz;
mod helpers;
#[cfg(test)]
mod infback;
#[cfg(test)]
mod inflate;
#[cfg(test)]
mod zlib_ng_cve;

#[cfg(test)]
#[macro_export]
macro_rules! assert_eq_rs_ng {
    ($tt:tt) => {{
        #[cfg(not(miri))]
        #[allow(clippy::macro_metavars_in_unsafe)]
        #[allow(unused_braces)]
        #[allow(unused_unsafe)]
        let _ng = unsafe {
            use libz_sys::*;

            // this function is not exposed by `libz_sys`
            extern "C" {
                #[allow(unused)]
                fn inflateCodesUsed(strm: *mut z_stream) -> core::ffi::c_ulong;

                #[allow(unused)]
                fn deflateGetDictionary(
                    strm: *const z_stream,
                    dictionary: *mut core::ffi::c_uchar,
                    dictLength: *mut core::ffi::c_uint,
                ) -> core::ffi::c_int;

                #[allow(unused)]
                fn inflateGetDictionary(
                    strm: *const z_stream,
                    dictionary: *mut core::ffi::c_uchar,
                    dictLength: *mut core::ffi::c_uint,
                ) -> core::ffi::c_int;

                #[allow(unused)]
                fn uncompress2(
                    dest: *mut u8,
                    destLen: *mut core::ffi::c_ulong,
                    source: *const u8,
                    sourceLen: *mut core::ffi::c_ulong,
                ) -> core::ffi::c_int;

                #[allow(unused)]
                fn crc32_z(
                    crc: core::ffi::c_ulong,
                    buf: *const Bytef,
                    len: libz_rs_sys::size_t,
                ) -> core::ffi::c_ulong;

                #[allow(unused)]
                fn crc32_combine64(
                    crc1: core::ffi::c_ulong,
                    crc2: core::ffi::c_ulong,
                    len2: libz_rs_sys::z_off64_t,
                ) -> core::ffi::c_ulong;

                #[allow(unused)]
                fn adler32_z(
                    crc: core::ffi::c_ulong,
                    buf: *const Bytef,
                    len: libz_rs_sys::size_t,
                ) -> core::ffi::c_ulong;

                #[allow(unused)]
                fn adler32_combine64(
                    adler1: core::ffi::c_ulong,
                    adler2: core::ffi::c_ulong,
                    len2: libz_rs_sys::z_off64_t,
                ) -> core::ffi::c_ulong;

                #[allow(unused)]
                fn get_crc_table() -> *const [u32; 256];
            }

            $tt
        };

        #[allow(clippy::macro_metavars_in_unsafe)]
        #[allow(unused_braces)]
        #[allow(unused_unsafe)]
        let _rs = unsafe {
            use libz_rs_sys::*;

            $tt
        };

        #[cfg(not(miri))]
        assert_eq!(_rs, _ng);

        _rs
    }};
}

#[cfg(test)]
#[allow(unused)]
macro_rules! extract_error_message {
    ($strm:expr) => {
        if !$strm.msg.is_null() {
            core::ffi::CStr::from_ptr($strm.msg).to_str()
        } else {
            Ok("NULL")
        }
    };
}

/// a bit of a sanity check that nothing weird happened with symbol resolution
#[cfg(not(miri))]
#[cfg(test)]
#[test]
fn zlib_rs_is_not_zlib_ng() {
    use std::ffi::CStr;

    unsafe {
        let rs = CStr::from_ptr(libz_rs_sys::zlibVersion());
        assert!(rs.to_str().unwrap().contains("zlib-rs"));

        let ng = CStr::from_ptr(libz_sys::zlibVersion());
        assert!(ng.to_str().unwrap().contains("zlib-ng"));
    }
}

/// a bit of a sanity check that nothing weird happened with symbol resolution
#[cfg(not(miri))]
#[cfg(test)]
#[test]
fn flags() {
    unsafe {
        let rs = libz_rs_sys::zlibCompileFlags();
        let ng = libz_sys::zlibCompileFlags();

        // the first 8 bits store the size of various types
        assert_eq!(rs & 0b11, ng & 0b11);
        assert_eq!((rs >> 2) & 0b11, (ng >> 2) & 0b11);
        assert_eq!((rs >> 4) & 0b11, (ng >> 4) & 0b11);
        assert_eq!((rs >> 6) & 0b11, (ng >> 6) & 0b11);

        // ZLIB_DEBUG
        assert_eq!(rs & (1 << 8), ng & (1 << 8));

        // PKZIP_BUG_WORKAROUND
        assert_eq!(rs & (1 << 20), ng & (1 << 20));

        // The sprintf variant used by gzprintf
        assert_eq!(rs & (0b111 << 24), ng & (0b111 << 24));

        assert_eq!(rs, ng);
    }
}

#[cfg(test)]
mod null {
    use core::ffi::{c_char, c_int};
    use core::mem::MaybeUninit;

    use zlib_rs::{inflate::InflateConfig, InflateFlush, ReturnCode};

    #[test]
    fn adler() {
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
    fn crc32() {
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
    fn uncompress() {
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
    fn compress_dest_len() {
        let mut dest = [0; 64];
        let source = [1, 2, 3];

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
    }

    #[test]
    fn compress_dest() {
        let dest = [0; 64];
        let source = [1, 2, 3];

        assert_eq_rs_ng!({
            compress(
                core::ptr::null_mut(),
                &mut (dest.len() as _),
                source.as_ptr(),
                source.len() as _,
            )
        });
    }

    #[test]
    fn compress_source() {
        let mut dest = [0; 64];
        let source = [1, 2, 3];

        assert_eq_rs_ng!({
            compress(
                dest.as_mut_ptr(),
                &mut (dest.len() as _),
                core::ptr::null(),
                source.len() as _,
            )
        });
    }

    #[test]
    fn compress_source_and_dest() {
        let dest = [0; 64];
        let source = [1, 2, 3];

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
    fn inflate_init() {
        assert_eq_rs_ng!({
            inflateInit_(
                core::ptr::null_mut(),
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            )
        });

        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::<z_stream>::zeroed();
            inflateInit_(
                strm.as_mut_ptr(),
                core::ptr::null(),
                core::mem::size_of::<z_stream>() as _,
            )
        });
    }

    #[test]
    fn inflate_init2() {
        assert_eq_rs_ng!({
            inflateInit2_(
                core::ptr::null_mut(),
                15,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            )
        });

        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::<z_stream>::zeroed();
            inflateInit2_(
                strm.as_mut_ptr(),
                15,
                core::ptr::null(),
                core::mem::size_of::<z_stream>() as _,
            )
        });
    }

    #[test]
    fn inflate() {
        assert_eq_rs_ng!({ inflate(core::ptr::null_mut(), Z_NO_FLUSH) });
    }

    #[test]
    fn inflate_end() {
        assert_eq_rs_ng!({ inflateEnd(core::ptr::null_mut()) });
        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::<z_stream>::zeroed();
            inflateEnd(strm.as_mut_ptr())
        });
    }

    #[test]
    fn inflate_end_after_inflate() {
        use libz_rs_sys::*;

        unsafe {
            let mut strm = MaybeUninit::<z_stream>::zeroed();

            let err = inflateInit_(
                strm.as_mut_ptr(),
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            );
            assert_eq!(err, Z_OK);

            let strm = strm.assume_init_mut();

            {
                let mut dest = vec![0u8; 100];

                strm.next_in = FERRIS_BYTES.as_ptr() as *mut u8;
                strm.avail_in = FERRIS_BYTES.len() as _;

                strm.next_out = dest.as_mut_ptr();
                strm.avail_out = dest.len() as _;

                let err = inflate(strm, Z_FINISH);
                assert_eq!(err, Z_STREAM_END);

                dest.truncate(strm.total_out as usize);
                assert_eq!(dest, b"Ferris");
            }

            let err = inflateEnd(strm);
            assert_eq!(err, Z_OK);
        }
    }

    #[test]
    fn inflate_copy() {
        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::<z_stream>::zeroed();
            inflateCopy(core::ptr::null_mut(), strm.as_mut_ptr())
        });
        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::<z_stream>::zeroed();
            inflateCopy(strm.as_mut_ptr(), core::ptr::null_mut())
        });
    }

    #[test]
    fn inflate_get_header() {
        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::<z_stream>::zeroed();
            let mut head = MaybeUninit::<gz_header>::zeroed();

            inflateInit_(
                strm.as_mut_ptr(),
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            );

            let a = inflateGetHeader(strm.as_mut_ptr(), core::ptr::null_mut());
            let b = inflateGetHeader(core::ptr::null_mut(), head.as_mut_ptr());

            let c = inflateEnd(strm.as_mut_ptr());

            (a, b, c)
        });
    }

    #[test]
    fn inflate_get_header_zeroed() {
        const FERRIS_BYTES_GZ_NO_HEADER: [u8; 26] = [
            31, 139, 8, 0, 0, 0, 0, 0, 0, 3, 115, 75, 45, 42, 202, 44, 6, 0, 174, 148, 97, 210, 6,
            0, 0, 0,
        ];

        const FERRIS_BYTES_GZ_WITH_HEADER: &[u8] = &[
            31, 139, 8, 28, 0, 0, 0, 0, 0, 3, 6, 0, 98, 97, 110, 97, 110, 97, 97, 112, 112, 108,
            101, 0, 112, 101, 97, 114, 0, 115, 75, 45, 42, 202, 44, 6, 0, 174, 148, 97, 210, 6, 0,
            0, 0,
        ];

        let input = b"Ferris";
        let mut buf = [0; 64];

        let config = zlib_rs::deflate::DeflateConfig {
            window_bits: 16 + 15,
            ..Default::default()
        };

        assert_eq_rs_ng!({
            let mut extra = *b"banana\0";
            let mut name = *b"apple\0";
            let mut comment = *b"pear\0";

            let mut strm = MaybeUninit::<z_stream>::zeroed();

            let err = deflateInit2_(
                strm.as_mut_ptr(),
                config.level,
                config.method as _,
                config.window_bits,
                config.mem_level,
                config.strategy as _,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            );
            assert_eq!(err, Z_OK);

            let strm = strm.assume_init_mut();

            let mut header = gz_header {
                text: 0,
                time: 0,
                xflags: 0,
                os: 3,
                extra: extra.as_mut_ptr(),
                extra_len: (extra.len() - 1) as _,
                extra_max: 0,
                name: name.as_mut_ptr(),
                name_max: 0,
                comment: comment.as_mut_ptr(),
                comm_max: 0,
                hcrc: 0,
                done: 0,
            };

            deflateSetHeader(strm, &mut header);
            assert_eq!(err, Z_OK);

            buf.fill(0);

            strm.next_in = input.as_ptr() as *mut u8;
            strm.avail_in = input.len() as _;

            strm.next_out = buf.as_mut_ptr();
            strm.avail_out = buf.len() as _;

            let err = deflate(strm, Z_FINISH);
            assert_eq!(err, Z_STREAM_END);

            let err = deflateEnd(strm);
            assert_eq!(err, Z_OK);

            assert_eq!(&buf[..strm.total_out as usize], FERRIS_BYTES_GZ_WITH_HEADER);
        });

        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::<z_stream>::zeroed();

            fn initialize_header_null() -> MaybeUninit<gz_header> {
                let mut head = MaybeUninit::<gz_header>::zeroed();

                unsafe {
                    core::ptr::write(
                        core::ptr::addr_of_mut!((*head.as_mut_ptr()).extra),
                        core::ptr::null_mut(),
                    );
                    core::ptr::write(
                        core::ptr::addr_of_mut!((*head.as_mut_ptr()).name),
                        core::ptr::null_mut(),
                    );
                    core::ptr::write(
                        core::ptr::addr_of_mut!((*head.as_mut_ptr()).comment),
                        core::ptr::null_mut(),
                    );
                }

                head
            }

            fn initialize_header_buffers(
                extra: &mut [u8],
                name: &mut [u8],
                comment: &mut [u8],
            ) -> MaybeUninit<gz_header> {
                let mut head = MaybeUninit::<gz_header>::zeroed();

                unsafe {
                    core::ptr::write(
                        core::ptr::addr_of_mut!((*head.as_mut_ptr()).extra),
                        extra.as_mut_ptr(),
                    );
                    core::ptr::write(
                        core::ptr::addr_of_mut!((*head.as_mut_ptr()).extra_max),
                        extra.len() as _,
                    );
                    core::ptr::write(
                        core::ptr::addr_of_mut!((*head.as_mut_ptr()).name),
                        name.as_mut_ptr(),
                    );
                    core::ptr::write(
                        core::ptr::addr_of_mut!((*head.as_mut_ptr()).name_max),
                        extra.len() as _,
                    );
                    core::ptr::write(
                        core::ptr::addr_of_mut!((*head.as_mut_ptr()).comment),
                        comment.as_mut_ptr(),
                    );
                    core::ptr::write(
                        core::ptr::addr_of_mut!((*head.as_mut_ptr()).comm_max),
                        extra.len() as _,
                    );
                }

                head
            }

            let err = inflateInit2_(
                strm.as_mut_ptr(),
                config.window_bits,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            );
            assert_eq!(err, Z_OK);

            let strm = strm.assume_init_mut();

            {
                let mut head = initialize_header_null();

                let err = inflateGetHeader(strm, head.as_mut_ptr());
                assert_eq!(err, Z_OK);

                let mut dest = [0u8; 100];

                strm.next_in = FERRIS_BYTES_GZ_NO_HEADER.as_ptr() as *mut u8;
                strm.avail_in = FERRIS_BYTES_GZ_NO_HEADER.len() as _;

                strm.next_out = dest.as_mut_ptr();
                strm.avail_out = dest.len() as _;

                let err = inflate(strm, Z_FINISH);
                assert_eq!(err, Z_STREAM_END);

                assert_eq!(&dest[..strm.total_out as usize], b"Ferris");

                // zlib-rs will properly initialize this struct, zlib-ng does not!
                if cfg!(miri) {
                    let _ = head.assume_init();
                }
            }

            inflateReset(strm);

            {
                let mut name_buf = [0u8; 3];
                let mut extra_buf = [0u8; 3];
                let mut comment_buf = [0u8; 3];
                let mut head =
                    initialize_header_buffers(&mut name_buf, &mut extra_buf, &mut comment_buf);

                let err = inflateGetHeader(strm, head.as_mut_ptr());
                assert_eq!(err, Z_OK);

                let mut dest = [0u8; 100];

                strm.next_in = FERRIS_BYTES_GZ_NO_HEADER.as_ptr() as *mut u8;
                strm.avail_in = FERRIS_BYTES_GZ_NO_HEADER.len() as _;

                strm.next_out = dest.as_mut_ptr();
                strm.avail_out = dest.len() as _;

                let err = inflate(strm, Z_FINISH);
                assert_eq!(err, Z_STREAM_END);

                assert_eq!(&dest[..strm.total_out as usize], b"Ferris");

                assert_eq!(extra_buf, [0u8; 3]);
                assert_eq!(name_buf, [0u8; 3]);
                assert_eq!(comment_buf, [0u8; 3]);

                // zlib-rs will properly initialize this struct, zlib-ng does not!
                if cfg!(miri) {
                    let _ = head.assume_init();
                }
            }

            inflateReset(strm);

            {
                let mut head = initialize_header_null();

                let err = inflateGetHeader(strm, head.as_mut_ptr());
                assert_eq!(err, Z_OK);

                let mut dest = [0u8; 100];

                strm.next_in = FERRIS_BYTES_GZ_WITH_HEADER.as_ptr() as *mut u8;
                strm.avail_in = FERRIS_BYTES_GZ_WITH_HEADER.len() as _;

                strm.next_out = dest.as_mut_ptr();
                strm.avail_out = dest.len() as _;

                let err = inflate(strm, Z_FINISH);
                assert_eq!(err, Z_STREAM_END);

                assert_eq!(&dest[..strm.total_out as usize], b"Ferris");

                // zlib-rs will properly initialize this struct, zlib-ng does not!
                if cfg!(miri) {
                    let _ = head.assume_init();
                }
            };

            inflateReset(strm);

            {
                let mut name_buf = [0u8; 3];
                let mut extra_buf = [0u8; 3];
                let mut comment_buf = [0u8; 3];
                let mut head =
                    initialize_header_buffers(&mut extra_buf, &mut name_buf, &mut comment_buf);

                let err = inflateGetHeader(strm, head.as_mut_ptr());
                assert_eq!(err, Z_OK);

                let mut dest = [0u8; 100];

                strm.next_in = FERRIS_BYTES_GZ_WITH_HEADER.as_ptr() as *mut u8;
                strm.avail_in = FERRIS_BYTES_GZ_WITH_HEADER.len() as _;

                strm.next_out = dest.as_mut_ptr();
                strm.avail_out = dest.len() as _;

                let err = inflate(strm, Z_FINISH);
                assert_eq!(err, Z_STREAM_END);

                assert_eq!(&dest[..strm.total_out as usize], b"Ferris");

                assert_eq!(&extra_buf, b"ban");
                assert_eq!(&name_buf, b"app");
                assert_eq!(&comment_buf, b"pea");

                // zlib-rs will properly initialize this struct, zlib-ng does not!
                if cfg!(miri) {
                    let _ = head.assume_init();
                }
            };

            let err = inflateEnd(strm);
            assert_eq!(err, Z_OK);
        });
    }

    #[test]
    fn inflate_mark() {
        assert_eq_rs_ng!({ inflateMark(core::ptr::null_mut()) });
    }

    #[test]
    fn inflate_prime() {
        assert_eq_rs_ng!({ inflatePrime(core::ptr::null_mut(), 1, 2) });
    }

    #[test]
    fn inflate_reset() {
        assert_eq_rs_ng!({ inflateReset(core::ptr::null_mut()) });
        assert_eq_rs_ng!({ inflateReset2(core::ptr::null_mut(), 10) });
    }

    #[test]
    fn inflate_set_dictionary() {
        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::<z_stream>::zeroed();
            inflateInit_(
                strm.as_mut_ptr(),
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            );
            let strm = unsafe { strm.assume_init_mut() };

            let dictionary = b"foobarbaz";

            let a = inflateSetDictionary(strm, core::ptr::null(), dictionary.len() as _);
            let b = inflateSetDictionary(
                core::ptr::null_mut(),
                dictionary.as_ptr(),
                dictionary.len() as _,
            );
            let c = inflateSetDictionary(strm, dictionary.as_ptr(), dictionary.len() as _);

            let d = inflateEnd(strm);

            (a, b, c, d)
        });
    }

    #[test]
    fn inflate_sync() {
        assert_eq_rs_ng!({ inflateSync(core::ptr::null_mut()) });
    }

    #[test]
    fn inflate_undocumented() {
        // public but undocumented
        assert_eq!(
            unsafe { libz_rs_sys::inflateResetKeep(core::ptr::null_mut()) },
            libz_rs_sys::Z_STREAM_ERROR,
        );
        assert_eq!(
            unsafe { libz_rs_sys::inflateSyncPoint(core::ptr::null_mut()) },
            libz_rs_sys::Z_STREAM_ERROR,
        );
        assert_eq!(
            unsafe { libz_rs_sys::inflateUndermine(core::ptr::null_mut(), 16) },
            libz_rs_sys::Z_STREAM_ERROR,
        );
    }

    #[test]
    fn deflate_init() {
        assert_eq_rs_ng!({
            deflateInit_(
                core::ptr::null_mut(),
                6,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            )
        });

        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::<z_stream>::zeroed();
            deflateInit_(
                strm.as_mut_ptr(),
                6,
                core::ptr::null(),
                core::mem::size_of::<z_stream>() as _,
            )
        });
    }

    #[test]
    fn deflate() {
        assert_eq_rs_ng!({ deflate(core::ptr::null_mut(), Z_NO_FLUSH) });
    }

    #[test]
    fn deflate_end() {
        assert_eq_rs_ng!({ deflateEnd(core::ptr::null_mut()) });
        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::<z_stream>::zeroed();
            deflateEnd(strm.as_mut_ptr())
        });
    }

    #[test]
    fn deflate_copy() {
        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::<z_stream>::zeroed();
            deflateCopy(core::ptr::null_mut(), strm.as_mut_ptr())
        });
        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::<z_stream>::zeroed();
            deflateCopy(strm.as_mut_ptr(), core::ptr::null_mut())
        });
    }

    #[test]
    #[cfg_attr(
        target_endian = "big",
        ignore = "we don't support DFLTCC, which changes the bounds in zlib-ng"
    )]
    fn deflate_bound() {
        assert_eq_rs_ng!({ deflateBound(core::ptr::null_mut(), 1024) });
    }

    #[test]
    fn deflate_params() {
        assert_eq_rs_ng!({ deflateParams(core::ptr::null_mut(), 6, 0) });

        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::<z_stream>::zeroed();
            deflateInit_(
                strm.as_mut_ptr(),
                6,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            );

            // invalid level and strategy
            let a = deflateParams(strm.as_mut_ptr(), 123, 0);
            let b = deflateParams(strm.as_mut_ptr(), 6, 123);

            let c = deflateEnd(strm.as_mut_ptr());

            (a, b, c)
        });
    }

    #[test]
    fn deflate_tune() {
        assert_eq_rs_ng!({ deflateTune(core::ptr::null_mut(), 1, 2, 3, 4) });
    }

    #[test]
    fn deflate_set_header() {
        assert_eq_rs_ng!({
            let mut head = MaybeUninit::<gz_header>::zeroed();

            deflateSetHeader(core::ptr::null_mut(), head.as_mut_ptr())
        });

        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::<z_stream>::zeroed();

            deflateInit_(
                strm.as_mut_ptr(),
                6,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            );

            let a = deflateSetHeader(strm.as_mut_ptr(), core::ptr::null_mut());

            let b = deflateEnd(strm.as_mut_ptr());

            (a, b)
        });
    }

    #[test]
    fn deflate_set_dictionary() {
        let dictionary = b"foobarbaz";

        assert_eq_rs_ng!({
            deflateSetDictionary(
                core::ptr::null_mut(),
                dictionary.as_ptr(),
                dictionary.len() as _,
            )
        });

        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::<z_stream>::zeroed();
            deflateInit_(
                strm.as_mut_ptr(),
                0,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            );
            let strm = unsafe { strm.assume_init_mut() };

            let a = deflateSetDictionary(strm, core::ptr::null(), dictionary.len() as _);
            let b = deflateSetDictionary(strm, dictionary.as_ptr(), dictionary.len() as _);

            let c = deflateEnd(strm);

            (a, b, c)
        });
    }

    #[test]
    fn deflate_reset() {
        assert_eq_rs_ng!({ deflateReset(core::ptr::null_mut()) });
    }

    #[test]
    fn deflate_prime() {
        assert_eq_rs_ng!({ deflatePrime(core::ptr::null_mut(), 1, 2) });
    }

    #[test]
    fn deflate_pending() {
        // libz_sys does not yet have this function
        unsafe {
            assert_eq!(
                libz_rs_sys::deflatePending(core::ptr::null_mut(), &mut 0, &mut 0),
                libz_rs_sys::Z_STREAM_ERROR,
            );
        }

        assert_eq!(
            unsafe {
                use libz_rs_sys::*;

                let mut strm = MaybeUninit::<z_stream>::zeroed();
                deflateInit_(
                    strm.as_mut_ptr(),
                    0,
                    zlibVersion(),
                    core::mem::size_of::<z_stream>() as _,
                );
                let strm = strm.assume_init_mut();

                let a = deflatePending(strm, core::ptr::null_mut(), &mut 0);
                let b = deflatePending(strm, &mut 0, core::ptr::null_mut());

                let c = deflateEnd(strm);

                (a, b, c)
            },
            (0, 0, 0)
        );
    }

    pub const FERRIS_BYTES: [u8; 14] = [120u8, 156, 115, 75, 45, 42, 202, 44, 6, 0, 8, 6, 2, 108];

    #[test]
    fn ferris_bytes() {
        let input = b"Ferris";
        let mut buf = [0; 64];

        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::<z_stream>::zeroed();

            let err = deflateInit_(
                strm.as_mut_ptr(),
                6,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            );
            assert_eq!(err, Z_OK);

            let strm = strm.assume_init_mut();

            buf.fill(0);

            strm.next_in = input.as_ptr() as *mut u8;
            strm.avail_in = input.len() as _;

            strm.next_out = buf.as_mut_ptr();
            strm.avail_out = buf.len() as _;

            let err = deflate(strm, Z_FINISH);
            assert_eq!(err, Z_STREAM_END);

            let err = deflateEnd(strm);
            assert_eq!(err, Z_OK);

            assert_eq!(&buf[..strm.total_out as usize], FERRIS_BYTES);
        });
    }

    #[test]
    fn inflate_reset_after_inflate() {
        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::<z_stream>::zeroed();

            let err = inflateInit_(
                strm.as_mut_ptr(),
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            );
            assert_eq!(err, Z_OK);

            let strm = strm.assume_init_mut();

            {
                let mut dest = [0u8; 100];

                strm.next_in = FERRIS_BYTES.as_ptr() as *mut u8;
                strm.avail_in = FERRIS_BYTES.len() as _;

                strm.next_out = dest.as_mut_ptr();
                strm.avail_out = dest.len() as _;

                let err = inflate(strm, Z_FINISH);
                assert_eq!(err, Z_STREAM_END);

                assert_eq!(&dest[..strm.total_out as usize], b"Ferris");
            }

            let err = inflateReset(strm);
            assert_eq!(err, Z_OK);

            {
                let mut dest = [0u8; 100];

                strm.next_in = FERRIS_BYTES.as_ptr() as *mut u8;
                strm.avail_in = FERRIS_BYTES.len() as _;

                strm.next_out = dest.as_mut_ptr();
                strm.avail_out = dest.len() as _;

                let err = inflate(strm, Z_FINISH);
                assert_eq!(err, Z_STREAM_END);

                assert_eq!(&dest[..strm.total_out as usize], b"Ferris");
            }

            let err = inflateEnd(strm);
            assert_eq!(err, Z_OK);
        });
    }

    #[test]
    fn inflate_reset_keep_after_inflate() {
        use libz_rs_sys::*;

        unsafe {
            let mut strm = MaybeUninit::<z_stream>::zeroed();

            let err = inflateInit_(
                strm.as_mut_ptr(),
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            );
            assert_eq!(err, Z_OK);

            let strm = strm.assume_init_mut();

            {
                let mut dest = [0u8; 100];

                strm.next_in = FERRIS_BYTES.as_ptr() as *mut u8;
                strm.avail_in = FERRIS_BYTES.len() as _;

                strm.next_out = dest.as_mut_ptr();
                strm.avail_out = dest.len() as _;

                let err = inflate(strm, Z_FINISH);
                assert_eq!(err, Z_STREAM_END);

                assert_eq!(&dest[..strm.total_out as usize], b"Ferris");
            }

            let err = inflateResetKeep(strm);
            assert_eq!(err, Z_OK);

            {
                let mut dest = [0u8; 100];

                strm.next_in = FERRIS_BYTES.as_ptr() as *mut u8;
                strm.avail_in = FERRIS_BYTES.len() as _;

                strm.next_out = dest.as_mut_ptr();
                strm.avail_out = dest.len() as _;

                let err = inflate(strm, Z_FINISH);
                assert_eq!(err, Z_STREAM_END);

                assert_eq!(&dest[..strm.total_out as usize], b"Ferris");
            }

            let err = inflateEnd(strm);
            assert_eq!(err, Z_OK);
        }
    }

    #[test]
    fn inflate_init_uninitialized() {
        use libz_rs_sys::*;

        unsafe {
            let mut strm = MaybeUninit::<z_stream>::uninit();

            core::ptr::write(core::ptr::addr_of_mut!((*strm.as_mut_ptr()).avail_in), 0);
            core::ptr::write(
                core::ptr::addr_of_mut!((*strm.as_mut_ptr()).next_in),
                core::ptr::null(),
            );

            core::ptr::write(
                core::ptr::addr_of_mut!((*strm.as_mut_ptr()).zalloc).cast(),
                zlib_rs::allocate::C.zalloc,
            );

            core::ptr::write(
                core::ptr::addr_of_mut!((*strm.as_mut_ptr()).zfree).cast(),
                zlib_rs::allocate::C.zfree,
            );

            core::ptr::write(
                core::ptr::addr_of_mut!((*strm.as_mut_ptr()).opaque),
                core::ptr::null_mut(),
            );

            let err = inflateInit_(
                strm.as_mut_ptr(),
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            );
            assert_eq!(err, Z_OK);

            let _ = strm.assume_init_mut();

            let err = inflateEnd(strm.as_mut_ptr());
            assert_eq!(err, Z_OK);
        }
    }

    #[test]
    fn deflate_init_uninitialized() {
        use libz_rs_sys::*;

        unsafe {
            let mut strm = MaybeUninit::<z_stream>::uninit();

            core::ptr::write(
                core::ptr::addr_of_mut!((*strm.as_mut_ptr()).zalloc).cast(),
                zlib_rs::allocate::C.zalloc,
            );

            core::ptr::write(
                core::ptr::addr_of_mut!((*strm.as_mut_ptr()).zfree).cast(),
                zlib_rs::allocate::C.zfree,
            );

            core::ptr::write(
                core::ptr::addr_of_mut!((*strm.as_mut_ptr()).opaque),
                core::ptr::null_mut(),
            );

            let err = deflateInit_(
                strm.as_mut_ptr(),
                6,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            );
            assert_eq!(err, Z_OK);

            let err = deflateEnd(strm.as_mut_ptr());
            assert_eq!(err, Z_OK);
        }
    }

    #[test]
    fn inflate_with_uninitialized_output_buffer() {
        let input = include_bytes!("test-data/issue-109.gz");

        let config = InflateConfig::default();

        assert_eq_rs_ng!({
            let mut stream = MaybeUninit::<z_stream>::zeroed();

            const VERSION: *const c_char = libz_rs_sys::zlibVersion();
            const STREAM_SIZE: c_int = core::mem::size_of::<z_stream>() as c_int;

            let err = unsafe {
                inflateInit2_(
                    stream.as_mut_ptr(),
                    16 + config.window_bits,
                    VERSION,
                    STREAM_SIZE,
                )
            };
            assert_eq!(err, 0);

            let stream = unsafe { stream.assume_init_mut() };

            let mut output = [MaybeUninit::<u8>::uninit(); 256];
            stream.next_out = output.as_mut_ptr().cast();
            stream.avail_out = output.len() as _;

            let chunk = &input[..128];
            stream.next_in = chunk.as_ptr().cast_mut();
            stream.avail_in = chunk.len() as _;

            let err = unsafe { inflate(stream, InflateFlush::NoFlush as _) };
            assert_eq!(err, ReturnCode::Ok as i32);

            let err = unsafe { inflateEnd(stream) };
            assert_eq!(err, ReturnCode::Ok as i32);
        });
    }
}

#[cfg(test)]
mod coverage {
    use core::mem::MaybeUninit;

    #[test]
    fn adler32() {
        let input = [1, 2, 3, 4];
        assert_eq_rs_ng!({ adler32(0, input.as_ptr(), input.len() as _) });
    }

    #[test]
    fn deflate_init2_() {
        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::zeroed();

            let a = deflateInit2_(
                strm.as_mut_ptr(),
                6,
                8,  // method,
                15, // windowBits,
                8,  // memLevel,
                0,  // strategy,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            );

            let b = deflateEnd(strm.as_mut_ptr());

            (a, b)
        });

        // invalid method
        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::zeroed();

            deflateInit2_(
                strm.as_mut_ptr(),
                6,
                1024, // method,
                15,   // windowBits,
                8,    // memLevel,
                0,    // strategy,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            )
        });

        // invalid strategy
        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::zeroed();

            deflateInit2_(
                strm.as_mut_ptr(),
                6,
                8,    // method,
                15,   // windowBits,
                8,    // memLevel,
                1024, // strategy,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            )
        });

        // configure an explicit allocator
        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::<z_stream>::zeroed();

            core::ptr::write(
                core::ptr::addr_of_mut!((*strm.as_mut_ptr()).zalloc).cast(),
                zlib_rs::allocate::C.zalloc,
            );

            core::ptr::write(
                core::ptr::addr_of_mut!((*strm.as_mut_ptr()).zfree).cast(),
                zlib_rs::allocate::C.zfree,
            );

            let err = deflateInit2_(
                strm.as_mut_ptr(),
                6,
                8,  // method,
                15, // windowBits,
                8,  // memLevel,
                0,  // strategy,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            );
            assert_eq!(err, Z_OK);

            let err = deflateEnd(strm.as_mut_ptr());
            assert_eq!(err, Z_OK);
        });
    }

    #[test]
    fn error_message() {
        use core::ffi::{c_char, CStr};
        use libz_rs_sys::*;

        fn cstr<'a>(ptr: *const c_char) -> &'a [u8] {
            // SAFETY: we trust the input
            unsafe { CStr::from_ptr(ptr) }.to_bytes()
        }

        // defined error values give a short message
        assert_eq!(cstr(zError(Z_NEED_DICT)), b"need dictionary");
        assert_eq!(cstr(zError(Z_NEED_DICT)), b"need dictionary");
        assert_eq!(cstr(zError(Z_STREAM_END)), b"stream end");
        assert_eq!(cstr(zError(Z_OK)), b"");
        assert_eq!(cstr(zError(Z_ERRNO)), b"file error");
        assert_eq!(cstr(zError(Z_STREAM_ERROR)), b"stream error");
        assert_eq!(cstr(zError(Z_DATA_ERROR)), b"data error");
        assert_eq!(cstr(zError(Z_MEM_ERROR)), b"insufficient memory");
        assert_eq!(cstr(zError(Z_BUF_ERROR)), b"buffer error");
        assert_eq!(cstr(zError(Z_VERSION_ERROR)), b"incompatible version");

        // other inputs return an empty string
        assert_eq!(cstr(zError(1234)), b"");
    }

    #[test]
    fn compress2() {
        let input = b"Ferris";
        let mut buf = [0; 64];

        assert_eq_rs_ng!({
            let mut dest_len = buf.len() as _;
            buf.fill(0);
            let err = compress2(
                buf.as_mut_ptr() as *mut u8,
                &mut dest_len,
                input.as_ptr() as *mut u8,
                input.len() as _,
                6,
            );

            (err, dest_len)
        });
    }

    #[test]
    fn deflate_invalid_flush() {
        let input = b"Ferris";
        let mut buf = [0; 64];

        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::<z_stream>::zeroed();

            let err = deflateInit_(
                strm.as_mut_ptr(),
                6,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            );
            assert_eq!(err, Z_OK);

            let strm = strm.assume_init_mut();

            strm.next_in = input.as_ptr() as *mut u8;
            strm.avail_in = input.len() as _;

            strm.next_out = buf.as_mut_ptr();
            strm.avail_out = buf.len() as _;

            let value = deflate(strm, 1234);

            let err = deflateEnd(strm);
            assert_eq!(err, Z_OK);

            value
        });
    }

    #[test]
    fn deflate_insufficient_output_space() {
        let input = b"Ferris";
        let mut buf = [0; 5];

        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::<z_stream>::zeroed();

            let err = deflateInit_(
                strm.as_mut_ptr(),
                6,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            );
            assert_eq!(err, Z_OK);

            let strm = strm.assume_init_mut();

            strm.next_in = input.as_ptr() as *mut u8;
            strm.avail_in = input.len() as _;

            strm.next_out = buf.as_mut_ptr();
            strm.avail_out = buf.len() as _;

            // Z_NO_FLUSH keeps the state in `Status::Busy`
            let err = deflate(strm, Z_NO_FLUSH);
            assert_eq!(err, Z_OK);

            let err = deflateEnd(strm);
            assert_eq!(err, Z_DATA_ERROR);

            err
        });
    }

    #[test]
    fn deflate_reset_after_deflate() {
        let input = b"Ferris";

        assert_eq_rs_ng!({
            let mut strm = MaybeUninit::<z_stream>::zeroed();

            let err = deflateInit_(
                strm.as_mut_ptr(),
                6,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            );
            assert_eq!(err, Z_OK);

            let strm = strm.assume_init_mut();

            {
                let mut buf = [0; 64];

                strm.next_in = input.as_ptr() as *mut u8;
                strm.avail_in = input.len() as _;

                strm.next_out = buf.as_mut_ptr();
                strm.avail_out = buf.len() as _;

                let err = deflate(strm, Z_FINISH);
                assert_eq!(err, Z_STREAM_END);

                assert_eq!(&buf[..strm.total_out as usize], super::null::FERRIS_BYTES);
            }

            let err = deflateReset(strm);
            assert_eq!(err, Z_OK);

            {
                let mut buf = [0; 64];

                strm.next_in = input.as_ptr() as *mut u8;
                strm.avail_in = input.len() as _;

                strm.next_out = buf.as_mut_ptr();
                strm.avail_out = buf.len() as _;

                let err = deflate(strm, Z_FINISH);
                assert_eq!(err, Z_STREAM_END,);

                assert_eq!(&buf[..strm.total_out as usize], super::null::FERRIS_BYTES);
            }

            deflateEnd(strm);
        });
    }
}

#[test]
fn deflate_chunked_input_all_levels() {
    use std::io::{Read, Write};
    use std::mem::MaybeUninit;

    assert_eq_rs_ng!({
        // translated from zpipe.c
        fn def<R: Read, W: Write, const CHUNK: usize>(
            source: &mut R,
            dest: &mut W,
            level: i32,
        ) -> i32 {
            let mut strm = MaybeUninit::zeroed();

            let mut in_buf = [0u8; CHUNK];
            let mut out_buf = [0u8; CHUNK];

            let mut ret;

            ret = unsafe {
                deflateInit_(
                    strm.as_mut_ptr(),
                    level,
                    zlibVersion(),
                    core::mem::size_of::<z_stream>() as _,
                )
            };
            if ret != Z_OK {
                return ret;
            }

            let strm = unsafe { strm.assume_init_mut() };

            loop {
                strm.avail_in = match source.read(&mut in_buf) {
                    Ok(0) => 0,
                    Ok(n) => n as u32,
                    Err(_) => {
                        unsafe { deflateEnd(strm) };
                        return Z_ERRNO;
                    }
                };
                strm.next_in = in_buf.as_mut_ptr();

                let flush = if strm.avail_in == 0 {
                    Z_FINISH
                } else {
                    Z_NO_FLUSH
                };

                loop {
                    strm.avail_out = CHUNK as u32;
                    strm.next_out = out_buf.as_mut_ptr();

                    ret = unsafe { deflate(strm, flush) };
                    assert_ne!(ret, Z_STREAM_ERROR);

                    let have = CHUNK - strm.avail_out as usize;
                    if dest.write_all(&out_buf[..have]).is_err() {
                        unsafe { deflateEnd(strm) };
                        return Z_ERRNO;
                    }

                    if strm.avail_out != 0 {
                        break;
                    }
                }

                if flush == Z_FINISH {
                    break;
                }
            }

            assert_eq!(ret, Z_STREAM_END);

            unsafe { deflateEnd(strm) };

            Z_OK
        }

        fn run<const CHUNK: usize>(level: i32) -> Vec<u8> {
            let input: Vec<_> = (0..4096).map(|x| x as u8).collect();
            let mut output = Vec::new();

            def::<_, _, CHUNK>(&mut input.as_slice(), &mut output, level);

            output
        }

        let mut outputs = Vec::new();

        for level in -1..=9 {
            outputs.push((level, 4, run::<{ 4 }>(level)));
            outputs.push((level, 1 << 10, run::<{ 1 << 10 }>(level)));
            outputs.push((level, 1 << 12, run::<{ 1 << 12 }>(level)));
            outputs.push((level, 1 << 14, run::<{ 1 << 14 }>(level)));
        }

        outputs
    });
}

#[test]
fn inflate_strict_is_off() {
    // this input will hit the `invalid distance too far back` error message when INFLATE_STRICT is
    // enabled. This flag is off by default.
    let source = [
        56, 203, 203, 203, 203, 203, 203, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131,
        131, 131, 131, 131, 131, 131, 131, 131, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 134,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 3, 131, 131,
        131, 131, 135, 135, 135, 135, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 134, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 131, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        134, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 3, 131, 131, 131, 131, 135, 56, 17, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 134,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 61, 135, 135, 135, 135, 135, 135, 135, 134, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 131, 203, 203, 203, 131, 131, 131, 203,
        135, 135, 135, 135, 135, 135, 135, 135, 134, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 131, 203, 203, 203, 131, 131, 131, 203, 135, 135, 135, 135, 135, 135, 135, 135,
        134, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 3, 131, 131, 131, 131, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 134, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 3,
        131, 131, 131, 131, 135, 135, 135, 135, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131,
        131, 131, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 134, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 131, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 134, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 3, 131, 131, 131, 131, 135, 56, 17, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 134, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 61, 135, 135, 135, 135, 135, 135, 135,
        134, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 131, 203, 203, 203, 131, 131,
        131, 203, 135, 135, 135, 135, 135, 135, 135, 135, 134, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 131, 203, 203, 203, 131, 131, 131, 203, 135, 135, 135, 135, 135, 135,
        135, 135, 134, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 3, 131, 131, 131, 131, 135, 135, 135, 135, 131, 131, 131, 131,
        131, 131, 131, 131, 131, 131, 131, 131, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 134,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 131, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 134, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 3, 131, 131, 131, 131, 135, 56, 17,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 134, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 61, 135,
        135, 135, 135, 135, 135, 135, 134, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        131, 203, 203, 203, 131, 131, 131, 203, 135, 135, 135, 135, 135, 135, 135, 135, 134, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 131, 203, 203, 203, 131, 131, 135, 135,
        135, 135, 135, 134, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 3, 131, 131, 131, 131, 135, 56, 17, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 134, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 61, 135, 135, 135, 135, 135, 135,
        135, 134, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 131, 203, 203, 203, 131,
        131, 131, 203, 135, 135, 135, 135, 135, 135, 135, 135, 134, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 131, 203, 203, 203, 131, 131, 131, 203, 135, 135, 135, 135, 135,
        135, 135, 135, 134, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 3, 131, 131, 131, 131, 135, 135, 135, 135, 131, 131, 131,
        131, 131, 131, 131, 131, 131, 131, 131, 131, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
        134, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 121,
    ];

    assert_eq_rs_ng!({
        let mut dest = vec![0u8; 1 << 16];
        let mut dest_len = dest.len() as _;

        let err = unsafe {
            uncompress(
                dest.as_mut_ptr(),
                &mut dest_len,
                source.as_ptr(),
                source.len() as _,
            )
        };

        (zlib_rs::ReturnCode::from(err), dest_len)
    });
}

#[test]
fn deflate_stored_window_out_of_bounds() {
    use core::ffi::c_int;
    use zlib_rs::ReturnCode;

    let mut source = [
        161, 161, 161, 161, 179, 179, 179, 179, 179, 179, 133, 133, 133, 133, 13, 13, 13, 13, 13,
        13, 13, 13, 13, 13, 13, 13, 13, 13, 95, 95, 95, 255, 179, 179, 179, 179, 179, 179, 179,
        179, 179, 43, 179, 8, 179, 133, 133, 133, 133, 13, 13, 122, 13, 13, 13, 13, 13, 13, 13, 13,
        13, 3, 13, 13, 95, 95, 95, 13, 13, 255, 255, 255, 189, 189, 189, 189, 189, 189, 189, 0,
        189, 189, 189, 189, 189, 189, 189, 189, 189, 173, 189, 189, 189, 189, 189, 189, 189, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 95, 95, 95, 255, 255, 255, 255, 255, 255, 255, 255, 255, 95, 95,
        95, 95, 255, 255, 189, 189, 189, 189, 189, 173, 189, 189, 189, 189, 189, 189, 189, 255,
        255, 245, 255, 255, 255, 255, 255, 189, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189,
        189, 189, 189, 255, 255, 255, 255, 189, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
        13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 125, 125, 125, 125, 125, 39, 39, 39, 39,
        39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 125, 125, 125, 125, 125, 125,
        125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 255, 18, 125, 125,
        125, 69, 125, 125, 125, 125, 255, 255, 255, 255, 125, 125, 125, 125, 125, 63, 125, 125,
        125, 125, 125, 125, 125, 125, 125, 223, 223, 223, 223, 223, 223, 223, 223, 39, 39, 39, 39,
        39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 255, 255, 255,
        255, 255, 255, 13, 13, 13, 13, 13, 13, 13, 95, 95, 95, 255, 179, 179, 179, 179, 179, 179,
        179, 179, 179, 43, 179, 8, 179, 133, 133, 133, 133, 13, 13, 122, 13, 13, 13, 13, 13, 13,
        13, 13, 13, 3, 13, 13, 95, 95, 95, 13, 13, 255, 255, 255, 189, 189, 189, 189, 189, 189,
        189, 0, 189, 189, 189, 189, 189, 189, 189, 189, 189, 173, 189, 189, 189, 189, 189, 189,
        189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 255,
        255, 255, 255, 189, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
        13, 13, 13, 13, 13, 13, 13, 125, 125, 125, 125, 125, 39, 39, 39, 39, 39, 39, 39, 39, 39,
        39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125,
        125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 255, 18, 125, 125, 125, 69, 125, 125,
        125, 125, 255, 255, 255, 255, 125, 125, 125, 125, 125, 63, 125, 125, 125, 125, 125, 125,
        125, 125, 125, 223, 223, 223, 223, 223, 223, 223, 223, 39, 39, 39, 39, 39, 39, 39, 39, 39,
        39, 39, 39, 39, 39, 39, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 255, 255, 255, 255, 255, 255, 189,
        189, 189, 189, 189, 189, 183, 183, 183, 183, 183, 189, 189, 189, 189, 189, 189, 189, 189,
        189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 189, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 189, 189, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 189, 189, 189, 189, 189, 66, 189, 65, 65, 65,
        65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        158, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 95, 95, 95,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 95, 95, 95, 95, 255, 255, 189, 189, 189, 189,
        189, 173, 189, 189, 189, 189, 189, 189, 189, 255, 255, 245, 255, 255, 255, 255, 255, 189,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        189, 189, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 5, 5,
        5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 39,
        39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39,
        39, 39, 39, 39, 39, 255, 255, 255, 189, 189, 189, 189, 189, 255, 255, 255, 255, 255, 248,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 141,
        141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141,
        141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 255, 255, 255, 81, 126, 81, 81, 81,
        81, 81, 81, 81, 85, 5, 5, 5, 5, 5, 5, 5, 5, 49, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
        5, 5, 5, 5, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 189,
        189, 189, 189, 189, 189, 189, 255, 255, 189, 189, 189, 189, 189, 189, 19, 189, 189, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 95, 95, 95, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 95, 95, 95, 95, 255, 255, 189, 189, 189, 189, 189, 173, 189, 189,
        189, 189, 189, 189, 189, 255, 255, 245, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 189, 189, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 189, 171, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 199, 199, 199, 199, 199, 199,
        199, 199, 199, 199, 199, 199, 199, 199, 199, 199, 199, 199, 199, 199, 199, 199, 199, 199,
        199, 199, 199, 199, 199, 199, 199, 199, 199, 199, 199, 199, 199, 199, 199, 199, 199, 199,
        199, 199, 199, 199, 199, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 191, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 7, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 189, 189, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 81, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 36, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 40, 255, 255, 255,
        255, 255, 255, 81, 81, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 255, 255,
    ]
    .to_vec();

    assert_eq_rs_ng!({
        let mut strm = std::mem::MaybeUninit::<z_stream>::zeroed();
        let err = unsafe {
            deflateInit2_(
                strm.as_mut_ptr(),
                0,
                Z_DEFLATED,
                8,
                3,
                Z_HUFFMAN_ONLY,
                zlibVersion(),
                std::mem::size_of::<z_stream>() as c_int,
            )
        };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        let strm = strm.assume_init_mut();

        let buf_size = unsafe { deflateBound(strm, source.len() as _) };

        let mut dest = vec![0; buf_size as usize];
        let chunk = 305u32;
        let flush = Z_NO_FLUSH;

        strm.next_in = source.as_mut_ptr().cast();
        strm.avail_in = chunk; // First chunk.
        strm.next_out = dest.as_mut_ptr().cast();
        strm.avail_out = dest.len().try_into().unwrap();

        // Deflate first chunk.
        let err = unsafe { deflate(strm, flush) };
        assert_eq!(ReturnCode::from(err), ReturnCode::Ok);

        // Change the parameters.
        let new_level = 0;
        let new_strategy = Z_DEFAULT_STRATEGY;
        let err = unsafe { deflateParams(strm, new_level, new_strategy) };
        match ReturnCode::from(err) {
            ReturnCode::Ok => {}
            ReturnCode::BufError => {
                // Flushing the current pending data may run us out of buffer space.
                // Worst case double the buffer size.
                let add_space = Ord::min(chunk, buf_size as u32);
                dest.resize(dest.len() + add_space as usize, 0);

                // If extend() reallocates, it may have moved in memory.
                strm.next_out = dest.as_mut_ptr();
                strm.avail_out += add_space;

                let err = unsafe { deflateParams(strm, new_level, new_strategy) };
                assert_eq!(ReturnCode::from(err), ReturnCode::Ok);
            }
            err => panic!("fatal {err:?}"),
        }

        // Deflate the rest in chunks.
        let mut left = source.len() as uInt - chunk;
        while left > 0 {
            // Write the chunk.
            let avail = Ord::min(chunk, left);
            strm.avail_in = avail;
            let err = unsafe { deflate(strm, flush) };
            match ReturnCode::from(err) {
                ReturnCode::Ok => {
                    left -= avail;
                }
                ReturnCode::BufError => {
                    // Worst case double the buffer size.
                    let add_space = Ord::min(chunk, buf_size as uInt);
                    dest.resize(dest.len() + add_space as usize, 0);

                    // If extend() reallocates, it may have moved in memory.
                    strm.next_out = dest.as_mut_ptr();
                    strm.avail_out += add_space;

                    left -= avail - strm.avail_in;
                }
                err => panic!("fatal {err:?}"),
            }
        }

        assert_eq!(left, 0);

        let _ = unsafe { deflateEnd(strm) };
    });
}

#[test]
fn test_crc32_z() {
    assert_eq_rs_ng!({ crc32_z(0, core::ptr::null(), 0) });
    assert_eq_rs_ng!({ crc32_z(1, core::ptr::null(), 0) });
    assert_eq_rs_ng!({ crc32_z(2, core::ptr::null(), 32) });
}

#[test]
fn test_crc32_combine64() {
    use libz_rs_sys::z_off64_t;

    let a = 0x98AB_CDEF;
    let b = 0x1234_5678;

    assert_eq_rs_ng!({ crc32_combine64(a, b, 0 as z_off64_t) });
    assert_eq_rs_ng!({ crc32_combine64(a, b, 32 as z_off64_t) });
    assert_eq_rs_ng!({ crc32_combine64(a, b, i32::MAX as z_off64_t) });

    if core::mem::size_of::<z_off64_t>() == 8 {
        assert_eq_rs_ng!({ crc32_combine64(a, b, i64::MAX as z_off64_t) });
    }
}

#[test]
fn test_adler32_z() {
    assert_eq_rs_ng!({ adler32_z(0, core::ptr::null(), 0) });
    assert_eq_rs_ng!({ adler32_z(1, core::ptr::null(), 0) });
    assert_eq_rs_ng!({ adler32_z(2, core::ptr::null(), 32) });
}

#[test]
fn test_adler32_combine64() {
    use libz_rs_sys::z_off64_t;

    let a = 0x98AB_CDEF;
    let b = 0x1234_5678;

    assert_eq_rs_ng!({ adler32_combine64(a, b, -1 as z_off64_t) });
    assert_eq_rs_ng!({ adler32_combine64(a, b, 0 as z_off64_t) });
    assert_eq_rs_ng!({ adler32_combine64(a, b, 32 as z_off64_t) });
    assert_eq_rs_ng!({ adler32_combine64(a, b, i64::MAX as z_off64_t) });
}

#[test]
fn test_get_crc_table() {
    assert_eq_rs_ng!({ get_crc_table().read() });
}
