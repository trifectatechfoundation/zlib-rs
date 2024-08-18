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

        if !cfg!(miri) {
            #[allow(unused_braces)]
            #[allow(unused_unsafe)]
            let ng = unsafe {
                use libz_sys::*;

                $tt
            };

            assert_eq!(rs, ng);
        }
    };
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

#[cfg(test)]
mod null {
    use core::mem::MaybeUninit;

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
    fn inflate_get_header_uninitialized() {
        const FERRIS_BYTES_GZ_NO_HEADER: [u8; 26] = [
            31, 139, 8, 0, 0, 0, 0, 0, 0, 3, 115, 75, 45, 42, 202, 44, 6, 0, 174, 148, 97, 210, 6,
            0, 0, 0,
        ];

        const FERRIS_BYTES_GZ_WITH_HEADER: &[u8] = &[
            31, 139, 8, 28, 0, 0, 0, 0, 0, 3, 6, 0, 98, 97, 110, 97, 110, 97, 97, 112, 112, 108,
            101, 0, 112, 101, 97, 114, 0, 115, 75, 45, 42, 202, 44, 6, 0, 174, 148, 97, 210, 6, 0,
            0, 0,
        ];

        let mut extra = *b"banana\0";
        let mut name = *b"apple\0";
        let mut comment = *b"pear\0";

        let input = b"Ferris";
        let mut buf = [0; 64];

        let config = zlib_rs::deflate::DeflateConfig {
            window_bits: 16 + 15,
            ..Default::default()
        };

        #[cfg(not(miri))]
        assert_eq_rs_ng!({
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
                let mut head = MaybeUninit::<gz_header>::uninit();

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
                let mut head = MaybeUninit::<gz_header>::uninit();

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

        #[cfg(not(miri))]
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

        #[cfg(not(miri))]
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

        #[cfg(not(miri))]
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

        #[cfg(not(miri))]
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
    #[cfg_attr(miri, ignore = "slow")]
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

            core::ptr::write(
                core::ptr::addr_of_mut!((*strm.as_mut_ptr()).zalloc).cast(),
                zlib_rs::allocate::Allocator::C.zalloc,
            );

            core::ptr::write(
                core::ptr::addr_of_mut!((*strm.as_mut_ptr()).zfree).cast(),
                zlib_rs::allocate::Allocator::C.zfree,
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
    #[cfg_attr(miri, ignore = "slow")]
    fn deflate_init_uninitialized() {
        use libz_rs_sys::*;

        unsafe {
            let mut strm = MaybeUninit::<z_stream>::uninit();

            core::ptr::write(
                core::ptr::addr_of_mut!((*strm.as_mut_ptr()).zalloc).cast(),
                zlib_rs::allocate::Allocator::C.zalloc,
            );

            core::ptr::write(
                core::ptr::addr_of_mut!((*strm.as_mut_ptr()).zfree).cast(),
                zlib_rs::allocate::Allocator::C.zfree,
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
    fn inflate_initialization_sets_allocator() {
        use libz_rs_sys::*;

        let mut strm = z_stream::default();
        strm.zalloc = None;
        strm.zfree = None;

        let ret = unsafe {
            inflateInit_(
                &mut strm,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            )
        };
        assert_eq!(ret, Z_OK);

        assert!(strm.zalloc.is_some());
        assert!(strm.zfree.is_some());

        let err = unsafe { inflateEnd(&mut strm) };
        assert_eq!(err, Z_OK);
    }

    #[test]
    fn inflate_initialization_touches_fields() {
        assert_eq_rs_ng!({
            let mut strm: z_stream = unsafe {
                let mut strm: MaybeUninit<z_stream> = MaybeUninit::zeroed();

                // in the libz_sys version of zlib-rs, the zalloc and zfree fields are function
                // pointers. NULL is not a valid value for a function pointer, so initializing the
                // zeroed struct is UB. We explicitly initialize these fields here.
                core::ptr::write(
                    core::ptr::addr_of_mut!((*strm.as_mut_ptr()).zalloc).cast(),
                    zlib_rs::allocate::Allocator::C.zalloc,
                );
                core::ptr::write(
                    core::ptr::addr_of_mut!((*strm.as_mut_ptr()).zfree).cast(),
                    zlib_rs::allocate::Allocator::C.zfree,
                );

                strm.assume_init()
            };

            let mut in_buf = *b"in";
            strm.next_in = in_buf.as_mut_ptr();
            strm.avail_in = in_buf.len() as _;

            let mut out_buf = *b"out";
            strm.next_out = out_buf.as_mut_ptr();
            strm.avail_out = out_buf.len() as _;

            let mut msg_buf = b"error".map(|c| c as i8);
            strm.msg = msg_buf.as_mut_ptr();

            strm.total_in = 0xdeadbeef;
            strm.total_out = 0xdeadbeef;

            strm.data_type = -123;
            strm.adler = 0xdeadbeef;
            strm.reserved = 0xdeadbeef;

            let ret = inflateInit_(
                &mut strm,
                zlibVersion(),
                core::mem::size_of::<z_stream>() as _,
            );
            assert_eq!(ret, Z_OK);

            // left alone
            assert_eq!(strm.next_in, in_buf.as_mut_ptr());
            assert_eq!(strm.avail_in, in_buf.len() as _);

            // left alone
            assert_eq!(strm.next_out, out_buf.as_mut_ptr());
            assert_eq!(strm.avail_out, out_buf.len() as _);

            // cleared
            assert_eq!(strm.msg, core::ptr::null_mut());

            // initialized
            assert_ne!(strm.state, core::ptr::null_mut());

            // cleared
            assert_eq!(strm.total_in, 0);
            assert_eq!(strm.total_out, 0);

            // initialized
            assert_eq!(strm.adler, 1);

            // zlib-ng leaves these alone, zlib-rs clears them
            assert!(strm.data_type == -123 || strm.data_type == 0);
            assert!(strm.reserved == 0xdeadbeef || strm.reserved == 0);

            let err = inflateEnd(&mut strm);
            assert_eq!(err, Z_OK);
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
                zlib_rs::allocate::Allocator::C.zalloc,
            );

            core::ptr::write(
                core::ptr::addr_of_mut!((*strm.as_mut_ptr()).zfree).cast(),
                zlib_rs::allocate::Allocator::C.zfree,
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
