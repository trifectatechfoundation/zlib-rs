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
}
