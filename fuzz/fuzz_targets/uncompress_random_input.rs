#![no_main]
use libfuzzer_sys::fuzz_target;
use zlib_rs::ReturnCode;

fuzz_target!(|source: &[u8]| {
    let mut dest_ng = vec![0u8; 1 << 16];
    let mut dest_rs = vec![0u8; 1 << 16];

    let mut dest_len_ng = dest_ng.len() as _;
    let mut dest_len_rs = dest_rs.len() as _;

    let err_ng = unsafe {
        ::libz_ng_sys::uncompress(
            dest_ng.as_mut_ptr(),
            &mut dest_len_ng,
            source.as_ptr(),
            source.len() as _,
        )
    };

    let err_rs = unsafe {
        ::libz_rs_sys::uncompress(
            dest_rs.as_mut_ptr(),
            &mut dest_len_rs,
            source.as_ptr(),
            source.len() as _,
        )
    };

    let err_ng = ReturnCode::from(err_ng);
    let err_rs = ReturnCode::from(err_rs);

    assert_eq!(err_ng, err_rs);

    if err_ng == ReturnCode::Ok {
        dest_ng.truncate(dest_len_ng);
        dest_rs.truncate(dest_len_rs as usize);

        assert_eq!(dest_ng, dest_rs);
    }
});
