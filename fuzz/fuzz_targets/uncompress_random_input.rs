#![no_main]
use libfuzzer_sys::fuzz_target;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(i32)]
enum ReturnCode {
    Ok = 0,
    StreamEnd = 1,
    NeedDict = 2,
    ErrNo = -1,
    StreamError = -2,
    DataError = -3,
    MemError = -4,
    BufError = -5,
    VersionError = -6,
}

unsafe fn uncompress(
    dest: *mut u8,
    dest_len: *mut std::ffi::c_ulong,
    source: *const u8,
    source_len: std::ffi::c_ulong,
) -> std::ffi::c_int {
    let lib = libloading::Library::new("/home/folkertdev/rust/zlib-ng/libz-ng.so").unwrap();

    type Func = unsafe extern "C" fn(
        dest: *mut u8,
        dest_len: *mut std::ffi::c_ulong,
        source: *const u8,
        source_len: std::ffi::c_ulong,
    ) -> std::ffi::c_int;

    let f: libloading::Symbol<Func> = lib.get(b"zng_uncompress").unwrap();

    f(dest, dest_len, source, source_len)
}

fuzz_target!(|source: Vec<u8>| {
    let mut dest_ng = vec![0u8; 1 << 16];
    let mut dest_rs = vec![0u8; 1 << 16];

    let mut dest_len_ng = dest_ng.len() as _;
    let mut dest_len_rs = dest_rs.len() as _;

    let err_ng = unsafe {
        uncompress(
            dest_ng.as_mut_ptr(),
            &mut dest_len_ng,
            source.as_ptr(),
            source.len() as _,
        )
    };
    let err_rs = unsafe {
        ::zlib::uncompress(
            dest_rs.as_mut_ptr(),
            &mut dest_len_rs,
            source.as_ptr(),
            source.len() as _,
        )
    };

    let err_ng = unsafe { std::mem::transmute::<_, ReturnCode>(err_ng) };
    let err_rs = unsafe { std::mem::transmute::<_, ReturnCode>(err_rs) };

    assert_eq!(err_ng, err_rs);

    if err_ng == ReturnCode::Ok {
        dest_ng.truncate(dest_len_ng as usize);
        dest_rs.truncate(dest_len_rs as usize);

        assert_eq!(dest_ng, dest_rs);
    }
});
