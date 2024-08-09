//! a binary just so we can look at the optimized  assembly

// we use the libz_sys but configure zlib-ng in zlib compat mode
use libz_sys as libz_ng_sys;

fn main() {
    let mut it = std::env::args();

    let _ = it.next().unwrap();

    let mut dest_vec = vec![0u8; 1 << 28];

    let mut dest_len = dest_vec.len() as std::ffi::c_ulong;
    let dest = dest_vec.as_mut_ptr();

    match it.next().unwrap().as_str() {
        "ng" => {
            let path = it.next().unwrap();
            let input = std::fs::read(&path).unwrap();

            let source = input.as_ptr();
            let source_len = input.len() as _;

            let err = unsafe { libz_ng_sys::uncompress(dest, &mut dest_len, source, source_len) };
            assert_eq!(err, 0);
        }
        "rs" => {
            let path = it.next().unwrap();
            let input = std::fs::read(&path).unwrap();

            let source = input.as_ptr();
            let source_len = input.len() as _;

            let err = unsafe { ::libz_rs_sys::uncompress(dest, &mut dest_len, source, source_len) };
            assert_eq!(err, 0);
        }
        other => panic!("invalid option '{other}', expected one of 'rs' or 'ng'"),
    }
}
