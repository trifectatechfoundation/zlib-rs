//! a binary just so we can look at the optimized assembly

fn main() {
    let mut it = std::env::args();

    let _ = it.next().unwrap();

    match it.next().unwrap().as_str() {
        "generic" => {
            let path = it.next().unwrap();
            let input = std::fs::read(path).unwrap();

            zlib_rs::crc32_generic::<5>(&input, 0);
        }

        "sse" => {
            let path = it.next().unwrap();
            let input = std::fs::read(path).unwrap();

            println!("{:#x}", zlib_rs::crc32_pclmulqdq(&input, 0));
        }

        "crc32fast" => {
            let path = it.next().unwrap();
            let input = std::fs::read(path).unwrap();

            let mut h = crc32fast::Hasher::new_with_initial(0);
            h.update(&input[..]);
            println!("{:#x}", h.finalize());
        }
        other => panic!("invalid option '{other}', expected one of 'rs' or 'ng'"),
    }
}
