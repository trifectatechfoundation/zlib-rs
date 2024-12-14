use zlib_rs::deflate::encode_static_len;

// Program to compute the lookup table STATIC_LENGTH_ENCODINGS
fn main() {
    const COLUMNS: usize = 4;
    let mut column: usize = 0;
    for lc in 0..256 {
        let (encoding, len) = encode_static_len(lc as u8);
        print!("h({:>4}, {:>2}), ", encoding, len);
        column += 1;
        if column == COLUMNS {
            println!("");
            column = 0;
        } else {
            print!(" ");
        }
    }
}
