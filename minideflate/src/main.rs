use std::path::PathBuf;

use zlib_rs::{deflate::DeflateConfig, inflate::InflateConfig, Flush};

/* Default read/write i/o buffer size based on GZBUFSIZE */
const BUFSIZE: u32 = 131072;

fn show_help() {
    print!(concat!("Usage: minideflate [-c][-d][-k] [-f|-h|-R|-F] [-m level] [-r/-t size] [-s flush] [-w bits] [-0 to -9] [input file]\n\n",
           "  -c : write to standard output\n",
           "  -d : decompress\n",
           "  -k : keep input file\n",
           "  -f : compress with Z_FILTERED\n",
           "  -h : compress with Z_HUFFMAN_ONLY\n",
           "  -R : compress with Z_RLE\n",
           "  -F : compress with Z_FIXED\n",
           "  -m : memory level (1 to 8)\n",
           "  -w : window bits..\n",
           "     :   -1 to -15 for raw deflate\n",
           "     :    0 to  15 for deflate (adler32)\n",
           "     :   16 to  31 for gzip (crc32)\n",
           "  -s : flush type (0 to 5)\n",
           "  -r : read buffer size\n",
           "  -t : write buffer size\n",
           "  -0 to -9 : compression level\n\n"));
}

fn main() -> std::io::Result<()> {
    let mut it = std::env::args();

    let _program_name = it.next();

    actual_main(it)
}

fn actual_main(it: impl Iterator<Item = String>) -> std::io::Result<()> {
    let mut it = it.peekable();

    if it.peek().is_none() {
        show_help();
        std::process::exit(64); // EX_USAGE
    }

    // default to STDIN and STDOUT
    let mut fin = FileOrShell::Shell;
    let mut fout = FileOrShell::Shell;

    let mut mem_level = 8;
    let mut window_bits = None;
    let mut strategy = libz_rs_sys::Z_DEFAULT_STRATEGY;
    let mut level = libz_rs_sys::Z_DEFAULT_COMPRESSION;
    let mut read_buf_size = BUFSIZE;
    let mut write_buf_size = BUFSIZE;
    let mut flush = libz_rs_sys::Z_NO_FLUSH;
    let mut copyout = false;
    let mut uncompr = false;
    let mut keep = false;

    fn parse_level(opt_level: &str) -> Option<i32> {
        match opt_level.as_bytes() {
            [b'-', level @ b'0'..=b'9'] => Some((level - b'0') as i32),
            _ => None,
        }
    }

    let input_file_path = 'blk: {
        while let Some(arg) = it.next() {
            let has_another_argument = it.peek().is_some();

            match arg.as_str() {
                "-m" if has_another_argument => {
                    mem_level = it.next().unwrap().parse().unwrap();
                }
                "-w" if has_another_argument => {
                    window_bits = Some(it.next().unwrap().parse().unwrap());
                }
                "-r" if has_another_argument => {
                    read_buf_size = it.next().unwrap().parse().unwrap();
                }
                "-t" if has_another_argument => {
                    write_buf_size = it.next().unwrap().parse().unwrap();
                }
                "-s" if has_another_argument => {
                    flush = it.next().unwrap().parse().unwrap();
                }
                "-c" => {
                    copyout = true;
                }
                "-d" => {
                    uncompr = true;
                }
                "-k" => {
                    keep = true;
                }
                "-f" => {
                    strategy = libz_rs_sys::Z_FILTERED;
                }
                "-F" => {
                    strategy = libz_rs_sys::Z_FIXED;
                }
                "-h" => {
                    strategy = libz_rs_sys::Z_HUFFMAN_ONLY;
                }
                "-R" => {
                    strategy = libz_rs_sys::Z_RLE;
                }
                opt_level if parse_level(opt_level).is_some() => {
                    level = parse_level(opt_level).unwrap();
                }
                "--help" => {
                    show_help();
                    std::process::exit(0);
                }
                other if other.starts_with('-') => {
                    show_help();
                    std::process::exit(64); // EX_USAGE
                }
                _ => break 'blk arg,
            }
        }

        String::new()
    };

    if !input_file_path.is_empty() {
        let file = std::fs::File::open(&input_file_path)?;
        fin = FileOrShell::File(file);

        if !copyout {
            let output_file_path = if !uncompr {
                let extension = match window_bits {
                    None => "gz",
                    Some(window_bits) => {
                        if window_bits < 0 {
                            "zraw"
                        } else if window_bits > zlib_rs::MAX_WBITS {
                            "gz"
                        } else {
                            "z"
                        }
                    }
                };

                PathBuf::from(format!("{}.{}", input_file_path, extension))
            } else {
                if let Some((_, out_ext)) = input_file_path.rsplit_once('.') {
                    if out_ext.eq_ignore_ascii_case(".zraw") && window_bits.is_none() {
                        eprintln!("Must specifiy window bits for raw deflate stream")
                    }
                }

                std::path::PathBuf::from(&input_file_path).with_extension("")
            };

            fout = FileOrShell::File(
                std::fs::File::options()
                    .write(true)
                    .truncate(true)
                    .create(true)
                    .open(output_file_path)?,
            );
        }
    }

    const MAX_WBITS: i32 = 15; // 32kb LZ77 window

    let window_bits = window_bits.unwrap_or(
        /* Auto-detect wrapper for inflateInit */
        if uncompr { MAX_WBITS + 32 } else { MAX_WBITS },
    );

    if uncompr {
        let config = zlib_rs::inflate::InflateConfig { window_bits };

        unsafe {
            inflate_params(
                &mut fin,
                &mut fout,
                read_buf_size as usize,
                write_buf_size as usize,
                config,
                flush.try_into().unwrap(),
            )?
        };
    } else {
        let config = zlib_rs::deflate::DeflateConfig {
            level,
            method: zlib_rs::deflate::Method::Deflated,
            window_bits,
            mem_level,
            strategy: strategy.try_into().unwrap(),
        };

        unsafe {
            deflate_params(
                &mut fin,
                &mut fout,
                read_buf_size as usize,
                write_buf_size as usize,
                config,
                flush.try_into().unwrap(),
            )?
        };
    }

    if let FileOrShell::File(file) = fin {
        drop(file);
        if !copyout && !keep {
            std::fs::remove_file(input_file_path)?;
        }
    }

    drop(fout);

    Ok(())
}

unsafe fn deflate_params(
    mut fin: impl std::io::Read,
    mut fout: impl std::io::Write,
    read_buf_size: usize,
    write_buf_size: usize,
    config: DeflateConfig,
    flush: Flush,
) -> std::io::Result<()> {
    let mut read_buf = vec![0; read_buf_size];
    let mut write_buf = vec![0; write_buf_size];

    let mut c_stream = libz_rs_sys::z_stream {
        next_out: write_buf.as_mut_ptr(),
        avail_out: write_buf_size as _,
        ..Default::default()
    };

    let mut err = libz_rs_sys::deflateInit2_(
        &mut c_stream,
        config.level,
        config.method as i32,
        config.window_bits,
        config.mem_level,
        config.strategy as i32,
        libz_rs_sys::zlibVersion(),
        core::mem::size_of::<libz_rs_sys::z_stream>() as _,
    );
    assert_eq!(err, libz_rs_sys::Z_OK);

    while err == libz_rs_sys::Z_OK {
        let read = fin.read(&mut read_buf)?;

        if read == 0 {
            break;
        }

        c_stream.next_in = read_buf.as_ptr();
        c_stream.avail_in = read as _;

        loop {
            err = libz_rs_sys::deflate(&mut c_stream, flush as i32);
            if err == libz_rs_sys::Z_STREAM_END {
                break;
            }
            assert_eq!(err, libz_rs_sys::Z_OK);

            if c_stream.next_out == write_buf.as_mut_ptr().add(write_buf_size) {
                fout.write_all(&write_buf)?;
                c_stream.next_out = write_buf.as_mut_ptr();
                c_stream.avail_out = write_buf_size as _;
            }

            if c_stream.next_in >= read_buf.as_ptr().add(read) {
                break;
            }
        }
    }

    // Finish the stream if necessary
    if flush as i32 != libz_rs_sys::Z_FINISH {
        c_stream.avail_in = 0;
        loop {
            if c_stream.next_out == write_buf.as_mut_ptr().add(write_buf_size) {
                fout.write_all(&write_buf)?;
                c_stream.next_out = write_buf.as_mut_ptr();
                c_stream.avail_out = write_buf_size as _;
            }

            err = libz_rs_sys::deflate(&mut c_stream, libz_rs_sys::Z_FINISH);
            if err == libz_rs_sys::Z_STREAM_END {
                break;
            }
            assert_eq!(err, libz_rs_sys::Z_OK);
        }
    }

    // Output remaining data in write buffer
    if c_stream.next_out != write_buf.as_mut_ptr() {
        let remaining = c_stream.next_out as usize - write_buf.as_ptr() as usize;
        fout.write_all(&write_buf[..remaining])?;
    }

    err = libz_rs_sys::deflateEnd(&mut c_stream);
    assert_eq!(err, libz_rs_sys::Z_OK);

    Ok(())
}

unsafe fn inflate_params(
    mut fin: impl std::io::Read,
    mut fout: impl std::io::Write,
    read_buf_size: usize,
    write_buf_size: usize,
    config: InflateConfig,
    flush: Flush,
) -> std::io::Result<()> {
    let mut read_buf = vec![0; read_buf_size];
    let mut write_buf = vec![0; write_buf_size];

    let mut d_stream = libz_rs_sys::z_stream {
        next_out: write_buf.as_mut_ptr(),
        avail_out: write_buf_size as _,
        ..Default::default()
    };

    let mut err = libz_rs_sys::inflateInit2(&mut d_stream, config.window_bits);
    assert_eq!(err, libz_rs_sys::Z_OK);

    while err == libz_rs_sys::Z_OK {
        let read = fin.read(&mut read_buf)?;

        if read == 0 {
            break;
        }

        d_stream.next_in = read_buf.as_ptr();
        d_stream.avail_in = read as _;

        loop {
            err = libz_rs_sys::inflate(&mut d_stream, flush as i32);
            if err == libz_rs_sys::Z_STREAM_END {
                break;
            }
            assert_eq!(err, libz_rs_sys::Z_OK);

            if d_stream.next_out == write_buf.as_mut_ptr().add(write_buf_size) {
                fout.write_all(&write_buf)?;
                d_stream.next_out = write_buf.as_mut_ptr();
                d_stream.avail_out = write_buf_size as _;
            }

            if d_stream.next_in >= read_buf.as_ptr().add(read) {
                break;
            }
        }
    }

    // Finish the stream if necessary
    if flush as i32 != libz_rs_sys::Z_FINISH {
        d_stream.avail_in = 0;
        loop {
            if d_stream.next_out == write_buf.as_mut_ptr().add(write_buf_size) {
                fout.write_all(&write_buf)?;
                d_stream.next_out = write_buf.as_mut_ptr();
                d_stream.avail_out = write_buf_size as _;
            }

            err = libz_rs_sys::inflate(&mut d_stream, libz_rs_sys::Z_FINISH);
            if err == libz_rs_sys::Z_STREAM_END {
                break;
            }
            assert_eq!(err, libz_rs_sys::Z_OK);
        }
    }

    // Output remaining data in write buffer
    if d_stream.next_out != write_buf.as_mut_ptr() {
        let remaining = d_stream.next_out as usize - write_buf.as_ptr() as usize;
        fout.write_all(&write_buf[..remaining])?;
    }

    err = libz_rs_sys::inflateEnd(&mut d_stream);
    assert_eq!(err, libz_rs_sys::Z_OK);

    Ok(())
}

#[derive(Debug)]
enum FileOrShell {
    File(std::fs::File),
    Shell,
}

impl std::io::Read for FileOrShell {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match self {
            FileOrShell::File(file) => file.read(buf),
            FileOrShell::Shell => std::io::stdin().read(buf),
        }
    }
}

impl std::io::Write for FileOrShell {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            FileOrShell::File(file) => file.write(buf),
            FileOrShell::Shell => std::io::stdout().write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            FileOrShell::File(file) => file.flush(),
            FileOrShell::Shell => std::io::stdout().flush(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::actual_main;

    fn minideflate<'a>(it: impl IntoIterator<Item = &'a str>) -> std::io::Result<()> {
        actual_main(it.into_iter().map(|s| s.to_string()))
    }

    #[test]
    fn end_to_end() -> std::io::Result<()> {
        let contents = include_str!("main.rs");

        let path = std::env::temp_dir().join("minideflate-test.txt");
        std::fs::write(&path, contents)?;

        minideflate(["-k", path.to_str().unwrap()])?;

        std::fs::remove_file(&path).unwrap();

        let compressed_path = path.with_extension("txt.gz");
        minideflate(["-d", "-k", compressed_path.to_str().unwrap()])?;

        std::fs::remove_file(&compressed_path).unwrap();

        Ok(())
    }
}
