# zlib-rs: a safer zlib
This repository contains a zlib API compatible Rust implementation of the zlib
file format. There are two public crates available:

* `zlib-rs`, a Rust implementation based on [zlib](https://www.zlib.net/manual.html)
  with a safe rust API
* `libz-rs-sys`, an unsafe zlib compatible C API for usage in non-Rust
  applications

## Acknowledgement

This project is heavily based on the [zlib](https://github.com/madler/zlib) and
[zlib-ng](https://github.com/zlib-ng/zlib-ng) projects.
