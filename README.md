# zlib-rs: a safer zlib

This repository contains a Rust implementation of the zlib file format that is compatible with the zlib API. 

This repository contains two public crates:

* `zlib-rs`, a Rust implementation based on [zlib](https://www.zlib.net/manual.html)
  with a safe rust API
* `libz-rs-sys`, a zlib-compatible C API for usage in non-Rust
  applications

## Acknowledgement

This project is heavily based on the [zlib](https://github.com/madler/zlib) and
[zlib-ng](https://github.com/zlib-ng/zlib-ng) projects.
