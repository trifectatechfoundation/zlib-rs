![checks](https://github.com/trifectatechfoundation/zlib-rs/actions/workflows/checks.yaml/badge.svg?branch=main)
[![codecov](https://codecov.io/gh/trifectatechfoundation/zlib-rs/graph/badge.svg?token=KZLDE24YVL)](https://codecov.io/gh/trifectatechfoundation/zlib-rs)
[![Crates.io](https://img.shields.io/crates/v/libz-rs-sys.svg)](https://crates.io/crates/libz-rs-sys)


# zlib-rs: a safer zlib

This repository contains a Rust implementation of the zlib file format that is compatible with the zlib API. 

This repository contains two public crates:

* [zlib-rs](https://crates.io/crates/zlib-rs/), a Rust implementation based on [zlib](https://www.zlib.net/manual.html)
  with a safe rust API. This API is under development and still unstable.
* [libz-rs-sys](https://crates.io/crates/libz-rs-sys/), a zlib-compatible C API for usage in non-Rust
  applications.

## How to use zlib-rs in your project

zlib-rs can be used in both Rust and C projects.

### Rust projects

By far the easiest way to use zlib-rs is through the [flate2](https://crates.io/crates/flate2) crate, by simply enabling the `zlib-rs` feature gate. This will enable the `zlib-rs`
backend.

## C projects

zlib-rs can be built as a shared object file for usage by C programs that dynamically link to zlib. Please see the example in [libz-rs-sys-cdylib](https://github.com/trifectatechfoundation/zlib-rs/tree/main/libz-rs-sys-cdylib).

## Acknowledgment

This project is heavily based on the [zlib](https://github.com/madler/zlib) and
[zlib-ng](https://github.com/zlib-ng/zlib-ng) projects.

## About

zlib-rs is part of Trifecta Tech Foundation's [Data compression initiative](https://trifectatech.org/initiatives/data-compression/).

## History

The initial development of zlib-rs was started and funded by the [Internet Security Research Group](https://www.abetterinternet.org/) as part of the [Prossimo project](https://www.memorysafety.org/).
