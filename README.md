![checks](https://github.com/trifectatechfoundation/zlib-rs/actions/workflows/checks.yaml/badge.svg?branch=main)
[![codecov](https://codecov.io/gh/trifectatechfoundation/zlib-rs/graph/badge.svg?token=KZLDE24YVL)](https://codecov.io/gh/trifectatechfoundation/zlib-rs)
[![Crates.io](https://img.shields.io/crates/v/libz-rs-sys.svg)](https://crates.io/crates/libz-rs-sys)


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

## About

zlib-rs is part of Trifecta Tech Foundation's [Data compression initiative](https://trifectatech.org/initiatives/data-compression/).

## History

The initial development of zlib-rs was started and funded by the [Internet Security Research Group](https://www.abetterinternet.org/) as part of the [Prossimo project](https://www.memorysafety.org/).
