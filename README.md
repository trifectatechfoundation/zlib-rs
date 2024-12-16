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

### C projects

zlib-rs can be built as a shared object file for usage by C programs that dynamically link to zlib. Please see the example in [libz-rs-sys-cdylib](https://github.com/trifectatechfoundation/zlib-rs/tree/main/libz-rs-sys-cdylib).

## Performance

Performance is generally on-par with [zlib-ng].

### Compiler Flags

Compiler flags that can be used to improve performance.

#### `-Ctarget-cpu=...`

Providing more information about the SIMD capabilities of the target machine can improve performance. E.g.

```
RUSTFLAGS="-Ctarget-cpu=native" cargo build --release ...
```

The resulting binary statically assumes the SIMD capabilities of the current machine.

Note: binaries built with `-Ctarget-cpu` almost certainly crash on systems that don't have the specified CPU! Only use this flag if you control how the binary is deployed, and can guarantee that the CPU assumptions are never violated.

#### `-Cllvm-args=-enable-dfa-jump-thread`

For best performance with very small input sizes, compile with:

```
RUSTFLAGS="-Cllvm-args=-enable-dfa-jump-thread" cargo build --release ...
```

This flag gives around a 10% boost when the input arrives in chunks of 16 bytes, and a couple percent when input arrives in chunks of under 1024 bytes. Beyond that, the effect is not significant. Using this flag can lead to longer compile times, but otherwise has no adverse effects.

## Acknowledgments

This project is heavily based on the [zlib](https://github.com/madler/zlib) and [zlib-ng] projects.

## About

zlib-rs is part of Trifecta Tech Foundation's [Data compression initiative](https://trifectatech.org/initiatives/data-compression/).

## History

The initial development of zlib-rs was started and funded by the [Internet Security Research Group](https://www.abetterinternet.org/) as part of the [Prossimo project](https://www.memorysafety.org/).

[zlib-ng]: https://github.com/zlib-ng/zlib-ng
