# `libz-rs-sys-cdylib`

Build `zlib-rs` as a drop-in replacement for the zlib dynamic library

```sh
# build the cdylib
# using `cargo build` will work but has limitations, see below
cargo build --release

# the extension of a cdylib varies per platform
cc zpipe.c target/release/libz_rs.so

# verify the implementation can compress and decompress our Cargo.toml
./zpipe < Cargo.toml | ./zpipe -d
```

By default this build uses libc `malloc`/`free` to (de)allocate memory, and only depends on the rust `core` library.
See below for the available feature flags.

## Feature Flags

### Allocators

We provide three options for the default allocator

**`c-allocator`**

```sh
cargo build --release --no-default-features --features "c-allocator"
```

Uses the libc `malloc` and `free` functions for memory allocation.

**`rust-allocator`**

```sh
cargo build --release --no-default-features --features "std,rust-allocator"
```
Uses the rust standard library global allocator for memory allocation.

**no allocator**

```sh
cargo build --release --no-default-features
```

No allocator is configured automatically. This means that, before [`inflateInit_`] or [`deflateInit_`] are called,
the user must set the `zalloc` and `zfree` fields of the `z_stream` to valid allocation and deallocation functions,
and the `opaque` field to either `NULL` or a pointer expected by the (de)allocation functions.

If no allocator is configured, the initialization functions will return `Z_STREAM_ERROR`.

### Symbol Prefix

Symbols in C programs all live in the same namespace. A common solution to prevent names from clashing is to prefix
all of a library's symbols with a prefix. We support prefixing the name at build time with the `custom-prefix` feature
flag. When enabled, the value of the `LIBZ_RS_SYS_PREFIX` is used as a prefix for all exported symbols. For example:

```ignore
> LIBZ_RS_SYS_PREFIX="MY_CUSTOM_PREFIX_" cargo build --release --features=custom-prefix

   Compiling libz-rs-sys v0.2.1 (/home/folkertdev/rust/zlib-rs/libz-rs-sys)
   Compiling libz-rs-sys-cdylib v0.2.1 (/home/folkertdev/rust/zlib-rs/libz-rs-sys-cdylib)
    Finished `release` profile [optimized + debuginfo] target(s) in 0.16s
> objdump -tT ../target/release/libz_rs.so | grep "uncompress"
00000000000758e0 l     O .got	0000000000000000              _ZN7zlib_rs7inflate10uncompress17hda65e03b54919c40E$got
0000000000025da0 l     F .text	000000000000029a              _ZN7zlib_rs7inflate10uncompress17hda65e03b54919c40E
000000000001d700 g     F .text	0000000000000051              MY_CUSTOM_PREFIX_uncompress
000000000001d700 g    DF .text	0000000000000051  Base        MY_CUSTOM_PREFIX_uncompress
```

### `![no_std]`

The dynamic library can be built without the rust `std` crate, e.g. for embedded devices that don't support it. Disabling
the standard library has the following limitations:

- CPU feature detection is currently disabled. This is true for both compile-time and run-time feature detection.
    This means `zlib-rs` will not make use of SIMD or other custom instructions.
- The `rust-allocator` should not be used. It internally enables the standard library, causing issues. Using `c-allocator`
    or not providing an allocator at build time is still supported.On embedded it is most common to provide a custom allocator
    that "allocates" into a custom array.

## Build for Distribution

A `cargo build` currently does not set some fields that are required or useful when using a dynamic library from C.
For instance, the soname and version are not set by a standard `cargo build`.

To build a proper, installable dynamic library, we recommend [`cargo-c`](https://github.com/lu-zero/cargo-c):

```
cargo install cargo-c
```

This tool deals with setting fields (soname, version) that a normal `cargo build` does not set (today).
It's configuration is in the `Cargo.toml`, where e.g. the library name or version can be changed.

```
> cargo cbuild --release
   Compiling zlib-rs v0.2.1
   Compiling libz-rs-sys v0.2.1
   Compiling libz-rs-sys-cdylib v0.2.1
    Finished `release` profile [optimized] target(s) in 1.86s
    Building pkg-config files
> tree target
target
├── CACHEDIR.TAG
└── x86_64-unknown-linux-gnu
    └── release
        ├── libz_rs.a
        ├── libz_rs.d
        ├── libz_rs.pc
        ├── libz_rs.so
        └── libz_rs-uninstalled.pc
```
