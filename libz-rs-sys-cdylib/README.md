# `libz-rs-sys-cdylib`

Build `zlib-rs` as a drop-in replacement for the zlib dynamic library

```sh
# build the cdylib
cargo build --release

# the extension of a cdylib varies per platform
cc zpipe.c target/release/libz_rs.so

# verify the implementation can compress and decompress our Cargo.toml
./zpipe < Cargo.toml | ./zpipe -d
```

By default this build uses libc `malloc`/`free` to (de)allocate memory, and only depends on the rust `core` library.

## Features

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
