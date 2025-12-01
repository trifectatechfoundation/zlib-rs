# `libz-rs-sys-cdylib`

Build `zlib-rs` as a drop-in replacement for the zlib dynamic library.

```sh
# build the cdylib
# using `cargo build` will work but has limitations, see below
cargo build --release

# the extension of a cdylib varies per platform
cc -o zpipe zpipe.c target/release/libz_rs.so -I .

# verify the implementation can compress and decompress our Cargo.toml
./zpipe < Cargo.toml | ./zpipe -d
```

By default this build uses libc `malloc`/`free` to (de)allocate memory, and only depends on the rust `core` library.
See below for the available feature flags.

The cdylib enables `panic="abort"`, meaning that when the rust code runs into a panic (e.g. from an assert), the program is aborted.
Panics indicate bugs, and we'd appreciate a bug report for them if they ever appear in the wild.

The `include/` directory contains a `zlib.h` and `zconf.h`. We are compatible with most system versions of these headers.

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

### `#![no_std]`

The dynamic library can be built without the rust `std` crate, e.g. for embedded devices that don't support it. Disabling
the standard library has the following limitations:

- CPU feature detection is currently disabled. This is true for both compile-time and run-time feature detection.
    This means `zlib-rs` will not make use of SIMD or other custom instructions.
- The `rust-allocator` should not be used. It internally enables the standard library, causing issues. Using `c-allocator`
    or not providing an allocator at build time is still supported. On embedded it is most common to provide a custom allocator
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


## Symbol versioning

Rust does not natively support symbol versioning. Because the rust compiler does version its own symbols, and most linkers only consider the first version script, applying custom versions to symbols requires post-processing of the binary. Here we use the rust compiler to build a static library, which is then converted by the linker to a dynamic library with versioned symbols.

```sh
RUSTFLAGS="-Cpanic=abort" cargo +nightly build --release --features=gz,gzprintf

cc -shared \
  -Wl,--gc-sections \
  -Wl,--whole-archive target/release/libz_rs.a \
  -Wl,--no-whole-archive \
  -Wl,--version-script=include/zlib.map \
  -Wl,--undefined-version \
  -Wl,-soname,libz_rs.so.1 \
  -lc \
  -o target/release/libz_rs.versioned.so

# To see that it worked:
objdump -T target/release/libz_rs.versioned.so | grep "ZLIB"
objdump -T target/release/libz_rs.versioned.so | grep -q -E "ZLIB_1.2.2.3 deflateTune" || (echo "symbol not found!" && exit 1)
```

The `zlib.map` version script is bundled just like the `zlib.h` header file.
