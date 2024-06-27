# `dynamic-libz-sys`

Loads a zlib dynamically, exposing an api similar to the `libz-sys` crate.

This is useful for debugging the logic in this crate, which quite closely matches zlib-ng.
We can locally modify and build zlib-ng to print or otherwise expose internal information,
and then run that version of zlib-ng from rust, giving it the same input as we give to our
rust implementation.

## usage

The dynamic library location is read from the `DYNAMIC_LIBZ_SYS` environment variable.
To use a custom dynamic library, use a command like this:

```sh
DYNAMIC_LIBZ_SYS="/home/folkertdev/rust/zlib-ng/libz.so" cargo test
```
