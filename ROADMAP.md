# zlib-rs development roadmap

## Future priorities

General priorities:

* **Additional Performance Optimization**.
  Our goal is to be competitive with zlib-ng if we are not already. Methods include
  better usage of platform-specific (SIMD) instructions, algorithmic improvements and
  more optimal data layout.

* **zlib-rs API Refinements**.
  Continue to improve the zlib-rs rust API. Aim for ease of use, clarity.

## Past priorities

* **integrate with flate2**.
  The `flate2` crate supports the `zlib-rs` feature since version [`1.0.29`](https://github.com/rust-lang/flate2-rs/releases/tag/1.0.29)

* **add `inflateBack` family of functions**.
  Our impression is that these functions are rarely used, but for compatibility
  they must be implemented.

* **add `gz` family of functions**.
  Our impression is that these functions are rarely used, but for compatibility
  they must be implemented.

* **SIMD support for NEON**.
  NEON support is partially implemented, but not complete and not benchmarked

* **SIMD support for AVX512**.
  There is currently no support for this instruction set, and support is unstable
  in rust itself.
