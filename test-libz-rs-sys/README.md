# tests for `libz-rs-sys`

**why are these tests here?**

Because C puts all symbols into one namespace. The `libz-rs-sys` crate defines the standard zlib api. But, we want to test against C implementations of that API. So we must, for testing, add a prefix to our names so that they don't clash with the symbols provided by the C implementation.

In this crate, our symbols are always distinct through some macro/feature flag magic. That means we can freely test and benchmark without any danger of mixing up the symbols. 
