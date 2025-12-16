#![cfg_attr(feature = "gzprintf", feature(c_variadic))]
extern crate libz_rs_sys;

pub use libz_rs_sys::*;

#[cfg(not(any(panic = "abort", feature = "__internal-test", test, doc, miri)))]
compile_error!("panic=\"abort\" is mandatory because unwinding to C is undefined behavior");
