extern crate libz_rs_sys;

pub use libz_rs_sys::*;

#[cfg(not(panic = "abort"))]
compile_error!("panic=\"abort\" is mandatory because unwinding to C is undefined behavior");
