extern crate libz_rs_sys;

pub use libz_rs_sys::*;

// TODO somehow make this work with the tests
// #[cfg(not(panic = "abort"))]
// compile_error!("panic=\"abort\" is mandatory because unwinding to C is undefined behavior");

#[cfg(feature = "gz")]
mod gz;

#[cfg(feature = "gz")]
pub use gz::*;
