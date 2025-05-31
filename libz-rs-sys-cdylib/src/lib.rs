#![cfg_attr(feature = "gzprintf", feature(c_variadic))]
extern crate libz_rs_sys;

pub use libz_rs_sys::*;

#[cfg(not(any(panic = "abort", feature = "__internal-test", test, doc, miri)))]
compile_error!("panic=\"abort\" is mandatory because unwinding to C is undefined behavior");

#[cfg(feature = "gz")]
mod gz;

#[cfg(feature = "gz")]
pub use gz::*;

#[cfg(feature = "gz")]
#[allow(unused)]
mod custom_prefix {
    #[cfg(feature = "custom-prefix")]
    macro_rules! prefix {
        ($name:expr) => {
            concat!(env!("LIBZ_RS_SYS_PREFIX"), stringify!($name))
        };
    }

    // NOTE: once we reach 1.0.0, the macro used for the `semver-prefix` feature should no longer include the
    // minor version in the name. The name is meant to be unique between semver-compatible versions!
    const _PRE_ONE_DOT_O: () = assert!(env!("CARGO_PKG_VERSION_MAJOR").as_bytes()[0] == b'0');

    #[cfg(feature = "semver-prefix")]
    macro_rules! prefix {
        ($name:expr) => {
            concat!(
                "LIBZ_RS_SYS_v",
                env!("CARGO_PKG_VERSION_MAJOR"),
                "_",
                env!("CARGO_PKG_VERSION_MINOR"),
                "_x_",
                stringify!($name)
            )
        };
    }

    #[cfg(all(not(feature = "custom-prefix"), not(feature = "semver-prefix"),))]
    macro_rules! prefix {
        ($name:expr) => {
            stringify!($name)
        };
    }

    #[cfg(all(not(feature = "custom-prefix"), not(feature = "semver-prefix"), test))]
    macro_rules! prefix {
        ($name:expr) => {
            concat!("LIBZ_RS_SYS_TEST_", stringify!($name))
        };
    }

    pub(crate) use prefix;
}

#[cfg(feature = "gz")]
pub(crate) use custom_prefix::prefix;
