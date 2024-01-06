#![no_main]
use libfuzzer_sys::fuzz_target;

const fn zswap32(q: u32) -> u32 {
    ((((q) >> 24) & 0xff) + (((q) >> 8) & 0xff00) + (((q) & 0xff00) << 8) + (((q) & 0xff) << 24))
}

const fn zswap32r(q: u32) -> u32 {
    u32::from_le(q.to_be())
}

fuzz_target!(|data: u64| {
    let bits16 = data as u16;
    let bits32 = data as u32;
    let bits64 = data;

    assert_eq!(zswap32(bits32), zswap32r(bits32));
});
