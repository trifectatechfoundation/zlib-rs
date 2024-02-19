use std::arch::is_aarch64_feature_detected;

use crate::deflate::{State, HASH_SIZE, STD_MIN_MATCH};

pub trait HashCalc {
    const HASH_CALC_OFFSET: usize;
    const HASH_CALC_MASK: u32;

    fn hash_calc(h: u32, val: u32) -> u32;

    fn update_hash(h: u32, val: u32) -> u32 {
        let h = Self::hash_calc(h, val);
        h & Self::HASH_CALC_MASK
    }

    fn quick_insert_string(state: &mut State, string: usize) -> u16 {
        let mut h = 0;

        let slice = &state.window.filled()[string + Self::HASH_CALC_OFFSET..];
        let val = u32::from_ne_bytes(slice[..4].try_into().unwrap());

        h = Self::hash_calc(h, val);
        h &= Self::HASH_CALC_MASK;

        let hm = h as usize;

        let head = state.head[hm];
        if head != string as u16 {
            state.prev[string & state.w_mask] = head;
            state.head[hm] = string as u16;
        }

        head
    }

    fn insert_string(state: &mut State, string: usize, count: usize) {
        let slice = &state.window.filled()[string + Self::HASH_CALC_OFFSET..];

        for (i, w) in slice.windows(4).take(count).enumerate() {
            let idx = string as u16 + i as u16;

            // HASH_CALC_VAR_INIT;
            let mut h = 0;

            // HASH_CALC_READ;
            let val = u32::from_ne_bytes(w.try_into().unwrap());

            // HASH_CALC(s, HASH_CALC_VAR, val);
            h = Self::hash_calc(h, val);
            h &= Self::HASH_CALC_MASK;

            // hm = HASH_CALC_VAR;
            let hm = h as usize;

            let head = state.head[hm];
            if head != idx {
                state.prev[idx as usize & state.w_mask] = head;
                state.head[hm] = idx;
            }
        }
    }
}

pub struct StandardHashCalc;

impl HashCalc for StandardHashCalc {
    const HASH_CALC_OFFSET: usize = 0;

    const HASH_CALC_MASK: u32 = 32768 - 1;

    fn hash_calc(_: u32, val: u32) -> u32 {
        const HASH_SLIDE: u32 = 16;
        val.wrapping_mul(2654435761) >> HASH_SLIDE
    }
}

pub struct RollHashCalc;

impl HashCalc for RollHashCalc {
    const HASH_CALC_OFFSET: usize = STD_MIN_MATCH - 1;

    const HASH_CALC_MASK: u32 = 32768 - 1;

    fn hash_calc(h: u32, val: u32) -> u32 {
        const HASH_SLIDE: u32 = 5;
        (h << HASH_SLIDE) ^ val
    }

    fn quick_insert_string(state: &mut State, string: usize) -> u16 {
        let slice = &state.window.filled()[string + Self::HASH_CALC_OFFSET..];

        let val = slice[0] as u32;
        state.ins_h = Self::hash_calc(state.ins_h as u32, val) as usize;
        state.ins_h &= Self::HASH_CALC_MASK as usize;

        let hm = state.ins_h;

        let head = state.head[hm];
        if head != string as u16 {
            state.prev[string & state.w_mask] = head;
            state.head[hm] = string as u16;
        }

        head
    }

    fn insert_string(state: &mut State, string: usize, count: usize) {
        let slice = &state.window.filled()[string + Self::HASH_CALC_OFFSET..][..count];

        for (i, val) in slice.iter().copied().enumerate() {
            let idx = string as u16 + i as u16;

            state.ins_h = Self::hash_calc(state.ins_h as u32, val as u32) as usize;
            state.ins_h &= Self::HASH_CALC_MASK as usize;
            let hm = state.ins_h;

            let head = state.head[hm];
            if head != idx {
                state.prev[idx as usize & state.w_mask] = head;
                state.head[hm] = idx;
            }
        }
    }
}

pub struct Crc32HashCalc;

impl Crc32HashCalc {
    pub fn is_supported() -> bool {
        if cfg!(target_arch = "x86") || cfg!(target_arch = "x86_64") {
            return true;
        }

        #[cfg(target_arch = "aarch64")]
        return is_aarch64_feature_detected!("crc");

        #[allow(unreachable_code)]
        false
    }
}

impl HashCalc for Crc32HashCalc {
    const HASH_CALC_OFFSET: usize = 0;

    const HASH_CALC_MASK: u32 = (HASH_SIZE - 1) as u32;

    #[cfg(target_arch = "x86")]
    fn hash_calc(h: u32, val: u32) -> u32 {
        unsafe { std::arch::x86::_mm_crc32_u32(h, val) }
    }

    #[cfg(target_arch = "x86_64")]
    fn hash_calc(h: u32, val: u32) -> u32 {
        unsafe { std::arch::x86_64::_mm_crc32_u32(h, val) }
    }

    #[cfg(target_arch = "aarch64")]
    fn hash_calc(h: u32, val: u32) -> u32 {
        unsafe { crate::crc32::arm::__crc32cw(h, val) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn crc32_hash_calc() {
        assert_eq!(Crc32HashCalc::hash_calc(0, 807411760), 2423125009);
        assert_eq!(Crc32HashCalc::hash_calc(0, 540024864), 1452438466);
        assert_eq!(Crc32HashCalc::hash_calc(0, 538980384), 435552201);
        assert_eq!(Crc32HashCalc::hash_calc(0, 807411760), 2423125009);
        assert_eq!(Crc32HashCalc::hash_calc(0, 540024864), 1452438466);
        assert_eq!(Crc32HashCalc::hash_calc(0, 538980384), 435552201);
        assert_eq!(Crc32HashCalc::hash_calc(0, 807411760), 2423125009);
        assert_eq!(Crc32HashCalc::hash_calc(0, 540024864), 1452438466);
        assert_eq!(Crc32HashCalc::hash_calc(0, 538980384), 435552201);
        assert_eq!(Crc32HashCalc::hash_calc(0, 807411760), 2423125009);
        assert_eq!(Crc32HashCalc::hash_calc(0, 540024864), 1452438466);
        assert_eq!(Crc32HashCalc::hash_calc(0, 538980384), 435552201);
        assert_eq!(Crc32HashCalc::hash_calc(0, 807411760), 2423125009);
        assert_eq!(Crc32HashCalc::hash_calc(0, 170926112), 500028708);
        assert_eq!(Crc32HashCalc::hash_calc(0, 537538592), 3694129053);
        assert_eq!(Crc32HashCalc::hash_calc(0, 538970672), 373925026);
        assert_eq!(Crc32HashCalc::hash_calc(0, 538976266), 4149335727);
        assert_eq!(Crc32HashCalc::hash_calc(0, 538976288), 1767342659);
        assert_eq!(Crc32HashCalc::hash_calc(0, 941629472), 4090502627);
        assert_eq!(Crc32HashCalc::hash_calc(0, 775430176), 1744703325);
    }

    #[test]
    fn roll_hash_calc() {
        assert_eq!(RollHashCalc::hash_calc(2565, 93), 82173);
        assert_eq!(RollHashCalc::hash_calc(16637, 10), 532394);
        assert_eq!(RollHashCalc::hash_calc(8106, 100), 259364);
        assert_eq!(RollHashCalc::hash_calc(29988, 101), 959717);
        assert_eq!(RollHashCalc::hash_calc(9445, 98), 302274);
        assert_eq!(RollHashCalc::hash_calc(7362, 117), 235573);
        assert_eq!(RollHashCalc::hash_calc(6197, 103), 198343);
        assert_eq!(RollHashCalc::hash_calc(1735, 32), 55488);
        assert_eq!(RollHashCalc::hash_calc(22720, 61), 727101);
        assert_eq!(RollHashCalc::hash_calc(6205, 32), 198528);
        assert_eq!(RollHashCalc::hash_calc(3826, 117), 122421);
        assert_eq!(RollHashCalc::hash_calc(24117, 101), 771781);
    }
}
