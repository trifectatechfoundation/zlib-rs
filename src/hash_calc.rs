use crate::deflate::{State, HASH_SIZE, STD_MIN_MATCH};

pub trait HashCalc {
    const HASH_CALC_OFFSET: usize;
    const HASH_CALC_MASK: u32;

    fn hash_calc(h: u32, val: u32) -> u32;

    fn update_hash(h: u32, val: u32) -> u32 {
        let h = Self::hash_calc(h, val);
        h & Self::HASH_CALC_MASK
    }

    fn hash_calc_read(strstart: *const u8) -> u32 {
        // looks very unsafe; why is this allright?
        let chunk = unsafe { std::slice::from_raw_parts(strstart, 4) };
        let mut buf = [0u8; 4];
        buf.copy_from_slice(chunk);

        u32::from_ne_bytes(buf)
    }

    fn quick_insert_string(state: &mut State, string: usize) -> u16 {
        let strstart = state.window.wrapping_add(string + Self::HASH_CALC_OFFSET);

        let mut h = 0;

        let val = Self::hash_calc_read(strstart);

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
        // safety: we have a mutable reference to the state, so nobody else can use this memory
        let mut strstart = state.window.wrapping_add(string + Self::HASH_CALC_OFFSET);
        let strend = strstart.wrapping_add(count);

        // NOTE: 4 = size_of::<u32>()
        let mut i = 0;
        while strstart < strend {
            let idx = string as u16 + i as u16;

            // HASH_CALC_VAR_INIT;
            let mut h = 0;

            // HASH_CALC_READ;
            // let mut buf = [0u8; 4];
            // buf.copy_from_slice(chunk);
            let buf = unsafe { *(strstart as *const [u8; 4]) };
            let val = u32::from_ne_bytes(buf);

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

            i += 1;
            strstart = strstart.wrapping_add(1);
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
        (h << HASH_SLIDE) ^ (val & 0xFF)
    }

    fn hash_calc_read(strstart: *const u8) -> u32 {
        (unsafe { *strstart }) as u32
    }

    fn quick_insert_string(state: &mut State, string: usize) -> u16 {
        let strstart = state.window.wrapping_add(string + Self::HASH_CALC_OFFSET);

        let val = Self::hash_calc_read(strstart);
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
        let mut strstart = state.window.wrapping_add(string + Self::HASH_CALC_OFFSET);
        let strend = strstart.wrapping_add(count);

        let mut i = 0;
        while strstart < strend {
            let idx = string as u16 + i as u16;

            let val = Self::hash_calc_read(strstart);
            state.ins_h = Self::hash_calc(state.ins_h as u32, val) as usize;
            state.ins_h &= Self::HASH_CALC_MASK as usize;
            let hm = state.ins_h;

            let head = state.head[hm];
            if head != idx {
                state.prev[idx as usize & state.w_mask] = head;
                state.head[hm] = idx;
            }

            i += 1;
            strstart = strstart.wrapping_add(1);
        }
    }
}

pub struct Crc32HashCalc;

impl HashCalc for Crc32HashCalc {
    const HASH_CALC_OFFSET: usize = 0;

    const HASH_CALC_MASK: u32 = (HASH_SIZE - 1) as u32;

    fn hash_calc(h: u32, val: u32) -> u32 {
        unsafe { std::arch::x86_64::_mm_crc32_u32(h, val) }
    }
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
