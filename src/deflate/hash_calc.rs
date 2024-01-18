use crate::deflate::{State, HASH_SIZE, STD_MIN_MATCH};

pub struct ZlibHashMap<'a> {
    pub prev: &'a mut [u16],
    pub head: &'a mut [u16; HASH_SIZE],

    // equal to the window_mask if prev.len() == window_size
    pub prev_mask: usize,
}

impl<'a> ZlibHashMap<'a> {
    pub fn new(prev: &'a mut [u16], head: &'a mut [u16; HASH_SIZE]) -> Self {
        assert!(prev.len().is_power_of_two());

        Self {
            prev_mask: prev.len() - 1,
            prev,
            head,
        }
    }

    pub fn clear(&mut self) {
        self.head.fill(0)
    }

    #[inline(always)]
    pub fn insert(&mut self, hash: u16, position: u16) -> u16 {
        let head = self.head[hash as usize];

        if head != position {
            let index = position as usize & self.prev_mask;
            // safety: the mask ensures the index is in bounds
            unsafe { *self.prev.get_unchecked_mut(index) = head };

            // safety: `hash` is a u16, and all u16 values are in bounds
            const _: () = assert!(HASH_SIZE >= u16::MAX as usize);
            unsafe { *self.head.get_unchecked_mut(hash as usize) = position };
        }

        head
    }

    #[inline(always)]
    pub fn get_prev(&self, position: u16) -> u16 {
        let index = position as usize & self.prev_mask;
        // safety: the mask ensures the index is in bounds
        unsafe { *self.prev.get_unchecked(index) }
    }

    #[inline(always)]
    pub fn get_head(&self, hash: u16) -> u16 {
        // safety: `hash` is a u16, and all u16 values are in bounds
        const _: () = assert!(HASH_SIZE >= u16::MAX as usize);
        unsafe { *self.head.get_unchecked(hash as usize) }
    }
}

pub trait HashCalc {
    const HASH_CALC_OFFSET: usize;
    const HASH_CALC_MASK: u16;

    fn hash_calc(h: u32, val: u32) -> u32;

    fn update_hash(h: u32, val: u32) -> u32 {
        let h = Self::hash_calc(h, val);
        h & Self::HASH_CALC_MASK as u32
    }

    fn quick_insert_string(state: &mut State, string: usize) -> u16 {
        let mut h = 0;

        let slice = &state.window.filled()[string + Self::HASH_CALC_OFFSET..];
        let val = u32::from_ne_bytes(slice[..4].try_into().unwrap());

        h = Self::hash_calc(h, val);
        h &= Self::HASH_CALC_MASK as u32;

        state.hash_map.insert(h as u16, string as u16)
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
            h &= Self::HASH_CALC_MASK as u32;

            state.hash_map.insert(h as u16, idx);
        }
    }
}

pub struct StandardHashCalc;

impl HashCalc for StandardHashCalc {
    const HASH_CALC_OFFSET: usize = 0;

    const HASH_CALC_MASK: u16 = 32768 - 1;

    fn hash_calc(_: u32, val: u32) -> u32 {
        const HASH_SLIDE: u32 = 16;
        val.wrapping_mul(2654435761) >> HASH_SLIDE
    }
}

pub struct RollHashCalc;

impl RollHashCalc {
    // so that we can test the actual implementation
    #[inline(always)]
    fn quick_insert_string_help(
        window: &[u8],
        hash_map: &mut ZlibHashMap,
        ins_h: &mut usize,
        string: usize,
    ) -> u16 {
        let slice = &window[string + Self::HASH_CALC_OFFSET..];

        let val = slice[0] as u32;
        *ins_h = Self::hash_calc(*ins_h as u32, val) as usize;
        *ins_h &= Self::HASH_CALC_MASK as usize;

        hash_map.insert(*ins_h as u16, string as u16)
    }
}

impl HashCalc for RollHashCalc {
    const HASH_CALC_OFFSET: usize = STD_MIN_MATCH - 1;

    const HASH_CALC_MASK: u16 = 32768 - 1;

    fn hash_calc(h: u32, val: u32) -> u32 {
        const HASH_SLIDE: u32 = 5;
        (h << HASH_SLIDE) ^ val
    }

    fn quick_insert_string(state: &mut State, string: usize) -> u16 {
        Self::quick_insert_string_help(
            state.window.filled(),
            &mut state.hash_map,
            &mut state.ins_h,
            string,
        )
    }

    fn insert_string(state: &mut State, string: usize, count: usize) {
        let slice = &state.window.filled()[string + Self::HASH_CALC_OFFSET..][..count];

        for (i, val) in slice.iter().copied().enumerate() {
            let idx = string as u16 + i as u16;

            state.ins_h = Self::hash_calc(state.ins_h as u32, val as u32) as usize;
            state.ins_h &= Self::HASH_CALC_MASK as usize;

            state.hash_map.insert(state.ins_h as u16, idx);
        }
    }
}

pub struct Crc32HashCalc;

impl HashCalc for Crc32HashCalc {
    const HASH_CALC_OFFSET: usize = 0;

    const HASH_CALC_MASK: u16 = {
        const _: () = assert!(HASH_SIZE > u16::MAX as usize);

        (HASH_SIZE - 1) as u16
    };

    fn hash_calc(h: u32, val: u32) -> u32 {
        unsafe { std::arch::x86_64::_mm_crc32_u32(h, val) }
    }
}

#[cfg(test)]
mod test {
    use crate::deflate::hash_calc::ZlibHashMap;

    use super::{HashCalc, RollHashCalc};

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

    #[test]
    fn construct_prev() {
        let window = "aaaabaaacaaa";
        //             ^   ^   ^
        //             1   5   9

        let mut ins_h = 3137;

        let mut prev = [0; 1 << 8];
        let mut head = [0; 1 << 16];

        let mut hash_map = ZlibHashMap::new(&mut prev, &mut head);

        for (i, _) in window.as_bytes().iter().take(window.len() - 2).enumerate() {
            RollHashCalc::quick_insert_string_help(window.as_bytes(), &mut hash_map, &mut ins_h, i);
        }

        assert_eq!(&prev[..10], &[0, 0, 0, 0, 0, 1, 0, 0, 0, 5]);

        // the hash (ins_h) points to the start of the chain, then the rest of the chain is stored
        // in `prev`.
        assert_eq!(head[ins_h], 9);
        assert_eq!(prev[9], 5);
        assert_eq!(prev[5], 1);
    }
}
