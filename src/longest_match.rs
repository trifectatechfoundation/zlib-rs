use crate::deflate::{memcmp_n, State, MIN_LOOKAHEAD, STD_MAX_MATCH, STD_MIN_MATCH};

type Pos = u16;

const EARLY_EXIT_TRIGGER_LEVEL: i8 = 5;

pub fn longest_match(state: &mut crate::deflate::State, cur_match: u16) -> usize {
    let (best_len, match_start) = longest_match_help::<false>(state, cur_match);
    state.match_start = match_start;

    best_len
}

pub fn longest_match_slow(state: &mut crate::deflate::State, cur_match: u16) -> usize {
    let (best_len, match_start) = longest_match_help::<true>(state, cur_match);
    state.match_start = match_start;

    best_len
}

fn longest_match_help<const SLOW: bool>(
    state: &crate::deflate::State,
    mut cur_match: u16,
) -> (usize, usize) {
    let mut match_start = state.match_start;

    let strstart = state.strstart;
    let wmask = state.w_mask;
    let window = state.window;
    let scan = window.wrapping_add(strstart);
    let mut mbase_start = window;
    let mut mbase_end: *mut u8;
    let mut limit: Pos;
    let limit_base: Pos;
    let early_exit: bool;

    let mut chain_length: usize;
    let mut best_len: usize;

    let nice_match;
    let lookahead = state.lookahead;
    let mut match_offset = 0;

    macro_rules! goto_next_chain {
        () => {
            chain_length -= 1;
            if chain_length > 0 {
                cur_match = state.prev[cur_match as usize & wmask];

                if cur_match > limit {
                    continue;
                }
            }

            return (best_len, match_start);
        };
    }

    // The code is optimized for STD_MAX_MATCH-2 multiple of 16.
    assert_eq!(STD_MAX_MATCH, 258, "Code too clever");

    best_len = if state.prev_length > 0 {
        state.prev_length
    } else {
        STD_MIN_MATCH - 1
    };

    let mut offset = best_len - 1;

    // we're assuming we can do a fast(ish) unaligned 64-bit read
    if best_len >= std::mem::size_of::<u32>() {
        offset -= 2;
        if best_len >= std::mem::size_of::<u64>() {
            offset -= 4;
        }
    }

    let scan_start;
    let mut scan_end;

    unsafe {
        scan_start = std::ptr::read(scan as *mut [u8; 8]);
        scan_end = std::ptr::read(scan.wrapping_add(offset) as *mut [u8; 8]);
    }

    mbase_end = mbase_start.wrapping_add(offset);

    // Do not waste too much time if we already have a good match
    chain_length = state.max_chain_length;
    if best_len >= state.good_match {
        chain_length >>= 2;
    }
    nice_match = state.nice_match;

    /* Stop when cur_match becomes <= limit. To simplify the code,
     * we prevent matches with the string of window index 0
     */
    // TODO saturating sub
    limit = if strstart > state.max_dist() {
        (strstart - state.max_dist()) as Pos
    } else {
        0
    };

    // look for a better string offset
    if SLOW {
        limit_base = limit;

        if best_len >= STD_MIN_MATCH {
            /* We're continuing search (lazy evaluation). */
            let mut hash;
            let mut pos: Pos;

            /* Find a most distant chain starting from scan with index=1 (index=0 corresponds
             * to cur_match). We cannot use s->prev[strstart+1,...] immediately, because
             * these strings are not yet inserted into the hash table.
             */
            let get_scan = |n: usize| unsafe { *scan.wrapping_add(n) as u32 };

            hash = (state.update_hash)(0, get_scan(1));
            hash = (state.update_hash)(hash, get_scan(2));

            for i in 3..=best_len {
                hash = (state.update_hash)(hash, get_scan(i));

                /* If we're starting with best_len >= 3, we can use offset search. */
                pos = state.head[hash as usize];
                if pos < cur_match {
                    match_offset = (i - 2) as Pos;
                    cur_match = pos;
                }
            }

            /* Update offset-dependent variables */
            limit = limit_base + match_offset;
            if cur_match <= limit {
                return break_matching(state, best_len, match_start);
            }

            mbase_start = mbase_start.wrapping_sub(match_offset as usize);
            mbase_end = mbase_end.wrapping_sub(match_offset as usize);
        }
    } else {
        // must initialize this variable
        limit_base = 0;
    }

    // NOTE skipping the slow stuff for now

    early_exit = state.level < EARLY_EXIT_TRIGGER_LEVEL;

    assert!(
        strstart <= state.window_size - MIN_LOOKAHEAD,
        "need lookahead"
    );

    loop {
        if cur_match as usize >= strstart {
            break;
        }

        // Skip to next match if the match length cannot increase or if the match length is
        // less than 2. Note that the checks below for insufficient lookahead only occur
        // occasionally for performance reasons.
        // Therefore uninitialized memory will be accessed and conditional jumps will be made
        // that depend on those values. However the length of the match is limited to the
        // lookahead, so the output of deflate is not affected by the uninitialized values.
        unsafe {
            #[inline(always)]
            unsafe fn check<const N: usize>(
                cur_match: u16,
                mbase_start: *const u8,
                mbase_end: *const u8,
                scan_start: *const u8,
                scan_end: *const u8,
            ) -> bool {
                let cur_match = cur_match as usize;

                if !memcmp_n::<N>(mbase_end.wrapping_add(cur_match), scan_end) {
                    !memcmp_n::<N>(mbase_start.wrapping_add(cur_match), scan_start)
                } else {
                    false
                }
            }

            let scan_start = scan_start.as_ptr();
            let scan_end = scan_end.as_ptr();

            if best_len < std::mem::size_of::<u32>() {
                loop {
                    if check::<2>(cur_match, mbase_start, mbase_end, scan_start, scan_end) {
                        break;
                    }

                    goto_next_chain!();
                }
            } else if best_len >= std::mem::size_of::<u64>() {
                loop {
                    if check::<8>(cur_match, mbase_start, mbase_end, scan_start, scan_end) {
                        break;
                    }

                    goto_next_chain!();
                }
            } else {
                loop {
                    if check::<4>(cur_match, mbase_start, mbase_end, scan_start, scan_end) {
                        break;
                    }

                    goto_next_chain!();
                }
            }
        }

        let len = {
            // TODO this just looks so incredibly unsafe!
            let src0 = unsafe { &*scan.wrapping_add(2).cast() };
            let src1 = unsafe { &*mbase_start.wrapping_add(cur_match as usize + 2).cast() };

            crate::compare256::compare256(src0, src1) + 2
        };
        assert!(
            scan as usize + len <= window as usize + (state.window_size - 1),
            "wild scan"
        );

        if len > best_len {
            match_start = (cur_match - match_offset) as usize;

            /* Do not look for matches beyond the end of the input. */
            if len > lookahead {
                return (lookahead, match_start);
            }
            best_len = len;
            if best_len >= nice_match {
                return (best_len, match_start);
            }

            offset = best_len - 1;
            if best_len >= std::mem::size_of::<u32>() {
                offset -= 2;
                if best_len >= std::mem::size_of::<u64>() {
                    offset -= 4;
                }
            }

            scan_end =
                unsafe { std::ptr::read_unaligned(scan.wrapping_add(offset) as *mut [u8; 8]) };

            // Look for a better string offset
            if SLOW && len > STD_MIN_MATCH && match_start + len < strstart {
                let mut pos: Pos;
                // uint32_t i, hash;
                // unsigned char *scan_endstr;

                /* Go back to offset 0 */
                cur_match -= match_offset;
                match_offset = 0;
                let mut next_pos = cur_match;

                for i in 0..=len - STD_MIN_MATCH {
                    pos = state.prev[(cur_match as usize + i) & wmask];
                    if pos < next_pos {
                        /* Hash chain is more distant, use it */
                        if pos <= limit_base + i as Pos {
                            return break_matching(state, best_len, match_start);
                        }
                        next_pos = pos;
                        match_offset = i as Pos;
                    }
                }
                /* Switch cur_match to next_pos chain */
                cur_match = next_pos;

                /* Try hash head at len-(STD_MIN_MATCH-1) position to see if we could get
                 * a better cur_match at the end of string. Using (STD_MIN_MATCH-1) lets
                 * us include one more byte into hash - the byte which will be checked
                 * in main loop now, and which allows to grow match by 1.
                 */
                let scan_endstr = scan.wrapping_add(len - (STD_MIN_MATCH + 1));
                let get_scan_endstr = |n: usize| unsafe { *scan_endstr.wrapping_add(n) as u32 };

                let mut hash;
                hash = (state.update_hash)(0, get_scan_endstr(0));
                hash = (state.update_hash)(hash, get_scan_endstr(1));
                hash = (state.update_hash)(hash, get_scan_endstr(2));

                pos = state.head[hash as usize];
                if pos < cur_match {
                    match_offset = (len - (STD_MIN_MATCH + 1)) as Pos;
                    if pos <= limit_base + match_offset {
                        return break_matching(state, best_len, match_start);
                    }
                    cur_match = pos;
                }

                /* Update offset-dependent variables */
                limit = limit_base + match_offset;
                mbase_start = window.wrapping_sub(match_offset as usize);
                mbase_end = mbase_start.wrapping_add(offset);
                continue;
            }

            mbase_end = mbase_start.wrapping_add(offset);
        } else if !SLOW && early_exit {
            // The probability of finding a match later if we here is pretty low, so for
            // performance it's best to outright stop here for the lower compression levels
            break;
        }

        goto_next_chain!();
    }

    (best_len, match_start)
}

fn break_matching(state: &State, best_len: usize, match_start: usize) -> (usize, usize) {
    (Ord::min(best_len, state.lookahead), match_start)
}
