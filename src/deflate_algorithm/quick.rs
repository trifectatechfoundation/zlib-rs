#![forbid(unsafe_code)]

use crate::{
    deflate::{
        fill_window, flush_pending, BlockState, BlockType, DeflateStream, State, StaticTreeDesc,
        MIN_LOOKAHEAD, STD_MAX_MATCH, STD_MIN_MATCH, WANT_MIN_MATCH,
    },
    Flush,
};

#[inline(always)]
fn memcmp_n_slice<const N: usize>(src0: &[u8], src1: &[u8]) -> bool {
    src0[..N] != src1[..N]
}

pub fn deflate_quick(stream: &mut DeflateStream, flush: Flush) -> BlockState {
    let mut state = &mut stream.state;

    let last = matches!(flush, Flush::Finish);

    macro_rules! quick_start_block {
        () => {
            state.emit_tree(BlockType::StaticTrees, last);
            state.block_open = 1 + last as u8;
            state.block_start = state.strstart as isize;
        };
    }

    macro_rules! quick_end_block {
        () => {
            if state.block_open > 0 {
                state.emit_end_block_and_align(&StaticTreeDesc::L.static_tree, last);
                state.block_open = 0;
                state.block_start = state.strstart as isize;
                flush_pending(stream);
                #[allow(unused_assignments)]
                {
                    state = &mut stream.state;
                }
                if stream.avail_out == 0 {
                    return match last {
                        true => BlockState::FinishStarted,
                        false => BlockState::NeedMore,
                    };
                }
            }
        };
    }

    if last && state.block_open != 2 {
        /* Emit end of previous block */
        quick_end_block!();
        /* Emit start of last block */
        quick_start_block!();
    } else if state.block_open == 0 && state.lookahead > 0 {
        /* Start new block only when we have lookahead data, so that if no
        input data is given an empty block will not be written */
        quick_start_block!();
    }

    loop {
        if state.pending.pending + State::BIT_BUF_SIZE.div_ceil(8) as usize
            >= state.pending_buf_size()
        {
            flush_pending(stream);
            state = &mut stream.state;
            if stream.avail_out == 0 {
                return if last
                    && stream.avail_in == 0
                    && state.bi_valid == 0
                    && state.block_open == 0
                {
                    BlockState::FinishStarted
                } else {
                    BlockState::NeedMore
                };
            }
        }

        if state.lookahead < MIN_LOOKAHEAD {
            fill_window(stream);
            state = &mut stream.state;

            if state.lookahead < MIN_LOOKAHEAD && matches!(flush, Flush::NoFlush) {
                return BlockState::NeedMore;
            }
            if state.lookahead == 0 {
                break;
            }

            if state.block_open == 0 {
                // Start new block when we have lookahead data,
                // so that if no input data is given an empty block will not be written
                quick_start_block!();
            }
        }

        if state.lookahead >= WANT_MIN_MATCH {
            let hash_head = (state.quick_insert_string)(state, state.strstart);
            let dist = state.strstart as isize - hash_head as isize;

            if dist <= state.max_dist() as isize && dist > 0 {
                let str_start = &state.window.filled()[state.strstart..];
                let match_start = &state.window.filled()[hash_head as usize..];

                if !memcmp_n_slice::<2>(str_start, match_start) {
                    let mut match_len =
                        crate::compare256::compare256_slice(&str_start[2..], &match_start[2..]) + 2;

                    if match_len >= WANT_MIN_MATCH {
                        if match_len > state.lookahead {
                            match_len = state.lookahead;
                        }

                        if match_len > STD_MAX_MATCH {
                            match_len = STD_MAX_MATCH;
                        }

                        // TODO do this with a debug_assert?
                        // check_match(s, state.strstart, hash_head, match_len);

                        state.emit_dist(
                            StaticTreeDesc::L.static_tree,
                            StaticTreeDesc::D.static_tree,
                            (match_len - STD_MIN_MATCH) as u8,
                            dist as usize,
                        );
                        state.lookahead -= match_len;
                        state.strstart += match_len;
                        continue;
                    }
                }
            }
        }

        let lc = state.window.filled()[state.strstart];
        state.emit_lit(StaticTreeDesc::L.static_tree, lc);
        state.strstart += 1;
        state.lookahead -= 1;
    }

    state.insert = if state.strstart < (STD_MIN_MATCH - 1) {
        state.strstart
    } else {
        STD_MIN_MATCH - 1
    };

    if last {
        quick_end_block!();
        return BlockState::FinishDone;
    }

    quick_end_block!();
    BlockState::BlockDone
}
