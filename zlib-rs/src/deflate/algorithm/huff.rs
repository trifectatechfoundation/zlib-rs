#![forbid(unsafe_code)]

use crate::{
    deflate::{fill_window, BlockState, DeflateStream},
    flush_block, Flush,
};

pub fn deflate_huff(stream: &mut DeflateStream, flush: Flush) -> BlockState {
    loop {
        /* Make sure that we have a literal to write. */
        if stream.state.lookahead == 0 {
            fill_window(stream);

            if stream.state.lookahead == 0 {
                match flush {
                    Flush::NoFlush => return BlockState::NeedMore,
                    _ => break, /* flush the current block */
                }
            }
        }

        /* Output a literal byte */
        let state = &mut stream.state;
        let lc = state.window.filled()[state.strstart];
        let bflush = state.tally_lit(lc);
        state.lookahead -= 1;
        state.strstart += 1;
        if bflush {
            flush_block!(stream, false);
        }
    }

    stream.state.insert = 0;

    if flush == Flush::Finish {
        flush_block!(stream, true);
        return BlockState::FinishDone;
    }

    if !stream.state.sym_buf.is_empty() {
        flush_block!(stream, false);
    }

    BlockState::BlockDone
}
