use crate::deflate::State;

pub fn slide_hash(state: &mut crate::deflate::State) {
    #[cfg(target_arch = "x86_64")]
    if std::is_x86_feature_detected!("avx2") {
        return avx2::slide_hash(state);
    }

    rust::slide_hash(state)
}

mod rust {
    use super::*;

    pub fn slide_hash(state: &mut State) {
        let wsize = state.w_size as u16;

        slide_hash_chain(state.hash_map.head, wsize);
        slide_hash_chain(state.hash_map.prev, wsize);
    }

    fn slide_hash_chain(table: &mut [u16], wsize: u16) {
        for m in table.iter_mut() {
            *m = m.saturating_sub(wsize);
        }
    }
}

#[cfg(target_arch = "x86_64")]
mod avx2 {
    use std::arch::x86_64::{
        __m256i, _mm256_loadu_si256, _mm256_set1_epi16, _mm256_storeu_si256, _mm256_subs_epu16,
    };

    use super::*;

    pub fn slide_hash(state: &mut State) {
        let wsize = state.w_size as u16;
        let ymm_wsize = unsafe { _mm256_set1_epi16(wsize as i16) };

        slide_hash_chain(state.hash_map.head, ymm_wsize);
        slide_hash_chain(state.hash_map.prev, ymm_wsize);
    }

    fn slide_hash_chain(table: &mut [u16], wsize: __m256i) {
        debug_assert_eq!(table.len() % 16, 0);

        for chunk in table.chunks_exact_mut(16) {
            let chunk = chunk.as_mut_ptr() as *mut __m256i;

            unsafe {
                let value = _mm256_loadu_si256(chunk);
                let result = _mm256_subs_epu16(value, wsize);
                _mm256_storeu_si256(chunk, result);
            }
        }
    }
}
