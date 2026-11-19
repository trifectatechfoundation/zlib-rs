#![allow(dead_code)]

pub struct CpuFeatures;

impl CpuFeatures {
    pub const NONE: usize = 0;
    pub const AVX2: usize = 1;
}

#[cfg(any(target_arch = "x86_64", target_arch = "x86"))]
mod x86_features {
    use core::sync::atomic::{AtomicU32, Ordering};

    const UNINITIALIZED: u32 = u32::MAX;
    pub const SSE: u32 = 1 << 0;
    pub const SSE42: u32 = 1 << 1;
    pub const PCLMULQDQ: u32 = 1 << 2;
    pub const AVX2_BMI: u32 = 1 << 3;
    pub const AVX512: u32 = 1 << 4; // Requires F + BW + VL

    static CACHE: AtomicU32 = AtomicU32::new(UNINITIALIZED);

    #[inline(always)]
    pub fn get() -> u32 {
        let val = CACHE.load(Ordering::Relaxed);
        if val != UNINITIALIZED {
            val
        } else {
            detect()
        }
    }

    #[cold]
    #[inline(never)]
    fn detect() -> u32 {
        #[cfg(miri)]
        {
            CACHE.store(0, Ordering::Relaxed);
            return 0;
        }

        #[cfg(not(miri))]
        {
            #[cfg(target_arch = "x86")]
            use core::arch::x86 as arch;
            #[cfg(target_arch = "x86_64")]
            use core::arch::x86_64 as arch;

            let mut feats = 0u32;
            let cpuid0 = arch::__cpuid(0);
            if cpuid0.eax < 1 {
                CACHE.store(feats, Ordering::Relaxed);
                return feats;
            }

            let cpuid1 = arch::__cpuid(1);
            // Leaf 1 EDX bit 25 = SSE
            if (cpuid1.edx & (1 << 25)) != 0 {
                feats |= SSE;
            }
            // Leaf 1 ECX bit 20 = SSE4.2
            if (cpuid1.ecx & (1 << 20)) != 0 {
                feats |= SSE42;
            }
            // Leaf 1 ECX: bit 1 = PCLMULQDQ, bit 19 = SSE4.1 (required for instructions used by pclmulqdq.rs)
            let req_pclmul = (1 << 1) | (1 << 19);
            if (cpuid1.ecx & req_pclmul) == req_pclmul {
                feats |= PCLMULQDQ;
            }

            // Check OSXSAVE (bit 27) and AVX (bit 28)
            let osxsave_avx = (1 << 27) | (1 << 28);
            if (cpuid1.ecx & osxsave_avx) == osxsave_avx {
                // SAFETY: CPUID Leaf 1 ECX bit 27 (OSXSAVE) is verified above, guaranteeing
                // that CR4.OSXSAVE is enabled by the OS and _xgetbv(0) will not trigger a #UD fault.
                let xcr0 = unsafe { arch::_xgetbv(0) };
                // Bit 1 = XMM, Bit 2 = YMM
                if (xcr0 & 0x6) == 0x6 && cpuid0.eax >= 7 {
                    let cpuid7 = arch::__cpuid_count(7, 0);
                    // BMI1 (bit 3), AVX2 (bit 5), BMI2 (bit 8)
                    let req_avx2 = (1 << 3) | (1 << 5) | (1 << 8);
                    if (cpuid7.ebx & req_avx2) == req_avx2 {
                        feats |= AVX2_BMI;
                    }

                    // Check AVX-512 state: opmask (bit 5), ZMM_Hi256 (bit 6), Hi16_ZMM (bit 7)
                    // 0x02 (XMM) + 0x04 (YMM) + 0x20 (k0-k7) + 0x40 (ZMM0-15) + 0x80 (ZMM16-31) = 0xe6
                    if (xcr0 & 0xe6) == 0xe6 {
                        // Leaf 7 EBX: AVX512F (bit 16), AVX512BW (bit 30), AVX512VL (bit 31)
                        let req_avx512 = (1u32 << 16) | (1u32 << 30) | (1u32 << 31);
                        if (cpuid7.ebx & req_avx512) == req_avx512 {
                            feats |= AVX512;
                        }
                    }
                }
            }

            CACHE.store(feats, Ordering::Relaxed);
            feats
        }
    }
}

#[inline(always)]
pub fn is_enabled_sse() -> bool {
    #[cfg(target_arch = "x86_64")]
    {
        true
    }
    #[cfg(target_arch = "x86")]
    {
        cfg!(target_feature = "sse") || (x86_features::get() & x86_features::SSE) != 0
    }
    #[cfg(not(any(target_arch = "x86_64", target_arch = "x86")))]
    {
        false
    }
}

#[inline(always)]
pub fn is_enabled_sse42() -> bool {
    #[cfg(any(target_arch = "x86_64", target_arch = "x86"))]
    {
        cfg!(target_feature = "sse4.2") || (x86_features::get() & x86_features::SSE42) != 0
    }
    #[cfg(not(any(target_arch = "x86_64", target_arch = "x86")))]
    {
        false
    }
}

#[inline(always)]
pub fn is_enabled_avx2_and_bmi2() -> bool {
    #[cfg(any(target_arch = "x86_64", target_arch = "x86"))]
    {
        (cfg!(target_feature = "avx2")
            && cfg!(target_feature = "bmi1")
            && cfg!(target_feature = "bmi2"))
            || (x86_features::get() & x86_features::AVX2_BMI) != 0
    }
    #[cfg(not(any(target_arch = "x86_64", target_arch = "x86")))]
    {
        false
    }
}

#[inline(always)]
pub fn is_enabled_avx512() -> bool {
    #[cfg(any(target_arch = "x86_64", target_arch = "x86"))]
    {
        (cfg!(target_feature = "avx512f")
            && cfg!(target_feature = "avx512bw")
            && cfg!(target_feature = "avx512vl"))
            || (x86_features::get() & x86_features::AVX512) != 0
    }
    #[cfg(not(any(target_arch = "x86_64", target_arch = "x86")))]
    {
        false
    }
}

#[inline(always)]
pub fn is_enabled_pclmulqdq() -> bool {
    #[cfg(any(target_arch = "x86_64", target_arch = "x86"))]
    {
        (cfg!(target_feature = "pclmulqdq") && cfg!(target_feature = "sse4.1"))
            || (x86_features::get() & x86_features::PCLMULQDQ) != 0
    }
    #[cfg(not(any(target_arch = "x86_64", target_arch = "x86")))]
    {
        false
    }
}

#[inline(always)]
pub fn is_enabled_neon() -> bool {
    cfg!(target_arch = "aarch64")
}

#[inline(always)]
pub fn is_enabled_crc() -> bool {
    #[cfg(target_arch = "aarch64")]
    {
        #[cfg(feature = "std")]
        {
            cfg!(target_feature = "crc") || std::arch::is_aarch64_feature_detected!("crc")
        }
        #[cfg(not(feature = "std"))]
        {
            cfg!(target_feature = "crc")
        }
    }
    #[cfg(not(target_arch = "aarch64"))]
    {
        false
    }
}

// FIXME: Remove lsx feature guard once MSRV is 1.89.0 or higher
#[inline(always)]
pub fn is_enabled_lsx() -> bool {
    #[cfg(all(target_arch = "loongarch64", feature = "lsx"))]
    {
        #[cfg(feature = "std")]
        {
            cfg!(target_feature = "lsx") || std::arch::is_loongarch_feature_detected!("lsx")
        }
        #[cfg(not(feature = "std"))]
        {
            cfg!(target_feature = "lsx")
        }
    }
    #[cfg(not(all(target_arch = "loongarch64", feature = "lsx")))]
    {
        false
    }
}

#[inline(always)]
pub fn is_enabled_simd128() -> bool {
    cfg!(target_arch = "wasm32") && cfg!(target_feature = "simd128")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cache_idempotence() {
        // Verifies that cold CPUID detection matches subsequent cached atomic loads.
        let first = (
            is_enabled_sse(),
            is_enabled_sse42(),
            is_enabled_avx2_and_bmi2(),
            is_enabled_avx512(),
            is_enabled_pclmulqdq(),
        );
        let second = (
            is_enabled_sse(),
            is_enabled_sse42(),
            is_enabled_avx2_and_bmi2(),
            is_enabled_avx512(),
            is_enabled_pclmulqdq(),
        );
        assert_eq!(first, second);
    }

    #[cfg(all(
        not(miri),
        feature = "std",
        any(target_arch = "x86_64", target_arch = "x86")
    ))]
    #[test]
    fn test_against_std_detection() {
        #[cfg(target_arch = "x86")]
        assert_eq!(is_enabled_sse(), std::is_x86_feature_detected!("sse"));

        assert_eq!(
            is_enabled_avx2_and_bmi2(),
            std::is_x86_feature_detected!("avx2")
                && std::is_x86_feature_detected!("bmi1")
                && std::is_x86_feature_detected!("bmi2")
        );

        assert_eq!(
            is_enabled_pclmulqdq(),
            std::is_x86_feature_detected!("pclmulqdq") && std::is_x86_feature_detected!("sse4.1")
        );

        assert_eq!(is_enabled_sse42(), std::is_x86_feature_detected!("sse4.2"));

        assert_eq!(
            is_enabled_avx512(),
            std::is_x86_feature_detected!("avx512f")
                && std::is_x86_feature_detected!("avx512bw")
                && std::is_x86_feature_detected!("avx512vl")
        );
    }

    #[cfg(all(
        feature = "std",
        not(miri),
        not(target_family = "wasm"),
        any(target_arch = "x86_64", target_arch = "x86")
    ))]
    #[test]
    fn test_multithreaded_consistency() {
        use std::thread;

        let expected = (
            is_enabled_sse(),
            is_enabled_sse42(),
            is_enabled_avx2_and_bmi2(),
            is_enabled_avx512(),
            is_enabled_pclmulqdq(),
        );

        let mut handles = Vec::new();
        for _ in 0..8 {
            handles.push(thread::spawn(move || {
                for _ in 0..100 {
                    let current = (
                        is_enabled_sse(),
                        is_enabled_sse42(),
                        is_enabled_avx2_and_bmi2(),
                        is_enabled_avx512(),
                        is_enabled_pclmulqdq(),
                    );
                    assert_eq!(current, expected);
                }
            }));
        }

        for handle in handles {
            handle.join().unwrap();
        }
    }

    #[cfg(all(not(miri), feature = "std", target_arch = "aarch64"))]
    #[test]
    fn test_against_std_detection_aarch64() {
        assert_eq!(
            is_enabled_crc(),
            std::arch::is_aarch64_feature_detected!("crc")
        );
    }
}
