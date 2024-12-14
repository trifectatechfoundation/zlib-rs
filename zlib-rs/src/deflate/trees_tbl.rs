#![forbid(unsafe_code)]

use crate::deflate::{
    Value, DIST_CODE_LEN, D_CODES, LENGTH_CODES, L_CODES, STD_MAX_MATCH, STD_MIN_MATCH,
};

const fn h(freq: u16, code: u16) -> Value {
    Value::new(freq, code)
}

#[rustfmt::skip]
pub const STATIC_LTREE: [Value; L_CODES + 2] = [
    h( 12,8), h(140,8), h( 76,8), h(204,8), h( 44,8),
    h(172,8), h(108,8), h(236,8), h( 28,8), h(156,8),
    h( 92,8), h(220,8), h( 60,8), h(188,8), h(124,8),
    h(252,8), h(  2,8), h(130,8), h( 66,8), h(194,8),
    h( 34,8), h(162,8), h( 98,8), h(226,8), h( 18,8),
    h(146,8), h( 82,8), h(210,8), h( 50,8), h(178,8),
    h(114,8), h(242,8), h( 10,8), h(138,8), h( 74,8),
    h(202,8), h( 42,8), h(170,8), h(106,8), h(234,8),
    h( 26,8), h(154,8), h( 90,8), h(218,8), h( 58,8),
    h(186,8), h(122,8), h(250,8), h(  6,8), h(134,8),
    h( 70,8), h(198,8), h( 38,8), h(166,8), h(102,8),
    h(230,8), h( 22,8), h(150,8), h( 86,8), h(214,8),
    h( 54,8), h(182,8), h(118,8), h(246,8), h( 14,8),
    h(142,8), h( 78,8), h(206,8), h( 46,8), h(174,8),
    h(110,8), h(238,8), h( 30,8), h(158,8), h( 94,8),
    h(222,8), h( 62,8), h(190,8), h(126,8), h(254,8),
    h(  1,8), h(129,8), h( 65,8), h(193,8), h( 33,8),
    h(161,8), h( 97,8), h(225,8), h( 17,8), h(145,8),
    h( 81,8), h(209,8), h( 49,8), h(177,8), h(113,8),
    h(241,8), h(  9,8), h(137,8), h( 73,8), h(201,8),
    h( 41,8), h(169,8), h(105,8), h(233,8), h( 25,8),
    h(153,8), h( 89,8), h(217,8), h( 57,8), h(185,8),
    h(121,8), h(249,8), h(  5,8), h(133,8), h( 69,8),
    h(197,8), h( 37,8), h(165,8), h(101,8), h(229,8),
    h( 21,8), h(149,8), h( 85,8), h(213,8), h( 53,8),
    h(181,8), h(117,8), h(245,8), h( 13,8), h(141,8),
    h( 77,8), h(205,8), h( 45,8), h(173,8), h(109,8),
    h(237,8), h( 29,8), h(157,8), h( 93,8), h(221,8),
    h( 61,8), h(189,8), h(125,8), h(253,8), h( 19,9),
    h(275,9), h(147,9), h(403,9), h( 83,9), h(339,9),
    h(211,9), h(467,9), h( 51,9), h(307,9), h(179,9),
    h(435,9), h(115,9), h(371,9), h(243,9), h(499,9),
    h( 11,9), h(267,9), h(139,9), h(395,9), h( 75,9),
    h(331,9), h(203,9), h(459,9), h( 43,9), h(299,9),
    h(171,9), h(427,9), h(107,9), h(363,9), h(235,9),
    h(491,9), h( 27,9), h(283,9), h(155,9), h(411,9),
    h( 91,9), h(347,9), h(219,9), h(475,9), h( 59,9),
    h(315,9), h(187,9), h(443,9), h(123,9), h(379,9),
    h(251,9), h(507,9), h(  7,9), h(263,9), h(135,9),
    h(391,9), h( 71,9), h(327,9), h(199,9), h(455,9),
    h( 39,9), h(295,9), h(167,9), h(423,9), h(103,9),
    h(359,9), h(231,9), h(487,9), h( 23,9), h(279,9),
    h(151,9), h(407,9), h( 87,9), h(343,9), h(215,9),
    h(471,9), h( 55,9), h(311,9), h(183,9), h(439,9),
    h(119,9), h(375,9), h(247,9), h(503,9), h( 15,9),
    h(271,9), h(143,9), h(399,9), h( 79,9), h(335,9),
    h(207,9), h(463,9), h( 47,9), h(303,9), h(175,9),
    h(431,9), h(111,9), h(367,9), h(239,9), h(495,9),
    h( 31,9), h(287,9), h(159,9), h(415,9), h( 95,9),
    h(351,9), h(223,9), h(479,9), h( 63,9), h(319,9),
    h(191,9), h(447,9), h(127,9), h(383,9), h(255,9),
    h(511,9), h(  0,7), h( 64,7), h( 32,7), h( 96,7),
    h( 16,7), h( 80,7), h( 48,7), h(112,7), h(  8,7),
    h( 72,7), h( 40,7), h(104,7), h( 24,7), h( 88,7),
    h( 56,7), h(120,7), h(  4,7), h( 68,7), h( 36,7),
    h(100,7), h( 20,7), h( 84,7), h( 52,7), h(116,7),
    h(  3,8), h(131,8), h( 67,8), h(195,8), h( 35,8),
    h(163,8), h( 99,8), h(227,8)
];

// precomputed lengths (as would be generated by the encode_len function)
// for all possible u8 values using STATIC_LTREE
#[rustfmt::skip]
pub const STATIC_LENGTH_ENCODINGS: [Value; 256] = [
    h(  64,  7),  h(  32,  7),  h(  96,  7),  h(  16,  7),
    h(  80,  7),  h(  48,  7),  h( 112,  7),  h(   8,  7),
    h(  72,  8),  h( 200,  8),  h(  40,  8),  h( 168,  8),
    h( 104,  8),  h( 232,  8),  h(  24,  8),  h( 152,  8),
    h(  88,  9),  h( 216,  9),  h( 344,  9),  h( 472,  9),
    h(  56,  9),  h( 184,  9),  h( 312,  9),  h( 440,  9),
    h( 120,  9),  h( 248,  9),  h( 376,  9),  h( 504,  9),
    h(   4,  9),  h( 132,  9),  h( 260,  9),  h( 388,  9),
    h(  68, 10),  h( 196, 10),  h( 324, 10),  h( 452, 10),
    h( 580, 10),  h( 708, 10),  h( 836, 10),  h( 964, 10),
    h(  36, 10),  h( 164, 10),  h( 292, 10),  h( 420, 10),
    h( 548, 10),  h( 676, 10),  h( 804, 10),  h( 932, 10),
    h( 100, 10),  h( 228, 10),  h( 356, 10),  h( 484, 10),
    h( 612, 10),  h( 740, 10),  h( 868, 10),  h( 996, 10),
    h(  20, 10),  h( 148, 10),  h( 276, 10),  h( 404, 10),
    h( 532, 10),  h( 660, 10),  h( 788, 10),  h( 916, 10),
    h(  84, 11),  h( 212, 11),  h( 340, 11),  h( 468, 11),
    h( 596, 11),  h( 724, 11),  h( 852, 11),  h( 980, 11),
    h(1108, 11),  h(1236, 11),  h(1364, 11),  h(1492, 11),
    h(1620, 11),  h(1748, 11),  h(1876, 11),  h(2004, 11),
    h(  52, 11),  h( 180, 11),  h( 308, 11),  h( 436, 11),
    h( 564, 11),  h( 692, 11),  h( 820, 11),  h( 948, 11),
    h(1076, 11),  h(1204, 11),  h(1332, 11),  h(1460, 11),
    h(1588, 11),  h(1716, 11),  h(1844, 11),  h(1972, 11),
    h( 116, 11),  h( 244, 11),  h( 372, 11),  h( 500, 11),
    h( 628, 11),  h( 756, 11),  h( 884, 11),  h(1012, 11),
    h(1140, 11),  h(1268, 11),  h(1396, 11),  h(1524, 11),
    h(1652, 11),  h(1780, 11),  h(1908, 11),  h(2036, 11),
    h(   3, 12),  h( 259, 12),  h( 515, 12),  h( 771, 12),
    h(1027, 12),  h(1283, 12),  h(1539, 12),  h(1795, 12),
    h(2051, 12),  h(2307, 12),  h(2563, 12),  h(2819, 12),
    h(3075, 12),  h(3331, 12),  h(3587, 12),  h(3843, 12),
    h( 131, 13),  h( 387, 13),  h( 643, 13),  h( 899, 13),
    h(1155, 13),  h(1411, 13),  h(1667, 13),  h(1923, 13),
    h(2179, 13),  h(2435, 13),  h(2691, 13),  h(2947, 13),
    h(3203, 13),  h(3459, 13),  h(3715, 13),  h(3971, 13),
    h(4227, 13),  h(4483, 13),  h(4739, 13),  h(4995, 13),
    h(5251, 13),  h(5507, 13),  h(5763, 13),  h(6019, 13),
    h(6275, 13),  h(6531, 13),  h(6787, 13),  h(7043, 13),
    h(7299, 13),  h(7555, 13),  h(7811, 13),  h(8067, 13),
    h(  67, 13),  h( 323, 13),  h( 579, 13),  h( 835, 13),
    h(1091, 13),  h(1347, 13),  h(1603, 13),  h(1859, 13),
    h(2115, 13),  h(2371, 13),  h(2627, 13),  h(2883, 13),
    h(3139, 13),  h(3395, 13),  h(3651, 13),  h(3907, 13),
    h(4163, 13),  h(4419, 13),  h(4675, 13),  h(4931, 13),
    h(5187, 13),  h(5443, 13),  h(5699, 13),  h(5955, 13),
    h(6211, 13),  h(6467, 13),  h(6723, 13),  h(6979, 13),
    h(7235, 13),  h(7491, 13),  h(7747, 13),  h(8003, 13),
    h( 195, 13),  h( 451, 13),  h( 707, 13),  h( 963, 13),
    h(1219, 13),  h(1475, 13),  h(1731, 13),  h(1987, 13),
    h(2243, 13),  h(2499, 13),  h(2755, 13),  h(3011, 13),
    h(3267, 13),  h(3523, 13),  h(3779, 13),  h(4035, 13),
    h(4291, 13),  h(4547, 13),  h(4803, 13),  h(5059, 13),
    h(5315, 13),  h(5571, 13),  h(5827, 13),  h(6083, 13),
    h(6339, 13),  h(6595, 13),  h(6851, 13),  h(7107, 13),
    h(7363, 13),  h(7619, 13),  h(7875, 13),  h(8131, 13),
    h(  35, 13),  h( 291, 13),  h( 547, 13),  h( 803, 13),
    h(1059, 13),  h(1315, 13),  h(1571, 13),  h(1827, 13),
    h(2083, 13),  h(2339, 13),  h(2595, 13),  h(2851, 13),
    h(3107, 13),  h(3363, 13),  h(3619, 13),  h(3875, 13),
    h(4131, 13),  h(4387, 13),  h(4643, 13),  h(4899, 13),
    h(5155, 13),  h(5411, 13),  h(5667, 13),  h(5923, 13),
    h(6179, 13),  h(6435, 13),  h(6691, 13),  h(6947, 13),
    h(7203, 13),  h(7459, 13),  h(7715, 13),  h( 163,  8)
];

#[rustfmt::skip]
pub const STATIC_DTREE: [Value; D_CODES] = [
    h( 0,5), h(16,5), h( 8,5), h(24,5), h( 4,5),
    h(20,5), h(12,5), h(28,5), h( 2,5), h(18,5),
    h(10,5), h(26,5), h( 6,5), h(22,5), h(14,5),
    h(30,5), h( 1,5), h(17,5), h( 9,5), h(25,5),
    h( 5,5), h(21,5), h(13,5), h(29,5), h( 3,5),
    h(19,5), h(11,5), h(27,5), h( 7,5), h(23,5)
];

#[rustfmt::skip]
pub const DIST_CODE: [u8; DIST_CODE_LEN] = [
     0,  1,  2,  3,  4,  4,  5,  5,  6,  6,  6,  6,  7,  7,  7,  7,  8,  8,  8,  8,
     8,  8,  8,  8,  9,  9,  9,  9,  9,  9,  9,  9, 10, 10, 10, 10, 10, 10, 10, 10,
    10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
    11, 11, 11, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,  0,  0, 16, 17,
    18, 18, 19, 19, 20, 20, 20, 20, 21, 21, 21, 21, 22, 22, 22, 22, 22, 22, 22, 22,
    23, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
    24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
    26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
    26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27,
    27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27,
    27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
    29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
    29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
    29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29
];

#[rustfmt::skip]
pub const LENGTH_CODE: [u8; STD_MAX_MATCH-STD_MIN_MATCH+1] = [
     0,  1,  2,  3,  4,  5,  6,  7,  8,  8,  9,  9, 10, 10, 11, 11, 12, 12, 12, 12,
    13, 13, 13, 13, 14, 14, 14, 14, 15, 15, 15, 15, 16, 16, 16, 16, 16, 16, 16, 16,
    17, 17, 17, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18, 18, 18, 18, 19, 19, 19, 19,
    19, 19, 19, 19, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
    21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 22, 22, 22, 22,
    22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23,
    23, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
    24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
    25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
    25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 26, 26, 26, 26, 26, 26, 26, 26,
    26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
    26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27,
    27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 28
];

pub const BASE_LENGTH: [u8; LENGTH_CODES] = [
    0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 14, 16, 20, 24, 28, 32, 40, 48, 56, 64, 80, 96, 112, 128,
    160, 192, 224, 0,
];

#[rustfmt::skip]
pub const BASE_DIST: [u16; D_CODES] = [
    0,     1,     2,     3,     4,     6,     8,    12,    16,    24,
   32,    48,    64,    96,   128,   192,   256,   384,   512,   768,
 1024,  1536,  2048,  3072,  4096,  6144,  8192, 12288, 16384, 24576
];
