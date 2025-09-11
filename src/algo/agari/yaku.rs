pub const YAKU_COUNT: usize = 50;

pub const YAKU_NAMES: [[&str; 3]; YAKU_COUNT] = [
    ["門前清自摸和", "Fully Concealed Hand", "Menzen"],
    ["立直", "Riichi", "Riichi"],
    ["ダブル立直", "Double Riichi", "Double Riichi"],
    ["一発", "Ippatsu", "Ippatsu"],
    ["平和", "Pinfu", "Pinfu"],
    ["断幺九", "All Simples", "Tanyao"],
    ["一盃口", "Pure Double Sequence", "Iipeikou"],
    ["二盃口", "Twice Pure Double Sequence", "Ryanpeikou"],
    ["役牌,白", "White Dragon", "Haku"],
    ["役牌,發", "Green Dragon", "Hatsu"],
    ["役牌,中", "Red Dragon", "Chun"],
    ["役牌,自風", "Seat Wind", "Jikaze"],
    ["役牌,場風", "Prevalent Wind", "Bakaze"],
    ["三色同順", "Mixed Triple Sequence", "Sanshoku"],
    ["一気通貫", "Pure Straight", "Ittsuu"],
    ["混全帯幺九", "Half Outside Hand", "Chanta"],
    ["純全帯幺九", "Fully Outside Hand", "Junchan"],
    ["対々和", "All Triplets", "Toitoi"],
    ["三暗刻", "Three Concealed Triplets", "Sanankou"],
    ["三槓子", "Three Quads", "Suukantsu"],
    ["三色同刻", "Triple Triplets", "Sanshoku Doukou"],
    ["小三元", "Little Three Dragons", "Shousangen"],
    ["混老頭", "All Terminals and Honors", "Honroutou"],
    ["七対子", "Seven Pairs", "Chiitoi"],
    ["混一色", "Half Flush", "Honitsu"],
    ["清一色", "Full Flush", "Chinitsu"],
    ["海底摸月", "Under the Sea", "Haitei"],
    ["河底撈魚", "Under the River", "Houtei"],
    ["嶺上開花", "After a Kan", "Rinshan"],
    ["槍槓", "Robbing a Kan", "Chankan"],
    ["ドラ", "Dora", "Dora"],
    ["赤ドラ", "Red Five", "Akadora"],
    ["裏ドラ", "Ura Dora", "Uradora"],
    ["抜きドラ", "Kita", "Nukidora"],
    ["流し満貫", "Nagashi Mangan", "Nagashi Mangan"],
    ["天和", "Blessing of Heaven", "Tenhou"],
    ["地和", "Blessing of Earth", "Chiihou"],
    ["大三元", "Big Three Dragons", "Daisuushii"],
    ["四暗刻", "Four Concealed Triplets", "Suuankou"],
    ["四暗刻単騎", "Single-wait Four Concealed Triplets", "Suuankou Tanki"],
    ["字一色", "All Honors", "Tsuuiisou"],
    ["緑一色", "All Green", "Ryuuiisou"],
    ["清老頭", "All Terminals", "Chinroutou"],
    ["国士無双", "Thirteen Orphans", "Kokushi"],
    ["国士無双十三面待ち", "Thirteen-wait Thirteen Orphans", "Kokushi Juusanmen"],
    ["小四喜", "Four Little Winds", "Shousuushii"],
    ["大四喜", "Four Big Winds", "Daisuushii"],
    ["四槓子", "Four Quads", "Suukantsu"],
    ["九蓮宝燈", "Nine Gates", "Chuuren"],
    ["純正九蓮宝燈", "True Nine Gates", "Chuuren Poutou"],
];

#[rustfmt::skip]
macro_rules! yaku {
    // Each arm matches the Japanese string literal and expands to its index.
    ("門前清自摸和") => (0_u8);
    ("立直") => (1_u8);
    ("ダブル立直") => (2_u8);
    ("一発") => (3_u8);
    ("平和") => (4_u8);
    ("断幺九") => (5_u8);
    ("一盃口") => (6_u8);
    ("二盃口") => (7_u8);
    ("役牌,白") => (8_u8);
    ("役牌,發") => (9_u8);
    ("役牌,中") => (10_u8);
    ("役牌,自風") => (11_u8);
    ("役牌,場風") => (12_u8);
    ("三色同順") => (13_u8);
    ("一気通貫") => (14_u8);
    ("混全帯幺九") => (15_u8);
    ("純全帯幺九") => (16_u8);
    ("対々和") => (17_u8);
    ("三暗刻") => (18_u8);
    ("三槓子") => (19_u8);
    ("三色同刻") => (20_u8);
    ("小三元") => (21_u8);
    ("混老頭") => (22_u8);
    ("七対子") => (23_u8);
    ("混一色") => (24_u8);
    ("清一色") => (25_u8);
    ("海底摸月") => (26_u8);
    ("河底撈魚") => (27_u8);
    ("嶺上開花") => (28_u8);
    ("槍槓") => (29_u8);
    ("ドラ") => (30_u8);
    ("赤ドラ") => (31_u8);
    ("裏ドラ") => (32_u8);
    ("抜きドラ") => (33_u8);
    ("流し満貫") => (34_u8);
    ("天和") => (35_u8);
    ("地和") => (36_u8);
    ("大三元") => (37_u8);
    ("四暗刻") => (38_u8);
    ("四暗刻単騎") => (39_u8);
    ("字一色") => (40_u8);
    ("緑一色") => (41_u8);
    ("清老頭") => (42_u8);
    ("国士無双") => (43_u8);
    ("国士無双十三面待ち") => (44_u8);
    ("小四喜") => (45_u8);
    ("大四喜") => (46_u8);
    ("四槓子") => (47_u8);
    ("九蓮宝燈") => (48_u8);
    ("純正九蓮宝燈") => (49_u8);
    ($other:literal) => { compile_error!(concat!("Unknown yaku ", $other)) };
}

macro_rules! localized_yaku {
    ($jp:tt, $language:expr) => {{ $crate::algo::agari::yaku::YAKU_NAMES[yaku!($jp) as usize][$language as usize] }};
}

pub(crate) use {localized_yaku, yaku};

#[derive(Debug, Clone, Copy)]
pub enum YakuLanguage {
    Japanese = 0,
    EnglishTenhou = 1,
    RomajiShort = 2,
}

#[inline(always)]
pub const fn localize_yaku(yaku: u8, language: YakuLanguage) -> &'static str {
    YAKU_NAMES[yaku as usize][language as usize]
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn yaku_macro_indexes_match() {
        for (idx, localization) in YAKU_NAMES.iter().enumerate() {
            let m_idx: u8 = match localization[0] {
                "門前清自摸和" => yaku!("門前清自摸和"),
                "立直" => yaku!("立直"),
                "ダブル立直" => yaku!("ダブル立直"),
                "一発" => yaku!("一発"),
                "平和" => yaku!("平和"),
                "断幺九" => yaku!("断幺九"),
                "一盃口" => yaku!("一盃口"),
                "二盃口" => yaku!("二盃口"),
                "役牌,白" => yaku!("役牌,白"),
                "役牌,發" => yaku!("役牌,發"),
                "役牌,中" => yaku!("役牌,中"),
                "役牌,自風" => yaku!("役牌,自風"),
                "役牌,場風" => yaku!("役牌,場風"),
                "三色同順" => yaku!("三色同順"),
                "一気通貫" => yaku!("一気通貫"),
                "混全帯幺九" => yaku!("混全帯幺九"),
                "純全帯幺九" => yaku!("純全帯幺九"),
                "対々和" => yaku!("対々和"),
                "三暗刻" => yaku!("三暗刻"),
                "三槓子" => yaku!("三槓子"),
                "三色同刻" => yaku!("三色同刻"),
                "小三元" => yaku!("小三元"),
                "混老頭" => yaku!("混老頭"),
                "七対子" => yaku!("七対子"),
                "混一色" => yaku!("混一色"),
                "清一色" => yaku!("清一色"),
                "海底摸月" => yaku!("海底摸月"),
                "河底撈魚" => yaku!("河底撈魚"),
                "嶺上開花" => yaku!("嶺上開花"),
                "槍槓" => yaku!("槍槓"),
                "ドラ" => yaku!("ドラ"),
                "赤ドラ" => yaku!("赤ドラ"),
                "裏ドラ" => yaku!("裏ドラ"),
                "抜きドラ" => yaku!("抜きドラ"),
                "流し満貫" => yaku!("流し満貫"),
                "天和" => yaku!("天和"),
                "地和" => yaku!("地和"),
                "大三元" => yaku!("大三元"),
                "四暗刻" => yaku!("四暗刻"),
                "四暗刻単騎" => yaku!("四暗刻単騎"),
                "字一色" => yaku!("字一色"),
                "緑一色" => yaku!("緑一色"),
                "清老頭" => yaku!("清老頭"),
                "国士無双" => yaku!("国士無双"),
                "国士無双十三面待ち" => yaku!("国士無双十三面待ち"),
                "小四喜" => yaku!("小四喜"),
                "大四喜" => yaku!("大四喜"),
                "四槓子" => yaku!("四槓子"),
                "九蓮宝燈" => yaku!("九蓮宝燈"),
                "純正九蓮宝燈" => yaku!("純正九蓮宝燈"),
                _ => unreachable!(),
            };

            assert_eq!(m_idx as usize, idx, "Macro index mismatch for {}", localization[0]);
        }
    }
}
