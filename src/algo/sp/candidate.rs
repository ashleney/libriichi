use super::MAX_TSUMOS_LEFT;
use super::tile::RequiredTile;
use crate::algo::agari::yaku::{YAKU_COUNT, yaku};
use crate::tile::Tile;
use std::cmp::Ordering;
use tinyvec::ArrayVec;

#[derive(Debug, Clone)]
pub struct WeightedYaku {
    /// Indexes of yaku and the chances of winning with them
    // this array is larger than needed but making it smaller leads to more complex code
    pub yaku: [f32; YAKU_COUNT],
    /// Average expected amount of dora
    pub dora: f32,
    /// Average expected amount of akadora
    pub aka_dora: f32,
    /// Average expected amount of uradora
    pub ura_dora: f32,
}

#[derive(Debug, Default)]
pub struct Candidate {
    /// 打牌
    pub tile: Tile,
    /// 巡目ごとの聴牌確率
    pub tenpai_probs: ArrayVec<[f32; MAX_TSUMOS_LEFT]>,
    /// 巡目ごとの和了確率
    pub win_probs: ArrayVec<[f32; MAX_TSUMOS_LEFT]>,
    /// 巡目ごとの期待値
    pub exp_values: ArrayVec<[f32; MAX_TSUMOS_LEFT]>,
    /// 有効牌及び枚数の一覧
    pub required_tiles: ArrayVec<[RequiredTile; 34]>,
    pub num_required_tiles: u8,
    /// 向聴戻しになるかどうか
    pub shanten_down: bool,
    /// Chances of a hand winning with certain yaku
    pub yaku: ArrayVec<[WeightedYaku; MAX_TSUMOS_LEFT]>,
}

#[derive(Default)]
pub struct RawCandidate<'a> {
    pub tile: Tile,
    pub tenpai_probs: &'a [f32],
    pub win_probs: &'a [f32],
    pub exp_values: &'a [f32],
    pub required_tiles: ArrayVec<[RequiredTile; 34]>,
    pub shanten_down: bool,
    pub yaku: &'a [WeightedYaku],
}

#[derive(Clone, Copy)]
pub enum CandidateColumn {
    EV,
    WinProb,
    TenpaiProb,
    NotShantenDown,
    NumRequiredTiles,
    DiscardPriority,
}

impl From<RawCandidate<'_>> for Candidate {
    fn from(
        RawCandidate {
            tile,
            tenpai_probs,
            win_probs,
            exp_values,
            required_tiles,
            shanten_down,
            yaku,
        }: RawCandidate<'_>,
    ) -> Self {
        let num_required_tiles = required_tiles.iter().map(|r| r.count).sum();
        let tenpai_probs = tenpai_probs.iter().map(|p| p.clamp(0., 1.)).collect();
        let win_probs = win_probs.iter().map(|p| p.clamp(0., 1.)).collect();
        let exp_values = exp_values.iter().map(|v| v.max(0.)).collect();
        let yaku = yaku.iter().cloned().collect();

        Self {
            tile,
            tenpai_probs,
            win_probs,
            exp_values,
            required_tiles,
            num_required_tiles,
            shanten_down,
            yaku,
        }
    }
}

impl Candidate {
    pub fn cmp(&self, other: &Self, by: CandidateColumn) -> Ordering {
        macro_rules! cmp_first {
            ($self_slice:expr, $other_slice:expr) => {{
                let self_val = $self_slice.first().copied().unwrap_or(0.);
                let other_val = $other_slice.first().copied().unwrap_or(0.);
                self_val.total_cmp(&other_val)
            }};
        }

        match by {
            CandidateColumn::EV => match cmp_first!(self.exp_values, other.exp_values) {
                Ordering::Equal => self.cmp(other, CandidateColumn::WinProb),
                o => o,
            },
            CandidateColumn::WinProb => match cmp_first!(self.win_probs, other.win_probs) {
                Ordering::Equal => self.cmp(other, CandidateColumn::TenpaiProb),
                o => o,
            },
            CandidateColumn::TenpaiProb => match cmp_first!(self.tenpai_probs, other.tenpai_probs) {
                Ordering::Equal => self.cmp(other, CandidateColumn::NotShantenDown),
                o => o,
            },
            CandidateColumn::NotShantenDown => match (self.shanten_down, other.shanten_down) {
                (false, true) => Ordering::Greater,
                (true, false) => Ordering::Less,
                _ => self.cmp(other, CandidateColumn::NumRequiredTiles),
            },
            CandidateColumn::NumRequiredTiles => match self.num_required_tiles.cmp(&other.num_required_tiles) {
                Ordering::Equal => self.cmp(other, CandidateColumn::DiscardPriority),
                o => o,
            },
            CandidateColumn::DiscardPriority => self.tile.cmp_discard_priority(other.tile),
        }
    }

    pub const fn csv_header(can_discard: bool) -> &'static [&'static str] {
        if can_discard {
            &[
                "Tile",
                "EV",
                "Win prob",
                "Tenpai prob",
                "Shanten down?",
                "Kinds",
                "Sum",
                "Required tiles",
            ]
        } else {
            &["EV", "Win prob", "Tenpai prob", "Kinds", "Sum", "Required tiles"]
        }
    }

    pub fn csv_row(&self, can_discard: bool) -> Vec<String> {
        let required_tiles = self
            .required_tiles
            .iter()
            .map(|r| format!("{}@{}", r.tile, r.count))
            .collect::<Vec<_>>()
            .join(",");
        if can_discard {
            vec![
                self.tile.to_string(),
                format!("{:.03}", self.exp_values.first().unwrap_or(&0.)),
                format!("{:.03}", self.win_probs.first().unwrap_or(&0.) * 100.),
                format!("{:.03}", self.tenpai_probs.first().unwrap_or(&0.) * 100.),
                if self.shanten_down { "Yes" } else { "No" }.to_owned(),
                self.required_tiles.len().to_string(),
                self.num_required_tiles.to_string(),
                required_tiles,
            ]
        } else {
            vec![
                format!("{:.03}", self.exp_values.first().unwrap_or(&0.)),
                format!("{:.03}", self.win_probs.first().unwrap_or(&0.) * 100.),
                format!("{:.03}", self.tenpai_probs.first().unwrap_or(&0.) * 100.),
                self.required_tiles.len().to_string(),
                self.num_required_tiles.to_string(),
                required_tiles,
            ]
        }
    }
}

impl WeightedYaku {
    #[inline]
    pub fn from_score(
        yaku: &Vec<u8>,
        dora: u8,
        aka_dora: u8,
        ura_dora: f32,
        win_double_riichi: bool,
        win_ippatsu: bool,
        win_haitei: bool,
    ) -> Self {
        let mut yaku_array = [0.; YAKU_COUNT];
        for &y in yaku {
            yaku_array[y as usize] = 1.;
        }
        if win_double_riichi {
            yaku_array[yaku!("ダブル立直") as usize] = 1.;
        }
        if win_ippatsu {
            yaku_array[yaku!("一発") as usize] = 1.;
        }
        if win_haitei {
            yaku_array[yaku!("海底摸月") as usize] = 1.;
        }
        Self {
            yaku: yaku_array,
            dora: dora as f32,
            aka_dora: aka_dora as f32,
            ura_dora,
        }
    }

    pub fn add(&mut self, other: &Self, prob: f32) {
        for (yaku, &p) in other.yaku.iter().enumerate() {
            self.yaku[yaku] += p * prob;
        }
        self.dora += other.dora * prob;
        self.aka_dora += other.aka_dora * prob;
        self.ura_dora += other.ura_dora * prob;
    }
}

impl WeightedYaku {
    /// List of yaku sorted by probability
    pub fn sorted_yaku(&self) -> Vec<(u8, f32)> {
        let mut yaku: Vec<_> = self
            .yaku
            .iter()
            .enumerate()
            .filter_map(|(yaku, &prob)| (prob > 0.).then_some((yaku as u8, prob)))
            .collect();
        yaku.sort_by(|(_, a), (_, b)| b.partial_cmp(a).unwrap());
        yaku
    }
}

impl Default for WeightedYaku {
    fn default() -> Self {
        Self {
            yaku: [0.0; YAKU_COUNT],
            dora: 0.0,
            aka_dora: 0.0,
            ura_dora: 0.0,
        }
    }
}
