use super::MAX_TSUMOS_LEFT;
use super::tile::RequiredTile;
use crate::tile::Tile;
use std::cmp::Ordering;

use tinyvec::ArrayVec;

#[derive(Debug)]
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
}

#[derive(Default)]
pub struct RawCandidate<'a> {
    pub tile: Tile,
    pub tenpai_probs: &'a [f32],
    pub win_probs: &'a [f32],
    pub exp_values: &'a [f32],
    pub required_tiles: ArrayVec<[RequiredTile; 34]>,
    pub shanten_down: bool,
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
        }: RawCandidate<'_>,
    ) -> Self {
        let num_required_tiles = required_tiles.iter().map(|r| r.count).sum();
        let tenpai_probs = tenpai_probs.iter().map(|p| p.clamp(0., 1.)).collect();
        let win_probs = win_probs.iter().map(|p| p.clamp(0., 1.)).collect();
        let exp_values = exp_values.iter().map(|v| v.max(0.)).collect();

        Self {
            tile,
            tenpai_probs,
            win_probs,
            exp_values,
            required_tiles,
            num_required_tiles,
            shanten_down,
        }
    }
}

impl Candidate {
    pub fn cmp(&self, other: &Self, by: CandidateColumn) -> Ordering {
        if self.tile == other.tile {
            return Ordering::Equal;
        }
        match by {
            CandidateColumn::EV => match self.exp_values[0].total_cmp(&other.exp_values[0]) {
                Ordering::Equal => self.cmp(other, CandidateColumn::WinProb),
                o => o,
            },
            CandidateColumn::WinProb => match self.win_probs[0].total_cmp(&other.win_probs[0]) {
                Ordering::Equal => self.cmp(other, CandidateColumn::TenpaiProb),
                o => o,
            },
            CandidateColumn::TenpaiProb => {
                match self.tenpai_probs[0].total_cmp(&other.tenpai_probs[0]) {
                    Ordering::Equal => self.cmp(other, CandidateColumn::NotShantenDown),
                    o => o,
                }
            }
            CandidateColumn::NotShantenDown => match (self.shanten_down, other.shanten_down) {
                (false, true) => Ordering::Greater,
                (true, false) => Ordering::Less,
                _ => self.cmp(other, CandidateColumn::NumRequiredTiles),
            },
            CandidateColumn::NumRequiredTiles => {
                match self.num_required_tiles.cmp(&other.num_required_tiles) {
                    Ordering::Equal => self.cmp(other, CandidateColumn::DiscardPriority),
                    o => o,
                }
            }
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
            &[
                "EV",
                "Win prob",
                "Tenpai prob",
                "Kinds",
                "Sum",
                "Required tiles",
            ]
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
