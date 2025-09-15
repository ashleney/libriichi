use super::candidate::{RawCandidate, WeightedYaku};
use super::state::{InitState, State};
use super::tile::{DiscardTile, DrawTile};
use super::{Candidate, CandidateColumn, MAX_TSUMOS_LEFT};
use crate::algo::agari::calc::{Agari, AgariCalculator};
use crate::algo::agari::yaku::yaku;
use crate::tile::Tile;
use crate::{must_tile, t, tu8};
use std::array::from_fn;
use std::rc::Rc;

use ahash::AHashMap;
use anyhow::{Result, ensure};

const MAX_TILES_LEFT: usize = 34 * 4 - 1 - 13;

/// 裏ドラの乗る確率のテーブル
const URADORA_PROB_TABLE: [[f32; 13]; 5] = include!("../data/uradora_prob_table.txt");

// 5 shanten is the realistic maximum that we can use to analyze
type StateCache<const MAX_TSUMO: usize> = [AHashMap<State, Rc<Values<MAX_TSUMO>>>; 6];

struct Values<const MAX_TSUMO: usize> {
    tenpai_probs: [f32; MAX_TSUMO],
    win_probs: [f32; MAX_TSUMO],
    exp_values: [f32; MAX_TSUMO],
    yaku: [WeightedYaku; MAX_TSUMO],
}

struct Scores {
    scores: [f32; 4],
    yaku: Vec<u8>,
    dora: u8,
    aka_dora: u8,
    ura_dora: f32,
}

enum ScoresOrValues<const MAX_TSUMO: usize> {
    // shanten == 0, and has yaku
    Scores(Scores),
    // shanten > 0
    Values(Rc<Values<MAX_TSUMO>>),
}

#[derive(Debug)]
pub struct SPCalculator<'a> {
    // Immutable states, used in agari calculator.
    pub tehai_len_div3: u8,
    pub chis: &'a [u8],
    pub pons: &'a [u8],
    pub minkans: &'a [u8],
    pub ankans: &'a [u8],
    pub bakaze: u8,
    pub jikaze: u8,
    pub is_menzen: bool,

    /// Unlike others, fuuro here includes ankan.
    pub num_doras_in_fuuro: u8,
    pub dora_indicators: &'a [Tile],
    pub calc_double_riichi: bool,
    pub calc_haitei: bool,
    pub prefer_riichi: bool,
    pub sort_result: bool,

    pub maximize_win_prob: bool,
    pub max_shanten: i8,
    pub calc_tegawari: Option<i8>,
    pub calc_shanten_down: Option<i8>,
    pub min_score: Option<i32>,
}

struct SPCalculatorState<'a, const MAX_TSUMO: usize> {
    sup: &'a SPCalculator<'a>,
    state: State,

    tsumo_prob_table: &'a [[f32; MAX_TSUMO]; 4],
    not_tsumo_prob_table: &'a [[f32; MAX_TSUMO]; MAX_TILES_LEFT + 1],

    discard_cache: StateCache<MAX_TSUMO>,
    draw_cache: StateCache<MAX_TSUMO>,
}

impl SPCalculator<'_> {
    /// Arguments:
    /// - can_discard: whether the tehai is 3n+2 or not.
    /// - tsumos_left: must be within [1, 17].
    /// - cur_shanten: must be >= 0.
    ///
    /// The return value will be sorted and index 0 will be the best choice.
    pub fn calc(&self, init_state: InitState, can_discard: bool, tsumos_left: u8, cur_shanten: i8) -> Result<Vec<Candidate>> {
        ensure!(cur_shanten >= 0, "can't calculate an agari hand");
        ensure!(tsumos_left <= MAX_TSUMOS_LEFT as u8);

        let max_tsumo = tsumos_left as usize;

        let state = State::from(init_state);
        let n_left_tiles = state.sum_left_tiles() as usize;

        if tsumos_left == 0 {
            // only ever simple
            let mut calc_state = SPCalculatorState::<0> {
                sup: self,
                state,
                tsumo_prob_table: &[[]; 4],
                not_tsumo_prob_table: &[[]; 123],
                discard_cache: Default::default(),
                draw_cache: Default::default(),
            };
            return Ok(calc_state.calc(can_discard, cur_shanten));
        }

        // Despite the bloating binary size, the use of const generics here may
        // help eliminate branches (eg. bound checks) and reduce buffer space,
        // and allow more aggressive loop unroll and vectorization.
        macro_rules! static_expand {
            ($($n:literal),*) => {
                match max_tsumo {
                    $($n => {
                        let tsumo_prob_table = build_tsumo_prob_table(n_left_tiles);
                        let not_tsumo_prob_table = build_not_tsumo_prob_table(n_left_tiles);
                        let mut calc_state = SPCalculatorState::<$n> {
                            sup: self,
                            state,
                            tsumo_prob_table: &tsumo_prob_table,
                            not_tsumo_prob_table: &not_tsumo_prob_table,
                            discard_cache: Default::default(),
                            draw_cache: Default::default(),
                        };
                        calc_state.calc(can_discard, cur_shanten)
                    },)*
                    _ => unreachable!(),
                }
            }
        }
        let candidates = static_expand!(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17);
        Ok(candidates)
    }
}

fn build_tsumo_prob_table<const MAX_TSUMO: usize>(n_left_tiles: usize) -> [[f32; MAX_TSUMO]; 4] {
    let mut table = [[0.; MAX_TSUMO]; 4];
    // 有効牌の枚数ごとに、この巡目で有効牌を引ける確率のテーブルを作成する。
    // tumo_prob_table_[i][j] = 有効牌の枚数が i + 1 枚の場合に j 巡目に有効牌が引ける確率
    for (i, row) in table.iter_mut().enumerate() {
        for (j, v) in row.iter_mut().enumerate() {
            *v = (i + 1) as f32 / (n_left_tiles - j) as f32;
        }
    }
    table
}

fn build_not_tsumo_prob_table<const MAX_TSUMO: usize>(n_left_tiles: usize) -> [[f32; MAX_TSUMO]; MAX_TILES_LEFT + 1] {
    let mut table = [[0.; MAX_TSUMO]; MAX_TILES_LEFT + 1];
    // 有効牌の合計枚数ごとに、これまでの巡目で有効牌が引けなかった確率のテーブルを作成する。
    // not_tumo_prob_table_[i][j] = 有効牌の合計枚数が i 枚の場合に j - 1 巡目までに有効牌が引けなかった確率
    //
    // The original version has only `n_left_tiles` rows, which can actually
    // overflow for hands like 9999m6677p88s335z that can be improved by all
    // kinds of tiles, and the number of all tiles left will be exactly
    // `n_left_tiles`. A test case covers this.
    for (i, row) in table.iter_mut().enumerate().take(n_left_tiles + 1) {
        row[0] = 1.;
        // n_left_tiles - i - j > 0 は残りはすべて有効牌の場合を考慮
        for j in 0..(MAX_TSUMO - 1).min(n_left_tiles - i) {
            row[j + 1] = row[j] * (n_left_tiles - i - j) as f32 / (n_left_tiles - j) as f32;
        }
    }
    table
}

impl<const MAX_TSUMO: usize> SPCalculatorState<'_, MAX_TSUMO> {
    fn calc(&mut self, can_discard: bool, cur_shanten: i8) -> Vec<Candidate> {
        if cur_shanten <= self.sup.max_shanten && MAX_TSUMO >= 1 {
            // 3向聴以下は聴牌確率、和了確率、期待値を計算する。
            let mut candidates = if can_discard {
                self.analyze_discard(cur_shanten)
            } else {
                self.analyze_draw(cur_shanten)
            };

            if self.sup.sort_result && !candidates.is_empty() {
                let by = if self.sup.maximize_win_prob {
                    CandidateColumn::WinProb
                } else {
                    CandidateColumn::EV
                };
                candidates.sort_by(|l, r| r.cmp(l, by));
            }
            candidates
        } else {
            // 4向聴以上は受入枚数のみ計算する。
            let mut candidates = if can_discard {
                self.analyze_discard_simple(cur_shanten)
            } else {
                self.analyze_draw_simple()
            };

            if self.sup.sort_result && !candidates.is_empty() {
                candidates.sort_by(|l, r| r.cmp(l, CandidateColumn::NotShantenDown));
            }
            candidates
        }
    }

    fn analyze_discard(&mut self, shanten: i8) -> Vec<Candidate> {
        // 打牌候補を取得する。
        let discard_tiles = self.state.get_discard_tiles(shanten, self.sup.tehai_len_div3);

        let mut candidates = Vec::with_capacity(discard_tiles.len());
        for DiscardTile { tile, shanten_diff } in discard_tiles {
            if shanten_diff == 0 {
                self.state.discard(tile);
                let required_tiles = self.state.get_required_tiles(self.sup.tehai_len_div3);
                let values = self.draw(shanten);
                self.state.undo_discard(tile);

                let mut tenpai_probs = values.tenpai_probs;
                if shanten == 0 {
                    // すでに聴牌している場合の例外処理
                    tenpai_probs.fill(1.);
                }

                let candidate = Candidate::from(RawCandidate {
                    tile,
                    tenpai_probs: &tenpai_probs,
                    win_probs: &values.win_probs,
                    exp_values: &values.exp_values,
                    required_tiles,
                    shanten_down: false,
                    yaku: &values.yaku,
                });
                candidates.push(candidate);
            } else if let Some(shanten_down_max) = self.sup.calc_shanten_down
                && shanten_diff == 1
                && shanten < shanten_down_max
            {
                self.state.discard(tile);
                let required_tiles = self.state.get_required_tiles(self.sup.tehai_len_div3);
                self.state.n_extra_tsumo += 1;
                let values = self.draw(shanten + 1);
                self.state.n_extra_tsumo -= 1;
                self.state.undo_discard(tile);

                let candidate = Candidate::from(RawCandidate {
                    tile,
                    tenpai_probs: &values.tenpai_probs,
                    win_probs: &values.win_probs,
                    exp_values: &values.exp_values,
                    required_tiles,
                    shanten_down: true,
                    yaku: &values.yaku,
                });
                candidates.push(candidate);
            }
        }
        candidates
    }

    fn analyze_draw(&mut self, shanten: i8) -> Vec<Candidate> {
        let required_tiles = self.state.get_required_tiles(self.sup.tehai_len_div3);
        let values = self.draw(shanten);

        let mut tenpai_probs = values.tenpai_probs;
        if shanten == 0 {
            // すでに聴牌している場合の例外処理
            tenpai_probs.fill(1.);
        }

        let candidate = Candidate::from(RawCandidate {
            tile: t!(?),
            tenpai_probs: &tenpai_probs,
            win_probs: &values.win_probs,
            exp_values: &values.exp_values,
            required_tiles,
            shanten_down: false,
            yaku: &values.yaku,
        });
        vec![candidate]
    }

    fn analyze_discard_simple(&mut self, shanten: i8) -> Vec<Candidate> {
        // 打牌候補を取得する。
        let discard_tiles = self.state.get_discard_tiles(shanten, self.sup.tehai_len_div3);
        discard_tiles
            .into_iter()
            .map(|DiscardTile { tile, shanten_diff }| {
                self.state.discard(tile);
                let required_tiles = self.state.get_required_tiles(self.sup.tehai_len_div3);
                self.state.undo_discard(tile);

                Candidate::from(RawCandidate {
                    tile,
                    required_tiles,
                    shanten_down: shanten_diff == 1,
                    ..Default::default()
                })
            })
            .collect()
    }

    fn analyze_draw_simple(&mut self) -> Vec<Candidate> {
        let required_tiles = self.state.get_required_tiles(self.sup.tehai_len_div3);
        let candidate = Candidate::from(RawCandidate {
            tile: t!(?),
            required_tiles,
            shanten_down: false,
            ..Default::default()
        });
        vec![candidate]
    }

    fn draw(&mut self, shanten: i8) -> Rc<Values<MAX_TSUMO>> {
        if let Some(tegawari_max_shanten) = self.sup.calc_tegawari
            && tegawari_max_shanten <= shanten
            && self.state.n_extra_tsumo == 0
        {
            self.draw_with_tegawari(shanten)
        } else {
            self.draw_without_tegawari(shanten)
        }
    }

    fn draw_with_tegawari(&mut self, shanten: i8) -> Rc<Values<MAX_TSUMO>> {
        self.draw_cache[shanten as usize]
            .get(&self.state)
            .cloned()
            .unwrap_or_else(|| self.draw_with_tegawari_slow(shanten))
    }

    fn draw_with_tegawari_slow(&mut self, shanten: i8) -> Rc<Values<MAX_TSUMO>> {
        let mut tenpai_probs = [0.; MAX_TSUMO];
        let mut win_probs = [0.; MAX_TSUMO];
        let mut exp_values = [0.; MAX_TSUMO];
        let mut yaku: [WeightedYaku; MAX_TSUMO] = from_fn(|_| WeightedYaku::default());

        // 自摸候補を取得する。
        let draw_tiles = self.state.get_draw_tiles(shanten, self.sup.tehai_len_div3);

        // 有効牌の合計枚数を計算する。【暫定対応】
        let sum_left_tiles = self.state.sum_left_tiles();

        for &DrawTile {
            tile,
            count,
            shanten_diff,
        } in &draw_tiles
        {
            if shanten_diff != -1 {
                // 有効牌以外の場合
                continue;
            }

            self.state.deal(tile);
            let scores_or_values = if shanten > 0 {
                ScoresOrValues::Values(self.discard(shanten - 1))
            } else if let Some(scores) = self.get_score(tile) {
                ScoresOrValues::Scores(scores)
            } else {
                self.state.undo_deal(tile);
                continue;
            };
            self.state.undo_deal(tile);

            // 【暫定対応】 (2021/9/24)
            // FIX_TEGAWARI_PROB について
            // draw_without_tegawari() で有効牌が引けない場合、有効牌以外のどの牌を引いたのかということは考慮していないため、
            // counts で管理している各牌の残りの合計枚数 > 現在の巡目の残り枚数という状況が発生し、結果的に確率値が1を超えてしまう。
            // 実際に正しい確率値を求めるには、draw_without_tegawari() でどの牌を引いたのかをすべてシミュレーションする必要があるが、
            // 計算量的に難しいので、巡目に関係なく、
            //「自摸の確率 = 牌の残り枚数 / 残り枚数の合計」で確率値が1を超えないように暫定対応した。

            for i in 0..MAX_TSUMO {
                // 【暫定対応】 (2021/9/24)
                let tump_prob = count as f32 / sum_left_tiles as f32;
                // let tump_prob = &TSUMO_PROB_TABLE[count as usize - 1][i];

                match &scores_or_values {
                    ScoresOrValues::Scores(scores) => {
                        let assume_riichi = self.sup.is_menzen && self.sup.prefer_riichi;
                        // 聴牌の場合は次で和了
                        // i 巡目で聴牌の場合はダブル立直成立
                        let win_double_riichi = assume_riichi && self.sup.calc_double_riichi && i == 0;
                        // i 巡目で聴牌し、次の巡目で和了の場合は一発成立
                        let win_ippatsu = assume_riichi;
                        // 最後の巡目で和了の場合は海底撈月成立
                        let win_haitei = self.sup.calc_haitei && i == MAX_TSUMO - 1;
                        let han_plus = win_double_riichi as usize + win_ippatsu as usize + win_haitei as usize;

                        win_probs[i] += tump_prob;
                        exp_values[i] += tump_prob * scores.scores[han_plus];
                        yaku[i].add(
                            &WeightedYaku::from_score(
                                &scores.yaku,
                                scores.dora,
                                scores.aka_dora,
                                scores.ura_dora,
                                win_double_riichi,
                                win_ippatsu,
                                win_haitei,
                            ),
                            tump_prob,
                        );
                    }
                    ScoresOrValues::Values(next_values) => {
                        if shanten == 1 {
                            // 1向聴の場合は次で聴牌
                            tenpai_probs[i] += tump_prob;
                        }
                        if i < MAX_TSUMO - 1 {
                            if shanten > 1 {
                                tenpai_probs[i] += tump_prob * next_values.tenpai_probs[i + 1];
                            }
                            win_probs[i] += tump_prob * next_values.win_probs[i + 1];
                            exp_values[i] += tump_prob * next_values.exp_values[i + 1];
                            yaku[i].add(&next_values.yaku[i + 1], tump_prob);
                        }
                    }
                }
            }
        }

        for DrawTile {
            tile,
            count,
            shanten_diff,
        } in draw_tiles
        {
            if shanten_diff != 0 {
                // 有効牌の場合
                continue;
            }

            self.state.deal(tile);
            self.state.n_extra_tsumo += 1;
            let next_values = self.discard(shanten);
            self.state.n_extra_tsumo -= 1;
            self.state.undo_deal(tile);

            for i in 0..MAX_TSUMO - 1 {
                // 【暫定対応】 (2021/9/24)
                let tump_prob = count as f32 / sum_left_tiles as f32;
                // let tump_prob = &TSUMO_PROB_TABLE[count as usize - 1][i];

                tenpai_probs[i] += tump_prob * next_values.tenpai_probs[i + 1];
                win_probs[i] += tump_prob * next_values.win_probs[i + 1];
                exp_values[i] += tump_prob * next_values.exp_values[i + 1];
                yaku[i].add(&next_values.yaku[i + 1], tump_prob);
            }
        }

        let values = Rc::new(Values {
            tenpai_probs,
            win_probs,
            exp_values,
            yaku,
        });
        self.draw_cache[shanten as usize].insert(self.state.clone(), Rc::clone(&values));

        values
    }

    fn draw_without_tegawari(&mut self, shanten: i8) -> Rc<Values<MAX_TSUMO>> {
        self.draw_cache[shanten as usize]
            .get(&self.state)
            .cloned()
            .unwrap_or_else(|| self.draw_without_tegawari_slow(shanten))
    }

    fn draw_without_tegawari_slow(&mut self, shanten: i8) -> Rc<Values<MAX_TSUMO>> {
        let mut tenpai_probs = [0.; MAX_TSUMO];
        let mut win_probs = [0.; MAX_TSUMO];
        let mut exp_values = [0.; MAX_TSUMO];
        let mut yaku: [WeightedYaku; MAX_TSUMO] = from_fn(|_| WeightedYaku::default());

        // 自摸候補を取得する。
        let draw_tiles = self.state.get_draw_tiles(shanten, self.sup.tehai_len_div3);

        // 有効牌の合計枚数を計算する。
        let sum_required_tiles: u8 = draw_tiles.iter().filter(|d| d.shanten_diff == -1).map(|d| d.count).sum();
        let not_tsumo_probs = &self.not_tsumo_prob_table[sum_required_tiles as usize];

        for DrawTile {
            tile,
            count,
            shanten_diff,
        } in draw_tiles
        {
            if shanten_diff != -1 {
                // 有効牌以外の場合
                continue;
            }

            self.state.deal(tile);
            let scores_or_values = if shanten > 0 {
                ScoresOrValues::Values(self.discard(shanten - 1))
            } else if let Some(scores) = self.get_score(tile) {
                ScoresOrValues::Scores(scores)
            } else {
                self.state.undo_deal(tile);
                continue;
            };
            self.state.undo_deal(tile);

            let tsumo_probs = &self.tsumo_prob_table[count as usize - 1];
            for i in 0..MAX_TSUMO {
                let m = not_tsumo_probs[i];
                if m == 0. {
                    // We are breaking here because `not_tsumo_probs[i..]` must
                    // all be zero, since `not_tsumo_probs` is monotonically
                    // decreasing.
                    //
                    // This divide-by-zero check is missing in the original
                    // version, which is very problematic.
                    break;
                }

                for j in i..MAX_TSUMO {
                    let n = not_tsumo_probs[j];
                    if n == 0. {
                        // `not_tsumo_probs[j..]` must all be zero, no need to
                        // proceed.
                        break;
                    }
                    // 現在の巡目が i の場合に j 巡目に有効牌を引く確率
                    let prob = tsumo_probs[j] * n / m;

                    match &scores_or_values {
                        ScoresOrValues::Scores(scores) => {
                            let assume_riichi = self.sup.is_menzen && self.sup.prefer_riichi;
                            // 聴牌の場合は次で和了
                            // i 巡目で聴牌の場合はダブル立直成立
                            let win_double_riichi = assume_riichi && self.sup.calc_double_riichi && i == 0;
                            // i 巡目で聴牌し、次の巡目で和了の場合は一発成立
                            let win_ippatsu = assume_riichi && j == i;
                            // 最後の巡目で和了の場合は海底撈月成立
                            let win_haitei = self.sup.calc_haitei && j == MAX_TSUMO - 1;
                            let han_plus = win_double_riichi as usize + win_ippatsu as usize + win_haitei as usize;

                            win_probs[i] += prob;
                            exp_values[i] += prob * scores.scores[han_plus];
                            yaku[i].add(
                                &WeightedYaku::from_score(
                                    &scores.yaku,
                                    scores.dora,
                                    scores.aka_dora,
                                    scores.ura_dora,
                                    win_double_riichi,
                                    win_ippatsu,
                                    win_haitei,
                                ),
                                prob,
                            );
                        }
                        ScoresOrValues::Values(next_values) => {
                            if shanten == 1 {
                                // 1向聴の場合は次で聴牌
                                tenpai_probs[i] += prob;
                            }
                            if j < MAX_TSUMO - 1 {
                                if shanten > 1 {
                                    // 2向聴以上で max_tsumo_ - 1 巡目以下の場合
                                    tenpai_probs[i] += prob * next_values.tenpai_probs[j + 1];
                                }
                                // 聴牌以上で max_tsumo_ - 1 巡目以下の場合
                                win_probs[i] += prob * next_values.win_probs[j + 1];
                                exp_values[i] += prob * next_values.exp_values[j + 1];
                                yaku[i].add(&next_values.yaku[j + 1], prob);
                            }
                        }
                    }
                }
            }
        }

        let values = Rc::new(Values {
            tenpai_probs,
            win_probs,
            exp_values,
            yaku,
        });
        self.draw_cache[shanten as usize].insert(self.state.clone(), Rc::clone(&values));

        values
    }

    fn discard(&mut self, shanten: i8) -> Rc<Values<MAX_TSUMO>> {
        self.discard_cache[shanten as usize]
            .get(&self.state)
            .cloned()
            .unwrap_or_else(|| self.discard_slow(shanten))
    }

    fn discard_slow(&mut self, shanten: i8) -> Rc<Values<MAX_TSUMO>> {
        // 打牌候補を取得する。
        let discard_tiles = self.state.get_discard_tiles(shanten, self.sup.tehai_len_div3);

        // 期待値が最大となる打牌を選択する。
        let mut max_tenpai_probs = [f32::MIN; MAX_TSUMO];
        let mut max_win_probs = [f32::MIN; MAX_TSUMO];
        let mut max_exp_values = [f32::MIN; MAX_TSUMO];
        let mut max_tiles = [t!(?); MAX_TSUMO];
        let mut max_values = [i32::MIN; MAX_TSUMO];
        let mut yaku: [WeightedYaku; MAX_TSUMO] = from_fn(|_| WeightedYaku::default());

        for DiscardTile { tile, shanten_diff } in discard_tiles {
            let values;
            if shanten_diff == 0 {
                // 向聴数が変化しない打牌
                self.state.discard(tile);
                values = self.draw(shanten);
                self.state.undo_discard(tile);
            } else if let Some(shanten_down_max) = self.sup.calc_shanten_down
                && self.state.n_extra_tsumo == 0
                && shanten_diff == 1
                && shanten < shanten_down_max
            {
                // 向聴戻しになる打牌
                self.state.discard(tile);
                self.state.n_extra_tsumo += 1;
                values = self.draw(shanten + 1);
                self.state.n_extra_tsumo -= 1;
                self.state.undo_discard(tile);
            } else {
                // 手牌に存在しない牌、または向聴落としが無効な場合に向聴落としとなる牌
                continue;
            };

            for i in 0..MAX_TSUMO {
                // 和了確率は下2桁まで一致していれば同じ、期待値は下0桁まで一致していれば同じとみなす。
                let value = if self.sup.maximize_win_prob {
                    values.win_probs[i] * 1e5
                } else {
                    values.exp_values[i]
                } as i32;
                let max_value = max_values[i];
                let max_tile = max_tiles[i];

                if value > max_value || value == max_value && tile.cmp_discard_priority(max_tile).is_gt() {
                    // 値が同等なら、DiscardPriorities が高い牌を優先して選択する。
                    max_tenpai_probs[i] = values.tenpai_probs[i];
                    max_win_probs[i] = values.win_probs[i];
                    max_exp_values[i] = values.exp_values[i];
                    max_values[i] = value;
                    max_tiles[i] = tile;
                    yaku[i] = values.yaku[i].clone();
                }
            }
        }

        let values = Rc::new(Values {
            tenpai_probs: max_tenpai_probs,
            win_probs: max_win_probs,
            exp_values: max_exp_values,
            yaku,
        });
        self.discard_cache[shanten as usize].insert(self.state.clone(), Rc::clone(&values));

        values
    }

    /// None: no yaku
    fn get_score(&self, win_tile: Tile) -> Option<Scores> {
        let calc = AgariCalculator {
            tehai: &self.state.tehai,
            is_menzen: self.sup.is_menzen,
            chis: self.sup.chis,
            pons: self.sup.pons,
            minkans: self.sup.minkans,
            ankans: self.sup.ankans,
            bakaze: self.sup.bakaze,
            jikaze: self.sup.jikaze,
            winning_tile: win_tile.deaka().as_u8(),
            is_ron: false,
        };
        let is_oya = self.sup.jikaze == tu8!(E);

        let additional_yakus = match (self.sup.is_menzen, self.sup.prefer_riichi) {
            (true, true) => 2,
            (true, false) => 1,
            (false, _) => 0,
        };
        let akas_in_hand = self.state.akas_in_hand.iter().filter(|&&b| b).count() as u8;
        let doras_in_hand = self
            .sup
            .dora_indicators
            .iter()
            .map(|ind| self.state.tehai[ind.next().as_usize()])
            .sum::<u8>();
        let num_doras = doras_in_hand + akas_in_hand + self.sup.num_doras_in_fuuro;

        // these are only used for human representation
        let mut tiles_in_fuuro = [0; 34];
        for &low in self.sup.chis {
            tiles_in_fuuro[low as usize] += 1;
            tiles_in_fuuro[low as usize + 1] += 1;
            tiles_in_fuuro[low as usize + 2] += 1;
        }
        for &pai in self.sup.pons {
            tiles_in_fuuro[pai as usize] += 3;
        }
        for &pai in self.sup.minkans.iter().chain(self.sup.ankans) {
            tiles_in_fuuro[pai as usize] += 4;
        }
        let proper_doras_in_fuuro = self
            .sup
            .dora_indicators
            .iter()
            .map(|ind| tiles_in_fuuro[ind.next().as_usize()])
            .sum::<u8>();
        let akas_in_fuuro = self.sup.num_doras_in_fuuro - proper_doras_in_fuuro;
        let owned_akas = akas_in_hand + akas_in_fuuro;
        let owned_doras = num_doras - owned_akas;

        // Although you can technically win the base hand with just 海底, the
        // original C++ version didn't take this into account and I also agree
        // with that.
        let mut agari_names = calc.agari_with_yaku(additional_yakus, num_doras)?;
        if self.sup.is_menzen {
            agari_names.yaku.push(yaku!("門前清自摸和"));
            if self.sup.prefer_riichi {
                agari_names.yaku.push(yaku!("立直"));
            }
        }
        let (fu, han) = match agari_names.agari {
            Agari::Normal { fu, han } => (fu, han),
            a @ Agari::Yakuman(_) => {
                return Some(Scores {
                    scores: [a.point(is_oya).tsumo_total(is_oya) as f32; 4],
                    yaku: agari_names.yaku,
                    dora: owned_doras,
                    aka_dora: owned_akas,
                    ura_dora: 0.,
                });
            }
        };

        // assuming best case ura-dora, ensure score is higher than minimum
        if let Some(min_score) = self.sup.min_score {
            let max_ura_dora = if self.sup.is_menzen && self.sup.prefer_riichi {
                let mut tehai_full = self.state.tehai;
                for &t in self.sup.ankans {
                    tehai_full[t as usize] += 4;
                }

                let mut tehai_ordered_by_count: Vec<_> = tehai_full.iter().enumerate().filter(|&(_, &c)| c > 0).collect();
                tehai_ordered_by_count.sort_unstable_by(|(_, l), (_, r)| r.cmp(l));

                let mut tiles_in_wall = self.state.tiles_in_wall;
                let mut found_ura_indicators = 0;
                let mut ura_count = 0;
                'outer: for (t, count) in tehai_ordered_by_count {
                    let ura_ind = must_tile!(t).prev().as_usize();
                    loop {
                        if found_ura_indicators >= self.sup.dora_indicators.len() {
                            break 'outer;
                        }
                        if tiles_in_wall[ura_ind] == 0 {
                            continue 'outer;
                        }
                        found_ura_indicators += 1;
                        ura_count += count;
                        tiles_in_wall[ura_ind] -= 1;
                    }
                }
                ura_count
            } else {
                0
            };
            let best_agari = Agari::Normal {
                fu,
                han: han + max_ura_dora,
            };
            if best_agari.point(is_oya).tsumo_total(is_oya) < min_score {
                return None;
            }
        }

        // 役ありの場合

        // ダブル立直、一発、海底撈月で最大3翻まで増加するので、
        // ベースとなる点数、+1翻の点数、+2翻の点数、+3翻の点数も計算しておく。
        if self.sup.is_menzen && self.sup.prefer_riichi {
            let mut scores: [f32; 4] = [0.; 4];
            let mut expected_uradora = 0.;
            if self.sup.dora_indicators.len() == 1 {
                // 裏ドラ考慮ありかつ表ドラが1枚以上の場合は、厳密に計算する。
                let mut n_indicators = [0; 5];
                let mut sum_indicators = 0;
                for (tid, &count) in self.state.tehai.iter().enumerate() {
                    if count == 0 {
                        continue;
                    }
                    // ドラ表示牌の枚数を数える。
                    let tile = must_tile!(tid);
                    let ind_count = self.state.tiles_in_wall[tile.prev().as_usize()];
                    n_indicators[count as usize] += ind_count;
                    sum_indicators += ind_count;
                }

                // 裏ドラの乗る確率を枚数ごとに計算する。
                let mut uradora_probs = [0.; 5];

                let n_left_tiles = self.state.sum_left_tiles();

                uradora_probs[0] = (n_left_tiles - sum_indicators) as f32 / n_left_tiles as f32;
                for i in 1..5 {
                    uradora_probs[i] = n_indicators[i] as f32 / n_left_tiles as f32;
                }
                for (i, s) in scores.iter_mut().enumerate() {
                    // 裏ドラ1枚の場合、最大4翻まで乗る可能性がある
                    for (j, &p) in uradora_probs.iter().enumerate() {
                        if p == 0. {
                            continue;
                        }
                        let agari = Agari::Normal {
                            fu,
                            han: han + i as u8 + j as u8,
                        };
                        *s += agari.point(is_oya).tsumo_total(is_oya) as f32 * p;
                        expected_uradora += j as f32 * p;
                    }
                }
            } else {
                // 裏ドラ考慮ありかつ表ドラが2枚以上の場合、統計データを利用する。
                for (i, s) in scores.iter_mut().enumerate() {
                    for (j, &p) in URADORA_PROB_TABLE[self.sup.dora_indicators.len() - 1].iter().enumerate() {
                        if p == 0. {
                            continue;
                        }
                        let agari = Agari::Normal {
                            fu,
                            han: han + i as u8 + j as u8,
                        };
                        *s += agari.point(is_oya).tsumo_total(is_oya) as f32 * p;
                        expected_uradora += j as f32 * p;
                    }
                }
            };

            Some(Scores {
                scores,
                yaku: agari_names.yaku,
                dora: owned_doras,
                aka_dora: owned_akas,
                ura_dora: expected_uradora,
            })
        } else {
            let mut scores: [f32; 4] = [0.; 4];
            // 裏ドラ考慮なしまたは表ドラが0枚の場合
            for (i, s) in scores.iter_mut().enumerate() {
                let agari = Agari::Normal { fu, han: han + i as u8 };
                *s = agari.point(is_oya).tsumo_total(is_oya) as f32;
            }
            Some(Scores {
                scores,
                yaku: agari_names.yaku,
                dora: owned_doras,
                aka_dora: owned_akas,
                ura_dora: 0.,
            })
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::algo::sp::CALC_SHANTEN_FN;
    use crate::hand::hand;
    use crate::tuz;

    fn feq(a: f32, b: f32) -> bool {
        (a - b).abs() <= f32::EPSILON
    }

    #[test]
    fn nanikiru() {
        let mut calc = SPCalculator {
            tehai_len_div3: 4,
            chis: &[],
            pons: &[],
            minkans: &[],
            ankans: &[],
            bakaze: tu8!(E),
            jikaze: tu8!(N),
            prefer_riichi: true,
            is_menzen: true,
            num_doras_in_fuuro: 0,
            dora_indicators: &t![P,],
            calc_double_riichi: false,
            calc_haitei: false,
            sort_result: true,
            maximize_win_prob: false,
            max_shanten: 3,
            calc_tegawari: Some(3),
            calc_shanten_down: Some(3),
            min_score: None,
        };

        let tehai = hand("45678m 34789p 3344z").unwrap();
        let mut tiles_seen = tehai;
        for ind in calc.dora_indicators {
            tiles_seen[ind.deaka().as_usize()] += 1;
        }
        let state = InitState {
            tehai,
            akas_in_hand: [false; 3],
            tiles_seen,
            akas_seen: [false; 3],
        };
        let can_discard = true;
        let tsumos_left = 8;
        let cur_shanten = CALC_SHANTEN_FN(&tehai, calc.tehai_len_div3);
        let candidates = calc.calc(state, can_discard, tsumos_left, cur_shanten).unwrap();
        assert_eq!(candidates[0].tile, t!(N));
        assert_eq!(candidates[1].tile, t!(W));
        assert!(candidates[0].exp_values > candidates[1].exp_values);

        // ---

        let tehai = hand("3667m 23489p 34688s").unwrap();
        let mut tiles_seen = tehai;
        for ind in calc.dora_indicators {
            tiles_seen[ind.deaka().as_usize()] += 1;
        }
        let state = InitState {
            tehai,
            akas_in_hand: [false; 3],
            tiles_seen,
            akas_seen: [false; 3],
        };
        let can_discard = true;
        let tsumos_left = 15;
        let cur_shanten = CALC_SHANTEN_FN(&tehai, calc.tehai_len_div3);
        let candidates = calc.calc(state.clone(), can_discard, tsumos_left, cur_shanten).unwrap();
        assert_eq!(candidates[0].tile, t!(9p));
        assert!(candidates[0].shanten_down);

        calc.maximize_win_prob = true;
        let candidates = calc.calc(state, can_discard, tsumos_left, cur_shanten).unwrap();
        assert_eq!(candidates[0].tile, t!(3m));
        assert!(!candidates[0].shanten_down);

        // ---

        let calc = SPCalculator {
            tehai_len_div3: 4,
            chis: &[],
            pons: &[],
            minkans: &[],
            ankans: &[],
            bakaze: tu8!(E),
            jikaze: tu8!(E),
            prefer_riichi: true,
            is_menzen: true,
            num_doras_in_fuuro: 0,
            dora_indicators: &t![6m,],
            calc_double_riichi: true,
            calc_haitei: true,
            sort_result: true,
            maximize_win_prob: false,
            max_shanten: 3,
            calc_tegawari: Some(3),
            calc_shanten_down: Some(3),
            min_score: None,
        };

        let tehai = hand("45677m 456778p 248s").unwrap();
        let mut tiles_seen = tehai;
        for ind in calc.dora_indicators {
            tiles_seen[ind.deaka().as_usize()] += 1;
        }
        let state = InitState {
            tehai,
            akas_in_hand: [false; 3],
            tiles_seen,
            akas_seen: [false; 3],
        };
        let can_discard = true;
        let tsumos_left = 15;
        let cur_shanten = CALC_SHANTEN_FN(&tehai, calc.tehai_len_div3);
        let candidates = calc.calc(state, can_discard, tsumos_left, cur_shanten).unwrap();
        let c = &candidates[0];
        assert_eq!(c.tile, t!(2s));
        assert_eq!(c.required_tiles.len(), 17);
        assert_eq!(c.num_required_tiles, 57);
        assert!(c.shanten_down);
        assert!(feq(c.tenpai_probs[0], 0.90023905));
        assert!(feq(c.win_probs[0], 0.34794784));
        assert!(feq(c.exp_values[0], 5894.7617));

        // ---

        let calc = SPCalculator {
            tehai_len_div3: 4,
            chis: &[],
            pons: &[],
            minkans: &[],
            ankans: &[],
            bakaze: tu8!(E),
            jikaze: tu8!(W),
            prefer_riichi: true,
            is_menzen: true,
            num_doras_in_fuuro: 0,
            dora_indicators: &t![1m,],
            calc_double_riichi: false,
            calc_haitei: false,
            sort_result: true,
            maximize_win_prob: false,
            max_shanten: 3,
            calc_tegawari: Some(3),
            calc_shanten_down: Some(3),
            min_score: None,
        };
        let tehai = hand("9999m 6677p 88s 335z 1m").unwrap();
        let mut tiles_seen = tehai;
        for ind in calc.dora_indicators {
            tiles_seen[ind.deaka().as_usize()] += 1;
        }
        let state = InitState {
            tehai,
            akas_in_hand: [false; 3],
            tiles_seen,
            akas_seen: [false; 3],
        };
        let cur_shanten = CALC_SHANTEN_FN(&tehai, calc.tehai_len_div3);
        let can_discard = true;
        let tsumos_left = 5;
        let candidates = calc.calc(state, can_discard, tsumos_left, cur_shanten).unwrap();
        assert_eq!(candidates.len(), 7);

        let c = &candidates[1];
        assert_eq!(c.tile, t!(1m));
        assert!(c.shanten_down);
        assert_eq!(c.required_tiles.len(), 33); // literally all kinds of tiles
        assert_eq!(c.num_required_tiles, 34 * 4 - tiles_seen.iter().sum::<u8>());
    }

    #[test]
    fn tsumo_only() {
        let calc = SPCalculator {
            tehai_len_div3: 4,
            chis: &[],
            pons: &[],
            minkans: &[],
            ankans: &[],
            bakaze: tu8!(E),
            jikaze: tu8!(W),
            prefer_riichi: true,
            is_menzen: true,
            num_doras_in_fuuro: 0,
            dora_indicators: &t![6m,],
            calc_double_riichi: true,
            calc_haitei: true,
            sort_result: true,
            maximize_win_prob: true,
            max_shanten: 3,
            calc_tegawari: Some(3),
            calc_shanten_down: Some(3),
            min_score: None,
        };

        let tehai = hand("45677m 456778p 48s").unwrap();
        let mut tiles_seen = tehai;
        for ind in calc.dora_indicators {
            tiles_seen[ind.deaka().as_usize()] += 1;
        }
        tiles_seen[tuz!(5s)] += 4;

        let state = InitState {
            tehai,
            akas_in_hand: [false; 3],
            tiles_seen,
            akas_seen: [false, false, true],
        };
        let cur_shanten = CALC_SHANTEN_FN(&tehai, calc.tehai_len_div3);
        let can_discard = false;
        let tsumos_left = 5;
        let candidates = calc.calc(state, can_discard, tsumos_left, cur_shanten).unwrap();
        assert_eq!(candidates.len(), 1);
        let c = &candidates[0];
        assert_eq!(c.tile, t!(?));
        assert_eq!(c.required_tiles.len(), 16);
        assert_eq!(c.num_required_tiles, 54);
        assert!(feq(c.tenpai_probs[0], 0.45017204));
        assert!(feq(c.win_probs[0], 0.03441279));
        assert!(feq(c.exp_values[0], 432.26678));
    }
}
