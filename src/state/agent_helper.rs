use super::PlayerState;
use crate::algo::agari::calc::{Agari, AgariCalculator, AgariWithYaku};
use crate::algo::agari::yaku::yaku;
use crate::algo::point::Point;
use crate::algo::shanten;
use crate::algo::sp::{Candidate, CandidateColumn, InitState, SPCalculator, SPOptions};
use crate::mjai::Event;
use crate::tile::Tile;
use crate::vec_ops::vec_add_assign;
use crate::{must_tile, t, tu8, tuz};

use anyhow::{Context, Result};
use tinyvec::array_vec;

impl PlayerState {
    /// Used by `BoardState` to check if a player is making 4 kans on his own.
    #[inline]
    #[must_use]
    pub fn kans_count(&self) -> usize {
        self.minkans.len() + self.ankans.len()
    }

    /// Used by `Agent` impls, must be called at 3n+2.
    #[must_use]
    pub fn discard_candidates(&self) -> [bool; 34] {
        let full = self.discard_candidates_aka();
        let mut ret = [false; 34];
        ret.copy_from_slice(&full[..34]);
        ret[tuz!(5m)] |= full[tuz!(5mr)];
        ret[tuz!(5s)] |= full[tuz!(5sr)];
        ret[tuz!(5p)] |= full[tuz!(5pr)];
        ret
    }

    /// Aka dora covered version of `discard_candidates`.
    #[must_use]
    pub fn discard_candidates_aka(&self) -> [bool; 37] {
        assert!(self.last_cans.can_discard, "tehai is not 3n+2");

        let mut ret = [false; 37];

        if self.riichi_accepted[0] {
            let last_self_tsumo = self.last_self_tsumo.expect("riichi accepted without last self tsumo");
            ret[last_self_tsumo.as_usize()] = true;
            return ret;
        }

        for (i, count) in self.tehai.iter().copied().enumerate() {
            if count == 0 {
                continue;
            }

            ret[i] = if self.riichi_declared[0] {
                if self.shanten == 1 {
                    self.next_shanten_discards[i]
                } else {
                    // shanten must be 0 here according to the rule
                    self.keep_shanten_discards[i]
                }
            } else {
                !self.forbidden_tiles[i]
            };
        }

        if ret[tuz!(5m)] && self.akas_in_hand[0] {
            ret[tuz!(5mr)] = true;
            ret[tuz!(5m)] = self.tehai[tuz!(5m)] > 1;
        }
        if ret[tuz!(5p)] && self.akas_in_hand[1] {
            ret[tuz!(5pr)] = true;
            ret[tuz!(5p)] = self.tehai[tuz!(5p)] > 1;
        }
        if ret[tuz!(5s)] && self.akas_in_hand[2] {
            ret[tuz!(5sr)] = true;
            ret[tuz!(5s)] = self.tehai[tuz!(5s)] > 1;
        }

        ret
    }

    /// Must be called at 3n+2.
    ///
    /// The return value indicates the tiles which can make the hand tenpai for
    /// real after being discarded, with the number of future tenpai tiles left
    /// and furiten considered, without depending on any incidental yaku, and is
    /// not affected by the riichi status of the player.
    #[must_use]
    pub fn discard_candidates_with_unconditional_tenpai(&self) -> [bool; 34] {
        let full = self.discard_candidates_with_unconditional_tenpai_aka();
        let mut ret = [false; 34];
        ret.copy_from_slice(&full[..34]);
        ret[tuz!(5m)] |= full[tuz!(5mr)];
        ret[tuz!(5s)] |= full[tuz!(5sr)];
        ret[tuz!(5p)] |= full[tuz!(5pr)];
        ret
    }

    /// Aka dora covered version of `discard_candidates_with_unconditional_tenpai`.
    #[must_use]
    pub fn discard_candidates_with_unconditional_tenpai_aka(&self) -> [bool; 37] {
        assert!(self.last_cans.can_discard, "tehai is not 3n+2");

        let mut ret = [false; 37];

        if self.tiles_left == 0 // haitei
            || self.shanten > 1 // impossible to discard-to-tenpai
            || self.shanten == 1 && !self.has_next_shanten_discard
        {
            return ret;
        }

        if let Some(last_self_tsumo) = self.last_self_tsumo {
            if self.waits[last_self_tsumo.deaka().as_usize()] {
                // already agari and any discard will result in furiten
                return ret;
            }
            if self.riichi_accepted[0] {
                if !self.at_furiten {
                    // already riichi and is not furiten (which is forever)
                    ret[last_self_tsumo.as_usize()] = true;
                }
                return ret;
            }
        } else if shanten::calc_all(&self.tehai, self.tehai_len_div3) == -1 {
            // Ditto but for discard after chi/pon
            return ret;
        }

        let tenpai_discards = if self.shanten == 1 {
            self.next_shanten_discards
        } else {
            self.keep_shanten_discards
        };

        // Replace and test
        tenpai_discards
            .iter()
            .copied()
            .enumerate()
            .filter(|&(tid, b)| b && !self.forbidden_tiles[tid])
            .for_each(|(discard, _)| {
                let mut tehai_3n1 = self.tehai;
                tehai_3n1[discard] -= 1;

                for (tsumo, seen) in self.tiles_seen.iter().copied().enumerate() {
                    if tsumo == discard || tehai_3n1[tsumo] == 4 {
                        continue;
                    }

                    let mut tehai_3n2 = tehai_3n1;
                    tehai_3n2[tsumo] += 1;
                    if shanten::calc_all(&tehai_3n2, self.tehai_len_div3) > -1 {
                        continue;
                    }

                    // Furiten
                    if self.discarded_tiles[tsumo] {
                        ret[discard] = false;
                        break;
                    }

                    // Must be placed after the furiten check above
                    if seen == 4 || ret[discard] {
                        continue;
                    }

                    let agari_calc = AgariCalculator {
                        tehai: &tehai_3n2,
                        is_menzen: self.is_menzen,
                        chis: &self.chis,
                        pons: &self.pons,
                        minkans: &self.minkans,
                        ankans: &self.ankans,
                        bakaze: self.bakaze.as_u8(),
                        jikaze: self.jikaze.as_u8(),
                        winning_tile: tsumo as u8,
                        is_ron: true,
                    };
                    ret[discard] = agari_calc.has_yaku();
                }
            });

        if ret[tuz!(5m)] && self.akas_in_hand[0] {
            ret[tuz!(5mr)] = true;
            ret[tuz!(5m)] = self.tehai[tuz!(5m)] > 1;
        }
        if ret[tuz!(5p)] && self.akas_in_hand[1] {
            ret[tuz!(5pr)] = true;
            ret[tuz!(5p)] = self.tehai[tuz!(5p)] > 1;
        }
        if ret[tuz!(5s)] && self.akas_in_hand[2] {
            ret[tuz!(5sr)] = true;
            ret[tuz!(5s)] = self.tehai[tuz!(5s)] > 1;
        }

        ret
    }

    #[inline]
    #[must_use]
    pub fn yaokyuu_kind_count(&self) -> u8 {
        tuz![1m, 9m, 1p, 9p, 1s, 9s, E, S, W, N, P, F, C]
            .iter()
            .map(|&i| self.tehai[i].min(1))
            .sum()
    }

    #[inline]
    #[must_use]
    pub fn rule_based_ryukyoku(&self) -> bool {
        if !self.last_cans.can_ryukyoku {
            return false;
        }
        self.rule_based_ryukyoku_slow()
    }

    fn rule_based_ryukyoku_slow(&self) -> bool {
        // Do not ryukyoku if the hand is already <= 2 shanten.
        if shanten::calc_all(&self.tehai, self.tehai_len_div3) <= 2 {
            return false;
        }

        // Ryukyoku if we are in the west round, because we usually don't need a
        // big hand to win.
        if self.bakaze == t!(W) {
            return true;
        }

        if self.is_all_last {
            // Ryukyoku if it is all-last and we are oya or we are not the last,
            // because it is hard to decide whether it is appropriate to not
            // ryukyoku.
            if self.oya == 0 || self.rank < 3 {
                return true;
            }

            // At all-last, we are the last and we are not oya. If even a
            // haneman tsumo cannot let us avoid the last, then do not ryukyoku.
            let mut scores = [-3000 - self.honba as i32 * 300; 4];
            scores[0] = 12000 + self.kyotaku as i32 * 1000 + self.honba as i32 * 300;
            scores[self.oya as usize] = -6000 - self.honba as i32 * 300;
            vec_add_assign(&mut scores, &self.scores);
            return self.get_rank(scores) < 3;
        }

        // Do not ryukyoku if we have >= 10 yaokyuu tiles.
        if self.yaokyuu_kind_count() >= 10 {
            return false;
        }

        // Do not ryukyoku if we have all the jihai kinds.
        if self.tehai[3 * 9..].iter().all(|&c| c > 0) {
            return false;
        }

        // Ryukyoku otherwise.
        true
    }

    #[inline]
    #[must_use]
    pub fn rule_based_agari(&self) -> bool {
        if !self.last_cans.can_agari() {
            return false;
        }
        self.rule_based_agari_slow(self.last_cans.can_ron_agari, self.rel(self.last_cans.target_actor))
    }

    fn rule_based_agari_slow(&self, is_ron: bool, target_rel: usize) -> bool {
        // Agari if it is not yet all-last, or we are oya ourselves, or we are
        // not the last place at all.
        if !self.is_all_last || self.oya == 0 || self.rank < 3 {
            return true;
        }

        if self.bakaze == t!(W) {
            // Agari if we are in the west round but it is not yet the real
            // all-last (W4).
            if self.kyoku < 3 {
                return true;
            }
        } else if self.scores.iter().all(|&s| s < 30000) {
            // Agari if 西入 is possible. Note that this condition is sound but
            // not complete.
            return true;
        }

        // Calculate the max theoretical score we can achieve through this agari.
        let max_win_point = if self.riichi_accepted[0] {
            let mut tehai_full = self.tehai;
            for t in &self.ankan_overview[0] {
                tehai_full[t.as_usize()] += 4;
            }

            let mut tehai_ordered_by_count: Vec<_> = tehai_full.iter().enumerate().filter(|&(_, &c)| c > 0).collect();
            tehai_ordered_by_count.sort_unstable_by(|(_, l), (_, r)| r.cmp(l));

            // Try possible uradoras one by one, starting from the most valuable one
            let mut tiles_seen = self.tiles_seen;
            let mut ura_indicators = array_vec!([_; 5]);
            'outer: for (t, _) in tehai_ordered_by_count {
                let ura_ind = must_tile!(t).prev();
                loop {
                    if ura_indicators.len() >= self.dora_indicators.len() {
                        // Break out of all loops.
                        break 'outer;
                    }
                    if tiles_seen[ura_ind.as_usize()] >= 4 {
                        // Try the next most-valuable possible uradora.
                        continue 'outer;
                    }
                    ura_indicators.push(ura_ind);
                    tiles_seen[ura_ind.as_usize()] += 1;
                }
            }

            // `unwrap` is safe because there is a condition guard in
            // `rule_based_agari`.
            self.agari_points(is_ron, &ura_indicators).unwrap()
        } else {
            // ditto
            self.agari_points(is_ron, &[]).unwrap()
        };

        // Calculate the best post-hora situation for us.
        let mut exp_scores = self.scores;
        if is_ron {
            exp_scores[0] += max_win_point.ron + self.kyotaku as i32 * 1000 + self.honba as i32 * 300;
            exp_scores[target_rel] -= max_win_point.ron + self.honba as i32 * 300;
        } else {
            // The player must be ko here.
            exp_scores[0] += max_win_point.tsumo_total(false) + self.kyotaku as i32 * 1000 + self.honba as i32 * 300;
            exp_scores.iter_mut().enumerate().skip(1).for_each(|(idx, s)| {
                if idx as u8 == self.oya {
                    *s -= max_win_point.tsumo_oya + self.honba as i32 * 100;
                } else {
                    *s -= max_win_point.tsumo_ko + self.honba as i32 * 100;
                }
            });
        }

        // The prerequisite `!(self.bakaze == t!(W) && self.kyoku == 3)` has
        // already been checked at the beginning.
        //
        // Agari if 西入 or keeping 西入 is possible. This condition is sound
        // and complete.
        if exp_scores.iter().all(|&s| s < 30000) {
            return true;
        }

        // Agari if the best post-hora situation in theory will make us avoid
        // taking the last place.
        self.get_rank(exp_scores) < 3
    }

    /// Err is returned if the hand cannot agari, or cannot retrieve the winning
    /// tile.
    ///
    /// This function should be called immediately, otherwise the state may
    /// change.
    ///
    /// `ura_indicators` is used only when the actor has an accepted riichi.
    pub fn agari_points(&self, is_ron: bool, ura_indicators: &[Tile]) -> Result<Point> {
        let winning_tile =
            if is_ron { self.last_kawa_tile } else { self.last_self_tsumo }.context("cannot find the winning tile")?;
        let agari = self
            .calculate_agari(winning_tile, is_ron, ura_indicators)?
            .context("not a hora hand")?;
        Ok(agari.agari.point(self.oya == 0))
    }

    pub fn calculate_agari(&self, winning_tile: Tile, is_ron: bool, ura_indicators: &[Tile]) -> Result<Option<AgariWithYaku>> {
        // Here, 天和 and 地和 are handled individually as special cases, and
        // there is no multi yakuman for these two.
        if !is_ron && self.can_w_riichi {
            return Ok(Some(AgariWithYaku::from_agari(
                Agari::Yakuman(1),
                vec![if self.is_oya() { yaku!("天和") } else { yaku!("地和") }],
            )));
        }

        let mut additional_yaku = vec![];
        let additional_hans = if is_ron {
            [
                (self.riichi_accepted[0], yaku!("立直")),
                (self.is_w_riichi, yaku!("ダブル立直")),
                (self.at_ippatsu, yaku!("一発")),
                (self.tiles_left == 0, yaku!("河底撈魚")),
                (self.chankan_chance.is_some(), yaku!("槍槓")),
            ]
            .iter()
            .map(|&(b, n)| {
                if b {
                    additional_yaku.push(n);
                    1
                } else {
                    0
                }
            })
            .sum::<u8>()
        } else {
            [
                (self.riichi_accepted[0], yaku!("立直")),
                (self.is_w_riichi, yaku!("ダブル立直")),
                (self.at_ippatsu, yaku!("一発")),
                (self.is_menzen, yaku!("門前清自摸和")),
                (self.tiles_left == 0 && !self.at_rinshan, yaku!("海底摸月")),
                (self.at_rinshan, yaku!("嶺上開花")),
            ]
            .iter()
            .map(|&(b, n)| {
                if b {
                    additional_yaku.push(n);
                    1
                } else {
                    0
                }
            })
            .sum::<u8>()
        };

        let mut tehai = self.tehai;
        let mut akas_owned = self.akas_in_hand.iter().filter(|d| **d).count() as u8;
        let mut doras_owned = self.doras_owned[0] - akas_owned;
        if is_ron {
            let tid = winning_tile.deaka().as_usize();
            tehai[tid] += 1;
            doras_owned += self.dora_factor[tid];
            if winning_tile.is_aka() {
                akas_owned += 1;
            };
        }
        let ura_doras_owned = if self.riichi_accepted[0] {
            ura_indicators
                .iter()
                .map(|&ura| {
                    let next = ura.next();
                    let mut count = tehai[next.as_usize()];
                    if self.ankan_overview[0].contains(&next) {
                        count += 4;
                    }
                    count
                })
                .sum::<u8>()
        } else {
            0
        };

        let agari_calc = AgariCalculator {
            tehai: &tehai,
            is_menzen: self.is_menzen,
            chis: &self.chis,
            pons: &self.pons,
            minkans: &self.minkans,
            ankans: &self.ankans,
            bakaze: self.bakaze.as_u8(),
            jikaze: self.jikaze.as_u8(),
            winning_tile: winning_tile.deaka().as_u8(),
            is_ron,
        };
        let agari_names = agari_calc
            .agari_with_yaku(additional_hans, doras_owned + akas_owned + ura_doras_owned)
            .map(|mut agari| {
                agari.yaku.extend(additional_yaku);
                agari.dora = doras_owned;
                agari.aka_dora = akas_owned;
                agari.ura_dora = ura_doras_owned;
                agari
            });

        Ok(agari_names)
    }

    /// Calculate the actual shanten at this point. Unlike `self.shanten`, this
    /// function properly calculates the shanten at 3n+2, which follows the
    /// definition of shanten most people acknowledge.
    pub fn real_time_shanten(&self) -> i8 {
        if !self.last_cans.can_discard {
            // 3n+1, `self.shanten` is accurate.
            return self.shanten;
        }

        if self.shanten > 0 {
            // 3n+2, not tenpai, shanten is `self.shanten - 1` if there is any
            // discard that can decrease the shanten number.
            return if self.has_next_shanten_discard {
                self.shanten - 1
            } else {
                self.shanten
            };
        }

        if let Some(tile) = self.last_self_tsumo {
            // 3n+2, tenpai after tsumo.
            return if self.waits[tile.deaka().as_usize()] { -1 } else { 0 };
        }

        // 3n+2, tenpai after chi or pon. `self.shanten` is 0, but the actual
        // shanten could be 0 or -1.
        //
        // At 223m 55p 45s, `self.shanten` is 1. After 6s chi, `self.shanten`
        // becomes 0 because `update_shanten` is always called after a chi/pon
        // event. The actual shanten is 0 as well.
        //
        // At 123m 55p 45s, `self.shanten` is 0. After 6s chi, `self.shanten`
        // becomes 0 because `update_shanten` clamps the value to be >= 0. The
        // actual shanten is -1.
        shanten::calc_all(&self.tehai, self.tehai_len_div3)
    }

    /// Can be called at both 3n+1 and 3n+2
    pub fn single_player_tables(&self, options: &SPOptions) -> Result<Vec<Candidate>> {
        let cur_shanten = self.real_time_shanten();
        if cur_shanten == -1 {
            return self
                .discard_candidates_aka()
                .iter()
                .enumerate()
                .filter(|&(_, &b)| b)
                .map(|(tile, _)| must_tile!(tile))
                .map(|tile| {
                    let mut state = self.clone();
                    state.update(&Event::Dahai {
                        actor: self.player_id,
                        pai: tile,
                        tsumogiri: Some(tile) == state.last_self_tsumo,
                    })?;
                    let table = state.single_player_tables(options)?;
                    Ok(table.into_iter().next().expect("No candidates"))
                })
                .collect::<Result<Vec<_>>>();
        }

        let mut can_discard = self.last_cans.can_discard;
        let (tsumos_left, calc_haitei) = if can_discard {
            (self.tiles_left / 4, self.tiles_left % 4 == 0)
        } else {
            let target = self.rel(self.last_cans.target_actor) as u8;
            // Let's just ignore chankan here.
            let tiles_left_at_next_tsumo = self.tiles_left.saturating_sub(4 - target);
            (tiles_left_at_next_tsumo / 4, tiles_left_at_next_tsumo % 4 == 0)
        };

        let num_doras_in_fuuro = if self.is_menzen && self.ankan_overview[0].is_empty() {
            0
        } else {
            let num_doras_in_tehai: u8 = self.dora_indicators.iter().map(|ind| self.tehai[ind.next().as_usize()]).sum();
            let num_akas = self.akas_in_hand.iter().filter(|&&b| b).count() as u8;
            self.doras_owned[0] - num_doras_in_tehai - num_akas
        };
        let prefer_riichi = self.riichi_declared[0] || self.last_cans.can_riichi || cur_shanten != 0;
        let calc_double_riichi = (can_discard && self.can_w_riichi) || self.is_w_riichi;

        // If the player has an accepted riichi and has just dealt a tile
        // (can_discard) that can't win (cur_shanten >= 0), treat the hand as if
        // the tile has been discarded.
        let mut tehai = self.tehai;
        let mut akas_in_hand = self.akas_in_hand;
        let is_discard_after_riichi = can_discard && self.riichi_accepted[0];
        if is_discard_after_riichi {
            let last_tsumo = self.last_self_tsumo.unwrap();
            tehai[last_tsumo.deaka().as_usize()] -= 1;
            match last_tsumo.as_u8() {
                tu8!(5mr) => akas_in_hand[0] = false,
                tu8!(5pr) => akas_in_hand[1] = false,
                tu8!(5sr) => akas_in_hand[2] = false,
                _ => (),
            }
            can_discard = false;
        }

        let init_state = InitState {
            tehai,
            akas_in_hand,
            tiles_seen: self.tiles_seen,
            akas_seen: self.akas_seen,
        };
        let sp_calc = SPCalculator {
            tehai_len_div3: self.tehai_len_div3,
            is_menzen: self.is_menzen,
            chis: &self.chis,
            pons: &self.pons,
            minkans: &self.minkans,
            ankans: &self.ankans,
            bakaze: self.bakaze.as_u8(),
            jikaze: self.jikaze.as_u8(),
            num_doras_in_fuuro,
            prefer_riichi,
            dora_indicators: &self.dora_indicators,
            calc_double_riichi,
            calc_haitei,
            sort_result: true,
            maximize_win_prob: options.maximize_win_prob,
            max_shanten: options.max_shanten,
            calc_tegawari: options.calc_tegawari.filter(|_| !self.riichi_declared[0]),
            calc_shanten_down: options.calc_shanten_down.filter(|_| !self.riichi_declared[0]),
            min_score: options.min_score.filter(|_| !self.riichi_declared[0]),
        };

        let mut max_ev_table = sp_calc.calc(init_state, can_discard, tsumos_left, cur_shanten)?;
        if is_discard_after_riichi {
            max_ev_table[0].tile = self.last_self_tsumo.unwrap();
        }

        max_ev_table.retain(|candidate| candidate.tile.is_unknown() || !self.forbidden_tiles[candidate.tile.deaka().as_usize()]);

        Ok(max_ev_table)
    }

    /// Possible actionable events for the current state, excluding dahai
    pub fn possible_actions(&self) -> Vec<Event> {
        let mut events: Vec<Event> = vec![];

        if self.last_cans.can_riichi {
            events.push(Event::Reach { actor: self.player_id });
        }
        if self.last_cans.can_chi_low {
            let pai = self.last_kawa_tile.unwrap();
            let first = pai.next();
            let can_akaize_consumed = match pai.as_u8() {
                tu8!(3m) | tu8!(4m) => self.akas_in_hand[0],
                tu8!(3p) | tu8!(4p) => self.akas_in_hand[1],
                tu8!(3s) | tu8!(4s) => self.akas_in_hand[2],
                _ => false,
            };
            let consumed = if can_akaize_consumed {
                [first.akaize(), first.next().akaize()]
            } else {
                [first, first.next()]
            };
            events.push(Event::Chi {
                actor: self.player_id,
                target: self.last_cans.target_actor,
                pai,
                consumed,
            });
        }
        if self.last_cans.can_chi_mid {
            let pai = self.last_kawa_tile.unwrap();
            let can_akaize_consumed = match pai.as_u8() {
                tu8!(4m) | tu8!(6m) => self.akas_in_hand[0],
                tu8!(4p) | tu8!(6p) => self.akas_in_hand[1],
                tu8!(4s) | tu8!(6s) => self.akas_in_hand[2],
                _ => false,
            };
            let consumed = if can_akaize_consumed {
                [pai.prev().akaize(), pai.next().akaize()]
            } else {
                [pai.prev(), pai.next()]
            };
            events.push(Event::Chi {
                actor: self.player_id,
                target: self.last_cans.target_actor,
                pai,
                consumed,
            });
        }
        if self.last_cans.can_chi_high {
            let pai = self.last_kawa_tile.unwrap();
            let last = pai.prev();
            let can_akaize_consumed = match pai.as_u8() {
                tu8!(6m) | tu8!(7m) => self.akas_in_hand[0],
                tu8!(6p) | tu8!(7p) => self.akas_in_hand[1],
                tu8!(6s) | tu8!(7s) => self.akas_in_hand[2],
                _ => false,
            };
            let consumed = if can_akaize_consumed {
                [last.prev().akaize(), last.akaize()]
            } else {
                [last.prev(), last]
            };
            events.push(Event::Chi {
                actor: self.player_id,
                target: self.last_cans.target_actor,
                pai,
                consumed,
            });
        }
        if self.last_cans.can_pon {
            let pai = self.last_kawa_tile.unwrap();
            let can_akaize_consumed = match pai.as_u8() {
                tu8!(5m) => self.akas_in_hand[0],
                tu8!(5p) => self.akas_in_hand[1],
                tu8!(5s) => self.akas_in_hand[2],
                _ => false,
            };
            let consumed = if can_akaize_consumed {
                [pai.akaize(), pai.deaka()]
            } else {
                [pai.deaka(); 2]
            };
            events.push(Event::Pon {
                actor: self.player_id,
                target: self.last_cans.target_actor,
                pai,
                consumed,
            });
        }
        if self.last_cans.can_daiminkan {
            let tile = self.last_kawa_tile.unwrap();
            let consumed = if tile.is_aka() {
                [tile.deaka(); 3]
            } else {
                [tile.akaize(), tile, tile]
            };
            events.push(Event::Daiminkan {
                actor: self.player_id,
                target: self.last_cans.target_actor,
                pai: tile,
                consumed,
            });
        }
        if self.last_cans.can_ankan {
            for tile in &self.ankan_candidates {
                events.push(Event::Ankan {
                    actor: self.player_id,
                    consumed: [tile.akaize(), *tile, *tile, *tile],
                });
            }
        }
        if self.last_cans.can_kakan {
            for tile in &self.kakan_candidates {
                let can_akaize_target = match tile.as_u8() {
                    tu8!(5m) => self.akas_in_hand[0],
                    tu8!(5p) => self.akas_in_hand[1],
                    tu8!(5s) => self.akas_in_hand[2],
                    _ => false,
                };
                let (pai, consumed) = if can_akaize_target {
                    (tile.akaize(), [tile.deaka(); 3])
                } else {
                    (tile.deaka(), [tile.akaize(), tile.deaka(), tile.deaka()])
                };
                events.push(Event::Kakan {
                    actor: self.player_id,
                    pai,
                    consumed,
                });
            }
        }

        events
    }

    /// Single player tables after possible actionable events
    pub fn single_player_tables_after_actions(&self, options: &SPOptions) -> Vec<(Option<Event>, Result<Vec<Candidate>>)> {
        let mut tables = vec![];
        if self.last_cans.can_riichi {
            // if can_riichi then no action is equivalent to an explicit deny of riichi
            let mut state = self.clone();
            state.last_cans.can_riichi = false;
            tables.push((None, state.single_player_tables(options)));
        } else {
            tables.push((None, self.single_player_tables(options)));
        }
        for event in self.possible_actions() {
            let mut state = self.clone();
            state.update(&event).unwrap();
            tables.push((Some(event), state.single_player_tables(options)));
        }
        tables
    }

    /// Single player tables for every possible event including dahai
    /// If candidates cannot be calculated for an event that we can do, they will be None
    /// Same as single_player_tables, candidates are sorted.
    pub fn single_player_tables_for_events(&self, options: &SPOptions) -> Vec<(Event, Candidate)> {
        let mut table = vec![];
        for (event, candidates) in self.single_player_tables_after_actions(options) {
            if let Some(event) = event {
                let candidate = if let Some(mut candidate) = candidates.unwrap().into_iter().next() {
                    candidate.tile = t!(?);
                    candidate
                } else {
                    // No valid discard that keeps shanten
                    Candidate::default()
                };
                table.push((event, candidate));
            } else {
                for candidate in candidates.unwrap() {
                    let event = if candidate.tile == t!(?) {
                        Event::None
                    } else {
                        Event::Dahai {
                            actor: self.player_id,
                            pai: candidate.tile,
                            tsumogiri: Some(candidate.tile) == self.last_self_tsumo,
                        }
                    };
                    table.push((event, candidate));
                }
            }
        }
        let by = if options.maximize_win_prob {
            CandidateColumn::WinProb
        } else {
            CandidateColumn::EV
        };
        table.sort_unstable_by(|(_, r), (_, l)| l.cmp(r, by));

        table
    }
}
