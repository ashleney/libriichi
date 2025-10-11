//! Generate mjai logs from a representation of the visible board
use crate::chi_type::ChiType;
use crate::mjai::Event;
use crate::tile::Tile;
use crate::{must_tile, t, tu8, tuz};

use anyhow::{Context, Result, bail, ensure};
use std::array::from_fn;
use std::iter::once;

#[derive(Debug, Clone, Default)]
pub struct Board {
    /// Round wind
    pub bakaze: Tile,
    /// Seat wind
    pub jikaze: Tile,
    /// Kyoku in the current round wind
    pub kyoku: u8,
    /// Repeat counters
    pub honba: u8,
    /// Riichi stick counters
    pub kyotaku: u8,
    /// Points in the current state
    pub scores: [i32; 4],
    /// Dora indicators
    pub dora_indicators: Vec<Tile>,
    /// discarded tiles, relative
    pub kawa: [Vec<Sutehai>; 4],
    /// Melds of players from earliest to latest, relative
    pub fuuro: [Fuuro; 4],
    /// Tehai in the current state
    pub tehai: Vec<Tile>,
}

#[derive(Debug, Clone, Copy)]
pub struct Sutehai {
    /// Discarded tile
    pub pai: Tile,
    /// Whether tile was discarded from hand
    pub tedashi: bool,
    /// Whether this tile is the riichi declaration tile
    pub riichi: bool,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Fuurohai {
    /// Tile in the meld
    pub tile: Tile,
    /// Whether the tile is sideways like a naitahai or kakan
    pub sideways: bool,
}

#[derive(Debug, Default, Clone)]
pub struct Fuuro {
    /// Tiles in the meld and whether they are from one's own hand
    pub tiles: Vec<Fuurohai>,
}

#[rustfmt::skip]
pub const FUURO_DIV: [&[&[u8]]; 14] = [
    &[&[3]],
    &[&[4]],
    &[],
    &[&[3, 3]],
    &[&[3, 4], &[4, 3]],
    &[&[4, 4]],
    &[&[3, 3, 3]],
    &[&[3, 3, 4], &[3, 4, 3], &[4, 3, 3]],
    &[&[3, 4, 4], &[4, 3, 4], &[4, 4, 3]],
    &[&[3, 3, 3, 3], &[4, 4, 4]],
    &[&[3, 3, 3, 4], &[3, 3, 4, 3], &[3, 4, 3, 3], &[4, 3, 3, 3]],
    &[&[3, 3, 4, 4], &[3, 4, 3, 4], &[3, 4, 4, 3], &[4, 3, 3, 4], &[4, 3, 4, 3], &[4, 4, 3, 3]],
    &[&[3, 4, 4, 4], &[4, 3, 4, 4], &[4, 4, 3, 4], &[4, 4, 4, 3]],
    &[&[4, 4, 4, 4]],
];

pub fn naki_to_events(naki: &[Fuurohai], actor: u8) -> Option<Vec<Event>> {
    let mut called = vec![];
    let mut consumed = vec![];
    let mut called_pos = None;
    for &Fuurohai { tile, sideways } in naki {
        if sideways {
            if called_pos.is_some_and(|p| p != consumed.len()) {
                return None; // multiple non-consecutive naitahai
            }
            called_pos = Some(consumed.len());
            called.push(tile);
        } else {
            consumed.push(tile);
        }
    }

    let rel_target = match called_pos {
        None => 0,
        Some(0) => 3,
        Some(p) if p == consumed.len() => 1,
        Some(_) => 2,
    };
    let target = (actor + rel_target) % 4;

    let identical = naki.iter().all(|fuurohai| fuurohai.tile.deaka() == naki[0].tile.deaka());
    Some(match (called.len(), consumed.len()) {
        (1, 2) if rel_target == 3 && !identical => {
            vec![Event::Chi {
                actor,
                target,
                pai: called[0],
                consumed: consumed.as_slice().try_into().unwrap(),
            }]
        }
        (1, 2) if identical => {
            vec![Event::Pon {
                actor,
                target,
                pai: called[0],
                consumed: consumed.as_slice().try_into().unwrap(),
            }]
        }
        (1, 3) if identical => {
            vec![Event::Daiminkan {
                actor,
                target,
                pai: called[0],
                consumed: consumed.as_slice().try_into().unwrap(),
            }]
        }
        (0, 4) if identical && rel_target == 0 => {
            vec![Event::Ankan {
                actor,
                consumed: consumed.as_slice().try_into().unwrap(),
            }]
        }
        (2, 2) if identical => {
            vec![
                Event::Pon {
                    actor,
                    target,
                    pai: called[0],
                    consumed: consumed.as_slice().try_into().unwrap(),
                },
                Event::Kakan {
                    actor,
                    pai: called[1],
                    consumed: consumed
                        .into_iter()
                        .chain(once(called[0]))
                        .collect::<Vec<_>>()
                        .try_into()
                        .unwrap(),
                },
            ]
        }
        (_, _) => return None,
    })
}

impl Fuuro {
    /// convert a meld to mjai events given absolute actor and target ids
    fn to_events(&self, actor: u8) -> Result<Vec<Event>> {
        let len = self.tiles.len();
        if len == 0 {
            return Ok(vec![]);
        }
        ensure!((3..=16).contains(&len), "invalid fuuro size {self:?}");
        'outer: for partition in FUURO_DIV[len - 3] {
            let mut offset = 0;
            let mut events = Vec::with_capacity(len / 2);
            for &size in *partition {
                let naki = self.tiles[offset..offset + size as usize].to_vec();
                let Some(naki_events) = naki_to_events(&naki, actor) else {
                    continue 'outer;
                };
                events.extend(naki_events.into_iter().rev());
                offset += size as usize;
            }
            events.reverse();
            return Ok(events);
        }

        bail!("invalid fuuro {self:?}");
    }
}

pub fn generate_mjai_logs(board: Board) -> Result<Vec<Event>> {
    let oya = board.kyoku - 1;
    let player_id = (4 + oya + board.jikaze.as_u8() - tu8!(E)) % 4;

    let player_abs = |player| (player as u8 + player_id) % 4;
    let player_rel = |player| (4 + player - player_id) % 4;
    let mut scores = board.scores;
    scores.rotate_right(player_id as usize);
    let mut kyotaku = board.kyotaku;
    let at_discard = board.tehai.len() % 3 == 2;

    // remaining tiles which have not been witnessed and could therefore be in someone's tehai
    let mut remaining_tiles = [4_i8; 37]; // may go negative in case it's not 3aka
    remaining_tiles[tuz!(5m)] = 3;
    remaining_tiles[tuz!(5p)] = 3;
    remaining_tiles[tuz!(5s)] = 3;
    remaining_tiles[tuz!(5mr)] = 1;
    remaining_tiles[tuz!(5pr)] = 1;
    remaining_tiles[tuz!(5sr)] = 1;
    for tile in board.dora_indicators.iter().chain(board.tehai.iter()) {
        remaining_tiles[tile.as_usize()] -= 1;
    }
    for &Sutehai { pai, .. } in board.kawa.iter().flatten() {
        remaining_tiles[pai.as_usize()] -= 1;
    }
    for &Fuurohai { tile, .. } in board.fuuro.iter().flat_map(|fuuro| &fuuro.tiles) {
        remaining_tiles[tile.as_usize()] -= 1;
    }

    // process fuuros by creating call events
    // separate kakan from other events, because they do not have a set order other than being after a pon
    let mut fuuro_events_abs: [Vec<Event>; 4] = from_fn(|_| vec![]);
    let mut kakan_events_abs: [Vec<Event>; 4] = from_fn(|_| vec![]);
    let mut naki_count_abs = [0; 4];
    for (rel_player, fuuro) in board.fuuro.iter().enumerate() {
        let actor = player_abs(rel_player);
        for event in fuuro.to_events(actor)? {
            match event {
                Event::Kakan { .. } => {
                    kakan_events_abs[actor as usize].push(event);
                }
                _ => {
                    fuuro_events_abs[actor as usize].push(event);
                    naki_count_abs[actor as usize] += 1;
                }
            }
        }
    }

    let mut sutehai_iter_rel = board.kawa.map(|kawa| kawa.into_iter().peekable());
    let mut fuuro_events_iter_abs = fuuro_events_abs.map(|fuuro_events| fuuro_events.into_iter().peekable());
    let mut kakan_events_iter_abs = kakan_events_abs.map(|fuuro_events| fuuro_events.into_iter().peekable());
    let initial_dora_indicator = *board.dora_indicators.first().context("missing dora indicator")?;
    let mut dora_iter = board.dora_indicators.into_iter().skip(1);

    // process the upcoming events by guessing what the next reasonable call and discard is
    // we prefer putting calls as early as possible to lessen an issue of some states being unrepresentable
    // multiple kan events in a single turn are possible and dora events are properly inserted
    let mut events = vec![];
    let mut current_actor = oya;
    let mut next_draw_event: Option<Event> = None;
    let mut kakan_candidates = [false; 34];
    loop {
        let current_actor_rel = player_rel(current_actor);

        let mut reveal_dora_at_discard = false;
        let mut turn_events = vec![];

        let draw_event = match &next_draw_event {
            Some(draw_event @ Event::Pon { pai, .. }) => {
                kakan_candidates[pai.as_usize()] = true;
                draw_event.clone()
            }
            Some(draw_event @ Event::Daiminkan { .. }) => {
                turn_events.push(draw_event.clone());
                reveal_dora_at_discard = true;
                Event::Tsumo {
                    actor: current_actor,
                    pai: t!(?),
                }
            }
            Some(draw_event) => draw_event.clone(), // chi
            None => Event::Tsumo {
                actor: current_actor,
                pai: t!(?),
            },
        };
        turn_events.push(draw_event.clone());

        // self-declared kan may only happen after tsumo (including from rinshan)
        // we arbitarily prefer kakan before ankan since it's technically earlier in the fuuro
        if matches!(draw_event, Event::Tsumo { .. }) {
            while let Some(kan_event) = kakan_events_iter_abs[current_actor as usize]
                .next_if(|event| matches!(event, Event::Kakan { pai, .. } if kakan_candidates[pai.as_usize()]))
                .or_else(|| fuuro_events_iter_abs[current_actor as usize].next_if(|event| matches!(event, Event::Ankan { .. })))
            {
                match kan_event {
                    Event::Kakan { pai, .. } if kakan_candidates[pai.as_usize()] => {
                        turn_events.push(kan_event);
                        if reveal_dora_at_discard {
                            turn_events.push(Event::Dora {
                                dora_marker: dora_iter.next().context("missing dora indicator")?,
                            });
                        }
                        turn_events.push(Event::Tsumo {
                            actor: current_actor,
                            pai: t!(?),
                        });
                        reveal_dora_at_discard = true;
                    }
                    Event::Ankan { .. } => {
                        turn_events.push(kan_event);
                        if reveal_dora_at_discard {
                            turn_events.push(Event::Dora {
                                dora_marker: dora_iter.next().context("missing dora indicator")?,
                            });
                            reveal_dora_at_discard = false;
                        }
                        turn_events.push(Event::Dora {
                            dora_marker: dora_iter.next().context("missing dora indicator")?,
                        });
                        turn_events.push(Event::Tsumo {
                            actor: current_actor,
                            pai: t!(?),
                        });
                    }
                    _ => unreachable!(),
                }
            }
        }

        // next sutehai is one of: discard that will be called by someone, discard visible in kawa, none because just a draw
        // next call is only valid if current actor is the target and the player's next discard is legal under kuikae
        // normally pon has priority over chi, however we can assume without consequences that the ponning player skipped the first time
        // it is possible that we for example decide to discard 1m, shimocha wants to chi (1m)2m3m but the next visible discard is 4m
        // this call would be legal if we could insert an implied discard if shimocha will feed another player's call, and therefore discard a non-kuikae tile
        // however in this situation we can always postpone the call from current actor to shimocha to the next discard
        // unfortunately we render a small set of board states with many calls and little discards unparseable
        let next_event_targetting_actor = fuuro_events_iter_abs.iter_mut().enumerate().find_map(|(call_actor, it)| {
            it.next_if(|event| {
                event.naki_info().is_some_and(|(target, _)| target == current_actor)
                    && sutehai_iter_rel[player_rel(call_actor as u8) as usize].peek().is_none_or(
                        |Sutehai {
                             pai: sutehai_pai,
                             tedashi,
                             ..
                         }| {
                            match event {
                                Event::Chi { pai, consumed, .. } => {
                                    *tedashi
                                        && pai != sutehai_pai
                                        && match ChiType::new(*consumed, *pai) {
                                            ChiType::Low => *sutehai_pai != pai.next().next().next(),
                                            ChiType::Mid => true,
                                            ChiType::High => *sutehai_pai != pai.prev().prev().prev(),
                                        }
                                }
                                Event::Pon { pai, .. } => *tedashi && pai != sutehai_pai,
                                _ => true, // daiminkan, no kuikae, tedashi irrelevant
                            }
                        },
                    )
            })
        });
        let sutehai;
        (sutehai, next_draw_event) = if let Some(event) = next_event_targetting_actor {
            let (_, pai) = event.naki_info().unwrap();
            let estimated_sutehai = Sutehai {
                pai,
                tedashi: true,
                riichi: false,
            };
            (Some(estimated_sutehai), Some(event))
        } else if let Some(sutehai) = sutehai_iter_rel[current_actor_rel as usize].next() {
            (Some(sutehai), None)
        } else {
            (None, None)
        };

        if let Some(sutehai) = sutehai {
            if sutehai.riichi {
                turn_events.push(Event::Reach { actor: current_actor });
                scores[current_actor as usize] += 1000;
                kyotaku = kyotaku.saturating_sub(1);
            }
            turn_events.push(Event::Dahai {
                actor: current_actor,
                pai: sutehai.pai,
                tsumogiri: !sutehai.tedashi,
            });
            if reveal_dora_at_discard {
                turn_events.push(Event::Dora {
                    dora_marker: dora_iter.next().context("missing dora indicator")?,
                });
            }
        } else {
            // no discard must mean it is the last event
            // we keep the tsumo only if the inputted tehai indicates it's our turn
            if !(turn_events.len() == 1
                && matches!(turn_events[0], Event::Tsumo { .. })
                && !(current_actor_rel == 0 && at_discard))
            {
                events.extend(turn_events);
            }
            break;
        }

        events.extend(turn_events);
        current_actor = if let Some(next_draw_event) = &next_draw_event {
            next_draw_event.actor().unwrap()
        } else {
            (current_actor + 1) % 4
        };
    }
    if sutehai_iter_rel.iter_mut().any(|iter| iter.peek().is_some())
        || fuuro_events_iter_abs.iter_mut().any(|iter| iter.peek().is_some())
        || kakan_events_iter_abs.iter_mut().any(|iter| iter.peek().is_some())
    {
        bail!(
            "Not all events were depleted: sutehai {:?} fuuro {:?} kakan {:?}",
            sutehai_iter_rel.map(|iter| iter.collect::<Vec<_>>()),
            fuuro_events_iter_abs.map(|iter| iter.collect::<Vec<_>>()),
            kakan_events_iter_abs.map(|iter| iter.collect::<Vec<_>>())
        );
    }

    // reverse pass to fill in tehai and tsumo tiles
    let mut tehais_abs: [Vec<Tile>; 4] = from_fn(|_| vec![t!(?); 13]);
    tehais_abs[player_id as usize] = board.tehai;
    for player in (0..=3).filter(|player| *player != player_id) {
        let expected_tehai_size = 13 - 3 * naki_count_abs[player as usize];
        let mut tehai = vec![];
        'outer: for (tile, count) in remaining_tiles.iter_mut().enumerate() {
            for _ in 0..*count {
                if tehai.len() as u8 >= expected_tehai_size {
                    break 'outer;
                }
                tehai.push(must_tile!(tile));
                *count -= 1;
            }
        }
        tehais_abs[player as usize] = tehai;
    }

    for event in events.iter_mut().rev() {
        match event {
            Event::Tsumo { actor, .. } => {
                *event = Event::Tsumo {
                    actor: *actor,
                    pai: tehais_abs[*actor as usize].pop().unwrap(),
                }
            }
            Event::Dahai { actor, pai, tsumogiri } => {
                if *tsumogiri {
                    tehais_abs[*actor as usize].push(*pai);
                } else {
                    tehais_abs[*actor as usize].insert(tehais_abs[*actor as usize].len() - 2, *pai);
                }
            }
            Event::Chi { actor, consumed, .. } | Event::Pon { actor, consumed, .. } => {
                tehais_abs[*actor as usize].extend(consumed.iter());
            }
            Event::Daiminkan { actor, consumed, .. } => {
                tehais_abs[*actor as usize].extend(consumed.iter());
            }
            Event::Kakan { actor, pai, .. } => {
                tehais_abs[*actor as usize].push(*pai);
            }
            Event::Ankan { actor, consumed, .. } => {
                tehais_abs[*actor as usize].extend(consumed.iter());
            }
            _ => {}
        }
    }

    let seen_aka = remaining_tiles[34..37].iter().any(|count| *count < 1);
    events.insert(
        0,
        Event::StartGame {
            id: Some(player_id),
            aka_flag: seen_aka,
            names: from_fn(|_| String::new()),
            kyoku_first: 1,
        },
    );
    let tehais = tehais_abs
        .into_iter()
        .map(|tehai| {
            tehai.try_into().map_err(|tehai_vec: Vec<Tile>| {
                anyhow::anyhow!("Incorrect tehai size {}, {} off", tehai_vec.len(), tehai_vec.len() as i8 - 13)
            })
        })
        .collect::<Result<Vec<[Tile; 13]>>>()?
        .try_into()
        .unwrap();
    events.insert(
        1,
        Event::StartKyoku {
            bakaze: board.bakaze,
            dora_marker: initial_dora_indicator,
            kyoku: board.kyoku,
            honba: board.honba,
            kyotaku,
            oya,
            scores,
            tehais,
        },
    );

    Ok(events)
}

#[cfg(test)]
mod test {
    use crate::convlog::mjai_to_tenhou;

    use super::*;

    fn to_sutehai(kawa: &[Tile]) -> Vec<Sutehai> {
        kawa.iter()
            .map(|&pai| Sutehai {
                pai,
                tedashi: true,
                riichi: false,
            })
            .collect::<Vec<_>>()
    }

    #[test]
    fn test_generate_logs() -> Result<()> {
        let board = Board {
            bakaze: t!(E),
            jikaze: t!(E),
            kyoku: 3,
            honba: 1,
            kyotaku: 1,
            scores: [32500, 7700, 37500, 21300],
            dora_indicators: t!(P,).into(),
            tehai: t!(3m, 4m, 5m, 6m, 2p, 3p, 4p, 4p, 5pr, 2s, 4s, 7s, 8s, 3m).into(),
            kawa: [
                to_sutehai(&t!(9p, N, 7p, 8p, P)),
                to_sutehai(&t!(N, 9p, 8p, 6p, 3m, S)),
                to_sutehai(&t!(1p, 5m, 1m, 1m, C, 9p)),
                to_sutehai(&t!(E, 9m, 1p, S, W)),
            ],
            fuuro: [
                Fuuro { tiles: vec![] },
                Fuuro {
                    tiles: vec![
                        Fuurohai {
                            tile: t!(8m),
                            sideways: false,
                        },
                        Fuurohai {
                            tile: t!(8m),
                            sideways: false,
                        },
                        Fuurohai {
                            tile: t!(8m),
                            sideways: true,
                        },
                    ],
                },
                Fuuro {
                    tiles: vec![
                        Fuurohai {
                            tile: t!(9s),
                            sideways: false,
                        },
                        Fuurohai {
                            tile: t!(9s),
                            sideways: false,
                        },
                        Fuurohai {
                            tile: t!(9s),
                            sideways: true,
                        },
                    ],
                },
                Fuuro { tiles: vec![] },
            ],
        };
        let events = generate_mjai_logs(board)?;
        let tenhou = mjai_to_tenhou(&events)?;
        assert_eq!(
            tenhou.to_string_pretty()?,
            r#"{
    "log": [ [ [ 2, 1, 1 ],
        [ 37500, 21300, 32500, 7700 ],
        [ 45 ],
        [],
        [ 11, 11, 12, 12, 12, 12, 13, 14, 15, 11, 11, 18, 21 ],
        [ 39, 39, "3939p39", 47, 29, 14, 14 ],
        [ 18, 21, 15, 11, 11, 47, 29 ],
        [ 15, 16, 16, 16, 17, 17, 17, 17, 18, 19, 19, 39, 41 ],
        [ 19, 21, 42, 43, 19, 21 ],
        [ 39, 41, 19, 21, 42, 43 ],
        [ 13, 14, 15, 16, 22, 23, 24, 24, 52, 32, 34, 29, 44 ],
        [ 27, 28, 45, 37, 38, 13 ],
        [ 29, 44, 27, 28, 45 ],
        [ 21, 22, 22, 22, 23, 23, 23, 24, 29, 28, 26, 44, 18 ],
        [ 18, "1818p18", 13, 42, 24, 25 ],
        [ 44, 29, 28, 26, 13, 42 ],
        [ "Ryuukyoku", [ 0, 0, 0, 0 ] ] ] ],
    "name": [ "", "", "", "" ],
    "rule": { "disp": "南", "aka": 0, "aka51": 1, "aka52": 1, "aka53": 1
    }
}"#
        );
        Ok(())
    }
}
