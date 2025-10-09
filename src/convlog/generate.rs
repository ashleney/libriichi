//! Generate mjai logs from a representation of the visible board
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
    let mut scores = board.scores;
    scores.rotate_right(player_id as usize);
    let mut kyotaku = board.kyotaku;

    // process fuuros by creating call events and creating discards we can no longer see
    // information about some discards is lost, we'll estimate them to be tedashi non-riichi for simplicity
    let mut called_sutehais_abs: [Vec<(Sutehai, u8)>; 4] = from_fn(|_| vec![]); // rel
    let mut fuuro_events_rel: [Vec<Event>; 4] = from_fn(|_| vec![]);
    let mut naki_count_abs = [0; 4];
    for (rel_player, fuuro) in board.fuuro.iter().enumerate() {
        let actor = player_abs(rel_player);
        fuuro_events_rel[rel_player] = fuuro.to_events(actor)?;
        naki_count_abs[actor as usize] = fuuro_events_rel[rel_player]
            .iter()
            .filter(|event| !matches!(event, Event::Kakan { .. }))
            .count() as u8;
        for event in &fuuro_events_rel[rel_player] {
            if let Some((target, pai)) = event.naki_info() {
                let sutehai = Sutehai {
                    pai,
                    tedashi: true,
                    riichi: false,
                };
                called_sutehais_abs[target as usize].push((sutehai, actor));
            }
        }
    }

    let at_discard = board.tehai.len() % 3 == 2;

    // construct player turns by attaching calls to the first viable dahai
    // contains: tsumo or call replacing tsumo, ankan/kakan calls, riichi declaration, dahai, dora reveal
    // included is the player that will want to call after the last event
    // the last turn may not have a dahai action if the provided Board is right after a call
    // unknown tsumo tiles and dora indicators will be filled in later based on what tiles could possibly be in there
    let mut turns: [Vec<(Vec<Event>, Option<u8>)>; 4] = from_fn(|_| vec![]);
    for (rel_player, (kawa, fuuro_events)) in board.kawa.iter().zip(fuuro_events_rel).enumerate() {
        let actor = player_abs(rel_player);
        let mut fuuro_iter = fuuro_events.into_iter().peekable();
        let mut kakan_candidates = vec![];
        for (sutehai, next_player) in called_sutehais_abs[actor as usize]
            .iter()
            .map(|(called_sutehai, next_player)| (Some(called_sutehai), Some(next_player)))
            .chain(kawa.iter().map(|sutehai| (Some(sutehai), None)))
            .chain(once((None, None)))
        {
            let mut reveal_dora_at_discard = false;
            let mut events = vec![];

            let is_first_oya_act = actor == oya && turns[actor as usize].is_empty();
            let draw_event = if !is_first_oya_act && let Some(next_event) = fuuro_iter.peek() {
                match next_event {
                    Event::Chi { .. } if sutehai.is_none_or(|sutehai| sutehai.tedashi) => fuuro_iter.next().unwrap(),
                    &Event::Pon { pai, .. } if sutehai.is_none_or(|sutehai| sutehai.tedashi) => {
                        kakan_candidates.push(pai);
                        fuuro_iter.next().unwrap()
                    }
                    Event::Daiminkan { .. } => {
                        events.push(fuuro_iter.next().unwrap());
                        reveal_dora_at_discard = true;
                        Event::Tsumo { actor, pai: t!(?) }
                    }
                    _ => Event::Tsumo { actor, pai: t!(?) },
                }
            } else {
                Event::Tsumo { actor, pai: t!(?) }
            };
            events.push(draw_event.clone());

            if !matches!(draw_event, Event::Chi { .. } | Event::Pon { .. }) {
                while let Some(next_naki) = fuuro_iter.peek() {
                    match next_naki {
                        Event::Ankan { .. } => {
                            if reveal_dora_at_discard {
                                events.push(Event::Dora { dora_marker: t!(?) });
                                reveal_dora_at_discard = false;
                            }
                            events.push(fuuro_iter.next().unwrap());
                            events.push(Event::Dora { dora_marker: t!(?) });
                            events.push(Event::Tsumo { actor, pai: t!(?) });
                        }
                        Event::Kakan { pai, .. } if kakan_candidates.contains(pai) => {
                            events.push(fuuro_iter.next().unwrap());
                            if reveal_dora_at_discard {
                                events.push(Event::Dora { dora_marker: t!(?) });
                            }
                            events.push(Event::Tsumo { actor, pai: t!(?) });
                            reveal_dora_at_discard = true;
                        }
                        _ => break,
                    }
                }
            }

            if let Some(sutehai) = sutehai {
                if sutehai.riichi {
                    events.push(Event::Reach { actor });
                    scores[actor as usize] += 1000;
                    kyotaku = kyotaku.saturating_sub(1);
                }
                events.push(Event::Dahai {
                    actor,
                    pai: sutehai.pai,
                    tsumogiri: !sutehai.tedashi,
                });
                if reveal_dora_at_discard {
                    events.push(Event::Dora { dora_marker: t!(?) });
                }
            }

            if !(events.len() == 1 && matches!(events[0], Event::Tsumo { .. }) && !(rel_player == 0 && at_discard)) {
                turns[actor as usize].push((events, next_player.copied()));
            }
        }
        if fuuro_iter.next().is_some() {
            bail!("more calls than tsumos");
        }
    }

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

    // reverse pass to fill in tehai and tsumo tiles
    let mut tehais: [Vec<Tile>; 4] = from_fn(|_| vec![t!(?); 13]);
    tehais[player_id as usize] = board.tehai;
    for player in (0..=3).filter(|player| *player != player_id) {
        // TODO: merge shouminkan and pon
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
        tehais[player as usize] = tehai;
    }

    for (player, turns) in turns.iter_mut().enumerate() {
        for (turn, _) in turns.iter_mut().rev() {
            for event in turn.iter_mut().rev() {
                match event {
                    Event::Tsumo { .. } => {
                        *event = Event::Tsumo {
                            actor: player as u8,
                            pai: tehais[player].pop().unwrap(),
                        }
                    }
                    Event::Dahai { tsumogiri, pai, .. } => {
                        if *tsumogiri {
                            tehais[player].push(*pai);
                        } else {
                            tehais[player].insert(tehais[player].len() - 2, *pai);
                        }
                    }
                    Event::Chi { consumed, .. } | Event::Pon { consumed, .. } => {
                        tehais[player].extend(consumed.iter());
                    }
                    Event::Daiminkan { consumed, .. } => {
                        tehais[player].extend(consumed.iter());
                    }
                    Event::Kakan { pai, .. } => {
                        tehais[player].push(*pai);
                    }
                    Event::Ankan { consumed, .. } => {
                        tehais[player].extend(consumed.iter());
                    }
                    _ => {}
                }
            }
        }
    }

    let mut events = vec![];
    let seen_aka = remaining_tiles[34..37].iter().any(|count| *count < 1);
    events.push(Event::StartGame {
        id: Some(player_id),
        aka_flag: seen_aka,
        names: from_fn(|_| String::new()),
        kyoku_first: 1,
    });
    let tehais = tehais
        .into_iter()
        .map(|tehai| {
            tehai.try_into().map_err(|tehai_vec: Vec<Tile>| {
                anyhow::anyhow!("Incorrect tehai size {}, {} off", tehai_vec.len(), tehai_vec.len() as i8 - 13)
            })
        })
        .collect::<Result<Vec<[Tile; 13]>>>()?
        .try_into()
        .unwrap();
    let mut dora_iter = board.dora_indicators.into_iter();
    events.push(Event::StartKyoku {
        bakaze: board.bakaze,
        dora_marker: dora_iter.next().context("missing dora indicator")?,
        kyoku: board.kyoku,
        honba: board.honba,
        kyotaku,
        oya,
        scores,
        tehais,
    });

    let mut current_player = oya;
    let mut turns_iter = turns.map(|turns| turns.into_iter());
    loop {
        let Some((mut turn_events, next_player)) = turns_iter[current_player as usize].next() else {
            break;
        };
        for event in &mut turn_events {
            if matches!(event, Event::Dora { .. }) {
                // this is the only place we can be sure the dora events are well ordered
                *event = Event::Dora {
                    dora_marker: dora_iter.next().context("missing dora indicator")?,
                };
            }
        }
        events.append(&mut turn_events);
        current_player = next_player.unwrap_or((current_player + 1) % 4);
    }
    if turns_iter.iter_mut().any(|turns| turns.len() != 0) {
        bail!(
            "Incorrect number of turns, remaining {:?}",
            turns_iter.map(|turns| turns.len())
        );
    }

    Ok(events)
}

#[cfg(test)]
mod test {
    use crate::convlog::mjai_to_tenhou;

    use super::*;

    fn to_sutehai(kawa: &[Tile]) -> Vec<Sutehai> {
        kawa.into_iter()
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
        assert_eq!(tenhou.to_string_pretty()?, r#"{
    "log": [ [ [ 2, 1, 1 ],
        [ 37500, 21300, 32500, 7700 ],
        [ 45 ],
        [],
        [ 11, 11, 12, 12, 12, 12, 13, 14, 18, 21, 15, 39, 39 ],
        [ "3939p39", 11, 11, 47, 29, 14, 14 ],
        [ 18, 21, 15, 11, 11, 47, 29 ],
        [ 15, 16, 16, 16, 17, 17, 17, 17, 18, 19, 19, 39, 41 ],
        [ 19, 21, 42, 43, 19, 21 ],
        [ 39, 41, 19, 21, 42, 43 ],
        [ 13, 14, 15, 16, 22, 23, 24, 24, 52, 32, 34, 29, 44 ],
        [ 27, 28, 45, 37, 38, 13 ],
        [ 29, 44, 27, 28, 45 ],
        [ 21, 22, 22, 22, 23, 23, 23, 24, 44, 29, 28, 18, 18 ],
        [ "1818p18", 26, 13, 42, 24, 25 ],
        [ 44, 29, 28, 26, 13, 42 ],
        [ "Ryuukyoku", [ 0, 0, 0, 0 ] ] ] ],
    "name": [ "", "", "", "" ],
    "rule": { "disp": "南", "aka": 0, "aka51": 1, "aka52": 1, "aka53": 1
    }
}"#);
        Ok(())
    }
}
