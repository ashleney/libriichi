//! mjai to tenhou
use anyhow::{Context, Result, bail};

use crate::convlog::tenhou::{ActionItem, EndStatus, HoraDetail, Kyoku, KyokuMeta, Log, RawLog, TenhouTile};
use crate::mjai::Event;
use crate::tu8;

/// Convert mjai events into a take tenhou action
pub fn mjai_to_tenhou(events: &[Event]) -> Result<RawLog> {
    let mut log = Log::default();
    let kyokus = &mut log.kyokus;
    let mut kyoku = &mut Kyoku::default();
    let mut action_tables = &mut kyoku.action_tables;

    let mut event_iter = events.iter();
    while let Some(event) = event_iter.next() {
        match *event {
            Event::StartGame { ref names, aka_flag, .. } => {
                log.names = names.clone();
                log.has_aka = aka_flag;
            }
            Event::StartKyoku {
                bakaze,
                dora_marker,
                kyoku: kyoku_num,
                honba,
                kyotaku,
                scores,
                tehais,
                ..
            } => {
                kyokus.push(Kyoku {
                    meta: KyokuMeta {
                        kyoku_num: (bakaze.as_u8() - tu8!(E)) + kyoku_num - 1,
                        honba,
                        kyotaku,
                    },
                    scoreboard: scores,
                    dora_indicators: vec![dora_marker],
                    ..Default::default()
                });
                kyoku = kyokus.last_mut().unwrap();
                action_tables = &mut kyoku.action_tables;
                for (action_table, tehai) in action_tables.iter_mut().zip(tehais) {
                    action_table.haipai = tehai;
                }
            }
            Event::Tsumo { actor, pai } => {
                action_tables[actor as usize].takes.push(ActionItem::Tile(pai));
            }
            Event::Dahai { actor, pai, tsumogiri } => {
                let action = if tsumogiri {
                    ActionItem::Tsumogiri(60)
                } else {
                    ActionItem::Tile(pai)
                };
                action_tables[actor as usize].discards.push(action);
            }
            Event::Chi {
                actor, pai, consumed, ..
            } => {
                let naki = format!(
                    "{}c{}{}",
                    TenhouTile::from(pai) as u8,
                    TenhouTile::from(consumed[0]) as u8,
                    TenhouTile::from(consumed[1]) as u8
                );
                action_tables[actor as usize].takes.push(ActionItem::Naki(naki));
            }
            Event::Pon {
                actor,
                target,
                pai,
                consumed,
            } => {
                let naki = match (target + 4 - actor) % 4 {
                    3 => {
                        format!(
                            "p{}{}{}",
                            TenhouTile::from(pai) as u8,
                            TenhouTile::from(consumed[0]) as u8,
                            TenhouTile::from(consumed[1]) as u8
                        )
                    }
                    2 => {
                        format!(
                            "{}{}p{}",
                            TenhouTile::from(consumed[0]) as u8,
                            TenhouTile::from(pai) as u8,
                            TenhouTile::from(consumed[1]) as u8,
                        )
                    }
                    1 => {
                        format!(
                            "{}{}p{}",
                            TenhouTile::from(consumed[0]) as u8,
                            TenhouTile::from(consumed[1]) as u8,
                            TenhouTile::from(pai) as u8
                        )
                    }
                    _ => bail!("cannot pon self"),
                };
                action_tables[actor as usize].takes.push(ActionItem::Naki(naki));
            }
            Event::Daiminkan {
                actor,
                target,
                pai,
                consumed,
            } => {
                let rel = (target + 4 - actor) % 4;
                let naki = match rel {
                    3 => {
                        format!(
                            "m{}{}{}{}",
                            TenhouTile::from(pai) as u8,
                            TenhouTile::from(consumed[0]) as u8,
                            TenhouTile::from(consumed[1]) as u8,
                            TenhouTile::from(consumed[2]) as u8
                        )
                    }
                    2 => {
                        format!(
                            "{}m{}{}{}",
                            TenhouTile::from(consumed[0]) as u8,
                            TenhouTile::from(pai) as u8,
                            TenhouTile::from(consumed[1]) as u8,
                            TenhouTile::from(consumed[2]) as u8,
                        )
                    }
                    1 => {
                        format!(
                            "{}{}{}m{}",
                            TenhouTile::from(consumed[0]) as u8,
                            TenhouTile::from(consumed[1]) as u8,
                            TenhouTile::from(consumed[2]) as u8,
                            TenhouTile::from(pai) as u8
                        )
                    }
                    _ => bail!("cannot daiminkan self"),
                };
                action_tables[actor as usize].takes.push(ActionItem::Naki(naki));
            }
            Event::Kakan { actor, pai, consumed } => {
                // TODO: Do proper kan by searching previous events
                let rel: u8 = 3;

                let naki = match rel {
                    3 => {
                        format!(
                            "k{}{}{}{}",
                            TenhouTile::from(pai) as u8,
                            TenhouTile::from(consumed[0]) as u8,
                            TenhouTile::from(consumed[1]) as u8,
                            TenhouTile::from(consumed[2]) as u8
                        )
                    }
                    2 => {
                        format!(
                            "{}k{}{}{}",
                            TenhouTile::from(consumed[0]) as u8,
                            TenhouTile::from(pai) as u8,
                            TenhouTile::from(consumed[1]) as u8,
                            TenhouTile::from(consumed[2]) as u8,
                        )
                    }
                    1 => {
                        format!(
                            "{}{}k{}{}",
                            TenhouTile::from(consumed[0]) as u8,
                            TenhouTile::from(consumed[1]) as u8,
                            TenhouTile::from(pai) as u8,
                            TenhouTile::from(consumed[2]) as u8,
                        )
                    }
                    _ => bail!("cannot kakan into a self-pon"),
                };
                action_tables[actor as usize].discards.push(ActionItem::Naki(naki));
            }
            Event::Ankan { actor, consumed } => {
                let naki = format!(
                    "{}{}{}a{}",
                    TenhouTile::from(consumed[0]) as u8,
                    TenhouTile::from(consumed[1]) as u8,
                    TenhouTile::from(consumed[2]) as u8,
                    TenhouTile::from(consumed[3]) as u8
                );
                action_tables[actor as usize].discards.push(ActionItem::Naki(naki));
            }
            Event::Dora { dora_marker } => {
                kyoku.dora_indicators.push(dora_marker);
            }
            Event::Reach { actor } => {
                let pai = match event_iter.next() {
                    None => bail!("Reach without response"),
                    Some(Event::Dahai { pai, tsumogiri, .. }) => {
                        if *tsumogiri {
                            60
                        } else {
                            TenhouTile::from(*pai) as u8
                        }
                    }
                    Some(_) => bail!("Reach not followed by dahai"),
                };
                let naki = format!("r{pai}");
                action_tables[actor as usize].discards.push(ActionItem::Naki(naki));
            }
            Event::Hora {
                actor,
                target,
                deltas,
                ref ura_markers,
            } => {
                if let Some(ura_markers) = ura_markers {
                    kyoku.ura_indicators = ura_markers.clone();
                }
                let deltas = deltas.context("missing score deltas")?;
                let affected_players = (0..4).filter(|player| deltas[*player as usize] < 0).collect::<Vec<_>>();
                let detail = HoraDetail {
                    who: actor,
                    target,
                    score_deltas: deltas,
                    pao: if affected_players.len() == 3 {
                        None
                    } else {
                        affected_players.iter().find(|player| **player != target).copied()
                    },
                    ..Default::default()
                };
                match &mut kyoku.end_status {
                    EndStatus::Hora { details } => details.push(detail),
                    EndStatus::Ryukyoku { .. } => kyoku.end_status = EndStatus::Hora { details: vec![detail] },
                }
            }
            Event::Ryukyoku { deltas } => {
                kyoku.end_status = EndStatus::Ryukyoku {
                    score_deltas: deltas.unwrap_or_default(),
                }
            }
            _ => {}
        }
    }

    log.try_into()
}
