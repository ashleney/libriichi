use crate::{
    convlog::tenhou::json_scheme::{RawKyoku, Rule},
    tile::Tile,
};

use super::json_scheme::{ActionItem, KyokuMeta, RawLog, ResultItem};

use anyhow::{Context, Error, Result, bail};
use serde::Serialize;
use serde_json;

/// The overview structure of log in tenhou.net/6 format.
#[derive(Debug, Clone, Default)]
pub struct Log {
    pub names: [String; 4],
    pub game_length: GameLength,
    pub has_aka: bool,
    pub kyokus: Vec<Kyoku>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Default)]
pub enum GameLength {
    #[default]
    Hanchan = 0,
    Tonpuu = 4,
}

/// Contains information about a kyoku.
#[derive(Debug, Clone, Default)]
pub struct Kyoku {
    pub meta: KyokuMeta,
    pub scoreboard: [i32; 4],
    pub dora_indicators: Vec<Tile>,
    pub ura_indicators: Vec<Tile>,
    pub action_tables: [ActionTable; 4],
    pub end_status: EndStatus,
}

#[derive(Debug, Clone)]
pub enum EndStatus {
    Hora { details: Vec<HoraDetail> },
    Ryukyoku { score_deltas: [i32; 4] },
}

#[derive(Debug, Clone, Default)]
pub struct HoraDetail {
    pub who: u8,
    pub target: u8,
    pub score_deltas: [i32; 4],
    pub pao: Option<u8>,
    pub yaku: Vec<String>,
}

/// A group of "配牌", "取" and "出", describing a player's
/// gaming status and actions throughout a kyoku.
#[derive(Debug, Clone, Default)]
pub struct ActionTable {
    pub haipai: [Tile; 13],
    pub takes: Vec<ActionItem>,
    pub discards: Vec<ActionItem>,
}

impl Log {
    /// Parse a tenhou.net/6 log from JSON string.
    #[inline]
    pub fn from_json_str(json_string: &str) -> Result<Self> {
        let raw_log: RawLog = serde_json::from_str(json_string)?;
        Self::try_from(raw_log)
    }
}

impl TryFrom<RawLog> for Log {
    type Error = Error;

    fn try_from(raw_log: RawLog) -> Result<Self, Self::Error> {
        let RawLog { logs, names, rule, .. } = raw_log;

        if rule.disp.contains('三') || rule.disp.contains("3-Player") {
            bail!("sanma log");
        }
        let game_length = if rule.disp.contains('東') || rule.disp.contains("East") {
            GameLength::Tonpuu
        } else {
            GameLength::Hanchan
        };
        let has_aka = rule.aka + rule.aka51 + rule.aka52 + rule.aka53 > 0;

        let mut kyokus = Vec::with_capacity(logs.len());
        for log in logs {
            let mut kyoku = Kyoku {
                meta: log.meta,
                scoreboard: log.scoreboard,
                dora_indicators: log.dora_indicators,
                ura_indicators: log.ura_indicators,
                action_tables: [
                    ActionTable {
                        haipai: log.haipai_0,
                        takes: log.takes_0,
                        discards: log.discards_0,
                    },
                    ActionTable {
                        haipai: log.haipai_1,
                        takes: log.takes_1,
                        discards: log.discards_1,
                    },
                    ActionTable {
                        haipai: log.haipai_2,
                        takes: log.takes_2,
                        discards: log.discards_2,
                    },
                    ActionTable {
                        haipai: log.haipai_3,
                        takes: log.takes_3,
                        discards: log.discards_3,
                    },
                ],
                end_status: EndStatus::default(),
            };

            if let Some(ResultItem::Status(status_text)) = log.results.first() {
                if status_text == "和了" {
                    let mut details = vec![];
                    for detail_tuple in log.results[1..].chunks_exact(2) {
                        if let [
                            ResultItem::ScoreDeltas(score_deltas),
                            ResultItem::HoraDetail(hora_detail_array),
                        ] = detail_tuple
                        {
                            let mut hora_detail_iter = hora_detail_array.iter();
                            let who = hora_detail_iter.next().context("invalid hora detail")?.as_u64().unwrap_or(0) as u8;
                            let target = hora_detail_iter.next().context("invalid hora detail")?.as_u64().unwrap_or(0) as u8;
                            let pao = hora_detail_iter.next().context("invalid hora detail")?.as_u64().unwrap_or(0) as u8;
                            let pao = if pao == who { None } else { Some(pao) };
                            let yaku = hora_detail_iter
                                .skip(1)
                                .map(|value| Ok(value.as_str().context("invalid hora detail")?.to_owned()))
                                .collect::<Result<Vec<_>>>()?;
                            let hora_detail = HoraDetail {
                                score_deltas: *score_deltas,
                                who,
                                target,
                                pao,
                                yaku,
                            };
                            details.push(hora_detail);
                        }
                    }
                    kyoku.end_status = EndStatus::Hora { details };
                } else {
                    let score_deltas = if let Some(ResultItem::ScoreDeltas(dts)) = log.results.get(1) {
                        *dts
                    } else {
                        [0; 4]
                    };
                    kyoku.end_status = EndStatus::Ryukyoku { score_deltas };
                }
            }

            kyokus.push(kyoku);
        }

        Ok(Self {
            names,
            game_length,
            has_aka,
            kyokus,
        })
    }
}

impl TryFrom<Log> for RawLog {
    type Error = Error;

    fn try_from(log: Log) -> Result<Self, Self::Error> {
        let Log {
            names,
            game_length,
            has_aka,
            kyokus,
        } = log;

        let disp = if matches!(game_length, GameLength::Hanchan) {
            "南"
        } else {
            "東"
        };
        let raw_kyokus = kyokus
            .into_iter()
            .map(|kyoku| {
                let results = match kyoku.end_status {
                    EndStatus::Hora { details } => std::iter::once(ResultItem::Status("和了".to_owned()))
                        .chain(details.into_iter().flat_map(|detail| {
                            [
                                ResultItem::ScoreDeltas(detail.score_deltas),
                                ResultItem::HoraDetail(vec![
                                    detail.who.into(),
                                    detail.target.into(),
                                    detail.pao.unwrap_or(detail.who).into(),
                                ]),
                            ]
                        }))
                        .collect::<Vec<_>>(),
                    EndStatus::Ryukyoku { score_deltas } => vec![
                        ResultItem::Status("Ryuukyoku".to_owned()),
                        ResultItem::ScoreDeltas(score_deltas),
                    ],
                };
                let [a, b, c, d] = kyoku.action_tables;
                RawKyoku {
                    meta: kyoku.meta,
                    scoreboard: kyoku.scoreboard,
                    dora_indicators: kyoku.dora_indicators,
                    ura_indicators: kyoku.ura_indicators,
                    haipai_0: a.haipai,
                    takes_0: a.takes,
                    discards_0: a.discards,
                    haipai_1: b.haipai,
                    takes_1: b.takes,
                    discards_1: b.discards,
                    haipai_2: c.haipai,
                    takes_2: c.takes,
                    discards_2: c.discards,
                    haipai_3: d.haipai,
                    takes_3: d.takes,
                    discards_3: d.discards,
                    results,
                }
            })
            .collect();
        Ok(Self {
            logs: raw_kyokus,
            names,
            rule: Rule {
                disp: disp.to_owned(),
                aka51: has_aka as u8,
                aka52: has_aka as u8,
                aka53: has_aka as u8,
                aka: 0,
            },
            ratingc: None,
            lobby: None,
            dan: None,
            rate: None,
            sx: None,
            sc: None,
        })
    }
}

impl Default for EndStatus {
    fn default() -> Self {
        Self::Ryukyoku { score_deltas: [0; 4] }
    }
}
