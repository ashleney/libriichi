use crate::{convlog::KyokuFilter, tile::Tile};

use super::json_scheme::{ActionItem, KyokuMeta, RawLog, ResultItem};

use anyhow::{Context, Error, Result, bail};
use serde::Serialize;
use serde_json::{self as json};

/// The overview structure of log in tenhou.net/6 format.
#[derive(Debug, Clone)]
pub struct Log {
    pub names: [String; 4],
    pub game_length: GameLength,
    pub has_aka: bool,
    pub kyokus: Vec<Kyoku>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum GameLength {
    Hanchan = 0,
    Tonpuu = 4,
}

/// Contains information about a kyoku.
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
pub struct ActionTable {
    pub haipai: [Tile; 13],
    pub takes: Vec<ActionItem>,
    pub discards: Vec<ActionItem>,
}

impl Log {
    /// Parse a tenhou.net/6 log from JSON string.
    #[inline]
    pub fn from_json_str(json_string: &str) -> Result<Self> {
        let raw_log: RawLog = json::from_str(json_string)?;
        Self::try_from(raw_log)
    }

    #[inline]
    pub fn filter_kyokus(&mut self, kyoku_filter: &KyokuFilter) {
        self.kyokus.retain(|l| kyoku_filter.test(l.meta.kyoku_num, l.meta.honba));
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
                end_status: EndStatus::Ryukyoku {
                    score_deltas: [0; 4], // default
                },
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
                            let mut hora_detail_iter = hora_detail_array.into_iter();
                            let who = hora_detail_iter.next().context("invalid hora detail")?.as_u64().unwrap_or(0) as u8;
                            let target = hora_detail_iter.next().context("invalid hora detail")?.as_u64().unwrap_or(0) as u8;
                            let pao = hora_detail_iter.next().context("invalid hora detail")?.as_u64().unwrap_or(0) as u8;
                            let pao = if pao == who { None } else { Some(pao) };
                            let yaku = hora_detail_iter
                                .skip(1)
                                .map(|value| Ok(value.as_str().context("invalid hora detail")?.to_string()))
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
