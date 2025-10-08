use crate::tile::Tile;

use super::TenhouTile;

use regex::Regex;
use serde::{Deserialize, Serialize};
use serde_json::{Serializer, Value, ser::PrettyFormatter};
use serde_tuple::{Deserialize_tuple as DeserializeTuple, Serialize_tuple as SerializeTuple};
use serde_with::{FromInto, serde_as};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RawLog {
    #[serde(rename = "log")]
    pub logs: Vec<RawKyoku>,
    #[serde(rename = "name")]
    pub names: [String; 4],
    pub rule: Rule,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub ratingc: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lobby: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dan: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rate: Option<Vec<f64>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sx: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sc: Option<Vec<f32>>,
}

#[derive(Debug, Serialize)]
pub struct RawPartialLog<'a> {
    #[serde(flatten)]
    pub parent: &'a RawLog,

    #[serde(rename = "log")]
    pub logs: &'a [RawKyoku],
}

/// An item corresponding to each elements in "配牌", "取" and "出".
#[serde_as]
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ActionItem {
    Tile(#[serde_as(as = "FromInto<TenhouTile>")] Tile),
    Tsumogiri(u8), // must be 60
    Naki(String),
}

#[serde_as]
#[derive(Debug, Clone, SerializeTuple, DeserializeTuple)]
pub struct RawKyoku {
    pub meta: KyokuMeta,
    pub scoreboard: [i32; 4],
    #[serde_as(as = "Vec<FromInto<TenhouTile>>")]
    pub dora_indicators: Vec<Tile>,
    #[serde_as(as = "Vec<FromInto<TenhouTile>>")]
    pub ura_indicators: Vec<Tile>,

    #[serde_as(as = "[FromInto<TenhouTile>; 13]")]
    pub haipai_0: [Tile; 13],
    pub takes_0: Vec<ActionItem>,
    pub discards_0: Vec<ActionItem>,

    #[serde_as(as = "[FromInto<TenhouTile>; 13]")]
    pub haipai_1: [Tile; 13],
    pub takes_1: Vec<ActionItem>,
    pub discards_1: Vec<ActionItem>,

    #[serde_as(as = "[FromInto<TenhouTile>; 13]")]
    pub haipai_2: [Tile; 13],
    pub takes_2: Vec<ActionItem>,
    pub discards_2: Vec<ActionItem>,

    #[serde_as(as = "[FromInto<TenhouTile>; 13]")]
    pub haipai_3: [Tile; 13],
    pub takes_3: Vec<ActionItem>,
    pub discards_3: Vec<ActionItem>,

    pub results: Vec<ResultItem>,
}

#[derive(Debug, Clone, SerializeTuple, DeserializeTuple, Default)]
pub struct KyokuMeta {
    pub kyoku_num: u8,
    pub honba: u8,
    pub kyotaku: u8,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ResultItem {
    Status(String),
    ScoreDeltas([i32; 4]),
    HoraDetail(Vec<Value>),
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct Rule {
    pub disp: String,
    pub aka: u8,
    pub aka51: u8,
    pub aka52: u8,
    pub aka53: u8,
}

impl RawLog {
    #[must_use]
    pub const fn get_names(&self) -> &[String; 4] {
        &self.names
    }

    #[inline]
    pub fn hide_names(&mut self) {
        self.names.iter_mut().zip('A'..='D').for_each(|(name, alias)| {
            name.clear();
            name.push(alias);
            name.push_str("さん");
        });
    }

    /// Split one raw tenhou.net/6 log into many by kyokus.
    #[must_use]
    pub fn split_by_kyoku(&self) -> Vec<RawPartialLog<'_>> {
        let mut ret = vec![];

        for kyoku in self.logs.chunks(1) {
            let kyoku_log = RawPartialLog {
                parent: self,
                logs: kyoku,
            };

            ret.push(kyoku_log);
        }

        ret
    }

    #[inline]
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.logs.is_empty()
    }

    #[inline]
    #[must_use]
    pub const fn len(&self) -> usize {
        self.logs.len()
    }

    /// Serialize into a human-readable String.
    /// Keeps parity with the downloadlogs.js tampermonkey script
    pub fn to_string_pretty(&self) -> serde_json::Result<String> {
        let mut buf = Vec::new();
        let formatter = PrettyFormatter::with_indent(b"    "); // 4 spaces
        let mut serializer = Serializer::with_formatter(&mut buf, formatter);
        self.serialize(&mut serializer)?;
        let expanded = String::from_utf8(buf).expect("Valid UTF-8 JSON output");

        let patterns = [
            (r"\n\s{7,}", " "),
            (r"\], \[", "],\n        ["),
            (r"\n\s+]", " ]"),
            (r"\n\s+},\n", " },\n"),
        ];
        let human = patterns.iter().fold(expanded, |string, (pattern, replacement)| {
            Regex::new(pattern).unwrap().replace_all(&string, *replacement).into_owned()
        });
        Ok(human)
    }
}

impl From<RawPartialLog<'_>> for RawLog {
    fn from(partial_log: RawPartialLog<'_>) -> Self {
        Self {
            logs: partial_log.logs.to_vec(),
            ..partial_log.parent.clone()
        }
    }
}
