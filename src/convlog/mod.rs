//! Provides methods to transform mahjong logs from tenhou.net/6 format into
//! mjai format.

mod conv;
mod kyoku_filter;

pub mod tenhou;

pub use conv::tenhou_to_mjai;
pub use kyoku_filter::KyokuFilter;
