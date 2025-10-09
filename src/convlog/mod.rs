//! Provides methods to transform mahjong logs from tenhou.net/6 format into mjai format.
//! original: <https://github.com/Equim-chan/mjai-reviewer/tree/master/convlog>

pub mod generate;
pub mod mjai_tenhou;
pub mod tenhou;
pub mod tenhou_mjai;

pub use mjai_tenhou::mjai_to_tenhou;
pub use tenhou_mjai::tenhou_to_mjai;

#[cfg(test)]
mod test {
    use crate::convlog::{tenhou::Log, *};
    use anyhow::Result;

    #[test]
    fn test_conversion() -> Result<()> {
        let tenhou = r#"{
    "log": [ [ [ 0, 0, 0 ],
        [ 25000, 25000, 25000, 25000 ],
        [ 46 ],
        [ 14 ],
        [ 23, 41, 21, 31, 38, 13, 26, 37, 24, 11, 39, 19, 34 ],
        [ 29, 18, 14, 11, 17, 45, 12, 47, 26, 27 ],
        [ 31, 21, 11, 60, 34, 60, 41, 60, "r29", 60 ],
        [ 38, 16, 31, 28, 42, 13, 42, 42, 29, 31, 37, 35, 26 ],
        [ 28, 39, 33, 34, 15, 45, 43, 32, 46, 16 ],
        [ 29, 35, 16, 13, 60, 60, 60, "r26", 60, 60 ],
        [ 52, 36, 38, 32, 34, 26, 22, 41, 53, 33, 51, 31, 17 ],
        [ 22, 43, 44, 33, 47, 22, 33, 25, 27 ],
        [ 41, 60, 60, 38, 60, 33, 60, "r26", 60 ],
        [ 32, 36, 38, 35, 22, 46, 45, 37, 25, 23, 39, 14, 11 ],
        [ 44, 24, 28, 19, 16, 11, 24, 46, 21 ],
        [ 11, 46, 45, 60, 44, 60, 32, 60, 60 ],
        [ "和了", [ 0, -8000, 11000, 0 ],
        [ 2, 1, 2 ] ] ] ],
    "name": [ "Aさん", "Bさん", "Cさん", "Dさん" ],
    "rule": { "disp": "南", "aka": 0, "aka51": 1, "aka52": 1, "aka53": 1
    }
}"#;
        let mjai = tenhou_to_mjai(&Log::from_json_str(tenhou)?)?;
        let converted_tenhou = mjai_to_tenhou(&mjai)?.to_string_pretty()?;
        println!("{converted_tenhou}");
        assert_eq!(tenhou, converted_tenhou);

        Ok(())
    }
}
