//! Port of killerducky's tile danger calculation.
//! original: <https://github.com/killerducky/killer_mortal_gui#dealin-rate>
use tinyvec::{ArrayVec, array_vec};

use crate::{must_tile, state::item::KawaItem, tile::Tile};

/// Simple kinds of waits used for danger calculation.
#[derive(Debug, Clone, Copy)]
pub enum WaitShape {
    Ryanmen,
    Kanchan,
    Penchan,
    Tanki,
    Shanpon,
}

/// Boardstate-agnostic wait type
#[derive(Debug, Clone)]
pub struct GeneralWait {
    pub tiles: ArrayVec<[u8; 2]>,
    pub waits: ArrayVec<[u8; 2]>,
    pub shape: WaitShape,
}

/// A specific wait that a player might have and all its flags.
#[derive(Clone, Debug)]
#[allow(dead_code)]
pub struct Wait {
    /// The shape of this wait
    pub kind: GeneralWait,
    /// Whether the wait is guaranteed safe under furiten rules
    pub genbutsu: bool,
    /// Amount of tile combinations that would make this wait possible
    /// Essentially the raw chance without weights applied
    pub combinations: u8,
    /// Whether the player discarded a 456 tile that's 2 away from a waited tile
    pub ura_suji: bool,
    /// Whether the player discarded a tile that's part of the wait shape
    pub matagi_suji_early: bool,
    /// Whether the player discarded a tile that's part of the wait shape as their riichi tile
    pub matagi_suji_riichi: bool,
    /// Whether the player discarded a 456 tile that's 3 away from a waited tile as their riichi tile
    pub riichi_suji_trap: bool,
    /// Whether any of the tiles that would complete this shape are dora
    pub dora_involved: bool,
    /// The chance of this being a player's wait after applying weights
    pub weight: f32,
}

impl Wait {
    /// The weight specifically for this wait
    /// Doubles the weight of shanpon.
    #[allow(dead_code)]
    pub fn individual_weight(&self) -> f32 {
        if matches!(self.kind.shape, WaitShape::Shanpon) {
            self.weight * 2.0
        } else {
            self.weight
        }
    }
}

/// The danger weights for a specific player for each tile
#[derive(Clone, Debug)]
pub struct PlayerDanger {
    pub tile_weights: [f32; 34],
    pub waits: Vec<Wait>,
}

impl PlayerDanger {
    /// Tiles sorted from most to least dangerous
    pub fn sorted_tile_weights(&self) -> Vec<(Tile, f32)> {
        let mut tile_weights = self
            .tile_weights
            .iter()
            .enumerate()
            .map(|(tile, weight)| (must_tile!(tile), *weight))
            .collect::<Vec<_>>();
        tile_weights.sort_unstable_by(|(_, a), (_, b)| b.partial_cmp(a).unwrap());
        tile_weights
    }

    /// Waits that this tile is involved in
    pub fn tile_waits(&self, tile: u8) -> Vec<Wait> {
        self.waits
            .iter()
            .filter(|wait| wait.kind.waits.contains(&tile))
            .cloned()
            .collect::<Vec<_>>()
    }
}

pub static POSSIBLE_WAITS: std::sync::LazyLock<Vec<GeneralWait>> = std::sync::LazyLock::new(|| {
    let mut waits_array: Vec<GeneralWait> = Vec::new();

    for suit in 0..3 {
        for number in 1..7 {
            waits_array.push(GeneralWait {
                tiles: array_vec![suit * 9 + number, suit * 9 + number + 1],
                waits: array_vec![suit * 9 + number - 1, suit * 9 + number + 2],
                shape: WaitShape::Ryanmen,
            });
        }
    }
    for suit in 0..3 {
        for number in 1..8 {
            waits_array.push(GeneralWait {
                tiles: array_vec![suit * 9 + number - 1, suit * 9 + number + 1],
                waits: array_vec![suit * 9 + number],
                shape: WaitShape::Kanchan,
            });
        }
    }

    for suit in 0..3 {
        waits_array.push(GeneralWait {
            tiles: array_vec![suit * 9, suit * 9 + 1],
            waits: array_vec![suit * 9 + 2],
            shape: WaitShape::Penchan,
        });
        waits_array.push(GeneralWait {
            tiles: array_vec![suit * 9 + 7, suit * 9 + 8],
            waits: array_vec![suit * 9 + 6],
            shape: WaitShape::Penchan,
        });
    }

    for suit in 0..=3 {
        for number in 0..9 {
            if suit == 3 && number > 6 {
                continue;
            }
            waits_array.push(GeneralWait {
                tiles: array_vec![suit * 9 + number],
                waits: array_vec![suit * 9 + number],
                shape: WaitShape::Shanpon,
            });
            waits_array.push(GeneralWait {
                tiles: array_vec![suit * 9 + number],
                waits: array_vec![suit * 9 + number],
                shape: WaitShape::Tanki,
            });
        }
    }

    waits_array
});

/// Calculate the chances of a player having specific waits
pub fn calculate_player_danger<const DETAILED: bool>(
    safe_tiles: [bool; 34],
    discards_before_riichi: Vec<u8>,
    riichi_tile: Option<u8>,
    unseen_tiles: [u8; 34],
    doras: Vec<u8>,
) -> PlayerDanger {
    let mut waits = if DETAILED { Some(Vec::new()) } else { None };
    let mut tile_weights = [0.0; 34];

    for wait in POSSIBLE_WAITS.iter() {
        let genbutsu = wait.waits.iter().any(|&tile| safe_tiles[tile as usize]);
        if !DETAILED {
            continue;
        }
        let combinations = if matches!(wait.shape, WaitShape::Shanpon) {
            (unseen_tiles[wait.tiles[0] as usize] * unseen_tiles[wait.tiles[0] as usize].saturating_sub(1)) / 2
        } else {
            wait.tiles.iter().map(|&tile| unseen_tiles[tile as usize]).product()
        };

        let mut ura_suji = false;
        let mut matagi_suji_early = false;
        let mut matagi_suji_riichi = false;
        if matches!(wait.shape, WaitShape::Ryanmen) {
            for discarded_tile in &discards_before_riichi {
                if !matches!(discarded_tile % 9, 3..6) {
                    continue;
                }
                if wait.tiles.contains(discarded_tile) {
                    continue;
                }
                for &wait_tile in &wait.tiles {
                    if discarded_tile.abs_diff(wait_tile) == 2 {
                        ura_suji = true;
                        break;
                    }
                }
            }
            for discarded_tile in &discards_before_riichi {
                if wait.tiles.contains(discarded_tile) {
                    matagi_suji_early = true;
                    break;
                }
            }
            if let Some(riichi_tile) = riichi_tile
                && wait.tiles.contains(&riichi_tile)
            {
                matagi_suji_riichi = true;
            }
        }
        let riichi_suji_trap = matches!(wait.shape, WaitShape::Kanchan)
            && riichi_tile.is_some_and(|riichi_tile| {
                matches!(riichi_tile % 9, 3..6) && wait.waits.iter().any(|wait_tile| riichi_tile.abs_diff(*wait_tile) == 3)
            });
        let dora_involved = wait
            .tiles
            .iter()
            .chain(wait.waits.iter())
            .any(|involved_tile| doras.contains(involved_tile));

        let weight = if genbutsu {
            0.0
        } else {
            let mut weight = combinations as f32;
            weight *= match wait.shape {
                WaitShape::Ryanmen => 3.5,
                WaitShape::Tanki | WaitShape::Shanpon if wait.tiles[0] >= 27 => 1.7,
                WaitShape::Tanki | WaitShape::Shanpon | WaitShape::Penchan => 1.0,
                WaitShape::Kanchan if riichi_suji_trap => 2.6,
                WaitShape::Kanchan => 0.21,
            };
            if ura_suji {
                weight *= 1.3;
            }
            if matagi_suji_early {
                weight *= 0.6;
            }
            if matagi_suji_riichi {
                weight *= 1.2;
            }
            if dora_involved {
                weight *= 1.2;
            }
            weight
        };

        for &wait_tile in &wait.waits {
            tile_weights[wait_tile as usize] += weight;
        }

        if DETAILED {
            waits.as_mut().unwrap().push(Wait {
                kind: wait.clone(),
                genbutsu,
                combinations,
                ura_suji,
                matagi_suji_early,
                matagi_suji_riichi,
                riichi_suji_trap,
                dora_involved,
                weight,
            });
        }
    }

    PlayerDanger {
        tile_weights,
        waits: waits.unwrap(),
    }
}

/// Determine genbutsu tiles based on furiten rules
/// assume that if a player tsumogiris then temporary safe tiles remain
pub fn determine_safe_tiles(kawa: &[tinyvec::TinyVec<[Option<KawaItem>; 24]>; 4]) -> [[bool; 34]; 4] {
    let mut safe_tiles = [[false; 34]; 4]; // furiten
    let mut temporary_safe_tiles = [[false; 34]; 4]; // temporary furiten, riichi furiten, or implied no wait change

    for turn in 0..(kawa.iter().map(|x| x.len()).max().unwrap_or_default()) {
        for kawa_actor in 0..=3 {
            if let Some(item) = kawa[kawa_actor].get(turn).cloned().flatten() {
                let tile = item.sutehai.tile.deaka();
                for player_temporary_safe_tiles in &mut temporary_safe_tiles {
                    player_temporary_safe_tiles[tile.as_usize()] = true;
                }
                safe_tiles[kawa_actor][tile.as_usize()] = true;
                if item.sutehai.is_tedashi {
                    temporary_safe_tiles[kawa_actor] = [false; 34];
                }
            }
        }
    }

    for (player, tiles) in temporary_safe_tiles.iter().enumerate() {
        for (tile, &is_safe) in tiles.iter().enumerate() {
            if is_safe {
                safe_tiles[player][tile] = true;
            }
        }
    }

    safe_tiles
}

/// Calculate the danger of any player having a specific wait
pub fn calculate_board_danger<const DETAILED: bool>(
    tiles_seen: &[u8; 34],
    kawa: &[tinyvec::TinyVec<[Option<KawaItem>; 24]>; 4],
    dora_indicators: &[Tile],
) -> [PlayerDanger; 4] {
    let unseen_tiles = tiles_seen.map(|x| 4 - x);
    determine_safe_tiles(kawa)
        .iter()
        .enumerate()
        .map(|(player, &safe_tiles)| {
            let discards_before_riichi = kawa[player]
                .iter()
                .filter_map(|item| item.as_ref().map(|item| item.sutehai))
                .take_while(|item| !item.is_riichi)
                .map(|x| x.tile.as_u8())
                .collect::<Vec<_>>();
            let riichi_tile = kawa[player]
                .iter()
                .filter_map(|item| item.as_ref().map(|item| item.sutehai))
                .find(|item| item.is_riichi)
                .map(|x| x.tile.as_u8());
            calculate_player_danger::<DETAILED>(
                safe_tiles,
                discards_before_riichi,
                riichi_tile,
                unseen_tiles,
                dora_indicators.iter().map(|x| x.next().as_u8()).collect::<Vec<_>>(),
            )
        })
        .collect::<Vec<_>>()
        .try_into()
        .unwrap()
}
