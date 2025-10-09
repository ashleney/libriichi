modified libriichi stripped of pyo3 and with some features added.

## Features of libriichi (made by Equim)
- fast and accurate riichi mahjong simulation
- fast shanten and agari calculation using pre-calculated cached data
- single-player tables calculating ukeire and EV of possible discards

## Modifications

### Added
- log converter from tenhou to mjai ported from Equim's mjai-reviewer
  - log converter from mjai to tenhou
  - generation of mjai events from visible state
- very basic danger calculation ported from killerducky's mortal ui
- basic parsing of mortal's metadata field for further analysis

### Changed
- all functions are public if there's a way to use them for anything other than internals
- agari calculation will also return a list of yaku, with optional english and romaji localization
- single-player tables now have more customization and always return some results
  - the user can set up to what shanten can EV be calculated (if exceeded, falls back to EV of 0 and only calculates ukeire)
  - EV will be calculated for both discards and calls (though it does not calculate the extra dora for kan)
  - it is not reasonable to calculate the EV of 6-shanten hands, they will always only return ukeire

### Removed
- arena
- datasets
- log validation
- python agents

