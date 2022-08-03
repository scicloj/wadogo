# Change Log

## [1.0.0-SNAPSHOT]

library revision and `clerk` notebook documentation

### Added

* `bands` - `:align`accepts a list of values or `:spread` keyword
* `defcustom` - custom scales based on `forward` and `inverse`
* `update-range` and `update-domain` helpers

### Changed

* `bands` scale returns a point by default (was: band info)
* numerical scales can accept data (extent is calculated then)
* `warn-on-reflection` removed
* `smile-mkl` dependency is removed to allow lighter uberjar

### Fixed

* inconsistency in `ticks` and `formatter` processing for some kinds (ordinal, continuous)

## [0.1.0-SNAPSHOT]

initial release
