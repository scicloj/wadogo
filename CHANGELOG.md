# Change Log

## [1.1.0-alpha1]

Update to Fastmath 3.0.0-alpha

### Fixed

* interpolated scale updated to new Fastmath API

## [1.0.1-SNAPSHOT]

### Added

* `scale?` predicate to check if given object is scale or not

## [1.0.0]

Library revision and `clerk` notebook documentation

### Added

* `bands` - `:align`accepts a list of values or `:spread` keyword
* `defcustom` - custom scales based on `forward` and `inverse`
* `update-range` and `update-domain` helpers

### Changed

* `bands` scale returns a point by default (was: band info)
* numerical scales can accept data (extent is calculated then)
* `warn-on-reflection` removed
* `smile-mkl` dependency is removed to allow lighter uberjar
* `ordinal` scale sorts (an option) and makes domain distinct by default, comparator can be provided

### Fixed

* inconsistency in `ticks` and `formatter` processing for some kinds (ordinal, continuous)

## [0.1.0-SNAPSHOT]

initial release
