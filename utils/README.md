# Oracle data generators

Scripts producing reference data for tests, written to `test/resources/wadogo/` (on the test classpath only, not packaged). Run from the repository root.

| Script | Oracle | Output |
|---|---|---|
| `utils/pretty/pretty_oracle.R` | R (>= 4.2) `pretty()`, `.pretty(bounds = FALSE)`, `pretty()` on UTC `POSIXct` | `pretty_linear.edn`, `pretty_datetime.edn` |
| `utils/d3/d3_oracle.mjs` | d3-scale 4.0.2 log ticks, linear `nice` | `d3_log_ticks.edn`, `d3_linear_nice.edn` |
| `utils/d3/d3_scales.mjs` | d3-scale 4.0.2 forward/inverse of linear, identity, log, pow, symlog, radial, sequential*, diverging*, quantize, quantile, threshold, band, point, ordinal, utc (with clamp, unknown, round) | `d3_scales.edn` |
| `utils/quantile/quantile_oracle.R` | R `quantile(type = 1..9)` | `r_quantiles.edn` |

```sh
Rscript utils/pretty/pretty_oracle.R
(cd utils/d3 && npm install) && node utils/d3/d3_oracle.mjs && node utils/d3/d3_scales.mjs
Rscript utils/quantile/quantile_oracle.R
clojure -M:test
```

Notes:

* Linear pretty ticks are the multiples `ns..nu` of `unit` inside the domain (as R axes, `GEPretty`), not `pretty()`'s `seq(lo, up, length.out)`, which keeps unrounded ends when the first or last multiple is within the rounding tolerance.
* d3 log ticks record the `:branch` d3 used: `:subdivisions` (all multipliers `1..base-1`) and `:powers` are compared with the corresponding candidate grids of wadogo `:pretty` log ticks; `:fallback` cases (d3 linear ticks) are not compared, wadogo uses R pretty linear ticks there.
* d3 scale cases: d3 diverging scales map `null` as `0` (no null check), wadogo returns `:unknown`, so `null` isn't generated for them. Quantile values equal to a d3 quantile are compared only when wadogo's quantile is the same double (fastmath interpolates in a different order, ~1e-15 relative difference).
* R type 2 quantiles: R treats `n*p` within 4 eps of an integer as an integer, Apache Commons Math (fastmath) doesn't; such cases are skipped in the test.
