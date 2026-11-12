# Oracle data generators

Scripts producing reference data for tests, written to `test/resources/wadogo/` (on the test classpath only, not packaged). Run from the repository root.

| Script | Oracle | Output |
|---|---|---|
| `utils/pretty/pretty_oracle.R` | R (>= 4.2) `pretty()`, `.pretty(bounds = FALSE)`, `pretty()` on UTC `POSIXct` | `pretty_linear.edn`, `pretty_datetime.edn` |
| `utils/d3/d3_oracle.mjs` | d3-scale 4.0.2 log ticks, linear `nice` | `d3_log_ticks.edn`, `d3_linear_nice.edn` |

```sh
Rscript utils/pretty/pretty_oracle.R
(cd utils/d3 && npm install) && node utils/d3/d3_oracle.mjs
clojure -M:test
```

Notes:

* Linear pretty ticks are the multiples `ns..nu` of `unit` inside the domain (as R axes, `GEPretty`), not `pretty()`'s `seq(lo, up, length.out)`, which keeps unrounded ends when the first or last multiple is within the rounding tolerance.
* d3 log ticks record the `:branch` d3 used: `:subdivisions` (all multipliers `1..base-1`) and `:powers` are compared with the corresponding candidate grids of wadogo `:pretty` log ticks; `:fallback` cases (d3 linear ticks) are not compared, wadogo uses R pretty linear ticks there.
