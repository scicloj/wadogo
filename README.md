# wadogo

Scales for Clojure. Domain -> range transformations of various types. Derived from [`d3-scale`](https://github.com/d3/d3-scale), originally implemented for [`cljplot`](https://github.com/generateme/cljplot)

## Name

`wadogo` supposed to mean `scale` in Swahili according to [google translate](https://translate.google.com/?sl=en&tl=sw&text=scale&op=translate). Quickly appeared that this is not true. But, `wadogo` is cute and means `little`.

## Scale types

### continuous -> continuous

* linear
* logarthmic
* symmetrical log ([read here](https://www.researchgate.net/profile/John_Webber4/publication/233967063_A_bi-symmetric_log_transformation_for_wide-range_data/links/0fcfd50d791c85082e000000.pdf))
* exponential (pow)
* interpolated
* date/time

### continuous -> discrete

* quantiles
* histogram (under development)
* threshold (under development)
* quantize (under development)

### discrete -> continuous

* bands

### discrete -> discrete

* ordinal

## Ticks and formatting

*soon*

## Usage

*soon*

### Examples

*soon*

## License

Copyright © 2021 Scicloj / GenerateMe

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
