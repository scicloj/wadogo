(ns usage
  (:require [notespace.api :as notespace]
            [notespace.kinds :as k]
            [wadogo.scale :as s]))

^k/hidden
(comment 
  (notespace/update-config #(assoc-in % [:by-ns *ns* :source-base-path] "notespace/")))


["# Wadogo scales"]

["Wadogo is the library which brings various transformations between domain and range, either continuous or discrete. It's based on [d3-scale](https://github.com/d3/d3-scale) and originally it was a part of [cljplot](https://github.com/generateme/cljplot) library.

You have an access to 13 different scales with unified api.

*The `wadogo` name came up after trying to translate word `scale` into different languages using [google translate](https://translate.google.com/?sl=en&tl=sw&text=scale&op=translate). Swahili word was very pleasant and was choosen on `zulip` by a community. Funny enough is that `wadogo` doesn't mean `scale` at all but `small` or `little`. It's just translate is wrong.*"] 

["## Common functions"]

["The main entry point is by requiring `wadogo.scale` namespace."]

(require '[wadogo.scale :as s])

["To illustrate functions we'll use the linear scale mapping `[0.0 1.0]` domain to `[-1.0 1.0]` range. To create any scale we use `scale` multimethod. Parameters are optional and there as some defaults for every scale kind."]

(def linear (s/scale :linear {:domain [0.0 1.0]
                              :range [-1.0 1.0]}))

linear

["### Fields"]

["Scale itself is a custom type, implementing `IFn` interface and containing following fields:

* `kind` - kind of the scale
* `domain` - domain of the transformation
* `range` - range of the transformation
* `data` - any data as a map, some scales store additional information there."]

(s/kind linear)
(s/domain linear)
(s/range linear)
(s/data linear)

["We can also read particular key from `data` field."]

(s/data linear :some-data)

["### Scaling"]

["To perform scaling, use a scale as a function or call `forward` function."]

(linear 0.5)
(s/forward linear 0.25)

["Inverse transformation is also possible"]

(s/inverse linear 0.5)

["### Size"]

["To get information about size of the domain or range (default), call `size`."]

(s/size linear :domain)
(s/size linear :range)

["Same as"]

(s/size linear)

["### Updating scale"]

["To change some of the fields while keeping the others you can call `with-` functions."]

(s/with-domain linear [-100.0 100.0])
(s/with-range linear [99.0 -99.0])
(s/with-data linear {:anything 11.0})
(s/with-data linear :anything 11.0)

["### Ticks"]

["*TODO*"]

["### Formatting"]

["*TODO*"]

["### Other"]

["List of all scales and type of their transformation is stored in `mapping` var."]

s/mapping

["## Numerical continuous -> continuous"]

["This group of scales transform continuous domain into continuous range.

We have here:

* linear
* logarithmic
* symmetric log
* exponential
* interpolated"]

["### Linear scale"]

["#### Default"]

["By default linear scale is an identity with arguments"]

wadogo.scale.linear/default-params

(s/scale :linear)

["#### Scaling"]

(def linear-scale (s/scale :linear {:domain [0.0 1.0]
                                    :range [-100.0 100.0]}))

linear-scale

["Forward"]

(->> [-1.0 0.0 0.2 0.5 0.8 1.0 2.0]
     (map linear-scale))

["Inverse"]

(->> [-200 -100 -50 0 50 100 200]
     (map (partial s/inverse linear-scale)))

["With descending range"]

(->> [-1.0 0.0 0.2 0.5 0.8 1.0 2.0]
     (map (s/with-range linear-scale [100.0 -100.0])))

["#### Ticks and formatting"]

["*TODO*"]

["### Log scale"]

["#### Default"]

wadogo.scale.log/default-params

(s/scale :log)

