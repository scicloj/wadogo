(ns wadogo.protocols
  (:refer-clojure :exclude [range]))

(defprotocol ScaleSettersProto
  (set-domain [s d] "Assign new domain")
  (set-range [s r] "Assign new range")
  (set-data [s info-key value] "Set attribute"))

(defprotocol ScaleGettersProto
  (domain [s])
  (range [s])
  (data [s]))

(defprotocol ScaleProto
  (forward [s v])
  (inverse [s v]))
