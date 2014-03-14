(ns typedsamp
  (:require [clojure.core.typed :as typed])
  )

;; (use 'clojure.core.typed)
;; (import (core.typed ann))

(ann typed/add [Number Number -> Number])
(defn add [a b]
  (+ a b))

(println (add 1 3))

