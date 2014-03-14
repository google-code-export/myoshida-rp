(ns typedsamp.core
  (:require [clojure.core.typed :refer [ann]]
            )
  (:gen-class)
  )


(ann add [Number Number -> Number])
(defn add [a b]
  (+ a b))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (add 1 "3"))
  )
