(ns cmdprs.core
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:require [clojure.pprint :refer [pprint]])
  (:gen-class))

(def cli-options
  ;; An option with a required argument
  [["-h" "--help" "Show help."]
   ["-v" "--version" "Show program version."]
   ["-p" "--port PORT" "Port number"
    :default 80
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ;; A non-idempotent option
   ["-V" nil "Verbosity level"
    :id :verbosity
    :default 0
    :assoc-fn (fn [m k _] (update-in m [k] inc))]
   ["-e" "--expr PATTERN" "Regular expression pattern"
    :assoc-fn
    (fn [m k v]
      ;(println (str "k=" m ", k=" k ", v=" v))
      (assoc m k
             (if (contains? m k)
               (conj (get m k) v)
               [v])))
    ]
   ;; A boolean option defaulting to nil
   ["-o" "--output FILE" "Output file path"]
   ])

(defn -main
  "Command line option parse sample program."
  [& args]
  (pprint (parse-opts args cli-options)))
