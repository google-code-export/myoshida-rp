(ns cmdprs.core
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:require [clojure.pprint :refer [pprint]])
  (:gen-class))


;; オプション仕様定義
(def option-spec
  [["-h" "--help" "Show help."]
   ["-v" "--version" "Show program version."]
   [nil "--verbose" "Output log verbosity."]
   ;; 引数付き
   ["-o" "--output FILE" "Output file path (Default: \"a.out\")"
    :default "a.out"
    ]
   ;; assoc-fn 指定
   ["-e" "--expr PATTERN" "Regular expression pattern"
    :assoc-fn
    (fn [m k v]
      ;; (println (str "m=" m ", k=" k ", v=" v))
      ;; (assoc m k v)
      (assoc m k
             (let [vec (get m k)]
               (if vec (conj vec v) [v])))
      )
    ]
   ])

(defn -main
  "Command line option parse sample program."
  [& args]
  ;; 引数を解析 (parse-opts 引数リスト オプション仕様)
  (let [info (parse-opts args option-spec)]
    ;; 解析結果を使用
    (pprint info))
  )
