(ns cmdprs.core
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:require [clojure.pprint :refer [pprint]])
  (:gen-class))


;; オプション仕様定義
(def option-spec
  ;; ショート形式 ロング形式 [オプション説明] [追加設定(キーと値) ...]
  [["-h" "--help" "Show help."]
   ;; デフォルト値を設定
   ["-v" "--version" "Show program version." :default false]
   ;; ショート形式を省略するときは nil
   [nil "--verbose" "Output log verbosity."]

   ;; 引数を取るオプション
   ["-o" "--output FILE" "Output file path"
    :default "a.out"
    ]
   ;; assoc-fn 指定
   ["-e" "--expr PATTERN" "Regular expression pattern"
    :assoc-fn
    (fn [m k v]
      ;(println (str "m=" m ", k=" k ", v=" v))
      (assoc m k
             (let [vec (get m k)]
               (if vec (conj vec v) [v])))
      )
    ]
   ;; A boolean option defaulting to nil
   ])


(defn -main
  "Command line option parse sample program."
  [& args]
  (pprint
   ;; (parse-opts 引数リスト オプション仕様
   (parse-opts args option-spec)))
