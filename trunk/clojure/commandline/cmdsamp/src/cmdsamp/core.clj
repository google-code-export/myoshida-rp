(ns cmdsamp.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            (:gen-class))
  (:import (java.io PrintWriter InputStreamReader))
  )
(use 'clojure.java.io)


(def program-name "cmdsamp")
(def program-version "0.0.1")
  
;; オプション仕様定義
(def option-spec
  [["-h" "--help" "Show help."]
   ["-v" "--version" "Show program version."]
   [nil "--verbose" "Output log verbosity."]
   ["-o" "--output FILE" "Output file path (Default: \"a.out\")"
    :default "a.out"
    ]
   ])

(defn print-version []
  (println-str program-name " Ver. " program-version))


(defn print-usage [options-summary]
  (println-str
   "Usage: " program-name " [Options] FILE [...]\n\n"
   "FILE: Input file path.\n\n"
   "Options:\n"
   options-summary))

(defn print-err-msg [errors]
  (let [errmsgs (if (vector? errmsgs) errors [errors])]
    (.println *err* (string/join
                     \newline (map #(str program-name ":Error:" %1) errmsgs)))
    ))


(defn exit [status msg]
  (.println (if (= status 0) System/out *err*)  msg)
  (System/exit status))



(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args option-spec)]
    ;; 中断処理
    (cond
     ;; -h
     (:help options)
     (do (print-usage summary) (System/exit 0))
     ;; -v
     (:version options)
     (do (print-version) (System/exit 0))
     ;; 解析時のエラー
     errors
     (do (print-err-msg errors) (System/exit 1))
     ;; 引数が 0 個
     (< (count arguments) 1)
     (do (print-err-msg "FILE isn't specified.")
         (print-usage summary)
         (System/exit 1))
     )
    ;; アプリの処理
    (println-str "Options   : " options)
    (println-str "Arguments : " arguments)
    ))
