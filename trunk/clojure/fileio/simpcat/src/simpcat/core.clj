(ns simpcat.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :refer [join]]
            (:gen-class))
  (:use [clojure.java.io])
  (:import (java.io PrintWriter InputStreamReader))
  )


(def program-name "simpcat")
(def program-version "0.0.1")
  
;; オプション仕様定義
(def option-spec
  [["-h" "--help" "Show help."]
   ["-v" "--version" "Show program version."]
   ["-o" "--output FILE" "Output file path (Default: standard output)"]
   ])

(defn version-msg []
  (str program-name " Ver. " program-version))


(defn usage [options-summary]
  (str
   "Usage: " program-name " [Options] [FILE ...]\n\n"
   "FILE: Input file path.\n\n"
   "Options:\n"
   options-summary))

(defn errs-msg [errs]
  (join
   \newline (map #(str program-name ":Error:" %1) errs)))


(defn exit [status msg]
  (.println (if (= status 0) System/out *err*)  msg)
  (System/exit status))


(defn print-file [fin fout]
  (doseq [str (line-seq fin)]
    (.println fout str)))  

(defn cat-files [fpaths fout]
  (if-not (empty? fpaths)
    (doseq [fpath fpaths]
      (with-open [fin (reader fpath)]
        (print-file fin fout)
        ))
    (print-file (reader System/in) fout)
    ))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args option-spec)]
    ;; 中断処理
    (cond
     (:help options) (exit 0 (usage summary))
     (:version options) (exit 0 version-msg)
     errors (exit 1 (errs-msg errors)))
    (if (:output options)
      (with-open [fout (PrintWriter. (writer (:output options)))]
        (cat-files arguments fout))
      (cat-files arguments System/out))
    ))
