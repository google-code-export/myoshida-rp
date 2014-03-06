(ns simpcat.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            (:gen-class))
  (:import (java.io PrintWriter InputStreamReader))
  )
(use 'clojure.java.io)


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
  (string/join
   \newline (map #(str program-name ":Error:" %1) errs)))


(defn exit [status msg]
  (.println (if (= status 0) System/out *err*)  msg)
  (System/exit status))


(defn catfile [fin fout]
  (doseq [str (line-seq fin)]
    (.println fout str)))  

(defn cat-files [fpaths out]
  (if (empty? fpaths)
    (catfile (InputStreamReader. System/in) out)
    (doseq [fpath fpaths]
      (with-open [fin (reader fpath)]
        (catfile fin out)
        )))
  )

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
