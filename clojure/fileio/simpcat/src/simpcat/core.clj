(ns simpcat.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            (:gen-class)))

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
  (.println (if (= status 0) *out* *err*)  msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args option-spec)]
    ;; Handle help and error conditions
    (cond
     (:help options) (exit 0 (usage summary))
     (:version options) (exit 0 version-msg)
     errors (exit 1 (errs-msg errors)))
    (pprint options)
    (pprint arguments)
    ))
