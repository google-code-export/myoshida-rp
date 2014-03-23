(ns cmdsamp.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :refer [join]]
            (:gen-class)))


(def program-name "cmdsamp")
(def program-version "0.0.1")
  
;; �I�v�V�����d�l��`
(def option-spec
  [["-h" "--help" "Show help."]
   ["-v" "--version" "Show program version."]
   [nil  "--verbose" "Output log verbosity."]
   ["-o" "--output OUT_FILE" "Output file path (Default: \"a.out\")"
    :default "a.out"
    ]
   ])

(defn print-version []
  (println program-name "Ver." program-version))

(defn print-usage [options-summary]
  (println
   "Usage:" program-name "[Options] FILE [...]\n"
   "FILE: Input file path.\n"
   (str "Options:\n" options-summary)))

(defn print-err-msg [errors]
  (let [errmsgs (if (vector? errors) errors [errors])]
    (.println *err* (join \newline
                          (map #(str program-name ":Error:" %1) errmsgs)))
    ))


(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args option-spec)]
    ;; ���f����
    (cond
     ;; --help
     (:help options)
     (do (print-usage summary) (System/exit 0))
     ;; --verion
     (:version options)
     (do (print-version) (System/exit 0))
     ;; ��͎��̃G���[
     errors
     (do (print-err-msg errors) (System/exit 1))
     ;; ������ 0 ��
     (< (count arguments) 1)
     (do (print-err-msg "FILE isn't specified.")
         (print-usage summary)
         (System/exit 1))
     )
    ;; �A�v���̏���
    (println "Options   : " options)
    (println "Arguments : " arguments)
    ))
