
(use 'clojure.java.io)

(with-open [fin (reader "japanese.txt" :encoding "JISAutoDetect")]
  (doseq [str (line-seq fin)]
    (println str))
  )
