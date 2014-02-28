
(use 'clojure.java.io)

(with-open [fin (reader "test.txt")]
  (doseq [str (line-seq fin)]
    (println str)
  ))
