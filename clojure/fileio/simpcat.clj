
(use 'clojure.java.io)
(import (java.io PrintWriter))

(defn catfile [fin fout]
  (doseq [str (line-seq fin)]
    (.println fout str)))  


(with-open [fout (PrintWriter. (writer "test.out"))]
  (.println fout "Hello world!!")
  (.println System/out "Hello world!!")
  )

(println (class (writer *out*)))


;; (doseq [fpath *command-line-args*]
;;   (let [fout (PrintWriter. "test.out"]
;;     (with-open [fin (reader "test.txt")]
;;       (catfile fin fout)
;;       )))


;; (defn catfile [fin]
;;   (println (class fin))
;;   (doseq [str (line-seq fin)]
;;     (println str)))

;; (with-open [fin (reader "test.txt")]
;;   (catfile fin))


;; (println (class (reader *in*)))
;; (println (class (reader System/in)))
;; (catfile (reader System/in))
