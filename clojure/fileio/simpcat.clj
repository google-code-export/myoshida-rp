
(use 'clojure.java.io)
(import (java.io PrintWriter))

(defn catfile [fin fout]
  (doseq [str (line-seq fin)]
    (.println fout str)))  


(with-open [fout (PrintWriter. (writer "test.out"))]
  (.println fout "Hello world!!")
  (.println System/out "Hello world!!")
  )

;; (doseq [fpath *command-line-args*]
;;   (let [fout (PrintWriter. "test.out"]
;;     (with-open [fin (reader "test.txt")]
;;       (catfile fin fout)
;;       )))


