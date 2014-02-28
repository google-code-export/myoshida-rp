
(use 'clojure.java.io)


(defn catfile [fin fout]
  (doseq [str (line-seq fin)]
    (.println fout str)))
  


(doseq [fpath *command-line-args*]
  (let [fout (new java.io.PrintWriter "test.out")]
    (with-open [fin (reader "test.txt")]
      (catfile fin fout)
      )))


