
(use 'clojure.java.io)
(import (java.io PrintWriter))

(with-open [fout (PrintWriter. (writer  "hello.txt"))]
  (.println fout (str "hello" " world"))
  )


