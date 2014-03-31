
(use 'clojure.java.io)

(with-open [fout (writer  "hello.txt" :append true)]
  (.write fout (str "hello" "world"))
  )

