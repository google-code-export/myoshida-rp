(spit "version.txt" "Ver. 1.2.3")
(println (slurp "version.txt"))

(spit "event.log" "foo\n" :append true)

(slurp "http://yohshiy.blog.fc2.com/blog-category-29.html")
(slurp *in*)





