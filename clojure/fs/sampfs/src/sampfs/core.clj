(ns sampfs.core
  (:gen-class))


;; ファイルパス

(absolute-path "../foo")

(base-name "~/foo/bar.txt")
(base-name "~/foo/bar.txt" true)

(expand-home path)

(extension path)

(file path & paths)


(home)

(name path)

(normalized-path path)

(ns-path n)
(path-ns path)

(parent path)


;; カレントディレクトリー変更

(chdir "..")

(with-cwd cwd & body)
(with-mutable-cwd & body)


;; ディレクトリー探索

(glob pattern)
(find-files path pattern)
(walk func path)



;; ファイルチェック、情報取得

(exists? path)

(directory? path)

(writeable? path)



(size path)

(mod-time path)


;; ファイル、ディレクトリー操作

(copy from to)
(copy+ src dest)
(copy-dir from to)

delete
delete-dir
(delete-dir root)


(mkdir path)
(mkdirs path)

(rename old-path new-path)

(chmod "+x" "/tmp/foo")   ; Sets executable for everyone
(chmod "u-wx" "/tmp/foo") ; Unsets owner write and executable



(temp-dir prefix)
(temp-dir prefix suffix)
(temp-dir prefix suffix tries)

(temp-file prefix)
(temp-file prefix suffix)
(temp-file prefix suffix tries)

(temp-name prefix)
(temp-name prefix suffix)

(tmpdir)





(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  )
