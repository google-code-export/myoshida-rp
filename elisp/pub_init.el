
;; コンパイル

(require 'compile)

(defvar yel-compile-auto-close t
  "* If non-nil, a window is automatically closed after (\\[compile])"
  )

(defadvice compile (after compile-aftercheck
			  activate compile)
  "Adds a funcion of windows auto-close."
  (let ((proc (get-buffer-process "*compilation*")))
    (if (and proc yel-compile-auto-close)
	(set-process-sentinel proc 'yel-compile-teardown))
    ))

(defun yel-compile-teardown (proc status)
  "Closes window automatically, if compile succeed"
  (let ((ps (process-status proc)))
    (if (eq ps 'exit)
	(if (eq 0 (process-exit-status proc))
	    (progn
	      (delete-other-windows)
	      (kill-buffer "*compilation*")
	      (message "---- Commpile Success ----")
	      )
	  (message "Commpile Failer")))
    (if (eq ps 'signal)
	(message "Commpile Abnormal end"))
    ))



;; tags 機能

(global-set-key (kbd "C-.") 'pop-tag-mark)
(global-set-key (kbd "M-.") 'my-find-tag)
(global-set-key (kbd "C-\\") 'my-tags-search)

(defun my-tags-search ()
  (interactive)
  (ring-insert find-tag-marker-ring (point-marker))
  (if (eq last-command this-command)
      (tags-loop-continue)
    (call-interactively 'tags-search)))
  

(defun my-find-tag ()
  (interactive)
  (if (eq last-command this-command)
      (find-tag last-tag t)
    (call-interactively 'find-tag)))


(defun my-kill-ring-save (beg end)
  (interactive "r")
  (if mark-active
      (kill-ring-save beg end)
    (save-excursion
      (let ((oldpoint (point)) (wbeg (point)) (wend (point))
	    (syntaxes "w"))
	(skip-syntax-backward syntaxes) (setq wbeg (point))
	(goto-char oldpoint)
	(skip-syntax-forward syntaxes) (setq wend (point))
	(kill-ring-save wbeg wend)
	(message (format "Saved \"%s\"" (current-kill 0)))))
    ))


;; 文字コード

(defun my-ja-grep ()
  (interactive)
  (let ((default-process-coding-system '(utf-8-dos . utf-8-unix)))
    (call-interactively 'grep)))


;; 削除
(global-set-key (kbd "<backspace>") 'my-hungry-backspace)
(defun my-hungry-backspace (beg end)
  "Delete the preceding character or whitespace."
  (interactive "r")
  (if (and mark-active delete-selection-mode)
      (delete-region beg end)
    (let ((here (point)))
      (skip-chars-backward "[\n\r\f\v\\s ]")
      (if (/= (point) here)
	  (delete-region (point) here)
	(backward-delete-char-untabify 1))
      )))
