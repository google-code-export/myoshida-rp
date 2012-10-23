;;; yafm.el --- Yet another follow mode for compilation/output buffers

;; Author: M. Yoshida
;; Created:  7-MAY-2004
;; Version: 1.0
;; Keywords: compile
 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:


;;; Installation
;; (autoload 'yafm-mode "yafm" "follow mode for compilation/occur buffers" t)
;; (add-hook 'compilation-mode-hook 'yafm-mode)
;; (add-hook 'occur-mode-hook 'yafm-mode)
;; (add-hook 'diff-mode-hook 'yafm-mode)


;;; Examples:
;;  
;; Do an occur for the word `package' in the NEWS file:
;; C-h n
;; M-x occur RTN package RTN

;; or test it on the current file:
;; (grep "grep -n 'def' yafm.el")
;; (occur "def")

;; To extend this code to handle other types of output buffer, you
;; need to add an entry to the alist `yafm-entry-alist'.

;; (setq yafm-entry-alist (cons
;; 			'(Buffer-menu-mode . 
;;                             (lambda (arg) (next-line arg) (Buffer-menu-other-window))
;; 			    )
;; 			yafm-entry-alist))
;; (add-hook 'buffer-menu-mode-hook 'yafm-mode)


;;; TODO
;; ??

;;; Code:


(defvar yafm-entry-alist '(
			   (compilation-mode . yafm-compilation-next-line)
			   (grep-mode . yafm-compilation-next-line)
			   (occur-mode . yafm-occur-next-line)
			   (diff-mode . yafm-diff-next-line)
			   (apropos-mode . (lambda (arg)
					     (next-line arg)
					     (apropos-follow)))
			   )
  "
This is association list of mode entries that yafm is used.
Form of each element is
 ( MODE-NAME . FUNCTION-MOVE-TO-NEXT-ITEM )  .
yafm を利用するモードのエントリの連想配列。各要素は
( モード名 . 次の項目への移動関数 )
の型式である。
次の項目への移動関数 は次の項目(compile ならば次のエラー)に移動し、
その対応するファイルのバッファをポップアップで開き、その該当個所に
移動する関数を指定します。さらにこの関数は next-line のように
引数をとり、負の引数ならば逆方向に移動しなければなりません。
")

(defvar yafm-next-line-key	 "n")
(defvar yafm-previous-line-key   "p")
(defvar yafm-quit-key            "q")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set key
(defun yafm-set-key ()
  (local-set-key yafm-next-line-key     'yafm-next-line)
  (local-set-key yafm-previous-line-key 'yafm-previous-line)
  (local-set-key yafm-quit-key          'yafm-quit)
  (local-set-key "o" 'other-window)
  (local-set-key " " 'next-screen)
  (local-set-key [delete] 'prev-screen)
  )


(defun yafm-mode (&optional arg)
  "yet anothor follow mode for compilation/occur buffers."
  (interactive "P")
  (setq yafm-mode (if (null arg)
		    (not yafm-mode)
		  (> (prefix-numeric-value arg) 0)))
  (when yafm-mode
    ;; yafm-mode ON
    (setq buffer-read-only t)
    (yafm-set-key)
    )
    ;; yafm-mode OFF
  )


;;; Setting minor-mode

(let ((name 'yafm-mode))
  (make-variable-buffer-local name)
  (or (assq name minor-mode-alist)
      (setq minor-mode-alist (cons (list name " yafm") minor-mode-alist)))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yafm-next-line (arg)
  "next-line of yafm-mode"
  (interactive "p")
  (let ((buf (current-buffer)))
    (funcall (cdr (assq major-mode yafm-entry-alist)) arg)
    (yafm-set-hilight buf)
    ))

(defun yafm-previous-line (arg)
  (interactive "p")
  "prev-line of yafm-mode"
  (yafm-next-line (- arg)))

  
(defun yafm-quit (ukey)
  (interactive "P")
  (if ukey
      (kill-buffer nil)
    (bury-buffer))
  (other-window 1)
  (delete-other-windows))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yafm-pre-command-done ()
  "Remove highlighing in both source and output buffers."
  (yafm-unhighlight 0)
  (yafm-unhighlight 1)
  (remove-hook 'pre-command-hook 'yeb-prev-command-done)
  )

(defun yafm-set-hilight ( buf )
  "make the highlight in the source and output buffer "
  ;; make the highlight in the sourcebuffer.
  (yafm-highlight 0 (line-beginning-position) (line-end-position))
  ;; make the highlight in the output buffer.    
  (pop-to-buffer buf)
  (switch-to-buffer buf)
  (yafm-highlight 1 (line-beginning-position) (line-end-position))
  (add-hook 'pre-command-hook 'yafm-pre-command-done)
  )


;;; Highlighting (copied from reftex.el -- cheers Carsten!)

;; Highlighting uses overlays.  If this is for XEmacs, we need to load
;; the overlay library, available in version 19.15
(and (not (fboundp 'make-overlay))
     (condition-case nil
         (require 'overlay)
       ('error 
        (error "Yafm needs overlay emulation (available in XEmacs 19.15)"))))

;; We keep a vector with several different overlays to do our highlighting.
(defvar yafm-highlight-overlays [nil nil])

;; Initialize the overlays
(aset yafm-highlight-overlays 0 (make-overlay 1 1))
(overlay-put (aref yafm-highlight-overlays 0) 'face 'highlight)
(aset yafm-highlight-overlays 1 (make-overlay 1 1))
(overlay-put (aref yafm-highlight-overlays 1) 'face 'highlight)

;; Two functions for activating and deactivation highlight overlays
(defun yafm-highlight (index begin end &optional buffer)
  "Highlight a region with overlay INDEX."
  (move-overlay (aref yafm-highlight-overlays index)
                begin end (or buffer (current-buffer))))
(defun yafm-unhighlight (index)
  "Detatch overlay INDEX."
  (delete-overlay (aref yafm-highlight-overlays index)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; compile-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yafm-compilation-next-line (arg)
  "next-line for compile-mode"
  (if (and (> arg 0) (bobp))
      (compile-goto-error)
    (compilation-next-error arg)
    (compile-goto-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; occur-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yafm-occur-next-line (arg)
  "next-line for occur-mode"
  (if (> arg 0)
      (occur-next arg)
    (occur-prev (- arg)))
  (occur-mode-goto-occurrence))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; diff-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yafm-diff-next-line (arg)
  "next-line for diff-mode"
  (if (> arg 0)
      (diff-hunk-next arg)
    (diff-hunk-prev (- arg)))
  (diff-goto-source))


(provide 'yafm)
;;; yafm.el ends here
