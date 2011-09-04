;;; ex-html-mode.el --- Minor mode for html edit

;; Copyright (C) 2011  Free Software Foundation, Inc.

;; Author: M. Yoshida (yohshiy)
;; Keywords: docs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Put ex-html-mode.el in path directory, and write this in .emacs.el

;; (autoload 'ex-html-mode "ex-html-mode" "Minor mode for html edit" t)
;; (add-hook 'html-mode-hook 'ex-html-mode)

;;; Code:

(defconst ex-html-mode-version "0.0.1"
  "ex-html-mode version number")

(defun ex-html-mode-version ()
  "Report the current version of ex-html-mode."
  (interactive)
  (message "ex-html-mode.el : %s" ex-html-mode-version))


(defvar ex-html-code-class-format
  "class=\"%s\""
  "Code syntax highlighting pre tag atribute format (<pre ???>)")

(defvar ex-html-code-class-alist
  '(
    ("bison" . "sh_bison") ("glsl" . "sh_glsl") ("m4" . "sh_m4") ("scala" . "sh_scala")
    ("c" . "sh_c") ("haxe" . "sh_haxe") ("makefile" . "sh_makefile") ("shell" . "sh_sh")
    ("caml" . "sh_caml") ("html" . "sh_html") ("oracle" . "sh_oracle") ("slang" . "sh_slang")
    ("changelog" . "sh_changelog") ("java" . "sh_java") ("pascal" . "sh_pascal") ("sml" . "sh_sml")
    ("cpp" . "sh_cpp") ("js" . "sh_javascript") ("perl" . "sh_perl") ("spec" . "sh_spec")
    ("cs" . "sh_csharp") ("js-dom" . "sh_javascript_dom") ("php" . "sh_php") ("sql" . "sh_sql")
    ("css" . "sh_css") ("latex" . "sh_latex") ("prolog" . "sh_prolog") ("tcl" . "sh_tcl")
    ("desktop" . "sh_desktop") ("ldap" . "sh_ldap") ("properties" . "sh_properties")
    ("xml" . "sh_xml") 
    ("diff" . "sh_diff") ("log" . "sh_log") ("python" . "sh_python") ("xorg" . "sh_xorg")
    ("flex" . "sh_flex") ("lsm" . "sh_lsm") ("ruby" . "sh_ruby"))
  "Code syntax highlighting class alist")

(defvar ex-html-default-code-class
  nil
  "Code syntax highlighting default class")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Minor mode implementation

(defvar ex-html-mode nil
  "nil disables ex-html, non-nil enables.")

(make-variable-buffer-local 'ex-html-mode)

(defun ex-html-mode (&optional arg)
  "Minor mode for html edit.
To submit a problem report, request a feature or get support, please
visit ex-html' homepage at http://yohshiy.blog.fc2.com/?tag=ex-html-mode.el

To see what version of ex-html you are running, enter
`\\[ex-html-version]'.

Key bindings:
\\{ex-html-mode-map}"
  (interactive "P")
  (setq ex-html-mode
        (if (null arg)
            ;; Toggle mode
            (not ex-html-mode)
          ;; Enable/Disable according to arg
          (> (prefix-numeric-value arg) 0))))

(defvar ex-html-mode-map (make-sparse-keymap)
  "Keymap for ex-html minor mode.")

(defvar ex-html-prefix-key (make-keymap)
  "Prefix key for ex-html-mode")

(define-key ex-html-mode-map "\C-c" ex-html-prefix-key)

(define-key ex-html-prefix-key "\C-y" 'ex-html-yank)
(define-key ex-html-prefix-key "\C-q" 'ex-html-quote-region)

;;;###autoload
(or (assoc 'ex-html-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(ex-html-mode "-Ex") minor-mode-alist)))

(or (assoc 'ex-html-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'ex-html-mode ex-html-mode-map)
		minor-mode-map-alist)))


;;;; Quote 

(setq ex-html-quote-alist
      '((?& . "&amp;") (?< . "&lt;") (?> . "&gt;")))

(defun ex-html-quoted-char (ch)
  (cdr (assoc ch ex-html-quote-alist)))

(defun ex-html-quote-region (abeg aend)
  "Quote <, >, &  in region."
  (interactive "r*")
  (let ((beg (min abeg aend))
	(end (max aend aend)))
    (save-excursion
      (goto-char beg)
      (save-restriction
	(narrow-to-region beg end)
	(while (re-search-forward "[&<>]" nil t)
	  (replace-match (ex-html-quoted-char (preceding-char))))
	))))
	

;;;; Code syntax highlight

(defun ex-html-yank ()
  "Yank with the <pre> tags, and quote <, >, &"
  (interactive)
  (let ((classset
	 (assoc (completing-read "Code class?: " ex-html-code-class-alist
				 nil nil ex-html-default-code-class)
		ex-html-code-class-alist))
	(completion-ignore-case t)
	beg end endmark)
    (insert "<pre ")
    (if classset
	(progn
	  (setq ex-html-default-code-class (car classset))
	  (insert (format ex-html-code-class-format (cdr classset))))
      (setq ex-html-default-code-class nil))
    (insert ">")
    (setq beg (point))
    (yank)
    (setq end (point))
    (setq endmark (point-marker))
    (ex-html-quote-region beg end)
    (goto-char (marker-position endmark))
    (set-marker endmark nil)
    )
  (insert "</pre>\n"))




(provide 'ex-html-mode)
;;; ex-html-mode.el ends here
