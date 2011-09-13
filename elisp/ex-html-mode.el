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

;; For Customize

(defgroup ex-html nil
  "Expand HTML editing mode"
  :group 'sgml)

(defcustom ex-html-link-alist
  '(("ex-html" .
     ("http://yohshiy.blog.fc2.com/?tag=ex-html-mode.el" t "ex-html-mode.el")))
  "Insert link candidate list. If display name is empty, the address is used."
  :type '(repeat
	  (cons
	   (string :tag "Keyword")
	   (list
	    (string :tag "Site address")
	    (boolean :tag "New window ?")
	    (string :tag "Display name")
	    )))
  :group 'ex-html)
  


(defcustom ex-html-code-class-format "class=\"%s\""
  "Code syntax highlighting pre tag atribute format (<pre ???>)"
  :type 'string
  :group 'ex-html)

(defcustom ex-html-code-class-alist
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
  "Code syntax highlighting class alist"
  :type '(repeat
	  (cons
	   (string :tag "Keyword   ")
	   (string :tag "Class name")))
  :group 'ex-html)

(defcustom ex-html-default-code-class
  ""
  "Code syntax highlighting default class"
  :type 'string
  :group 'ex-html)

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

(defvar ex-html-insert-prefix-key (make-keymap)
  "Prefix key for insert in ex-html-mode")

(define-key ex-html-mode-map "\C-c" ex-html-prefix-key)

(define-key ex-html-prefix-key "\C-y" 'ex-html-yank)
(define-key ex-html-prefix-key "\C-q" 'ex-html-quote-region)
(define-key ex-html-prefix-key "il" 'ex-html-insert-registered-link)
(define-key ex-html-prefix-key "i\C-y" 'ex-html-yank-as-link)


;;
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


;; Link

(defun ex-html-valid-string-p (str)
  "If the string is valid(not empty), return t."
  (if (and str (stringp str) (not (string= str "")))
      t
    nil))


(defun ex-html-insert-registered-link ()
  "Insert a registerd link."
  (interactive)
  (let ((link-info
	 (cdr (assoc (completing-read "Link keyword ? : " ex-html-link-alist nil t)
		     ex-html-link-alist)))
	(completion-ignore-case t))
    (if link-info
	(insert (format "<a href=\"%s\"%s>%s</a>"
			(nth 0 link-info)
			(if (nth 1 link-info) " target=\"_blank\"" "")
			(if (ex-html-valid-string-p (nth 2 link-info))
			    (nth 2 link-info) (nth 0 link-info))
			))
      (message "Keywoprd is not match"))
    ))


(defun ex-html-yank-as-link (&optional arg)
  "Yank as html link. if after C-u, new window link."
  (interactive "P")
  (let (beg endm dispstr)
    (if (and mark-active transient-mark-mode)
	;; Region selected
	(progn
	  (setq beg (min (point) (mark)))
	  (setq endm (make-marker))
	  (set-marker endm (max (point) (mark)))
	  (goto-char beg)))
    (insert "<a href=\"")
    (yank)
    (if arg
	(insert " target=\"_blank\""))
    (insert "\">")
    (if endm
	(progn
	  (ex-html-quote-region (point) (marker-position endm))
	  (goto-char (marker-position endm))
	  (set-marker endm nil))
      (setq dispstr (read-string "Display string : "))
      (if (ex-html-valid-string-p dispstr)
	  (progn
	    (setq beg (point))
	    (insert dispstr)
	    (ex-html-quote-region beg (point)))
	(yank))
	)
    (insert "</a>"))
  )

(provide 'ex-html-mode)
;;; ex-html-mode.el ends here
