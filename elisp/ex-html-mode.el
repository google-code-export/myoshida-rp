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

(defconst ex-html-mode-version "0.0.3"
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


(defcustom ex-html-file-base-url
  ""
  "The URL for uploaded file "
  :type 'string
  :group 'ex-html)  


(defcustom ex-html-target-mode-auto-newline
  t
  "Blog target is auto newline mode."
  :type 'boolean
  :group 'ex-html)

(defcustom ex-html-mode-hook nil
  "Hook run by command `ex-html-mode'."
  :type 'hook
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
          (> (prefix-numeric-value arg) 0)))
  (if ex-html-mode
      (run-hooks 'ex-html-mode-hook))
  )

(defvar ex-html-mode-map (make-sparse-keymap)
  "Keymap for ex-html minor mode.")

(defvar ex-html-prefix-key (make-keymap)
  "Prefix key for ex-html-mode")

(defvar ex-html-insert-prefix-key (make-keymap)
  "Prefix key for insert in ex-html-mode")

(define-key ex-html-mode-map "\C-c" ex-html-prefix-key)
(define-key ex-html-mode-map "\C-m" 'ex-html-newline)

(define-key ex-html-prefix-key "\C-y" 'ex-html-yank)
(define-key ex-html-prefix-key "\C-q" 'ex-html-quote-region)
(define-key ex-html-prefix-key "il" 'ex-html-insert-registered-link)
(define-key ex-html-prefix-key "i\C-y" 'ex-html-yank-as-link)
(define-key ex-html-prefix-key "iu" 'ex-html-insert-ul)
(define-key ex-html-prefix-key "io" 'ex-html-insert-ol)
(define-key ex-html-prefix-key "id" 'ex-html-insert-dl)
(define-key ex-html-prefix-key "it" 'ex-html-insert-table)
(define-key ex-html-prefix-key "ii" 'ex-html-insert-img)

;; Html Quote fun
(defun ex-html-insert-amp () "insert quoted &" (interactive) (insert "&amp;"))
(defun ex-html-insert-lt () "insert quoted <" (interactive) (insert "&lt;"))
(defun ex-html-insert-gt () "insert quoted >" (interactive) (insert "&gt;"))
(defun ex-html-insert-space () "insert quoted space" (interactive) (insert "&nbsp;"))
(define-key ex-html-prefix-key "&" 'ex-html-insert-amp)
(define-key ex-html-prefix-key "<" 'ex-html-insert-lt)
(define-key ex-html-prefix-key ">" 'ex-html-insert-gt)
(define-key ex-html-prefix-key " " 'ex-html-insert-space)

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
	(insert "\">")
      (insert "\" target=\"_blank\">"))
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


;; newline

(defun ex-html-newline ()
  "newline, If ex-html-target-mode-auto-newline is nil, inserting html newline tag."
  (interactive)
  (unless ex-html-target-mode-auto-newline
    (insert "<br />"))
  (newline))

(defun ex-html-insert-newline ()
  (unless ex-html-target-mode-auto-newline
      (newline-and-indent)))


;; insert list

(defun ex-html-insert-string-newline (str)
  (insert str)
  (unless ex-html-target-mode-auto-newline
      (newline-and-indent)))

(defun ex-html-insert-list-tag (tagname)
  (ex-html-insert-string-newline (format "<%s>" tagname))
  (let (str)
    (while (ex-html-valid-string-p
	    (setq str (read-string "List Item : ")))
      (ex-html-insert-string-newline (format "<li>%s</li>" str))))
  (ex-html-insert-string-newline (format "</%s>" tagname))
  )

(defun ex-html-insert-ul ()
  "Insert unordered list"
  (interactive)
  (ex-html-insert-list-tag "ul"))

(defun ex-html-insert-ol ()
  "Insert ordered list"
  (interactive)
  (ex-html-insert-list-tag "ol"))

(defun ex-html-insert-dl ()
  "Insert definition list"
  (interactive)
  (ex-html-insert-string-newline "<dl>")
  (let (str)
    (while (ex-html-valid-string-p
	    (setq str (read-string "Definition Term : ")))
      (ex-html-insert-string-newline (format "<dt>%s</dt>" str))
      (if (ex-html-valid-string-p
	   (setq str (read-string "Definition Description : ")))
	  (ex-html-insert-string-newline (format "<dd>%s</dd>" str)))
      ))
  (ex-html-insert-string-newline "</dl>")
  )


;; Insert table
(defun ex-html-insert-table ()
  "Insert table"
  (interactive)
  (ex-html-insert-string-newline "<table>")
  (let ((col 1) str thlist)
    ;; Header
    (insert "<tr>")
    (while (ex-html-valid-string-p
	    (setq str (read-string (format "Table Header %d : " col))))
      (setq col (+ col 1))
      (setq thlist (cons str thlist))
      (insert (format "<th> %s </th>" str)))
    (ex-html-insert-string-newline "</tr>")
    (setq thlist (reverse thlist))
    ;; Rows
    (catch 'empty-row
      (while t
	(unless (ex-html-valid-string-p
		 (setq str (read-string (format "Table Description \"%s\" [Quit] : "
						(car thlist)))))
	    (throw 'empty-row t))
	(let ((tdlist (cdr thlist)) curth)
	  (insert "<tr>")
	  (insert (format "<td> %s </td>" str))
	  (while (setq curth (car tdlist))
	    (if (ex-html-valid-string-p
		 (setq str (read-string (format "Table Description \"%s\" : "
						curth))))
		(insert (format "<td> %s </td>" str))
	      (insert "<td>&nbsp;</td>"))
	    (setq tdlist (cdr tdlist)))
	  (ex-html-insert-string-newline "</tr>"))
	)))
  (ex-html-insert-string-newline "</table>"))


;; insert image
(defun ex-html-insert-img (fpath title)
  "Insert img tag"
  (interactive "fLocal image file : \nsImage title : ")
  (let ((basename (file-name-nondirectory fpath)))
    (insert (format "<img src='%s' title='%s' alt='%s' />"
		    (concat ex-html-file-base-url basename) title title))
  ))

(provide 'ex-html-mode)
;;; ex-html-mode.el ends here
