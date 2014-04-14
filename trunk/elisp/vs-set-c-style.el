;;; vs-set-c-style.el --- Visual Studio like C/C++ style for c-mode

;; Keywords: c, tools

;;
;; It is free software; you can redistribute it and/or modify it under the
;; terms of either:
;;
;; a) the GNU General Public License as published by the Free Software
;; Foundation; either version 1, or (at your option) any later version, or
;;
;; b) the "Artistic License".

;;; Commentary:

;; Intstall:
;;
;; Adds in your emacs initialize file (~/.emacs.d/init.el)
;;
;;     (autoload 'vs-set-c-style "vs-set-c-style")
;;     (add-hook 'c-mode-hook 'vs-set-c-style)
;;     (add-hook 'c++-mode-hook 'vs-set-c-style)
;;

;;; Code:


(defconst vs-c-style
  `("bsd"
    (c-recognize-knr-p . nil)
    (c-enable-xemacs-performance-kludge-p . t) ; speed up indentation in XEmacs
    (c-basic-offset . 4)
    (c-hanging-braces-alist . ((defun-open          before after)
                               (defun-close 	    before after)
                               (class-open  	    before after)
                               (class-close 	    before)
                               (inexpr-class-open   before after)
                               (inexpr-class-close  before after)
                               (namespace-open      before after)
                               (namespace-close     before after)
			       (inline-open  	    before after)
                               (inline-close 	    before after)
                               (block-open   	    before after)
                               (block-close  	    before after)
                               (extern-lang-open    before after)
                               (extern-lang-close   before after)
                               (statement-case-open before after)
                               (substatement-open   before after)
			       ))
    (c-offsets-alist . ((inline-open . 0)
                        (substatement-open . 0)
                        (case-label . +)
                        (statement-case-open . 0)
                        (case-label . 0)))
    (c-hanging-colons-alist . ((case-label)
                               (label after)
                               (access-label after)
                               (member-init-intro before)))
    (c-hanging-semi&comma-criteria . (c-semi&comma-inside-parenlist))
    (c-cleanup-list . (defun-close-semi
    		       list-close-comma
    		       scope-operator
    		       compact-empty-funcall))
    )
  "Visual Studio like style")

(defun vs-set-c-style ()
  "Set the current buffer's c-style to Visual Studio like style. "
  (c-add-style "vs" vs-c-style t))

(provide 'vs-set-c-style)
