;;; cheat.el --- Regiter & Open cheatsheets inside emacs -*- lexical-binding: t; coding: utf-8 -*-
;; Author: Pierre BELLON <bellon.pierre@gmail.com>
;; URL: https://github.com/pbellon/cheat
;; Version: 0.1.1
;; Keywords: cheat, cheatsheet, org
;; Package-Requires:  ((emacs "24.2") (bui "1.2.1"))

;;; Commentary:
;; cheat is an utility to register some cheatsheets inside emacs and easily open
;; them afterward thanks to =cheat/<cheatsheet id>= prefixed functions like 
;; included cheat/emacs or cheat/org.

;;; Code: 
(require 'cheat-lib)

(defun cheat/reload-sheets (&optional no-macro)
  "Init cheat/sheats"
  (interactive)
  (update-sheets-list)
  (unless no-macro
    (declare-all-functions))
)

(defun cheat ()
  (interactive)
  (complete-sheet-command))

(defun cheat/setup ()
  (update-sheets-list)
  (declare-all-functions)
)

(defun cheat/list-sheets ()
  "Find all cheatsheets under sheets directories"
  (interactive)
  (unless all-sheets (update-sheets-list))
  (bui-get-display-entries 'sheets-buffer 'list))


;; Custom special symbols for org-mode
(defvar cheat/org-entities
   '(("lsbr" "\\[" nil "&#91;" "[" "[" "[")
     ("rsbr" "\\]" nil "&#93;" "]" "]" "]")))

(dolist (symbol cheat/org-entities)
          (add-to-list 'org-entities-user symbol))

(provide 'cheat-lib)
(provide 'cheat)

;;; cheat.el ends here
