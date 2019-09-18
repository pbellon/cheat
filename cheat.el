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
(require 'bui)
(require 'cheat-lib)
  
;; Custom special symbols for org-mode
(defvar cheat/org-entities
   '(("lsbr" "\\[" nil "&#91;" "[" "[" "[")
     ("rsbr" "\\]" nil "&#93;" "]" "]" "]")))

(dolist (symbol cheat/org-entities)
          (add-to-list 'org-entities-user symbol))

(declare-function 'cheat--update-sheets-list "cheat-lib")
(declare-function 'cheat--declare-all-functions "cheat-lib")
(declare-function 'cheat--complete-sheet-command "cheat-lib")

(bui-define-interface cheat--list-sheets-buffer list
  :buffer-name "* Cheatsheets *"
  :get-entries-function 'list-sheets-buffer-entries
  :format '((title       nil 20 t)
            (category    nil 20 t)
            (command     command-as-button 20 t)
            (description nil 70 t)
            (path        bui-list-get-file-name 20 :right-aligned t)))


;;;###autoload
(defun cheat ()
  (interactive)
  (cheat--complete-sheet-command))

;;;###autoload
(defun cheat/setup ()
  (cheat--update-sheets-list)
  (cheat--declare-all-functions)
)
;;;###autoload
(defun cheat/list-sheets ()
  "Find all cheatsheets under sheets directories"
  (interactive)
  (unless cheat--all-sheets (cheat--update-sheets-list))
  (bui-get-display-entries 'cheat--list-sheets-buffer 'list))

;;;###autoload
(defun cheat/reload-sheets ()
  "Init cheat/sheats"
  (interactive)
  (cheat/setup)
)

;;;###autoload
(defun cheat/list-categories ()
  (interactive)
  (unless cheat--all-sheets (cheat--update-sheets-list))
  (message "%s" 
    (mapcar
      (lambda (cat) (format "\n\t%s" cat))
      (cheat--list-all-categories))))

(provide 'cheat)

;;; cheat.el ends here
