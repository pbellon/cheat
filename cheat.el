;;; cheat.el --- Regiter & Open cheatsheets inside emacs -*- lexical-binding: t; coding: utf-8 -*-
;; Author: Pierre BELLON <bellon.pierre@gmail.com>
;; URL: https://github.com/pbellon/cheat
;; Version: 0.1.0
;; Keywords: cheat, cheatsheet, org

;;; Commentary:
;; cheat is an utility to register some cheatsheets inside emacs and easily open
;; them afterward thanks to =cheat/<cheatsheet id>= prefixed functions like 
;; included cheat/emacs or cheat/org.

;;; Code: 
(require 'cheat-lib)
(require 'bui)

(defun sheet-function--macro (sheet)
  "Register a new cheat/<command> interactive function"
  (let
    (
      (fn-name (intern (cheat-fn-name sheet)))
      (title (cheat-title sheet))
      (path (cheat-path sheet))
    )
    `(defun ,fn-name ()
        "Opens ,title cheatsheet"
        (interactive)
        (open-sheet ,title ,path))))

(defun cheat/init ()
  (update-sheets-list)
  (cheat--macro))

;; Main entry point to register all defined cheatsheets in cheat/sheets
(defmacro cheat--macro ()
  `(progn ,@(mapcar
              'sheet-function--macro
              (filtered-sheets))))

(defun cheat/reload-sheets ()
  "Init cheat/sheats"
  (interactive)
  (update-sheets-list)
  (cheat--macro)
)

(with-eval-after-load 'bui
  (defun command-as-button (command &rest properties)
    "Return a command button"
    ;; (message "command: %s" command)
    ;; (message "typeof command: %s" (type-of command))
    ;; (message "points: %s" (point))
    (list command
          :supertype 'help-function
          'action (lambda (cmd) (funcall (intern command)))
    )
  )
          
  (defun sheet-as-list-entry (sheet)
    `((title    . ,(cheat-title   sheet))
      (command  . ,(cheat-fn-name sheet))
      (path     . ,(cheat-path    sheet))))

  (defun sheets-buffer-entries ()
    (mapcar 'sheet-as-list-entry cheat/sheets))
  
  (bui-define-interface sheets-buffer list
   :buffer-name "* Cheatsheets *"
   :get-entries-function 'sheets-buffer-entries
   :format '((title    nil 20 t)
             (category nil 50 t)
             (command  command-as-button 20 t)
             (path     bui-list-get-file-name 20 :right-aligned t)))

  (defun cheat/list-sheets ()
    "Find all cheatsheets under sheets directories"
    (interactive)
    (unless all-sheets (update-sheets-list))
    (bui-get-display-entries 'sheets-buffer 'list))
)



;; Custom special symbols for org-mode
(defvar cheat/org-entities
   '(("lsbr" "\\[" nil "&#91;" "[" "[" "[")
     ("rsbr" "\\]" nil "&#93;" "]" "]" "]")))

(with-eval-after-load 'org
  (dolist (symbol cheat/org-entities)
           (add-to-list 'org-entities-user symbol)))

(provide 'cheat-lib)
(provide 'cheat)

;;; cheat.el ends here
