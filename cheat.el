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
(require 'org)
(require 'bui)

(require 'cheat-lib)

(defvar cheat/sheets nil
  "The global cheatsheets list"
)

;; base path, useful to get the path of a cheatsheet relative to this file
(setq cheat/root (if load-file-name (file-name-directory load-file-name)))
(setq cheat/root-sheets (format "%ssheets" cheat/root))

(defcustom cheat/sheets-folders `(,cheat/root ,cheat/root-sheets)
  "The list of folders cheat/ should analyses"
  :type '(list string)
  :group 'cheat)

(defun cheat/reload-sheets ()
  "Init cheat/sheats"
  (interactive)
  (reload-sheets)
  (cheat/init)
)

(with-eval-after-load 'bui
  (setq cmd "cheat/adoc")
  
  (defun command-as-button (command &rest properties)
    "Return a command button"
    (message "command: %s" command)
    (message "typeof command: %s" (type-of command))
    (message "points: %s" (point))
    (list command
          :supertype 'help-function
          'action (lambda (cmd)
                    (message "EXEC %S" command)
                    (funcall (intern command)))
          ))
  (defun sheet-as-list-entry (sheet)
    `((title    . ,(cheat-title   sheet))
      (command  . ,(cheat-fn-name sheet))
      (path     . ,(cheat-path    sheet))))

  (defun sheets-buffer-entries ()
    (mapcar 'sheet-as-list-entry cheat/sheets))
  
  (bui-define-interface sheets-buffer list
   :buffer-name "* Cheatsheets *"
   :get-entries-function 'sheets-buffer-entries
   :format '((title   nil 50 t)
             (command command-as-button 20 t)
             (path    bui-list-get-file-name 20 :right-aligned t)))
)

(defun cheat/list-sheets ()
  "Find all cheatsheets under sheets directories"
  (interactive)
  (unless cheat/sheets (reload-sheets))
  (bui-get-display-entries 'sheets-buffer 'list))


(defun cheat/register (sheet)
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

;; Custom special symbols for org-mode
(defvar cheat/org-entities
   '(("lsbr" "\\[" nil "&#91;" "[" "[" "[")
     ("rsbr" "\\]" nil "&#93;" "]" "]" "]")))

;; Main entry point to register all defined cheatsheets in cheat/sheets
(defmacro cheat/init ()
  `(progn ,@(mapcar
              'cheat/register
              cheat/sheets)))

(with-eval-after-load 'org
  (dolist (symbol cheat/org-entities)
           (add-to-list 'org-entities-user symbol)))


(provide 'cheat)

;;; cheat.el ends here
