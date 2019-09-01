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

;; base path, useful to get the path of a cheatsheet relative to this file
(setq cheat/root (file-name-directory load-file-name))
(setq cheat/root-sheets (format "%ssheets" cheat/root))

(defun cheat/sheet-path (name)
  "Get the absolute file path for a given sheet org file relative to this file"
  (expand-file-name name cheat/root))

(defvar cheat/sheets nil
  "The global cheatsheets list"
)

(defun buffer-exists (bname)
  (not (eq nil (get-buffer bname))))

(defun cheat/open (wname fname)
  "Opens a buffer with the given `wname` as frame name and insert content from `fname` filename"
  (let ((bname (concat "*" wname " Cheatsheet*")))
    (if (buffer-exists bname)
      (switch-to-buffer bname)
      (let (($b (generate-new-buffer bname)))
        (set-buffer-major-mode $b)
        (switch-to-buffer $b)
        (insert-file-contents fname)
        (org-mode)
        (read-only-mode t)))))

(defcustom cheat/sheets-folders `(,cheat/root ,cheat/root-sheets)
  "The list of folders cheat/ should analyses"
  :type '(list string)
  :group 'cheat)

(defun prop (key props) (cdr (assoc key props)))

(defun get-org-keywords ()
  "Parse the buffer and return a cons list of (property . value)"
  (org-element-map (org-element-parse-buffer 'element) 'keyword
    (lambda (keyword) (cons
                        (org-element-property :key keyword)
                        (org-element-property :value keyword)))))
  
(defun get-org-keyword (key &optional kwds)
  "return the value associated to key"
  (unless kwds (setq kwds (get-org-keywords)))
  (prop key kwds))

(defun parse-sheet (path)
  "Returns properties of a cheat sheet file"
  (let ((props))
    (with-temp-buffer
      (insert-file-contents path)
      (let ((keywords (get-org-keywords)))
        (let (
          (command (get-org-keyword "COMMAND" keywords))
          (title   (get-org-keyword "TITLE"   keywords))
        )
        (setq props `(
            ("command" . ,command)
            ("title"   . ,title)
            ("path"    . ,path)))
        )))))

(defun cheat/get-sheets-in (folder-path)
  "Returns all sheets located under folder-path"
  (let ((sheets))
    (dolist (f (directory-files folder-path t ".org$") sheets)
      (let ((sheet (parse-sheet f)))
        (let ((cmd (prop "command" sheet))
              (title (prop "title" sheet)))
          (message "get-sheets-in sheet: %s" sheet)
          (if (and (not (eq cmd nil)) (not (eq title nil)))
            (add-to-list 'sheets sheet)
          )
        )
      )
    )
  )
)

;; (message "get sheets in ~/.emacs.d/quelpa/build/cheat/sheets: %s" (cheat/get-sheets-in  "~/.emacs.d/quelpa/build/cheat/sheets"))

(defun reload-sheets ()
  (setq cheat/sheets nil)
  "Add to cheat/sheets cheat org file located under cheat/sheets-folders"
  (dolist (dir cheat/sheets-folders)
    (let ((folder-sheets (cheat/get-sheets-in dir)))
      (unless (eq folder-sheets nil)
        (dolist (sheet folder-sheets)
          (unless (eq sheet nil)
            (add-to-list 'cheat/sheets sheet)
          )
        )
      )
    )))

;; (message "Folders: %s" cheat/sheets-folders)
;; (reload-sheets)
;; (message "Sheets: %s" cheat/sheets)

(defun cheat/reload-sheets ()
  "Init cheat/sheats"
  (interactive)
  (reload-sheets)
  (cheat/init)
)

(defun cheat/list-sheets ()
  "Find all cheatsheets under sheets directories"
  (interactive)
  (unless cheat/sheets (reload-sheets))
  (dolist (el cheat/sheets)
    (message
      "%s is registered with cheat/%s function"
      (prop "title" el)
      (prop "command" el))))

(defun cheat/register (sheet)
  "Register a new cheat/<command> interactive function"
  (let
    (
      (cheat-command (prop "command" sheet))
      (cheat-path (prop "path" sheet))
      (cheat-title (prop "title" sheet))
    )
    (let ((cheat-fn-name (intern (format "cheat/%s" cheat-command))))
      `(defun ,cheat-fn-name ()
         "Opens ,cheat-title cheatsheet"
         (interactive)
         (cheat/open ,cheat-title ,cheat-path)))))

;; Custom special symbols for org-mode
(defvar cheat/org-entities
   '(("lsbr" "\\[" nil "&#91;" "[" "[" "[")
     ("rsbr" "\\]" nil "&#93;" "]" "]" "]")))

;; Main entry point to register all defined cheatsheets in 'cheat/sheets
(defmacro cheat/init ()
  `(progn ,@(mapcar
              'cheat/register
              cheat/sheets)))

(with-eval-after-load 'org
  (dolist (symbol cheat/org-entities)
           (add-to-list 'org-entities-user symbol)))

(provide 'cheat)
;;; cheat.el ends here
