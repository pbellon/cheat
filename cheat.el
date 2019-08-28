;; cheat.el --- Regiter & Open cheatsheets inside emacs -*- lexical-binding: t; coding: utf-8 -*-

;; Author: Pierre BELLON <bellon.pierre@gmail.com>
;; Homepage: https://github.com/pbellon/cheat
;; Keywords: cheat cheatsheet org

;;; Commentary:
;; cheat is an utility to register some cheatsheets inside emacs and easily open
;; them afterward thanks to =cheat/<cheatsheet id>= prefixed functions like 
;; included cheat/emacs or cheat/org.

;;; Code: 
;; base path, useful to get the path of a cheatsheet relative to this file
(defconst cheat/root (file-name-directory load-file-name))

(defun cheat/sheet-path (name)
  "Get the absolute file path for a given sheet org file relative to this file"
  (expand-file-name (concat "./sheets/" name) cheat/root))

;; The global cheatsheets list, append '(<name> "Frame name" "path/to/cheatsheet.org"))
;; and call (cheat/init) to register a new cheatsheet
(defvar cheat/sheets
  '(
    (adoc "Asciidoc" (cheat/sheet-path "asciidoc.org"))
    (org "OrgMode" (cheat/sheet-path "org-mode.org"))
    (org-syntax "OrgMode Syntax" (cheat/sheet-path "org-mode-syntax.org"))
    (emacs "Emacs" (cheat/sheet-path "emacs.org"))
   )
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

(defun cheat/register (cheat-name cheat-wname cheat-fname)
  "Register a new cheat/<name> interactive function"
  (let ((cheat-fn-name (intern (format "cheat/%s" cheat-name))))
    `(defun ,cheat-fn-name ()
       (interactive)
       (cheat/open ,cheat-wname ,cheat-fname))))

;; Custom special symbols for org-mode
(defvar cheat/org-entities
   '(("lsbr" "\\[" nil "&#91;" "[" "[" "[")
     ("rsbr" "\\]" nil "&#93;" "]" "]" "]")))

;; Main entry point to register all defined cheatsheets in 'cheat/sheets
(defmacro cheat/init ()
  `(progn ,@(mapcar
             (lambda (x)
               (cheat/register (nth 0 x) (nth 1 x) (nth 2 x)))
             cheat/sheets)))

(with-eval-after-load 'org
  (dolist (symbol cheat/org-entities)
           (add-to-list 'org-entities-user symbol)))

(provide 'cheat)
