;;; cheat-lib.el --- Core functions -*- lexical-binding: t; coding: utf-8 -*-
;; Author: Pierre BELLON <bellon.pierre@gmail.com>
;; URL: https://github.com/pbellon/cheat
;; Version: 0.1.0
;; Keywords: cheat, cheatsheet, org

;;; Code
(defconst cheat/version "1.0.0")

(defun prop (key props) (cdr (assoc key props)))

(defun buffer-exists (bname)
  (not (eq nil (get-buffer bname))))

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
          (command     (get-org-keyword "COMMAND"     keywords))
          (title       (get-org-keyword "TITLE"       keywords))
          (name        (get-org-keyword "NAME"        keywords))
          (description (get-org-keyword "DESCRIPTION" keywords))
        )
        (setq props `(
            ("command"     . ,command)
            ("description" . ,description)
            ("title"       . ,title)
            ("name"        . ,name)
            ("path"        . ,path)
          )
        ))))))

(defun get-sheets-in (folder-path)
  "Returns all sheets located under folder-path"
  (let ((sheets))
    (dolist (f (directory-files folder-path t ".org$") sheets)
      (let ((sheet (parse-sheet f)))
        (let ((cmd (cheat-command sheet))
              (title (cheat-title sheet)))
          (if (and (not (eq cmd nil)) (not (eq title nil)))
            (add-to-list 'sheets sheet)))))))

(defun cheat-command (sheet)
  "Returns command from sheet alist"
  (prop "command" sheet))

(defun cheat-description (sheet)
  "Returns description from sheet alist"
  (prop "description" sheet))

(defun cheat-title (sheet)
  "Return the buffer title from sheet  alist"
  (prop "title" sheet))

(defun cheat-name (sheet)
  "Return the buffer name from sheet  alist"
  (prop "name" sheet))

(defun cheat-path (sheet)
  "Return path to sheet's file from sheet alist"
  (prop "path" sheet))

(defun cheat-fn-name (sheet)
  "Returns the cheat/<command> function declaration name"
 (intern (format "cheat/%s" (cheat-command sheet))))

(defun reload-sheets ()
  "Add to cheat/sheets cheat org file located under cheat/sheets-folders"
  (setq cheat/sheets nil)
  (dolist (dir cheat/sheets-folders)
    (let ((folder-sheets (get-sheets-in dir)))
      (unless (eq folder-sheets nil)
        (dolist (sheet folder-sheets)
          (unless (eq sheet nil)
            (add-to-list 'cheat/sheets sheet)))))))

(defun open-sheet (wname fname)
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


(provide 'cheat-lib)

;;; cheat-lib.el ends here




