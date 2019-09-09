;;; cheat-lib.el --- Core functions -*- lexical-binding: t; coding: utf-8 -*-

;;; Code
(require 'org)

(defconst cheat/version "0.1.1")

(defcustom cheat/categories '("js" "emacs" "git")
  "The categories to display when calling cheat/list-sheets"
  :type '(list string)
  :group 'cheat)

(message "Categories: %s" cheat/categories)
;; base directory to retrieve sheets
(defvar cheat/root
  (if load-file-name
      (file-name-directory load-file-name)))

(defcustom cheat/sheets-folders `(,cheat/root)
  "The list of folders cheat/ should analyses"
  :type '(list string)
  :group 'cheat)(defun cheat-command (sheet)
  "Returns command from sheet alist"
  (prop "command" sheet))

(defvar all-sheets nil
  "Hold all parsed sheets took from sheets folders"
)

;; props helpers
(defun prop (key props) (cdr (assoc key props)))

(defun cheat-command (sheet)
  "Returns command from sheet alist"
  (prop "command" sheet))

(defun cheat-category (sheet)
  (prop "category" sheet))

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
 (format "cheat/%s" (cheat-command sheet)))


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
  (with-temp-buffer
    (insert-file-contents path)
    (let ((keywords (get-org-keywords)))
      (let (
        (command     (get-org-keyword "COMMAND"     keywords))
        (category    (get-org-keyword "CATEGORY"    keywords))
        (title       (get-org-keyword "TITLE"       keywords))
        (name        (get-org-keyword "NAME"        keywords))
        (description (get-org-keyword "DESCRIPTION" keywords)))
        `(
           ("command"     . ,command)
           ("category"    . ,category)
           ("description" . ,description)
           ("title"       . ,title)
           ("name"        . ,name)
           ("path"        . ,path)
        )))))

(defun get-sheets-in (folder-path)
  "Returns all sheets located under folder-path"
  (let ((sheets))
    (dolist (f (directory-files folder-path t ".org$") sheets)
        (let ((sheet (parse-sheet f)))
        (let ((cmd (cheat-command sheet))
              (title (cheat-title sheet)))
            (if (and (not (eq cmd nil)) (not (eq title nil)))
                (push sheet sheets)))))))

(defun filtered-sheets ()
  "Returns all sheets filtered by category (use cheat/categories to define which category to keep)"
  (let ((filtered))
    (dolist (sheet all-sheets filtered)
      (if (member (cheat-category sheet) cheat/categories)
          (push sheet filtered)))
    filtered
  ))

(defun get-all-categories ()
  "Return the list of all categories from all cheatsheets files"
  (let ((categories))
    (dolist (sheet all-sheets categories)
      (let ((category (cheat-category sheet)))
        (unless
          (or (eq category nil)
              (member category categories))
          (push category categories)
        )))
    categories))

(setq cheat/sheets-folders '("~/Dev/cheat/sheets"))
(update-sheets-list)
(message "Total sheets: %s" (length all-sheets))
(message "Sheets filtered by category: %s" (length (filtered-sheets)))
(message "All categories: %s" (get-all-categories))

(defun update-sheets-list ()
  (setq all-sheets nil)
  (dolist (dir cheat/sheets-folders)
    (if (file-directory-p dir)
      (let ((folder-sheets (get-sheets-in dir)))
        (unless (eq folder-sheets nil)
          (dolist (sheet folder-sheets)
            (unless (eq sheet nil)
              (push sheet all-sheets))
            ))
        ))
    ))

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
