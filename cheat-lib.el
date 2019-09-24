;;; cheat-lib.el --- Core functions -*- lexical-binding: t; coding: utf-8 -*-

;;; Code
(require 'org)
(require 'table)

(defconst cheat/version "0.1.1")

(defcustom cheat/categories 
  '(
     "JavaScript"
     "Emacs"
     "Markup"
     "CLI"
     "Org"
     "HTML"
     "C-like"
     "Ruby"
     "Python"
   )
  "The categories to display when calling cheat/list-sheets"
  :type '(list string)
  :group 'cheat)

(defcustom cheat/completion-backend nil
  "The completion backend to use when calling cheat interactive function"
  :type 'string
  :options '('ivy 'helm)
  :group 'cheat)

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

(defun cheat-list-path (sheet)
  (file-relative-name (cheat-path sheet) "~"))

(defun cheat-fn-name (sheet)
  "Returns the cheat/<command> function declaration name"
 (format "cheat/%s" (cheat-command sheet)))

(defvar completion-prompt "Cheatsheet command: ") 

(defun completion-line (sheet)
  (let ((cmd      (cheat-command     sheet))
        (cmd-size (length (cheat-command sheet)))
        (desc     (or (cheat-description sheet) "")))
    (let ((sep (table--multiply-string " " (- 20 cmd-size))))
      (format "%s%s%s" cmd sep desc))))

(defun completion-list (sheets)
  (cl-mapcar
    (lambda (sheet)
      (propertize (completion-line sheet) 'property (cheat-command sheet)))
    sheets))

(defun on-complete (str)
  (let ((cmd (get-text-property 0 'property str)))
    (open-sheet-by-command cmd)))

(defun helm-complete-sheet-command (sheets)
  (generic-complete-sheet-command sheets)
)

(defun ivy-complete-sheet-command (sheets)
  (ivy-read completion-prompt
    (completion-list sheets)
    :action #'on-complete :sort t))

(defun generic-complete-sheet-command (sheets)
  (open-sheet-by-command 
    (completing-read completion-prompt (mapcar #'cheat-command sheets) nil nil "")))

;;;###autoload
(defun cheat--complete-sheet-command ()
  (let ((sheets (filtered-sheets)))
    (cl-case cheat/completion-backend
      (helm (helm-complete-sheet-command sheets))
      (ivy  (ivy-complete-sheet-command sheets))
      (t    (generic-complete-sheet-command sheets)))))

;; base directory to retrieve sheets
(defvar cheat/root
  (if load-file-name
      (file-name-directory load-file-name)))

(defvar pkg-sheets-folder (expand-file-name "sheets/" cheat/root)) 

(defcustom cheat/sheets-folders `(,pkg-sheets-folder)
  "The list of folders cheat/ should analyses"
  :type '(list string)
  :group 'cheat)

(defvar cheat--all-sheets nil
  "Hold all parsed sheets took from sheets folders"
)

(defvar all-registered-sheets nil
  "Hold all sheets currently having an alias"
)

;;;###autoload
(defun cheat--declare-all-functions ()
  ;; Unregister previously created sheets
  (unless (eq all-registered-sheets nil)
    (dolist (sheet all-registered-sheets)
      (message "Unregistering %s" (cheat-fn-name sheet))
      (fmakunbound (intern (cheat-fn-name sheet)))))

  (setq all-registered-sheets
    (dolist
      (sheet (filtered-sheets))
      (defalias (intern (cheat-fn-name sheet))
        (lambda ()
          (interactive)
          (open-sheet sheet)))
      sheet)
    ))


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
                (push sheet sheets)
                (message "Could not add %s to cheatsheet list because either command or title is null" f)
              ))))))

(defun filtered-sheets ()
  "Returns all sheets filtered by category (use cheat/categories to define which category to keep)"
  (let ((filtered))
    (dolist (sheet cheat--all-sheets filtered)
      (if (member (cheat-category sheet) cheat/categories)
          (push sheet filtered)))
    filtered
  ))

;;;###autoload
(defun cheat--list-all-categories ()
  "Return all available categories based on loaded sheets in cheat--all-sheets"
  (let ((categories))
    (dolist (sheet cheat--all-sheets categories)
      (let ((category (cheat-category sheet)))
        (unless
          (or (eq category nil)
              (= (length category) 0)
              (member category categories))
          (push category categories)
        )))
    categories))


;;;###autoload
(defun cheat--update-sheets-list ()
  (setq cheat--all-sheets nil)
  (dolist (dir cheat/sheets-folders)
    (if (file-directory-p dir)
      (let ((folder-sheets (get-sheets-in dir)))
        (unless (eq folder-sheets nil)
          (dolist (sheet folder-sheets)
            (unless (eq sheet nil)
              (push sheet cheat--all-sheets))
            ))
        ))
    ))

(defun in-sheet-buffer ()
  (string-suffix-p "Cheatsheet*" (buffer-name)))

(defun find-sheet-by-command (command)
  (seq-find
    (lambda (sheet)
      (string= command (cheat-command sheet)))
    (filtered-sheets))
)

(defun open-sheet-by-command (command)
  (let ((sheet (find-sheet-by-command command)))
    (if sheet
      (open-sheet sheet))))

(defun open-sheet (sheet)
 (open-sheet-window (cheat-title sheet) (cheat-path sheet)))

(defun open-sheet-window (wname fname)
  "Opens a buffer with the given `wname` as frame name and insert content from `fname` filename"
  (let 
    (
      (bname (concat "*" wname " Cheatsheet*"))
      (split-direction (if (in-sheet-buffer) 'below 'right))
    )
    (if (buffer-exists bname)
      (switch-to-buffer bname)
      (let 
        (($w (split-window nil nil split-direction)))
        (select-window $w)
        (let (($b (generate-new-buffer bname)))
          (set-buffer-major-mode $b)
          (switch-to-buffer $b)
          (insert-file-contents fname)
          (org-mode)
          (read-only-mode t))))))

(defun command-as-button (command &rest properties)
  "Return a command button"
  (list command
        :supertype 'help-function
        'action (lambda (cmd) (funcall (intern command)))))

(defun sheet-as-list-entry (sheet)
  `((title    .    ,(cheat-title    sheet))
    (command  .    ,(cheat-fn-name  sheet))
    (category .    ,(cheat-category sheet))
    (description . ,(cheat-description sheet))
    (path     .    ,(cheat-list-path sheet))))

(defun list-sheets-buffer-entries ()
  (mapcar 'sheet-as-list-entry (filtered-sheets)))

(provide 'cheat-lib)

;;; cheat-lib.el ends here
