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
(defun cheat--prop (key props)
  "Return value for given key in props"
  (and props
    (cdr (assoc key props)))
)

(defun cheat--command (sheet)
  "Returns command from sheet alist"
  (cheat--prop "command" sheet))

(defun cheat--category (sheet)
  (cheat--prop "category" sheet))

(defun cheat--description (sheet)
  "Returns description from sheet alist"
  (cheat--prop "description" sheet))

(defun cheat--title (sheet)
  "Return the buffer title from sheet  alist"
  (cheat--prop "title" sheet))

(defun cheat--name (sheet)
  "Return the buffer name from sheet  alist"
  (cheat--prop "name" sheet))

(defun cheat--path (sheet)
  "Return path to sheet's file from sheet alist"
  (cheat--prop "path" sheet))

(defun cheat--list-path (sheet)
  (file-relative-name (cheat--path sheet) "~"))

(defun cheat--fname (sheet)
  "Returns the cheat/<command> function declaration name"
 (format "cheat/%s" (cheat--command sheet)))

(defvar cheat--completion-prompt "Cheatsheet command: ") 

(defun cheat--completion-line (sheet)
  (let ((cmd      (cheat--command     sheet))
        (cmd-size (length (cheat--command sheet)))
        (desc     (or (cheat--description sheet) "")))
    (let ((sep (table--multiply-string " " (- 20 cmd-size))))
      (format "%s%s%s" cmd sep desc))))

(defun cheat--completion-list (sheets)
  (cl-mapcar
    (lambda (sheet)
      (cheat--propertize (cheat--completion-line sheet) 'property (cheat--command sheet)))
    sheets))

(defun on-complete (str)
  (let ((cmd (get-text-property 0 'property str)))
    (cheat--open-sheet-by-command cmd)))

(defun cheat--helm-complete (sheets)
  (cheat--generic-complete sheets)
)

(defun cheat--ivy-complete (sheets)
  (ivy-read cheat--completion-prompt
    (cheat--completion-list sheets)
    :action #'on-complete :sort t))

(defun cheat--generic-complete (sheets)
  (cheat--open-sheet-by-command 
    (completing-read cheat--completion-prompt (mapcar #'cheat--command sheets) nil nil "")))

;;;###autoload
(defun cheat--complete-sheet-command ()
  (let ((sheets (cheat--list-filtered-sheets)))
    (cl-case cheat/completion-backend
      (helm (cheat--helm-complete sheets))
      (ivy  (cheat--ivy-complete sheets))
      (t    (cheat--generic-complete sheets)))))

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

(defvar cheat--all-registered-sheets nil
  "Hold all sheets currently having an alias"
)

;;;###autoload
(defun cheat--declare-all-functions ()
  ;; Unregister previously created sheets
  (unless (eq cheat--all-registered-sheets nil)
    (dolist (sheet cheat--all-registered-sheets)
      (message "Unregistering %s" (cheat--fname sheet))
      (fmakunbound (intern (cheat--fname sheet)))))

  (setq cheat--all-registered-sheets
    (dolist
      (sheet (cheat--list-filtered-sheets))
      (defalias (intern (cheat--fname sheet))
        (lambda ()
          (interactive)
          (cheat--open-sheet sheet)))
      sheet)
    ))


(defun cheat--buffer-exists (bname)
  (not (eq nil (get-buffer bname))))

(defun cheat--org-keywords ()
  "Parse the buffer and return a cons list of (cheat--property . value)"
  (org-element-map (org-element-parse-buffer 'element) 'keyword
    (lambda (keyword) (cons
                        (org-element-property :key keyword)
                        (org-element-property :value keyword)))))
  
(defun cheat--org-keyword (key &optional kwds)
  "return the value associated to key"
  (unless kwds (setq kwds (cheat--org-keywords)))
  (cheat--prop key kwds))

(defun cheat--parse-sheet (path)
  "Returns properties of a cheat sheet file"
  (with-temp-buffer
    (insert-file-contents path)
    (let ((keywords (cheat--org-keywords)))
      (let (
        (command     (cheat--org-keyword "COMMAND"     keywords))
        (category    (cheat--org-keyword "CATEGORY"    keywords))
        (title       (cheat--org-keyword "TITLE"       keywords))
        (name        (cheat--org-keyword "NAME"        keywords))
        (description (cheat--org-keyword "DESCRIPTION" keywords)))
        `(
           ("command"     . ,command)
           ("category"    . ,category)
           ("description" . ,description)
           ("title"       . ,title)
           ("name"        . ,name)
           ("path"        . ,path)
        )))))

(defun cheat--get-sheets-in (folder-path)
  "Returns all sheets located under folder-path"
  (let ((sheets))
    (dolist (f (directory-files folder-path t ".org$") sheets)
        (let ((sheet (cheat--parse-sheet f)))
        (let ((cmd (cheat--command sheet))
              (title (cheat--title sheet)))
            (if (and (not (eq cmd nil)) (not (eq title nil)))
                (push sheet sheets)
                (message "Could not add %s to cheatsheet list because either command or title is null" f)
              ))))))

(defun cheat--list-filtered-sheets ()
  "Returns all sheets filtered by category (use cheat/categories to define which category to keep)"
  (let ((filtered))
    (dolist (sheet cheat--all-sheets filtered)
      (if (member (cheat--category sheet) cheat/categories)
          (push sheet filtered)))
    filtered
  ))

;;;###autoload
(defun cheat--list-all-categories ()
  "Return all available categories based on loaded sheets in cheat--all-sheets"
  (let ((categories))
    (dolist (sheet cheat--all-sheets categories)
      (let ((category (cheat--category sheet)))
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
      (let ((folder-sheets (cheat--get-sheets-in dir)))
        (unless (eq folder-sheets nil)
          (dolist (sheet folder-sheets)
            (unless (eq sheet nil)
              (push sheet cheat--all-sheets))
            ))
        ))
    ))

(defun in-sheet-buffer ()
  (let ((bname (buffer-name)))
    (or
      (string-suffix-p "Cheatsheet*" bname)
      (string-suffix-p "Cheatsheets*" bname)
    )))

(defun cheat--find-sheet-by-command (command)
  (seq-find
    (lambda (sheet)
      (string= command (cheat--command sheet)))
    (cheat--list-filtered-sheets))
)

(defun cheat--open-sheet-by-command (command)
  (let ((sheet (cheat--find-sheet-by-command command)))
    (if sheet
      (cheat--open-sheet sheet))))

(defun cheat--open-sheet (sheet)
 (cheat--open-sheet-window (cheat--title sheet) (cheat--path sheet)))

(defun cheat--open-sheet-window (wname fname)
  "Opens a buffer with the given `wname` as frame name and insert content from `fname` filename"
  (let 
    (
      (bname (concat "*" wname " Cheatsheet*"))
    )
    (if (cheat--buffer-exists bname)
      (switch-to-buffer bname)
      (let 
        (($w (split-window)))
        (select-window $w)
        (let (($b (generate-new-buffer bname)))
          (set-buffer-major-mode $b)
          (switch-to-buffer $b)
          (insert-file-contents fname)
          (org-mode)
          (read-only-mode t))))))

(defun cheat--command-as-button (command &rest properties)
  "Return a command button"
  (list command
        :supertype 'help-function
        'action (lambda (cmd) (funcall (intern command)))))

(defun cheat--sheet-as-list-entry (sheet)
  `((title    .    ,(cheat--title    sheet))
    (command  .    ,(cheat--fname  sheet))
    (category .    ,(cheat--category sheet))
    (description . ,(cheat--description sheet))
    (path     .    ,(cheat--list-path sheet))))

(defun cheat--list-sheets-buffer-entries ()
  (mapcar 'cheat--sheet-as-list-entry (cheat--list-filtered-sheets)))

(provide 'cheat-lib)

;;; cheat-lib.el ends here
