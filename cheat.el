(defconst cheat/root (file-name-directory load-file-name))

(defun cheat/sheet-path (name)
  "Get the absolute file path for a given sheet org file"
  (expand-file-name (concat "./sheets/" name) cheat/root))

(defconst cheat/sheets 
  '((adoc "Asciidoc" (cheat/sheet-path "asciidoc.org"))
    (org "OrgMode" (cheat/sheet-path "org-mode.org"))))

(defun buffer-exists (bname)
  (not (eq nil (get-buffer bname))))

(defun cheat/open (wname fname)
  (let ((bname (concat "* " wname " Cheatsheet *")))
    (if (buffer-exists bname)
      (switch-to-buffer bname)
      (let (($b (generate-new-buffer bname)))
        (set-buffer-major-mode $b)
        (switch-to-buffer $b)
        (insert-file-contents fname)
        (org-mode)
        (read-only-mode t)))))

(defun cheat/register (cheat-name cheat-wname cheat-fname)
  "Register a new cheatsheet, will create cheat/<name> interactive function"
  (let ((cheat-fn-name (intern (format "cheat/%s" cheat-name))))
    `(defun ,cheat-fn-name ()
       (interactive)
       (cheat/open ,cheat-wname ,cheat-fname))))

(defvar cheat/org-entities
   '(
     ("lsbr" "\\[" nil "&#91;" "[" "[" "[")
     ("rsbr" "\\]" nil "&#93;" "]" "]" "]")
     ("zwsp" "\\hspace{0pt}" "&#8203;" "" "​" "​")))

(defmacro cheat/init ()
  `(progn ,@(mapcar
             (lambda (x)
               (cheat/register (nth 0 x) (nth 1 x) (nth 2 x)))
             cheat/sheets)))

(with-eval-after-load 'org
  (dolist (symbol cheat/org-entities)
           (add-to-list 'org-entities-user symbol)))

(provide 'cheat)
