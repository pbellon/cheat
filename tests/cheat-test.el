(require 'cheat-lib)

(setq root "~/Dev/cheat/tests/")
(setq test-sheets-dir (concat root "test-sheets"))

(setq cheat/sheets-folders `(,test-sheets-dir))
;; (message "cheat/version: %s" cheat/version)
;; (message "sheets dir %s" test-sheets-dir)
;; (message "sheets folder %s" cheat/sheets-folders)

(defun fake-sheet ()
  '(("name"    . "Fake")
    ("path"    . "my/fake/path.org")
    ("title"   . "Fake Title")
    ("command" . "fake")))

(ert-deftest test-get-sheets-in ()
  (should (= (length (get-sheets-in test-sheets-dir)) 3)))

(ert-deftest test-parse-sheet ()
  (let ((sheet (parse-sheet (concat test-sheets-dir "/first.org"))))
    (should 
     (and
      (equal (cheat-title sheet) "First Cheatsheet")
      (equal (cheat-command sheet) "first")
      (equal (cheat-description sheet) "My first cheatsheet")
      ))))

(ert-deftest test-reload-sheets ()
  (setq cheat/sheets nil)
  (reload-sheets)
  (should (= (length cheat/sheets) 3)))

(ert-deftest test-reload-sheets-multiple ()
  (setq cheat/sheets nil)
  (reload-sheets)
  (reload-sheets)
  (reload-sheets)
  (should (= (length cheat/sheets) 3)))

(ert-deftest test-cheat-path ()
  (should (equal (cheat-path (fake-sheet)) "my/fake/path.org")))

(ert-deftest test-cheat-name ()
  (should (equal (cheat-name (fake-sheet)) "Fake")))

(ert-deftest test-cheat-title ()
  (should (equal (cheat-title (fake-sheet)) "Fake Title")))

(ert-deftest test-cheat-command ()
  (should (equal (cheat-command (fake-sheet)) "fake")))

(ert-deftest test-cheat-fn-name ()
  (should (equal (cheat-fn-name (fake-sheet)) "cheat/fake")))
