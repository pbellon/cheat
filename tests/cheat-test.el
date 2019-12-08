(require 'cheat-lib)

(defun before-test ()
  "Clean variables before test"
  (setq root "./tests/")
  (setq cheat--all-sheets nil)
  (setq test-sheets-dir (concat root "test-sheets"))
  (setq cheat--sheets-folders `(,test-sheets-dir))
  (setq cheat--categories '("First" "Second"))
)

(defun fake-sheet ()
  '(("name"    . "Fake")
    ("path"    . "my/fake/path.org")
    ("title"   . "Fake Title")
    ("command" . "fake")))

(ert-deftest test-cheat--get-sheets-in ()
  (before-test)
  (should (= (length (cheat--get-sheets-in test-sheets-dir)) 3)))

(ert-deftest test-cheat--parse-sheet ()
  (let ((sheet (cheat--parse-sheet (concat test-sheets-dir "/first.org"))))
    (should 
     (and
      (equal (cheat--title sheet) "First Cheatsheet")
      (equal (cheat--command sheet) "first")
      (equal (cheat--description sheet) "My first cheatsheet")
      ))))

(ert-deftest test-cheat--update-sheets-list ()
  (before-test)
  (cheat--update-sheets-list)
  (should (= (length cheat--all-sheets) 3)))

(ert-deftest test-cheat--update-sheets-list-multiple ()
  (before-test)
  (cheat--update-sheets-list)
  (cheat--update-sheets-list)
  (cheat--update-sheets-list)
  (should (= (length cheat--all-sheets) 3)))

(ert-deftest test-cheat--path ()
  (should (equal (cheat--path (fake-sheet)) "my/fake/path.org")))

(ert-deftest test-cheat--name ()
  (should (equal (cheat--name (fake-sheet)) "Fake")))

(ert-deftest test-cheat--title ()
  (should (equal (cheat--title (fake-sheet)) "Fake Title")))

(ert-deftest test-cheat--command ()
  (should (equal (cheat--command (fake-sheet)) "fake")))

(ert-deftest test-cheat--fname ()
  (should (equal (cheat--fname (fake-sheet)) "cheat/fake")))

(ert-deftest test-cheat--list-filtered-sheets ()
  (before-test)
  (cheat--update-sheets-list)
  (setq cheat--categories '("First"))
  (should (= (length (cheat--list-filtered-sheets)) 2))
  (setq cheat--categories '("Second"))
  (should (= (length (cheat--list-filtered-sheets)) 1)))

(ert-deftest test-cheat--declare-all-functions ()
  (before-test)
  (setq cheat--categories '("First"))
  (cheat--update-sheets-list)
  (should (fboundp 'cheat/first))
  (should (= (length cheat--all-registered-sheets) 2))
  (setq cheat--categories '("Second"))
  (cheat--update-sheets-list)
  (should (not (fboundp 'cheat/first)))
  (should (fboundp 'cheat/third))
)
