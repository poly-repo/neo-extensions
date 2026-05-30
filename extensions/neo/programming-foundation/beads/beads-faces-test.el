;;; beads-faces-test.el --- Tests for beads-faces -*- lexical-binding: t -*-

(require 'ert)
(require 'beads-faces)

(ert-deftest beads-faces/priority-number-accepts-numeric-strings ()
  (should (= (beads--priority-number "2" 9) 2))
  (should (= (beads--priority-number 1 9) 1))
  (should (= (beads--priority-number nil 9) 9)))

(ert-deftest beads-faces/priority-string-accepts-numeric-strings ()
  (should (equal (beads--priority-string "2" 9) "P2"))
  (should (equal (beads--priority-string 0 9) "P0")))

(ert-deftest beads-faces/format-priority-accepts-string-priority ()
  (should
   (equal
    (substring-no-properties
     (beads--format-priority '((priority . "1"))))
    "P1")))

(provide 'beads-faces-test)
;;; beads-faces-test.el ends here
