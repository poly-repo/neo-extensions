;;; beads-filter-test.el --- Tests for beads-filter -*- lexical-binding: t -*-

(require 'ert)
(require 'beads-filter)

(ert-deftest beads-filter/by-priority-accepts-string-priority ()
  (let ((filter (beads-filter-by-priority 2)))
    (should
     (funcall (beads-filter-predicate filter)
              '((priority . "2"))))
    (should-not
     (funcall (beads-filter-predicate filter)
              '((priority . "3"))))))

(ert-deftest beads-filter/by-parent-accepts-parent-alias ()
  (let ((filter (beads-filter-by-parent "omega-6hnb")))
    (should
     (funcall (beads-filter-predicate filter)
              '((parent . "omega-6hnb"))))
    (should
     (funcall (beads-filter-predicate filter)
              '((parent_id . "omega-6hnb"))))
    (should-not
     (funcall (beads-filter-predicate filter)
              '((parent . "omega-other"))))))

(provide 'beads-filter-test)
;;; beads-filter-test.el ends here
