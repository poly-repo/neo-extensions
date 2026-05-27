;;; beads-transient-test.el --- Tests for beads-transient -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'beads-transient)
(require 'beads-filter)

(ert-deftest beads-transient/status-filter-default-uses-status-value ()
  (should
   (equal
    (beads--status-filter-default
     '(:config (:type status :value "open")))
    "open")))

(ert-deftest beads-transient/status-filter-default-maps-blocked-filter ()
  (should
   (equal
    (beads--status-filter-default
     '(:config (:type blocked :value t)))
    "blocked")))

(ert-deftest beads-transient/status-filter-default-ignores-non-string-values ()
  (should-not
   (beads--status-filter-default
    '(:config (:type ready :value t)))))

(ert-deftest beads-transient/filter-status-uses-string-default ()
  (with-temp-buffer
    (setq major-mode 'beads-list-mode)
    (setq-local beads-list--filter (beads-filter-blocked))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _choices _predicate _require-match
                        &optional initial-input &rest _)
                 (should (stringp initial-input))
                 (should (equal initial-input "blocked"))
                 "all"))
              ((symbol-function 'beads-list-refresh)
               (lambda (&optional _silent)
                 nil)))
      (beads-filter-status)
      (should-not beads-list--filter))))

(provide 'beads-transient-test)
;;; beads-transient-test.el ends here
