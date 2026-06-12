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

(ert-deftest beads-transient/filter-parent-offers-only-epics ()
  (with-temp-buffer
    (setq major-mode 'beads-list-mode)
    (let (seen-filters seen-choices)
      (cl-letf (((symbol-function 'beads-client-list)
                 (lambda (&optional filters)
                   (setq seen-filters filters)
                   '(((id . "omega-6hnb")
                      (title . "Build the Haskell version of Mlody")
                      (issue_type . "epic")
                      (labels . ["haskell" "mlody"]))
                     ((id . "omega-91i")
                      (title . "Bridge all input/output/config values")
                      (issue_type . "epic")
                      (labels . ["mlody"]))
                     ((id . "omega-6hnb.1")
                      (title . "Add a desugaring pass family with a type-sugar sub-pass")
                      (issue_type . "task")
                      (labels . ["mlody"])))))
                ((symbol-function 'completing-read)
                 (lambda (_prompt choices _predicate _require-match &rest _)
                   (setq seen-choices choices)
                   "omega-6hnb: Build the Haskell version of Mlody"))
                ((symbol-function 'beads-list-refresh)
                 (lambda (&optional _silent)
                   nil))
                ((symbol-function 'message)
                  (lambda (&rest _args)
                   nil)))
        (beads-filter-parent)
        (should (equal seen-filters '(:issue-type "epic")))
        (should
         (member "omega-6hnb: Build the Haskell version of Mlody"
                 seen-choices))
        (should
         (member "omega-91i: Bridge all input/output/config values"
                 seen-choices))
        (should-not
         (member "omega-6hnb.1: Add a desugaring pass family with a type-sugar sub-pass"
                 seen-choices))
        (should
         (funcall (beads-filter-predicate beads-list--filter)
                  '((parent . "omega-6hnb"))))
        (should-not
         (funcall (beads-filter-predicate beads-list--filter)
                  '((parent . "omega-6hnb.1"))))))))

(provide 'beads-transient-test)
;;; beads-transient-test.el ends here
