;;; beads-detail-test.el --- Tests for beads-detail -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'beads-detail)
(require 'beads-filter)
(require 'beads-list)

(ert-deftest beads-detail/view-children-applies-parent-filter ()
  (let ((issue '((id . "omega-6hnb")
                 (title . "Build the Haskell version of Mlody"))))
    (with-temp-buffer
      (setq-local beads-detail--current-issue issue)
      (cl-letf (((symbol-function 'beads-list)
                 (lambda ()
                   (with-current-buffer (get-buffer-create "*Beads Issues*")
                     (unless (eq major-mode 'beads-list-mode)
                       (beads-list-mode))
                     (setq-local beads-list--filter nil))))
                ((symbol-function 'beads-list-refresh)
                 (lambda (&optional _silent)
                   nil))
                ((symbol-function 'beads-show-hint)
                 (lambda ()
                   nil))
                ((symbol-function 'message)
                 (lambda (&rest _args)
                   nil)))
        (beads-detail-view-children)
        (with-current-buffer "*Beads Issues*"
          (should beads-list--filter)
          (should
           (funcall (beads-filter-predicate beads-list--filter)
                    '((parent . "omega-6hnb"))))
          (should-not
           (funcall (beads-filter-predicate beads-list--filter)
                    '((parent . "omega-6hnb.1")))))))))

(ert-deftest beads-list/goto-issue-opens-unwrapped-show-payload ()
  (let (opened-issue)
    (cl-letf (((symbol-function 'beads-list--get-issue-at-point)
               (lambda ()
                 '((id . "omega-of13")
                   (title . "Preview title"))))
              ((symbol-function 'beads-client-request)
               (lambda (_operation _args)
                 '(((id . "omega-of13")
                    (title . "Move REPL rendering to Doc or Text")
                    (parent . "omega-6hnb")))))
              ((symbol-function 'beads-detail-open)
               (lambda (issue)
                 (setq opened-issue issue))))
      (beads-list-goto-issue)
      (should
       (equal opened-issue
              '((id . "omega-of13")
                (title . "Move REPL rendering to Doc or Text")
                (parent . "omega-6hnb")))))))

(provide 'beads-detail-test)
;;; beads-detail-test.el ends here
