;;; beads-detail-test.el --- Tests for beads-detail -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'beads-detail)
(require 'beads-filter)
(require 'beads-list)

(ert-deftest beads-detail/view-children-applies-parent-filter ()
  (let ((issue '((id . "omega-6hnb")
                 (title . "Epoch: build the Haskell version of Mlody"))))
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

(provide 'beads-detail-test)
;;; beads-detail-test.el ends here
