;;; neo-workflow-ui.el --- UI for Neo Workflow Mode -*- lexical-binding: t; -*-

;; Adapted from workflow/neo-workflow-ui.el.
;; Tabulated-list summary mode kept for compat; board rendering is in
;; neo-workflow-status.el.

(require 'tabulated-list)
(require 'neo-workflow-db)
(require 'neo-workflow-models)

;;;###autoload
(define-derived-mode neo-workflow-summary-mode tabulated-list-mode "Neo Workflow"
  "Major mode to view stack-level summaries of projects."
  (setq tabulated-list-format [("Stack" 30 t)
                               ("#Branches" 10 nil)
                               ("Open PRs" 10 nil)
                               ("Merged" 10 nil)
                               ("CI OK" 8 nil)
                               ("CI Fail" 8 nil)
                               ("CI Skip" 8 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Stack" nil))
  (tabulated-list-init-header)
  (neo-workflow-refresh))

(defun neo-workflow-refresh ()
  "Refresh the tabulated-list entries for stack-level summary."
  (setq tabulated-list-entries (neo-workflow-make-stack-entries))
  (tabulated-list-print t))

(defun neo-workflow-make-stack-entries ()
  "Return tabulated-list entries, one per stack.
Data aggregated from all branches in the stack."
  (mapcar
   (lambda (stack)
     (let* ((summary (neo-stack-summary (plist-get stack :id)))
            (stack-name (plist-get stack :name))
            (row (vector stack-name
                         (number-to-string (plist-get summary :branches))
                         (number-to-string (plist-get summary :open-prs))
                         (number-to-string (plist-get summary :merged))
                         (number-to-string (plist-get summary :ci-passed))
                         (number-to-string (plist-get summary :ci-failed))
                         (number-to-string (plist-get summary :ci-skipped)))))
       (list stack-name row)))
   (neo-db-get-all-stacks)))

(defvar neo-workflow-buffer-name "*Neo Workflow*"
  "Name of the Neo Workflow buffer.")

(defun neo-workflow-open ()
  "Open the Neo Workflow buffer showing all projects."
  (interactive)
  (let ((buf (get-buffer-create neo-workflow-buffer-name)))
    (with-current-buffer buf
      (neo-workflow-summary-mode)
      (neo-workflow-refresh))
    (pop-to-buffer buf)))

(provide 'neo-workflow-ui)
;;; neo-workflow-ui.el ends here
