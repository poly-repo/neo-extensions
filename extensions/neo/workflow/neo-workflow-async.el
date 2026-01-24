;;; neo-workflow-async.el --- Async fetch and cache for Neo Workflow -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'async)
(require 'neo-workflow-db)

(defvar neo-workflow-refresh-hook nil
  "Hook run after async data refresh to update the UI.")

;; --------------------------
;; Async fetch helpers
;; --------------------------

(defun neo-fetch-prs-for-stack (stack callback)
  "Fetch PRs for STACK asynchronously and call CALLBACK with results."
  (async-start
   `(lambda ()
      ;; Runs in background
      (require 'json)
      (require 'subr-x)
      ;; Call 'gh' CLI to list PRs for stack branches
      (let ((stack-name ,(neo-stack-name stack)))
        (let ((json-output (shell-command-to-string
                            (format "gh pr list --json number,title,headRefName,state,mergedAt --repo %s/%s"
                                    ,(symbol-name (oref stack :repo-owner))
                                    ,(symbol-name (oref stack :repo-name))))))
          (json-parse-string json-output :object-type 'plist))))
   (lambda (result)
     ;; Runs in main thread
     (neo-db-update-prs-for-stack stack result)
     (run-hooks 'neo-workflow-refresh-hook)
     (when callback (funcall callback result)))))

(defun neo-fetch-ci-for-branch (branch callback)
  "Fetch CI/check runs for BRANCH asynchronously."
  (async-start
   `(lambda ()
      (require 'json)
      (let ((branch-name ,(neo-branch-name branch))
            (repo-owner ,(symbol-name (oref branch :repo-owner)))
            (repo-name ,(symbol-name (oref branch :repo-name))))
        (let ((json-output
               (shell-command-to-string
                (format "gh run list --branch %s --json conclusion,status,name --repo %s/%s"
                        branch-name repo-owner repo-name))))
          (json-parse-string json-output :object-type 'plist))))
   (lambda (result)
     (neo-db-update-ci-for-branch branch result)
     (run-hooks 'neo-workflow-refresh-hook)
     (when callback (funcall callback result)))))

(defun neo-fetch-issue-for-branch (branch callback)
  "Fetch Issue(s) linked to BRANCH asynchronously."
  (async-start
   `(lambda ()
      (require 'json)
      (let ((branch-name ,(neo-branch-name branch))
            (repo-owner ,(symbol-name (oref branch :repo-owner)))
            (repo-name ,(symbol-name (oref branch :repo-name))))
        ;; This assumes PR number = issue number if using GitHub PRs
        (let ((json-output
               (shell-command-to-string
                (format "gh pr view %s --json number,title,state --repo %s/%s"
                        branch-name repo-owner repo-name))))
          (json-parse-string json-output :object-type 'plist))))
   (lambda (result)
     (neo-db-update-issue-for-branch branch result)
     (run-hooks 'neo-workflow-refresh-hook)
     (when callback (funcall callback result)))))

;; --------------------------
;; Bulk refresh functions
;; --------------------------

(defun neo-refresh-stack-async (stack)
  "Fetch all data for STACK asynchronously and update DB/UI."
  ;; fetch PRs
  (neo-fetch-prs-for-stack
   stack
   (lambda (_prs)
     ;; after PRs, fetch CI for the branch in stack
     (if-let ((branch-name (neo-db-get-branch-for-stack (neo-stack-id stack))))
       (let ((branch (neo-load-branch branch-name)))
         (when branch
           (neo-fetch-ci-for-branch branch nil)
           (neo-fetch-issue-for-branch branch nil)))))))

(defun neo-refresh-all-stacks ()
  "Refresh all stacks asynchronously."
  (dolist (stack (neo-db-get-all-stacks))
    (neo-refresh-stack-async stack)))

(provide 'neo-workflow-async)
