;;; neo-workflow-context.el --- Workspace strategy for Neo Workflow -*- lexical-binding: t; -*-

;; Adapted from workflow/neo-workflow-context.el.
;; magit dependency removed; git state is read via neo-workflow-git helpers.

(require 'neo-workflow-git)

(defcustom neo/workflow-workspace-strategy 'worktree
  "Strategy used to create a workspace for issues.

Possible values:

`auto'              Choose strategy based on repository state.
`repo'              Switch branches in the current repository.
`worktree'          Create or reuse a git worktree.
`repo-if-clean'     Switch branches only if the repository is clean.
`worktree-if-dirty' Use a worktree when the repository has uncommitted changes."
  :type '(choice
          (const :tag "Auto" auto)
          (const :tag "Always use repo" repo)
          (const :tag "Always use worktree" worktree)
          (const :tag "Repo if clean" repo-if-clean)
          (const :tag "Worktree if dirty" worktree-if-dirty))
  :group 'neo-workflow)

(defun neo/workflow-dirty-p (&optional dir)
  "Return non-nil if the Git repository at DIR has uncommitted changes.
DIR defaults to `default-directory'."
  (let ((default-directory (or dir default-directory)))
    (let ((status (condition-case nil
                      (neo/workflow-git-repo-status)
                    (error nil))))
      (when status
        (or (neo-workflow-git-repo-status-open-changes status)
            (neo-workflow-git-repo-status-untracked-changes status))))))

(defun neo/workflow-clean-p (&optional dir)
  "Return non-nil if the Git repository at DIR is clean."
  (not (neo/workflow-dirty-p dir)))

(defun neo/workflow-safe-for-branch-switch-p (&optional dir)
  "Return non-nil if DIR is in a safe state for switching branches."
  (let ((default-directory (or dir default-directory)))
    (let ((status (condition-case nil
                      (neo/workflow-git-repo-status)
                    (error nil))))
      (and status
           (not (neo-workflow-git-repo-status-open-changes status))
           (not (neo-workflow-git-repo-status-conflicts status))
           (not (neo-workflow-git-repo-status-rebase-in-progress status))))))

(defun neo/workflow-state (&optional dir)
  "Return a symbol describing the Git state at DIR."
  (let ((default-directory (or dir default-directory)))
    (let ((status (condition-case nil
                      (neo/workflow-git-repo-status)
                    (error nil))))
      (cond
       ((null status) 'not-a-repo)
       ((neo-workflow-git-repo-status-rebase-in-progress status) 'rebasing)
       ((neo-workflow-git-repo-status-conflicts status) 'merging)
       ((neo/workflow-dirty-p) 'dirty)
       (t 'clean)))))

(defun neo--workflow-choose-workspace-strategy (&optional dir)
  "Return the workspace strategy symbol for DIR.
Applies `neo/workflow-workspace-strategy' to the current repo state."
  (pcase neo/workflow-workspace-strategy
    ('auto
     (if (neo/workflow-dirty-p dir) 'worktree 'repo))
    ('repo-if-clean
     (unless (neo/workflow-clean-p dir)
       (user-error "Repository is dirty"))
     'repo)
    ('worktree-if-dirty
     (if (neo/workflow-dirty-p dir) 'worktree 'repo))
    (strategy strategy)))

(provide 'neo-workflow-context)
;;; neo-workflow-context.el ends here
