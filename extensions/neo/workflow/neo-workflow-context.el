;(require 'magit)

(defcustom neo/workflow-workspace-strategy 'worktree
  "Strategy used to create a workspace for GitHub issues.

Possible values:

`auto'              Choose strategy based on repository state.
`repo'              Switch branches in the current repository.
`worktree'          Create or reuse a git worktree.
`repo-if-clean'     Switch branches only if the repository is clean.
`worktree-if-dirty' Use a worktree when the repository has uncommitted changes.

The repository is considered \"dirty\" if it has uncommitted or untracked files."
  :type '(choice
          (const :tag "Auto" auto)
          (const :tag "Always use repo" repo)
          (const :tag "Always use worktree" worktree)
          (const :tag "Repo if clean" repo-if-clean)
          (const :tag "Worktree if dirty" worktree-if-dirty))
  :group 'neo-github)

(defun neo--workflow-choose-workspace-strategy (&optional dir)
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

(defun neo/workflow-dirty-p (&optional dir)
  "Return non-nil if the Git repository at DIR has uncommitted changes.
DIR defaults to `default-directory'.  Untracked files count as dirty."
  (let ((default-directory (or dir default-directory)))
    (when (magit-toplevel)
      (or (magit-anything-modified-p)))))
;; (magit-anything-untracked-p)))))

(defun neo/workflow-clean-p (&optional dir)
  "Return non-nil if the Git repository at DIR is clean."
  (not (neo/workflow-dirty-p dir)))

(defun neo/workflow-safe-for-branch-switch-p (&optional dir)
  "Return non-nil if the repository at DIR is in a safe state for switching branches."
  (let ((default-directory (or dir default-directory)))
    (and (magit-toplevel)
         (neo/workflow-clean-p)
         (not (magit-rebase-in-progress-p))
         (not (magit-merge-in-progress-p))
         (not (magit-cherry-pick-in-progress-p))
         (not (magit-anything-modified-p)))))

(defun neo/workflow-state (&optional dir)
  "Return a symbol describing the Git state at DIR."
  (let ((default-directory (or dir default-directory)))
    (cond
     ((not (magit-toplevel))        'not-a-repo)
     ((magit-rebase-in-progress-p)  'rebasing)
     ((magit-merge-in-progress-p)   'merging)
     ((magit-cherry-pick-in-progress-p) 'cherry-picking)
     ((magit-bisect-in-progress-p)  'bisecting)
     ((neo/workflow-dirty-p)        'dirty)
     (t                             'clean))))


(provide 'neo-workflow-context)
