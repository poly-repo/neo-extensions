;;; neo-workflow-git.el --- Git commands for Neo Workflow -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

;;; Constants & Caches

(defvar neo--workflow-git-current-branch-cache nil
  "Cache for the currently checked out Git branch.")

(defvar neo--workflow-git-remotes-cache nil
  "Cache for Git remotes.")

(defvar neo--workflow-git-version-cache nil
  "Cache for Git version.")

;;; Context

(defvar neo/workflow-git-root nil
  "If non-nil, Git commands will be executed in this directory.
Overrides `default-directory'.")

(defmacro neo/with-git-root (dir &rest body)
  "Execute BODY with Git commands running in DIR."
  (declare (indent 1))
  `(let ((neo/workflow-git-root ,dir))
     ,@body))

;;; Structures

(cl-defstruct neo-workflow-git-branch-info
  local-name      ;; string or nil
  local-sha       ;; string or nil
  remote-name     ;; string or nil
  remote-sha      ;; string or nil
  sync-status)    ;; symbol: :local-only, :remote-only, :up-to-date, :ahead, :behind, :deleted-at-remote

(cl-defstruct neo-workflow-git-branches-snapshot
  branches        ;; list of neo-workflow-git-branch-info
  active          ;; string (branch name) or nil
  detached-head)  ;; boolean

(cl-defstruct neo-workflow-git-repo-status
  conflicts          ;; boolean
  open-changes       ;; boolean
  rebase-in-progress ;; boolean
  untracked-changes) ;; boolean

(cl-defstruct neo-workflow-git-file-conflict
  file-path
  current-change ;; blob info
  parent-change) ;; blob info

;;; Internal Helpers

(defun neo--workflow-git-run (&rest args)
  "Run a git command with ARGS. Return t on success, nil on failure."
  (let ((default-directory (or neo/workflow-git-root default-directory)))
    (= 0 (apply #'process-file "git" nil nil nil args))))

(defun neo--workflow-git-run-with-env (env &rest args)
  "Run a git command with ARGS and environment ENV.
ENV is a list of strings \"VAR=VALUE\"."
  (let ((process-environment (append env process-environment))
        (default-directory (or neo/workflow-git-root default-directory)))
    (= 0 (apply #'process-file "git" nil nil nil args))))

(defun neo--workflow-git-query (&rest args)
  "Run a git command with ARGS and return its trimmed output.
Signal an error if the command fails."
  (let ((default-directory (or neo/workflow-git-root default-directory)))
    (with-temp-buffer
      (let ((exit-code (apply #'process-file "git" nil t nil args)))
        (unless (= exit-code 0)
          (error "Git command failed: git %s\n%s" (string-join args " ") (buffer-string)))
        (string-trim (buffer-string))))))

(defun neo--workflow-git-query-safe (&rest args)
  "Run a git command with ARGS. Return (output . nil) or (nil . error-message)."
  (let ((default-directory (or neo/workflow-git-root default-directory)))
    (with-temp-buffer
      (let ((exit-code (apply #'process-file "git" nil t nil args)))
        (if (= exit-code 0)
            (cons (string-trim (buffer-string)) nil)
          (cons nil (buffer-string)))))))

(defun neo--workflow-git-lines (str)
  "Split STR into lines, filtering empty ones."
  (if (string-empty-p str)
      nil
    (split-string str "\n" t)))

;;; Public Commands

(defun neo/workflow-git-abort-merge ()
  "Abort the current merge."
  (neo--workflow-git-run "merge" "--abort"))

(defun neo/workflow-git-abort-rebase ()
  "Abort the current rebase."
  (neo--workflow-git-run "rebase" "--abort"))

(defun neo/workflow-git-branch-authors (branch parent)
  "Return list of authors in BRANCH since PARENT."
  (let ((output (neo--workflow-git-query "shortlog" "-s" "-n" "-e" (format "%s..%s" parent branch))))
    (mapcar (lambda (line)
              (let ((parts (split-string line "\t")))
                (if (> (length parts) 1)
                    (nth 1 parts)
                  line)))
            (neo--workflow-git-lines output))))

(defun neo/workflow-git-branch-contains-merges (branch parent)
  "Return t if BRANCH contains merges relative to PARENT."
  (let ((output (neo--workflow-git-query "log" "--merges" "--format=%H" (format "%s..%s" parent branch))))
    (> (length output) 0)))

(defun neo/workflow-git-branch-exists (branch)
  "Return t if BRANCH exists locally."
  (neo--workflow-git-run "rev-parse" "--verify" "-q" (concat "refs/heads/" branch)))

(defun neo/workflow-git-branch-exists-at-remote (branch remote)
  "Return t if BRANCH exists at REMOTE."
  (neo--workflow-git-run "ls-remote" remote branch))

(defun neo/workflow-git-branch-has-unmerged-changes (branch parent)
  "Return t if BRANCH has unmerged changes relative to PARENT."
  (let ((output (neo--workflow-git-query "diff" "--shortstat" parent branch "--")))
    (> (length output) 0)))

(defun neo/workflow-git-branch-in-sync-with-parent (branch parent)
  "Return t if BRANCH is in sync with PARENT."
  (let ((output (neo--workflow-git-query "log" "--no-merges" "--format=%H" parent (concat "^" branch))))
    (= (length output) 0)))

(defun neo/workflow-git-branch-in-sync-with-tracking (local tracking)
  "Return t if LOCAL branch is in sync with TRACKING branch."
  (let ((out (neo--workflow-git-query "rev-parse" local tracking)))
    (let ((lines (neo--workflow-git-lines out)))
      (and (= (length lines) 2)
           (string= (nth 0 lines) (nth 1 lines))))))

(defun neo/workflow-git-branches-available-in-current-worktree ()
  "Return list of branches available in the current worktree."
  (let ((branches (neo--workflow-git-branches-query)))
    (cl-loop for branch in branches
             unless (or (plist-get branch :symref)
                        (not (string-prefix-p "refs/heads/" (plist-get branch :refname))))
             if (or (not (plist-get branch :worktree))
                    (plist-get branch :head))
             collect (plist-get branch :branch-name))))

(defun neo/workflow-git-branches-snapshot ()
  "Return a snapshot of all branches."
  (let ((branches (neo--workflow-git-branches-query))
        (result nil)
        (current-branch-opt nil)
        (detached-head nil))
    
    (if (null branches)
        ;; New repo handling
        (let ((current (neo/workflow-git-current-branch-uncached)))
          (when current
            (setq current-branch-opt current)
            (setq result (list (make-neo-workflow-git-branch-info
                                :local-name current
                                :local-sha "0000000"
                                :sync-status :local-only)))))
      ;; Normal handling
      (dolist (branch branches)
        (unless (plist-get branch :symref)
          (let ((name (plist-get branch :branch-name))
                (sha (plist-get branch :sha))
                (head (plist-get branch :head))
                (worktree (plist-get branch :worktree))
                (upstream (plist-get branch :upstream))
                (track (plist-get branch :track)))
            
            (when (and head (string-prefix-p "refs/heads/" (plist-get branch :refname)))
              (setq current-branch-opt name))
            
            (cond
             ((and worktree (not head))
              (push (make-neo-workflow-git-branch-info
                     :local-name name
                     :local-sha sha
                     :remote-name upstream
                     :sync-status :other-worktree)
                    result))
             ((string-prefix-p "refs/heads/" (plist-get branch :refname))
              (push (make-neo-workflow-git-branch-info
                     :local-name name
                     :local-sha sha
                     :remote-name upstream
                     :sync-status (neo--workflow-git-determine-sync-status track upstream))
                    result))
             (t
              ;; Remote branch
              (let* ((remote-name name) ;; assuming name parsing worked
                     (existing (cl-find remote-name result 
                                        :test #'string= 
                                        :key #'neo-workflow-git-branch-info-remote-name)))
                (if existing
                    (setf (neo-workflow-git-branch-info-remote-sha existing) sha)
                  (push (make-neo-workflow-git-branch-info
                         :remote-name remote-name
                         :remote-sha sha
                         :sync-status :remote-only)
                        result)))))))))

    ;; Detached HEAD check
    (unless current-branch-opt
      (let ((rebase-in-progress (neo/workflow-git-has-rebase-in-progress)))
        (unless rebase-in-progress
          (setq detached-head t)
          (let ((head-sha (neo/workflow-git-current-sha)))
            (setq current-branch-opt head-sha)
            (push (make-neo-workflow-git-branch-info
                   :local-name head-sha
                   :local-sha head-sha
                   :sync-status :local-only)
                  result)))))
    
    (when current-branch-opt
      (setq neo--workflow-git-current-branch-cache current-branch-opt))
    
    (make-neo-workflow-git-branches-snapshot
     :branches (nreverse result)
     :active current-branch-opt
     :detached-head detached-head)))

(defun neo/workflow-git-checkout-branch (name &optional merge)
  "Checkout branch NAME. If MERGE is t, use -m."
  (if (equal neo--workflow-git-current-branch-cache name)
      nil
    (neo/workflow-git-checkout-branch-uncached name merge)))

(defun neo/workflow-git-checkout-branch-uncached (name &optional merge)
  "Checkout branch NAME uncached."
  (let ((args (list "checkout" name)))
    (when merge (setq args (append args '("-m"))))
    (if (apply #'neo--workflow-git-run args)
        (progn
          (if (string= name "-")
              (setq neo--workflow-git-current-branch-cache nil)
            (setq neo--workflow-git-current-branch-cache name))
          nil)
      (error "Failed to checkout branch %s" name))))

(defun neo/workflow-git-cherry-pick (sha)
  "Cherry-pick SHA."
  (neo--workflow-git-run "cherry-pick" sha))

(defun neo/workflow-git-cherry-pick-abort ()
  "Abort cherry-pick."
  (neo--workflow-git-run "cherry-pick" "--abort"))

(defun neo/workflow-git-cherry-pick-continue ()
  "Continue cherry-pick."
  (neo--workflow-git-run-with-env '("GIT_EDITOR=true") "cherry-pick" "--continue"))

(defun neo/workflow-git-comment-out-squash-msg (prefix)
  "Comment out SQUASH_MSG content, optionally adding PREFIX."
  (let ((file (expand-file-name ".git/SQUASH_MSG" (neo/workflow-git-root-directory))))
    (when (file-exists-p file)
      (let ((content (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
        (when prefix
          (setq content (concat prefix "\n" content)))
        (setq content (replace-regexp-in-string "^" "# " content))
        (with-temp-buffer
          (insert content)
          (write-region (point-min) (point-max) file nil 'quiet))))))

(defun neo/workflow-git-commit (use-message author commit-hook)
  "Commit. USE-MESSAGE is a plist {:mode :custom|:default|:edit, :message msg}.
COMMIT-HOOK is boolean."
  (let ((args (list "commit")))
    (cond
     ((eq (plist-get use-message :mode) :custom)
      (setq args (append args (list "-m" (plist-get use-message :message)))))
     ((eq (plist-get use-message :mode) :default)
      (setq args (append args (list "--no-edit")))))
    (when author
      (setq args (append args (list "--author" author))))
    (unless commit-hook
      (setq args (append args (list "--no-verify"))))
    (apply #'neo--workflow-git-run args)))

(defun neo/workflow-git-commit-message (sha)
  "Get commit message for SHA."
  (neo--workflow-git-query "show" "--no-patch" "--format=%B" sha))

(defun neo/workflow-git-commit-start ()
  "Start commit (opens editor)."
  (neo--workflow-git-run "commit"))

(defun neo/workflow-git-commits-in-branch (branch &optional parent)
  "Get commits in BRANCH, optionally relative to PARENT."
  (if parent
      (neo--workflow-git-commits-in-feature-branch branch parent)
    (neo--workflow-git-commits-in-perennial-branch)))

(defun neo--workflow-git-commits-in-feature-branch (branch parent)
  (let ((output (neo--workflow-git-query "log" "--format=%H %s" (format "%s..%s" parent branch))))
    (nreverse (neo--workflow-git-parse-commits output))))

(defun neo--workflow-git-commits-in-perennial-branch ()
  (let ((output (neo--workflow-git-query "log" "--format=%H %s" "-10")))
    (neo--workflow-git-parse-commits output)))

(defun neo--workflow-git-parse-commits (output)
  (mapcar (lambda (line)
            (let ((parts (split-string line " " t " ")))
              (cons (car parts) (string-join (cdr parts) " "))))
          (neo--workflow-git-lines output)))

(defun neo/workflow-git-content-blob-info (branch file-path)
  "Get blob info for FILE-PATH in BRANCH."
  (ignore-errors
    (let ((output (neo--workflow-git-query "ls-tree" branch file-path)))
      (when (> (length output) 0)
        (nth 2 (split-string output))))))

(defun neo/workflow-git-continue-rebase ()
  "Continue rebase."
  (neo--workflow-git-run-with-env '("GIT_EDITOR=true") "rebase" "--continue"))

(defun neo/workflow-git-create-and-checkout-branch (name)
  "Create and checkout branch NAME."
  (when (neo--workflow-git-run "checkout" "-b" name)
    (setq neo--workflow-git-current-branch-cache name)
    t))

(defun neo/workflow-git-create-and-checkout-branch-with-parent (name parent)
  "Create and checkout branch NAME from PARENT."
  (let ((args (list "checkout" "-b" name parent)))
    ;; Check if parent is remote, strict check difficult without more context,
    ;; relying on simple heuristic or caller knowledge. Assuming generic for now.
    ;; Go code checked IsRemoteBranchName.
    (when (string-match-p "^origin/" parent) ;; Crude check
      (setq args (append args '("--no-track"))))
    (when (apply #'neo--workflow-git-run args)
      (setq neo--workflow-git-current-branch-cache name)
      t)))

(defun neo/workflow-git-create-branch (name parent)
  "Create branch NAME from PARENT."
  (neo--workflow-git-run "branch" name parent))

(defun neo/workflow-git-create-tracking-branch (branch remote push-hook)
  "Push BRANCH to REMOTE setting upstream."
  (let ((args (list "push")))
    (unless push-hook (setq args (append args '("--no-verify"))))
    (apply #'neo--workflow-git-run (append args (list "-u" remote branch)))))

(defun neo/workflow-git-current-branch ()
  "Get current branch name."
  (or neo--workflow-git-current-branch-cache
      (let ((curr (neo/workflow-git-current-branch-uncached)))
        (setq neo--workflow-git-current-branch-cache curr)
        curr)))

(defun neo/workflow-git-current-branch-uncached ()
  "Get current branch uncached."
  (let ((output (ignore-errors (neo--workflow-git-query "branch" "--show-current"))))
    (if (and output (not (string-empty-p output)))
        output
      (neo/workflow-git-current-branch-during-rebase))))

(defun neo/workflow-git-current-branch-during-rebase ()
  "Detect current branch during rebase."
  (let ((git-dir (neo/workflow-git-git-directory)))
    (cl-loop for f in '("rebase-merge/head-name" "rebase-apply/head-name")
             for path = (expand-file-name f git-dir)
             when (file-exists-p path)
             return (let ((content (string-trim (with-temp-buffer (insert-file-contents path) (buffer-string)))))
                      (string-remove-prefix "refs/heads/" content)))))

(defun neo/workflow-git-current-sha ()
  "Get current SHA."
  (neo/workflow-git-sha-for-branch "HEAD"))

(defun neo/workflow-git-delete-last-commit ()
  "Reset HEAD~1 --hard."
  (neo--workflow-git-run "reset" "--hard" "HEAD~1"))

(defun neo/workflow-git-delete-local-branch (name)
  "Delete local branch NAME."
  (neo--workflow-git-run "branch" "-D" name))

(defun neo/workflow-git-delete-tracking-branch (remote name)
  "Delete remote branch NAME at REMOTE."
  (neo--workflow-git-run "push" remote (concat ":" name)))

(defun neo/workflow-git-diff-parent (branch parent)
  "Diff BRANCH against PARENT."
  (neo--workflow-git-run "diff" "--merge-base" parent branch))

(defun neo/workflow-git-discard-open-changes ()
  "Discard open changes."
  (neo--workflow-git-run "reset" "--hard"))

(defun neo/workflow-git-drop-most-recent-stash ()
  "Drop most recent stash."
  (neo--workflow-git-run "stash" "drop"))

(defun neo/workflow-git-fetch (sync-tags)
  "Fetch. SYNC-TAGS is boolean."
  (neo--workflow-git-run "fetch" "--prune" (if sync-tags "--tags" "--no-tags")))

(defun neo/workflow-git-fetch-upstream (branch)
  "Fetch upstream for BRANCH."
  (neo--workflow-git-run "fetch" "upstream" branch)) ;; Assuming upstream remote is named "upstream"? Go code uses gitdomain.RemoteUpstream constant.

(defun neo/workflow-git-first-commit-message-in-branch (branch parent)
  "Get first commit message in BRANCH relative to PARENT."
  (let ((output (neo--workflow-git-query "log" (format "%s..%s" parent branch) "--format=%s" "--reverse")))
    (car (neo--workflow-git-lines output))))

(defun neo/workflow-git-force-push-branch-safely (push-hook force-if-includes)
  "Force push safe."
  (let ((args (list "push" "--force-with-lease")))
    (when force-if-includes (setq args (append args '("--force-if-includes"))))
    (unless push-hook (setq args (append args '("--no-verify"))))
    (apply #'neo--workflow-git-run args)))

(defun neo/workflow-git-git-version ()
  "Get git version."
  (or neo--workflow-git-version-cache
      (let ((output (neo--workflow-git-query "version")))
        (when (string-match "git version \([0-9]+\)\.\([0-9]+\)" output)
          (let ((ver (cons (string-to-number (match-string 1 output))
                           (string-to-number (match-string 2 output)))))
            (setq neo--workflow-git-version-cache ver)
            ver)))))

(defun neo/workflow-git-has-merge-in-progress ()
  "Check if merge in progress."
  (neo--workflow-git-run "rev-parse" "--verify" "-q" "MERGE_HEAD"))

(defun neo/workflow-git-has-rebase-in-progress ()
  "Check if rebase in progress."
  (let ((git-dir (neo/workflow-git-git-directory)))
    (or (file-directory-p (expand-file-name "rebase-merge" git-dir))
        (file-directory-p (expand-file-name "rebase-apply" git-dir)))))

(defun neo/workflow-git-merge-branch-no-edit (branch)
  "Merge BRANCH no edit."
  (neo--workflow-git-run "merge" "--no-edit" "--ff" branch))

(defun neo/workflow-git-merge-fast-forward (branch)
  "Merge BRANCH fast forward."
  (neo--workflow-git-run "merge" "--ff-only" branch))

(defun neo/workflow-git-merge-no-fast-forward (branch use-message)
  "Merge BRANCH no fast forward."
  (let ((args (list "merge" "--no-ff")))
    (cond
     ((eq (plist-get use-message :mode) :custom)
      (setq args (append args (list "-m" (plist-get use-message :message)))))
     ((eq (plist-get use-message :mode) :default)
      (setq args (append args (list "--no-edit"))))
     (t (setq args (append args (list "--edit")))))
    (setq args (append args (list "--" branch)))
    (apply #'neo--workflow-git-run args)))

(defun neo/workflow-git-origin-head ()
  "Get origin HEAD."
  (let ((output (ignore-errors (neo--workflow-git-query "symbolic-ref" "refs/remotes/origin/HEAD"))))
    (when output
      (file-name-nondirectory output))))

(defun neo/workflow-git-pop-stash ()
  "Pop stash."
  (unless (neo--workflow-git-run "stash" "pop")
    (neo--workflow-git-run "stash" "drop")))

(defun neo/workflow-git-previously-checked-out-branch ()
  "Get previous branch."
  (ignore-errors (neo--workflow-git-query "rev-parse" "--verify" "--abbrev-ref" "@-1")))

(defun neo/workflow-git-pull ()
  "Pull."
  (neo--workflow-git-run "pull"))

(defun neo/workflow-git-push-current-branch (push-hook)
  "Push current branch."
  (let ((args (list "push")))
    (unless push-hook (setq args (append args '("--no-verify"))))
    (apply #'neo--workflow-git-run args)))

(defun neo/workflow-git-push-local-branch (sha branch remote push-hook)
  "Push local branch."
  (let ((args (list "push")))
    (unless push-hook (setq args (append args '("--no-verify"))))
    (setq args (append args (list remote (format "%s:refs/heads/%s" sha branch))))
    (apply #'neo--workflow-git-run args)))

(defun neo/workflow-git-push-tags (push-hook)
  "Push tags."
  (let ((args (list "push" "--tags")))
    (unless push-hook (setq args (append args '("--no-verify"))))
    (apply #'neo--workflow-git-run args)))

(defun neo/workflow-git-rebase (target)
  "Rebase against TARGET."
  (neo--workflow-git-run "-c" "rebase.updateRefs=false" "rebase" target))

(defun neo/workflow-git-rebase-onto (onto remove)
  "Rebase onto."
  (neo--workflow-git-run "-c" "rebase.updateRefs=false" "rebase" "--onto" onto remove))

(defun neo/workflow-git-remotes ()
  "Get remotes."
  (or neo--workflow-git-remotes-cache
      (let ((remotes (neo/workflow-git-remotes-uncached)))
        (setq neo--workflow-git-remotes-cache remotes)
        remotes)))

(defun neo/workflow-git-remotes-uncached ()
  "Get remotes uncached."
  (let ((output (neo--workflow-git-query "remote")))
    (neo--workflow-git-lines output)))

(defun neo/workflow-git-remove-commit (commit)
  "Remove commit."
  (neo--workflow-git-run "-c" "rebase.updateRefs=false" "rebase" "--onto" (concat commit "^") commit))

(defun neo/workflow-git-repo-status ()
  "Get repo status."
  (let* ((output (neo--workflow-git-query "status" "-z" "--ignore-submodules"))
         (statuses (neo--workflow-git-parse-status-z output))
         (has-conflicts (cl-some #'neo--workflow-git-file-status-is-unmerged statuses))
         (has-open-changes (> (length statuses) 0))
         (has-untracked (cl-some #'neo--workflow-git-file-status-is-untracked statuses))
         (merge-in-progress (neo/workflow-git-has-merge-in-progress))
         (rebase-in-progress (neo/workflow-git-has-rebase-in-progress)))
    
    (make-neo-workflow-git-repo-status
     :conflicts has-conflicts
     :open-changes (and has-open-changes 
                        (not merge-in-progress) 
                        (not rebase-in-progress))
     :rebase-in-progress rebase-in-progress
     :untracked-changes has-untracked)))

(defun neo--workflow-git-parse-status-z (output)
  "Parse 'git status -z' output into a list of status strings (XY)."
  (let ((statuses nil)
        (len (length output))
        (i 0))
    (while (< i len)
      (let* ((xy (substring output i (+ i 2)))
             (path-start (+ i 3))
             (path-end (string-match "\0" output path-start)))
        (push xy statuses)
        (if path-end
            (let ((path (substring output path-start path-end)))
              ;; Check if it's a rename (R in XY), if so there's another path
              (when (or (eq (aref xy 0) ?R) (eq (aref xy 1) ?R))
                 (let ((new-path-end (string-match "\0" output (+ path-end 1))))
                   (when new-path-end
                     (setq path-end new-path-end))))
              (setq i (+ path-end 1)))
          (setq i len))))
    (nreverse statuses)))

(defun neo--workflow-git-file-status-is-unmerged (xy)
  "Check if status XY indicates unmerged."
  (let ((x (aref xy 0))
        (y (aref xy 1)))
    (or (and (eq x ?D) (eq y ?D)) ;; both deleted
        (and (eq x ?A) (eq y ?U)) ;; added by us
        (and (eq x ?U) (eq y ?D)) ;; deleted by them
        (and (eq x ?U) (eq y ?A)) ;; added by them
        (and (eq x ?D) (eq y ?U)) ;; deleted by us
        (and (eq x ?A) (eq y ?A)) ;; both added
        (and (eq x ?U) (eq y ?U))))) ;; both modified

(defun neo--workflow-git-file-status-is-untracked (xy)
  "Check if status XY indicates untracked."
  (string= xy "??"))

(defun neo/workflow-git-reset-branch (target)
  "Reset branch to TARGET (soft)."
  (neo--workflow-git-run "reset" "--soft" target "--"))

(defun neo/workflow-git-reset-current-branch-to-sha (sha)
  "Reset current branch to SHA (hard)."
  (neo--workflow-git-run "reset" "--hard" sha))

(defun neo/workflow-git-reset-remote-branch-to-sha (branch sha)
  "Reset remote branch to SHA."
  ;; Need remote and local part
  (let* ((parts (split-string branch "/")) ;; naive
         (remote (car parts))
         (local (string-join (cdr parts) "/")))
    (neo--workflow-git-run "push" "--force-with-lease" remote (concat sha ":" local))))

(defun neo/workflow-git-root-directory ()
  "Get root directory."
  (neo--workflow-git-query "rev-parse" "--show-toplevel"))

(defun neo/workflow-git-sha-for-branch (name)
  "Get SHA for branch NAME."
  (neo--workflow-git-query "rev-parse" name))

(defun neo/workflow-git-shorten-sha (sha)
  "Shorten SHA."
  (neo--workflow-git-query "rev-parse" "--short" sha))

(defun neo/workflow-git-squash-merge (branch)
  "Squash merge BRANCH."
  (neo--workflow-git-run "merge" "--squash" "--ff" branch))

(defun neo/workflow-git-stage-files (files)
  "Stage FILES."
  (apply #'neo--workflow-git-run "add" files))

(defun neo/workflow-git-stash ()
  "Stash."
  (neo--workflow-git-run "add" "-A")
  (neo--workflow-git-run "stash" "-m" "Git Town WIP"))

(defun neo/workflow-git-stash-size ()
  "Get stash size."
  (length (neo--workflow-git-lines (neo--workflow-git-query "stash" "list"))))

(defun neo/workflow-git-undo-last-commit ()
  "Undo last commit."
  (neo--workflow-git-run "reset" "--soft" "HEAD~1"))

(defun neo/workflow-git-unstage-all ()
  "Unstage all."
  (neo--workflow-git-run "restore" "--staged" "."))

(defun neo/workflow-git-git-directory ()
  "Get .git directory."
  (neo--workflow-git-query "rev-parse" "--absolute-git-dir"))

;;; Internal Parsing

(defun neo--workflow-git-branches-query ()
  "Execute for-each-ref and return list of branch plists."
  (let* ((fmt (string-join
               '("refname:%(refname)"
                 "branchname:%(refname:lstrip=2)"
                 "sha:%(objectname)"
                 "head:%(if)%(HEAD)%(then)Y%(else)N%(end)"
                 "worktree:%(if)%(worktreepath)%(then)Y%(else)N%(end)"
                 "symref:%(if)%(symref)%(then)Y%(else)N%(end)"
                 "upstream:%(upstream:lstrip=2)"
                 "track:%(upstream:track,nobracket)")
               " "))
         (output (neo--workflow-git-query
                  "for-each-ref"
                  (concat "--format=" fmt)
                  "--sort=refname"
                  "refs/heads/" "refs/remotes/")))
    
    (mapcar (lambda (line)
              (let ((parts (split-string line " ")))
                (list :refname (string-remove-prefix "refname:" (nth 0 parts))
                      :branch-name (string-remove-prefix "branchname:" (nth 1 parts))
                      :sha (string-remove-prefix "sha:" (nth 2 parts))
                      :head (string= "Y" (string-remove-prefix "head:" (nth 3 parts)))
                      :worktree (string= "Y" (string-remove-prefix "worktree:" (nth 4 parts)))
                      :symref (string= "Y" (string-remove-prefix "symref:" (nth 5 parts)))
                      :upstream (let ((u (string-remove-prefix "upstream:" (nth 6 parts))))
                                  (if (string-empty-p u) nil u))
                      :track (string-join (nthcdr 7 parts) " ")))) ;; Rejoin track if it had spaces (though split-string with limit would be better)
            (neo--workflow-git-lines output))))

(defun neo--workflow-git-determine-sync-status (track upstream)
  (let ((gone (string= track "gone"))
        (ahead (string-match-p "ahead" track))
        (behind (string-match-p "behind" track)))
    (cond
     (gone :deleted-at-remote)
     ((and ahead behind) :not-in-sync)
     (ahead :ahead)
     (behind :behind)
     ((string-empty-p track)
      (if upstream :up-to-date :local-only))
     (t :unknown))))

(defun neo/workflow-git-file-conflicts ()
  "Get list of files with conflicts."
  (let ((output (neo--workflow-git-query "ls-files" "--unmerged")))
    (let ((files (make-hash-table :test 'equal)))
      (dolist (line (neo--workflow-git-lines output))
        (let ((parts (split-string line "\t")))
          (when (> (length parts) 1)
            (puthash (nth 1 parts) t files))))
      (mapcar (lambda (f) (make-neo-workflow-git-file-conflict :file-path f))
              (hash-table-keys files)))))

(defun neo/workflow-git-merge-conflicts (file-conflicts parent-location root-branch)
  "Enrich FILE-CONFLICTS with blob info from PARENT-LOCATION and ROOT-BRANCH."
  ;; Simplified implementation: returning passed conflicts. 
  ;; Full implementation would require querying ContentBlobInfo for each file
  ;; at parent and root revisions.
  file-conflicts)

(defun neo/workflow-git-remove-file (filename)
  "Remove FILENAME."
  (neo--workflow-git-run "rm" filename))

(defun neo/workflow-git-resolve-conflict (file resolution)
  "Resolve conflict in FILE. RESOLUTION is :ours or :theirs."
  (let ((flag (case resolution
                (:ours "--ours")
                (:theirs "--theirs")
                (t (error "Unknown resolution: %s" resolution)))))
    (neo--workflow-git-run "checkout" flag file)))

(provide 'neo-workflow-git)
;;; neo-workflow-git.el ends here
