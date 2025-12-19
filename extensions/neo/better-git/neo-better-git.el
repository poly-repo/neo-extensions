;;; -*- lexical-binding: t -*-

(neo/use-package transient)

(neo/use-package magit
  :config
  (setq magit-save-repository-buffers 'dontask)
  ;; TODO we should handle this in a less ad-hoc way. One possibility is use-package keychord here, but this can be done only
  ;; when we're able to merge multiple use-package for the same package
  (when (featurep 'key-chord) (key-chord-define-global "//" 'magit-status))
  (transient-append-suffix 'magit-branch "C"
    '("K" "delete all merged" neo/delete-merged-branches))
  :custom
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function 'magit-restore-window-configuration)
  (magit-list-refs-sortby "-creatordate") ; doesn't seem to have any effect
  (magit-refs-show-commit-count 'branch) ; may be too expsive
  (magit-completing-read-function 'magit-builtin-completing-read)
  (magit-read-worktree-directory-function #'magit-read-worktree-directory-offsite)
  (git-commit-summary-max-length 120)
;  (project-switch-commands 'magit-project-status) ; TODO replace with a function that does more (switch to perspective if one available, show a readme o.org file if available, otherwise magit status
  (magit-status-show-untracked-files 'all)
  (magit-section-initial-visibility-alist '(
					    (worktrees . show)
					    (unstaged . show)
					    (untracked . show)
					    (unpushed . show)
					    (stashes . hide)
					    (commit . hide)
					    (branches . hide)))
  (magit-section-cache-visibility t)
  
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands
		 (list #'magit-project-status "Magit" 109) ;; add Git with char 'm'
		 t))

  ;; NOTE: order of functions in this hook is important, we make this clear with setq
  ;; instead of gambling with add-hook. There's magit-add-section-hook that might be better.
  ;; TODO: find the right place for magit-insert-branch-description (might also be
  ;; useful in magit-refs-sections-hook)
  ;; NOTE: apparently magit-add-section-hook is there to solve the ordering problem. TODO: investigate.
  (setq magit-status-sections-hook
        '(magit-insert-worktrees
	  magit-insert-status-headers
          magit-insert-merge-log
          magit-insert-rebase-sequence
          magit-insert-am-sequence
          magit-insert-sequencer-sequence
          magit-insert-bisect-output
          magit-insert-bisect-rest
          magit-insert-bisect-log
          magit-insert-untracked-files
          magit-insert-unstaged-changes
          magit-insert-staged-changes
          magit-insert-stashes
          magit-insert-unpushed-to-pushremote
          magit-insert-unpushed-to-upstream-or-recent
          magit-insert-unpulled-from-pushremote
          magit-insert-unpulled-from-upstream
          magit-insert-modules
          magit-insert-local-branches))

  ;; TODO <f12> on laptop is problematic as one would probably have media keys
  :bind
  ("<f12> s" . 'magit-status)
  ("<f12> g" . 'counsel-git-grep))

(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "w")
              #'neo/magit-worktree-or-issue-action))

(defconst neo/git-branch-stop-words
  '("a" "an" "the" "and" "or" "to" "of" "in" "on" "for"
    "with" "when" "it" "is" "be" "also" "should"))

(defun neo/git--slugify (title &optional max-words)
  "Turn TITLE into a short, human-readable git-safe slug."
  (let* ((max-words (or max-words 6))
         (s (downcase title)))
    (setq s (replace-regexp-in-string "[^a-z0-9 ]" "" s))
    (setq s (split-string s "[[:space:]]+" t))
    (setq s (seq-remove
             (lambda (w) (member w neo/git-branch-stop-words))
             s))
    (mapconcat #'identity (seq-take s max-words) "-")))

(defun neo/git-branch-from-issue (num title)
  "Generate a git branch name from NUM and TITLE."
  (format "%s-%s/%s"
	  (user-login-name)
          num
          (neo/git--slugify title)))

(defun magit-worktree-create (branch directory &optional start-point)
  "Create a new worktree for BRANCH at DIRECTORY.
START-POINT defaults to HEAD. Does not visit the directory.
Assumes BRANCH is already sanitized for filesystem use."
  (let ((start (or start-point "HEAD"))
        (expanded-dir (magit--expand-worktree directory)))
    ;; Ensure parent directories exist
    (unless (file-directory-p (file-name-directory expanded-dir))
      (make-directory (file-name-directory expanded-dir) t))
    ;; Create worktree
    (if (zerop (magit-run-git "worktree" "add" "-b" branch expanded-dir start))
        expanded-dir
      (error "Failed to create worktree %s at %s" branch expanded-dir))))

(defun neo/magit-worktree-create (directory branch &optional start-point)
  "Create a worktree for BRANCH at DIRECTORY, only if it doesn't already exist."
  (let* ((start (or start-point "HEAD"))
         (expanded-dir (magit--expand-worktree directory))
         (existing-branch (magit-git-string "rev-parse" "--verify" branch)))
    ;; If the directory exists, assume the worktree exists
    (if (file-directory-p expanded-dir)
        expanded-dir
      ;; Ensure parent directories exist
      (unless (file-directory-p (file-name-directory expanded-dir))
        (make-directory (file-name-directory expanded-dir) t))
      ;; If branch already exists, create worktree without -b
      (if existing-branch
          (if (zerop (magit-run-git "worktree" "add" expanded-dir branch))
              expanded-dir
            (error "Failed to create worktree %s at %s" branch expanded-dir))
        ;; Branch doesn't exist: create with -b
        (if (zerop (magit-run-git "worktree" "add" "-b" branch expanded-dir start))
            expanded-dir
          (error "Failed to create worktree %s at %s" branch expanded-dir))))))


(defun neo/switch-to-project (path)
  (projectile-add-known-project path)
  (projectile-invalidate-cache nil)
  (projectile-switch-project-by-name path))

(defun neo/ensure-workspace ()
  (magit-section-case
    (worktree
     (neo/switch-to-project (oref it value)))
    (issue
     (let* ((issue (oref it value))
            (id (oref issue number))
            (title (oref issue title))
            (branch (neo/git-branch-from-issue id title))
            (directory (expand-file-name
			(replace-regexp-in-string "/" "-" branch)
			"~/.local/share/wtrees/")))
       (neo/magit-worktree-create directory branch)
       (neo/switch-to-project directory)))))

(defun neo/magit-worktree-or-issue-action ()
  "Run projectile-switch-project when on worktree or issue sections."
  (interactive)
  (neo/ensure-workspace))

(with-eval-after-load 'magit
  ;; Refresh the repo status buffer whenever you save a file in that repo.
  (add-hook 'after-save-hook #'magit-after-save-refresh-status t))

(require 'neo-better-git-brancher)

;; (defun neo/magit-branches-safe-to-delete ()
;;   "Return a list of local branches that can be safely deleted.
;; A branch is safe to delete if all its commits are in main or the merge base."
;;   (let ((branches (magit-list-local-branch-names))
;;         (safe-branches '()))
;;     (dolist (branch branches)
;;       (unless (string= branch "main")
;;         (when (or (neo/magit-cherry-check-safe branch)
;;                   (neo/magit-no-diff-with-main branch))
;;           (push branch safe-branches))))
;;       safe-branches))

;; (defun neo/magit-no-diff-with-main (branch)
;;   "Check if BRANCH has no diffs with main. Return t if there are no diffs."
;;   (let ((diff-output (magit-git-lines "diff" (concat branch "...main"))))
;;     (not diff-output)))  ; Return t if diff-output is nil, meaning no diffs.

;; (defun neo/magit-cherry-check-safe (branch)
;;   "Check if BRANCH has any commits not in the merge base with main.
;; Return t if the branch is safe to delete (i.e., no commits are missing from main)."
;;   (let* ((merge-base (magit-git-string "merge-base" "main" branch))
;;          (branch-tree (magit-git-string "rev-parse" (concat branch "^{tree}")))
;;          (commit-id (magit-git-string "commit-tree" branch-tree "-p" merge-base "-m" "_"))
;;          (cherry-output (magit-git-lines "cherry" "main" commit-id)))
;;     (seq-every-p (lambda (line) (string-prefix-p "-" line)) cherry-output)))

(defun neo/delete-merged-branches ()
  "Delete branches that are safe to delete, after a silent fetch."
  (interactive)
  ;; fetch all remotes quietly to update origin/* refs
  (with-temp-buffer
    (magit-git-insert "fetch" "--all" "--prune" "--quiet"))
  ;; compute deletable branches
  (let ((branches-to-delete (neo/magit-branches-safe-to-delete-configurable)))
    (if branches-to-delete
        (if (yes-or-no-p (concat "Delete branches? ["
                                 (mapconcat 'identity branches-to-delete ", ") "]"))
            (magit-branch-delete branches-to-delete t))
      (message "Nothing to delete"))))

;; (defun neo/delete-merged-branches ()
;;   (interactive)
;;   (magit-fetch-all-prune)
;;   (let ((branches-to-delete (neo/magit-branches-safe-to-delete-configurable)))
;;     (if branches-to-delete
;;         (if (yes-or-no-p (concat "Delete branches? ["
;;                                  (mapconcat 'identity branches-to-delete ", ") "]"))
;;             (magit-branch-delete branches-to-delete t))
;;       (message "Nothing to delete"))))



;;; TODO find a good binding for forge-post-submit otherwise markdown mode takes over C-c C-c
;;; maybe C-x #
(neo/use-package forge
  :after magit
  :config
  (set-face-attribute 'forge-issue-completed nil :strike-through t)
  (setq forge-topic-list-columns
        '(("#" 5 forge-topic-list-sort-by-number
           ;           (lambda (a b) (> (car a) (car b)))
           (:right-align t) number nil)
          ("Title" 40 t nil title nil)
          ("Milestone" 10 t nil milestone nil)
          ("State" 10 t nil state nil)
          ("Updated" 10 t nil updated nil)))
  (add-to-list
   'forge-alist
   '("poly-repo.github.com"
     "api.github.com"
     "github.com"
     forge-github-repository))
  (add-to-list
   'forge-alist
   '("kaspar4.github.com"
     "api.github.com"
     "github.com"
     forge-github-repository))
  )

(use-package gptel
  :init
  (setq-default gptel-default-mode 'markdown-mode
		gptel-post-response-functions #'gptel-end-of-response
		gptel-model "gemini-2.5-pro" ; Pick your default model
		gptel-backend (gptel-make-gemini "Gemini"
						 :key (auth-source-pick-first-password :host "gemini.google.com" :user "token")
						 :stream t)))

(use-package gptel-magit
  :ensure t
  :hook (magit-mode . gptel-magit-install))

(neo/use-package git-timemachine)

;; TODO offer defaults for commit types and scopes if files at the top of the project are not available (maybe scope could be derived from the files touched as well)
;; TODO Move all the commit stuff to a :required neo-better-git-commit.el file.
;; TODO for now we hardcoded scopes
(defun neo/load-commit-scopes (file)
  "Load commit scopes from FILE, one per line."
  (if (not (file-exists-p file))
      (message "CANNOT FIND %s [cwd=%s]" file default-directory))
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (split-string (buffer-string) "\n" t "^[ \t]*"))))


(defvar neo/commit-types
  '(("feat" . ("✨"  . "A new feature"))
    ("fix" . ("🐛" ."A bug fix"))
    ("refactor" . ("♻" . "A code change that neither fixes a bug nor adds a feature"))
    ("docs" . ("📝" . "Documentation only changes"))
    ("test" . ("🧪" . "Adding missing tests or correcting existing tests"))
    ("build" . ("🧰" . "Changes that affect the build system or external dependencies"))
    ("chore" . ("🔧" . "Other changes that don't modify src or test files"))
    ("wip" . ("🚧" . "WIP, not for merging into main"))
    ("other" . ("🧩" . "Other changes that don't belong to any other category. Think hard before using this")))
  "Mapping of commit types to emoji and descriptions, following conventional commits.")


(defun neo/git-commit-insert-type-scope ()
  "Prompt for commit type and scope, then insert at beginning of commit buffer with emoji annotations."
  (when t ; (eq major-mode 'git-commit-mode)
    (let* ((selection (neo/select-commit-type))
           (type (car selection))
           (emoji (cadr selection))
           (scopes '("starlark" "mlody" "neo" "git" "github" "bazel" "ci" "omega"))
;              (scopes (neo/load-commit-scopes "SCOPES"))
           (scope (if scopes
                      (completing-read "Scope: " scopes nil t)
                    "*")))
      (save-excursion
        (goto-char (point-min))
        (insert (format "%s %s(%s): \n\n" emoji type scope)))
      (end-of-line))))

;; TODO make it more discoverable or I'll forget about it soon. Maybe
;; do something with it when editing github commit/pr/issues etc.
(neo/use-package emoji-github)

(defvar neo/commit-types
  '(("feat"     "✨" "A new feature")
    ("fix"      "🐛" "A bug fix")
    ("refactor" "♻" "A code change that neither fixes a bug nor adds a feature")
    ("docs"     "📝" "Documentation only changes")
    ("test"     "🧪" "Adding missing tests or correcting existing tests")
    ("build"    "🧰" "Changes that affect the build system or external dependencies")
    ("chore"    "🔧" "Other changes that don't modify src or test files")
    ("wip"      "🚧" "WIP, not for merging into main")
    ("other"    "🧩" "Other changes that don't belong to any other category. Think hard before using this"))
  "List of commit types with their emoji and descriptions, following conventional commits.")

(defun neo/select-commit-type ()
  "Select a commit type from `neo/commit-types`.
With Marginalia enabled, displays emoji and description as separate fields.
Returns a list of (TYPE EMOJI)."
  (interactive)
  (let ((candidates (mapcar #'car neo/commit-types)))
    (let* ((completion-extra-properties
            (when (featurep 'marginalia)
              `(:annotation-function
                ,(lambda (cand)
		   (message ">>>> %s" cand)
                   (when-let* ((item (assoc cand neo/commit-types))
                               (emoji (cadr item))
                               (desc (caddr item)))
                     (marginalia--fields
                      (emoji)
                      (desc :truncate 0.8)))))))
           (type (completing-read "Commit type: " candidates nil t)))
      (when type
        (when-let* ((item (assoc type neo/commit-types))
                    (emoji (cadr item)))
          (list type emoji))))))


(add-hook 'git-commit-setup-hook #'neo/git-commit-insert-type-scope)
