;;; -*- lexical-binding: t -*-

(require 'vc-git)

;; NOTE: deliberately NOT declaring `(neo/use-package transient)` here.
;; `transient' is already installed explicitly by neo-programming-foundation
;; (a dependency-free sibling extension, always present alongside better-git
;; under full-monty) and, failing that, automatically by Elpaca as one of
;; magit's own package dependencies. A second, independent top-level
;; `neo/use-package transient' declaration here used to race the one in
;; neo-programming-foundation: because `neo/use-package' defaults to
;; `:ensure (:wait t)', every top-level declaration synchronously
;; `elpaca-wait's and then `elpaca-split-queue's, so two independent
;; `transient' declarations plus magit's own automatic dependency enqueue
;; could register `transient' in three different Elpaca queues. Elpaca then
;; fails the newest of those registrations against magit's queue with
;; "Unhandled build error: (transient \"dependent magit in past queue\")"
;; during `neo/full-monty' -- which looks like magit's install hanging while
;; Elpaca silently re-clones transient before surfacing the error.
(neo/use-package magit
  :after transient
  :config
  (setq magit-save-repository-buffers 'dontask)
  ;; `key-chord' itself is only installed/enabled by neo:questionable-defaults,
  ;; not a :requires of this extension -- `with-eval-after-load' (matching
  ;; neo-ui-side-windows.el and neo-programming-foundation.el's own key-chord
  ;; bindings) picks up the binding whenever key-chord loads, regardless of
  ;; which of the two extensions gets installed first.
  (with-eval-after-load 'key-chord
    (key-chord-define-global "//" 'magit-status))
  (transient-append-suffix 'magit-branch "C"
    '("K" "delete all merged" neo/delete-merged-branches))
  :custom
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function 'magit-restore-window-configuration)
  (magit-list-refs-sortby "-creatordate") ; doesn't seem to have any effect
  (magit-refs-show-commit-count 'branch) ; may be too expsive
  (magit-completing-read-function 'magit-builtin-completing-read)
  (magit-read-worktree-directory-function #'magit-read-worktree-directory-offsite)
  (magit-repository-directories '(("~/Projects" . 1)))
  (git-commit-summary-max-length 120)
  (git-commit-major-mode 'markdown-mode)
					;  (project-switch-commands 'magit-project-status) ; TODO replace with a function that does more (switch to perspective if one available, show a readme o.org file if available, otherwise magit status
  (magit-status-show-untracked-files 'all)
  (magit-section-initial-visibility-alist '(
					    (worktrees . show)
					    (unstaged . show)
					    (untracked . show)
					    (unpushed . show)
					    (stashes . hide)
					    (commit . show)
					    (local . hide) ; maybe this is the "Branches" section
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
              #'neo/magit-worktree-action))

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


(defun neo/magit-worktree-create (directory branch &optional start-point)
  "Create a worktree for BRANCH at DIRECTORY, only if it doesn't already exist."
  (let* ((start (or start-point "origin/main"))
         (expanded-dir (magit--expand-worktree directory))
         (existing-branch (magit-git-string "rev-parse" "--verify" branch)))
    (magit-run-git "fetch" "origin")

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
	    (let ((default-directory expanded-dir))
	      (shell-command "direnv allow") ; well, if we cannot trust ourselves we're in deeper troubles
              expanded-dir)
          (error "Failed to create worktree %s at %s" branch expanded-dir))))))


(defun neo/switch-to-project (path)
  (projectile-add-known-project path)
  (projectile-invalidate-cache nil)
  (projectile-switch-project-by-name path))

(defun neo--better-git-main-window ()
  "Return a non-side window from the selected frame."
  (or (seq-find
       (lambda (window)
         (and (window-live-p window)
              (not (window-parameter window 'window-side))))
       (window-list nil 'no-mini))
      (selected-window)))

(defun neo--better-git-low-priority-buffer-p (buffer)
  "Return non-nil when BUFFER should yield to a project Magit status window."
  (let ((buffer-name (buffer-name buffer)))
    (or (string-prefix-p "*scratch*" buffer-name)
        (member buffer-name '("*Messages*" "*Warnings*")))))

(defun neo--better-git-fallback-window ()
  "Return a main window showing a low-priority buffer."
  (seq-find
   (lambda (window)
     (and (window-live-p window)
          (not (window-parameter window 'window-side))
          (neo--better-git-low-priority-buffer-p (window-buffer window))))
   (window-list nil 'no-mini)))

(defun neo--better-git-project-magit-buffer (project-root)
  "Return a Magit status buffer for PROJECT-ROOT, preferring the current perspective."
  (let ((project-root (file-name-as-directory (expand-file-name project-root))))
    (seq-find
     (lambda (buffer)
       (and (buffer-live-p buffer)
            (with-current-buffer buffer
              (and (derived-mode-p 'magit-status-mode)
                   default-directory
                   (file-equal-p project-root
                                 (file-name-as-directory
                                  (expand-file-name default-directory)))))))
     (append
      (when (and (featurep 'perspective)
                 (fboundp 'persp-buffers)
                 (fboundp 'persp-curr))
        (persp-buffers (persp-curr)))
      (when (fboundp 'magit-mode-get-buffers)
        (magit-mode-get-buffers))))))

(defun neo--better-git-display-buffer-same-window (buffer &optional window)
  "Display BUFFER in WINDOW using `display-buffer-same-window'."
  (with-selected-window (or window (neo--better-git-main-window))
    (display-buffer-same-window buffer nil)))

(defun neo--better-git-show-magit-buffer (buffer &optional window)
  "Display BUFFER in WINDOW without disturbing side windows."
  (neo--better-git-display-buffer-same-window buffer window))

(defun neo--better-git-ensure-project-magit-status (project-root &optional force)
  "Ensure PROJECT-ROOT has a visible Magit status buffer in the current perspective.

When FORCE is non-nil, reuse the main window if no lower-priority
window is available."
  (when (and (vc-git-responsible-p project-root)
             (fboundp 'magit-status-setup-buffer))
    (let ((buffer
           (or (neo--better-git-project-magit-buffer project-root)
               (let ((magit-display-buffer-function
                      #'neo--better-git-display-buffer-same-window))
                 (with-selected-window (neo--better-git-main-window)
                   (magit-status-setup-buffer project-root))))))
      (when (and (featurep 'perspective)
                 (fboundp 'persp-add-buffer)
                 (buffer-live-p buffer))
        (persp-add-buffer buffer))
      (let ((visible-window (get-buffer-window buffer))
            (fallback-window (neo--better-git-fallback-window)))
        (when (and (not visible-window)
                   (or fallback-window force))
          (neo--better-git-show-magit-buffer
           buffer
           (or fallback-window
               (neo--better-git-main-window)))))
      buffer)))

(defun neo/better-git-switch-to-project (path)
  "Switch to PATH and show its Magit status without disturbing side windows."
  (neo/switch-to-project path)
  (neo--better-git-ensure-project-magit-status path t))

(defun neo/ensure-worktree ()
  (magit-section-case
    (worktree
     (neo/better-git-switch-to-project (oref it value)))))

(defun neo/magit-worktree-action ()
  "Switch to the worktree at point in the Magit status buffer."
  (interactive)
  (neo/ensure-worktree))

(defun neo--better-git-worktree-sync (directory &rest _)
  "Switch NEO to the worktree at DIRECTORY after Magit creates it.
`magit-worktree-checkout'/`magit-worktree-branch' only run `git worktree
add' and show a Magit status buffer for DIRECTORY -- they never touch
projectile/persp/treemacs, unlike the `w' key on a Worktrees-section entry
\(`neo/magit-worktree-action'), which goes through
`neo/better-git-switch-to-project'. Route the native transient through the
same function so both paths stay in sync."
  (let ((expanded (magit--expand-worktree directory)))
    (when (file-directory-p expanded)
      (neo/better-git-switch-to-project expanded))))

(with-eval-after-load 'magit-worktree
  (advice-add 'magit-worktree-checkout :after #'neo--better-git-worktree-sync)
  (advice-add 'magit-worktree-branch :after #'neo--better-git-worktree-sync))

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

(defconst neo/forge-legacy-database-file
  (expand-file-name "~/.emacs.d/.cache/forge-database.sqlite")
  "Legacy Forge database path used before Neo's profile-specific cache.")

(defun neo--forge-database-has-repositories-p (database-file)
  "Return non-nil when DATABASE-FILE exists and contains tracked repositories."
  (when (file-exists-p database-file)
    (require 'sqlite)
    (let (db)
      (unwind-protect
          (condition-case nil
              (progn
                (setq db (sqlite-open database-file))
                (> (caar (sqlite-select db "SELECT COUNT(*) FROM repository")) 0))
            (error nil))
        (when (and db (sqlitep db))
          (sqlite-close db))))))

(defun neo/forge-migrate-legacy-database-if-needed ()
  "Seed Neo's Forge database from the legacy profile when it is still empty."
  (let ((target (if (boundp 'forge-database-file)
                    forge-database-file
                  (expand-file-name "forge/database.sqlite" no-littering-var-directory)))
        (legacy neo/forge-legacy-database-file))
    (when (and (neo--forge-database-has-repositories-p legacy)
               (not (neo--forge-database-has-repositories-p target)))
      (make-directory (file-name-directory target) t)
      (copy-file legacy target t t t t)
      (message "Neo: migrated Forge repository metadata from %s" legacy))))

(defun neo--forge-register-repository-at-directory (directory)
  "Register DIRECTORY in Forge using only local git metadata.

Neo marks locally discovered repositories as tracked so commands like
pull-request creation are available before the first successful
network-backed Forge pull."
  (when (file-accessible-directory-p directory)
    (let ((default-directory (file-name-as-directory (expand-file-name directory))))
      (when-let* ((repo (ignore-errors
                         (and (magit-gitdir)
                              (forge-get-repository :stub?)))))
        (setq repo (forge-get-repository repo nil :insert!))
        (oset repo worktree (magit-toplevel))
        (oset repo condition :tracked)
        repo))))

(defun neo/forge-seed-discovered-repositories ()
  "Seed Forge's repository database from Neo's discovered projects.

Missing projects and repositories on unsupported remotes are skipped
silently so one broken entry does not block startup."
  (interactive)
  (let ((count 0))
    (when (boundp 'projectile-known-projects)
      (dolist (project (delete-dups (copy-sequence projectile-known-projects)))
        (when (ignore-errors
                (neo--forge-register-repository-at-directory project))
          (setq count (1+ count)))))
    count))

(defun neo/forge-seed-current-project ()
  "Register the current Projectile project in Forge."
  (when-let* ((project-root (and (fboundp 'projectile-project-root)
                                (ignore-errors (projectile-project-root)))))
    (neo--forge-register-repository-at-directory project-root)))

(defun neo/forge-bootstrap-repositories ()
  "Prepare Forge repository metadata for Neo startup."
  (neo/forge-migrate-legacy-database-if-needed))

(defun neo/forge-remove-topic-sections-from-status ()
  "Remove Forge topic sections that should not appear in Magit status."
  (remove-hook 'magit-status-sections-hook #'forge-insert-issues)
  (remove-hook 'magit-status-sections-hook #'forge-insert-discussions))


;;; TODO find a good binding for forge-post-submit otherwise markdown mode takes over C-c C-c
;;; maybe C-x #
(neo/use-package forge
  :after magit
  :config
  (neo/forge-bootstrap-repositories)
  (neo/forge-remove-topic-sections-from-status)
  (with-eval-after-load 'projectile
    (neo/forge-seed-discovered-repositories)
    (add-hook 'projectile-after-switch-project-hook
              #'neo/forge-seed-current-project))
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

;; (use-package gptel
;;   :init
;;   (setq-default gptel-default-mode 'markdown-mode
;; 		gptel-post-response-functions #'gptel-end-of-response
;; 		gptel-model "gemini-2.5-pro" ; Pick your default model
;; 		gptel-backend (gptel-make-gemini "Gemini"
;; 						 :key (auth-source-pick-first-password :host "gemini.google.com" :user "token")
;; 						 :stream t)))

;; (use-package gptel-magit
;;   :ensure t
;;   :hook (magit-mode . gptel-magit-install))

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
  (let* ((max-len 8)
	 (candidates (mapcar (lambda (item)
			      (let ((type (car item)))
				(propertize type
					    'display (format (format "%%-%ds" max-len) type))))
                neo/commit-types))
	 (completion-extra-properties
            (when (featurep 'marginalia)
              `(:annotation-function
		(lambda (cand)
		  (when-let* ((item (assoc cand neo/commit-types))
			      (emoji (nth 1 item))
			      (desc  (nth 2 item)))
		    (marginalia--fields
		     ((format " %-2s" emoji))
		     (desc :truncate 0.8)))))))
           (type (completing-read "Commit type: " candidates nil t)))
    (when type
      (when-let* ((item (assoc type neo/commit-types))
                  (emoji (cadr item)))
	(list type emoji)))))

(add-hook 'git-commit-setup-hook #'neo/git-commit-insert-type-scope)
