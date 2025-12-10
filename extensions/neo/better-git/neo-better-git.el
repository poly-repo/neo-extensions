(neo/use-package transient)

(neo/use-package magit
  :config
  (setq magit-save-repository-buffers 'dontask)
  (key-chord-define-global "//" 'magit-status)
  ;; (transient-append-suffix 'magit-branch "C"
  ;;   '("K" "delete all merged" neo/delete-merged-branches))
  :custom
  (magit-list-refs-sortby "-creatordate") ; doesn't seem to have any effect
  (magit-refs-show-commit-count 'branch) ; may be too expsive
  (magit-completing-read-function 'magit-builtin-completing-read)
  (magit-read-worktree-directory-function #'magit-read-worktree-directory-offsite)
  (git-commit-summary-max-length 120)
  (project-switch-commands 'magit-project-status) ; TODO replace with a function that does more (switch to perspective if one available, show a readme o.org file if available, otherwise magit status
  (magit-status-show-untracked-files 'all)
  (magit-section-initial-visibility-alist '(
					    (stashes . show)
					    (untracked . show)))
  
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(magit-project-status "Magit" 109))) ;; add Git with char 'm'

  ;; NOTE: order of functions in this hook is important, we make this clear with setq
  ;; instead of gambling with add-hook. There's magit-add-section-hook that might be better.
  ;; TODO: find the right place for magit-insert-branch-description (might also be
  ;; useful in magit-refs-sections-hook)
  (setq magit-status-sections-hook
        '(magit-insert-status-headers
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
  :hook
  (after-save 'magit-after-save-refresh-status)

  ;; TODO <f12> on laptop is problematic as one would probably have media keys
  :bind
  ("<f12> s" . 'magit-status)
  ("<f12> g" . 'counsel-git-grep))

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
