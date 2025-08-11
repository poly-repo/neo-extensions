(neo/use-package magit
  :config
  (setq magit-save-repository-buffers 'dontask)
  (key-chord-define-global "//" 'magit-status)
  (transient-append-suffix 'magit-branch "C"
    '("K" "delete all merged" neo/delete-merged-branches))
  :custom
  (magit-list-refs-sortby "-creatordate") ; doesn't seem to have any effect
  (magit-refs-show-commit-count 'branch) ; may be too expsive
  (magit-completing-read-function 'magit-builtin-completing-read)
  (git-commit-summary-max-length 50)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

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
  :bind
  ("<f12> s" . 'magit-status)
  ("<f12> g" . 'counsel-git-grep))
