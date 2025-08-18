(neo/use-package aider
  :config
  ;; For latest claude sonnet model
  (setq aider-args '("--model" "gemini" )) ;; add --no-auto-commits if you don't want it
  (setenv "GEMINI_API_KEY" (auth-source-pick-first-password :host "gemini.google.com" :user "token"))
  (global-set-key (kbd "C-c a") 'aider-transient-menu) ;; for wider screen
  ;; or use aider-transient-menu-2cols / aider-transient-menu-1col, for narrow screen
  (aider-magit-setup-transients) ;; add aider magit function to magit menu
  ;; auto revert buffer
  (global-auto-revert-mode 1)
  (auto-revert-mode 1))
