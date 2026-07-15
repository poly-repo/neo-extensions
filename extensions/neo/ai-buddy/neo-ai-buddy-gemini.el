;;; -*- lexical-binding: t -*-

(neo/use-package ai-code
  :custom
  (ai-code-backends-infra-use-side-window nil) ; NOTE: we do want to manage the side window ourselves
  :config
  ;; use codex as backend, other options are 'gemini, 'github-copilot-cli, 'opencode, 'grok, 'claude-code-ide, 'claude-code, 'cursor
  (ai-code-set-backend 'gemini)
  ;; Enable global keybinding for the main menu
  ;; Optional: Use eat if you prefer, by default it is vterm
  (setq ai-code-backends-infra-terminal-backend 'eat) ;; for openai codex, github copilot cli, opencode, grok, cursor-cli; for claude-code-ide.el, you can check their config
  ;; Optional: Turn on auto-revert buffer, so that the AI code change automatically appears in the buffer
  (global-auto-revert-mode 1)
  (setq auto-revert-interval 1) ;; set to 1 second for faster update
  (neo/register-disposable-process 
   '(rx "*" "gemini" (zero-or-more anything) "*"))
  ;; Optional: Set up Magit integration for AI commands in Magit popups
  (with-eval-after-load 'magit
    (ai-code-magit-setup-transients)))

(provide 'neo-ai-buddy-gemini)
