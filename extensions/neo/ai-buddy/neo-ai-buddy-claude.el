;;; -*- lexical-binding: t -*-

;; (use-package claude-code-ide
;;   :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
;;   :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
;;   :config
;;   (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

;; Install the MCP HTTP server dependency before the package that requires it,
;; so clean aggregate bootstraps do not discover it only during the dependent
;; package's build.  The explicit wait takes effect when Neo eventually replays
;; this stored declaration.
(neo/use-package web-server
  :ensure (:wait t)
  :demand t)

(neo/use-package claude-code-ide
  :ensure (claude-code-ide :host github :repo "manzaltu/claude-code-ide.el")
  :after web-server
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (setq claude-code-ide-terminal-backend 'eat)
  (claude-code-ide-emacs-tools-setup))

(provide 'neo-ai-buddy-claude)
