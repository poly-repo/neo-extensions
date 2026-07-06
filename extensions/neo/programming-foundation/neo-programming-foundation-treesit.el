;;; -*- lexical-binding: t -*-
;; Grammar sources are declared in this extension's manifest.el as
;; `:tree-sitter-grammars'; collection across all installed extensions
;; and the actual grammar builds are handled centrally by
;; core/neo-treesit.el. This file only keeps the UI-level treesit
;; configuration (font-lock level, folding).

(neo/use-package treesit
  :ensure nil				; built-in
  :custom
  (treesit-font-lock-level 3)		; 4 should be more, but as of 30.2 it is less (for python)
  )

(neo/use-package treesit-fold
  :custom
  (treesit-fold-line-count-format " <%d lines> ")
  :config
  (global-treesit-fold-indicators-mode 1))

(with-eval-after-load 'treesit-fold
  (defun neo/treesit-fold-disable-indicator-refresh-on-typing ()
    "Do not refresh tree-sitter fold indicators refreshing on every character
typed. This helps improve performance."
    (remove-hook 'post-command-hook #'treesit-fold-indicators--post-command t))

  (advice-add 'treesit-fold-indicators--enable
              :after #'neo/treesit-fold-disable-indicator-refresh-on-typing))

;; (use-package treesit-auto
;;   :custom
;;   (setq treesit-auto-install nil)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

(provide 'neo-programming-foundation-treesit)
