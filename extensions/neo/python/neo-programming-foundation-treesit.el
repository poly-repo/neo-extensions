;;; -*- lexical-binding: t -*-

(neo/use-package treesit
  :builtin
  :custom
  (treesit-font-lock-level 4)		; 4 should be more, but as of 30.2 it is less (for python)
  )

(neo/use-package tree-sitter-langs
  :after tree-sitter
  :config
  (tree-sitter-langs-install-latest-grammar))

(neo/use-package treesit-fold
  :custom
  (treesit-fold-line-count-format " <%d lines> ")
  :config
  (global-treesit-fold-indicators-mode 1))

;; (with-eval-after-load 'treesit-fold
;;   (defun neo/treesit-fold-disable-indicator-refresh-on-typing ()
;;     "Do not refresh tree-sitter fold indicators refreshing on every character
;; typed. This helps improve performance."
;;     (remove-hook 'post-command-hook #'treesit-fold-indicators--post-command t))

;;   (advice-add 'treesit-fold-indicators--enable
;;               :after #'neo/treesit-fold-disable-indicator-refresh-on-typing))

(defvar neo/treesit-default-grammar-dir (neo/cache-file-path "tree-sitter")
  "Default directory for tree-sitter grammars when none is provided.")

(neo/use-package treesit-auto
  :init
  (setq treesit-extra-load-path `(,neo/treesit-default-grammar-dir))
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(defun neo/treesit--install-with-default-dir (orig-fn lang &optional out-dir)
  "Call ORIG-FN installing LANG grammar, defaulting OUT-DIR if nil."
  (funcall orig-fn
           lang
           (or out-dir neo/treesit-default-grammar-dir)))

;; NOTE: this advice is because treesit-auto calls
;; treesit-install-grammar without passing a directory and hence
;; defaults to a tree-sitter directory under user-emacs-directory
(advice-add 'treesit-install-language-grammar
            :around #'neo/treesit--install-with-default-dir)

(provide 'neo-programming-foundation-treesit)
