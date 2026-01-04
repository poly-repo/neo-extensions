;; TODO unfortunately we do not have an ABI version we should patch
;; sources to define a variable that incluses it.  As a workaround we
;; keep grammars segregated by emacs versions. A bit wasteful but at
;; least correct.
(defconst neo/treesit-grammar-dir
  (expand-file-name
   (format "~/.cache/neo-devel/tree-sitter/emacs-%d.%d/"
           emacs-major-version
           emacs-minor-version)))

(setq treesit-extra-load-path (list neo/treesit-grammar-dir))

(defconst neo/treesit-required-languages
  '(bash
    c
    cpp
    css
    go
    python
    toml
    yaml)
  "Tree-sitter languages required by NEO.")

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun neo/treesit-install-grammars (&optional languages)
  "Install Tree-sitter grammars into neo cache directory."
  (let* ((langs (or languages (mapcar #'car treesit-language-source-alist)))
         (treesit--install-directory neo/treesit-grammar-dir))
    (unless (file-directory-p treesit--install-directory)
      (make-directory treesit--install-directory t))
    (dolist (lang langs)
      (unless (treesit-language-available-p lang)
        (message "[neo] Installing tree-sitter grammar for %s…" lang)
        (treesit-install-language-grammar lang neo/treesit-grammar-dir)))))

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
