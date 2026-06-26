;;; neo-mlody-starlark-mode.el --- Legacy Starlark mode for mlody files -*- lexical-binding: t -*-

;;; Commentary:

;; Provides `neo-mlody-starlark-mode', a major mode for editing legacy
;; Python/Starlark-based .mlody files.
;; Derives from `neo-starlark-ts-mode', a treesit-based Starlark mode defined
;; here, to inherit syntax highlighting and indentation without depending on
;; the bazel package.

;;; Code:

(require 'treesit)
(require 'python)

(add-to-list 'treesit-language-source-alist
             '(starlark "https://github.com/tree-sitter-grammars/tree-sitter-starlark"))

(defvar neo--starlark-ts-font-lock-settings
  (treesit-font-lock-rules
   :language 'starlark
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'starlark
   :feature 'string
   '((string) @font-lock-string-face)

   :language 'starlark
   :feature 'keyword
   '(["def" "if" "elif" "else" "for" "in" "return" "pass"
      "break" "continue" "and" "or" "not" "load" "lambda"]
     @font-lock-keyword-face)

   :language 'starlark
   :feature 'constant
   '(["True" "False" "None"] @font-lock-constant-face)

   :language 'starlark
   :feature 'definition
   '((function_definition name: (identifier) @font-lock-function-name-face))

   :language 'starlark
   :feature 'number
   '([(integer) (float)] @font-lock-number-face))
  "Treesit font-lock settings for `neo-starlark-ts-mode'.")

(define-derived-mode neo-starlark-ts-mode prog-mode "Starlark"
  "Major mode for Starlark files backed by tree-sitter."
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-add 0)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local indent-line-function #'python-indent-line)
  (when (treesit-ready-p 'starlark)
    (treesit-parser-create 'starlark)
    (setq-local treesit-font-lock-settings neo--starlark-ts-font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment definition)
                  (keyword string)
                  (constant number)
                  ()))
    (treesit-major-mode-setup)))

(define-derived-mode neo-mlody-starlark-mode neo-starlark-ts-mode "MLody-Starlark"
  "Major mode for editing legacy Python/Starlark `.mlody' files.
Derives from `neo-starlark-ts-mode' for treesit Starlark support
and registers the legacy `mlody-lsp' language server.")

(if (fboundp 'neo/eglot-set-server)
    (neo/eglot-set-server 'neo-mlody-starlark-mode '("mlody-lsp")))

(provide 'neo-mlody-starlark-mode)

;;; neo-mlody-starlark-mode.el ends here
