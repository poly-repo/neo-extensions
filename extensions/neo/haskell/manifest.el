;;; -*- lexical-binding: t -*-
(neo/extension
 :name "haskell"
 :title "Lazy by default, eager about types."
 :publisher "neo"
 :description "Haskell support: haskell-mode + HLS via eglot, lsp-haskell fallback, Hoogle lookup, tree-sitter, folding, import/align helpers, REPL+build commands, snippets, and autoformat with stylish-haskell."
 :categories (neo)
 :keywords (programming haskell)
 :requires ("neo:programming-foundation")
 ;; Pinned to v0.23.1: the grammar's master branch renames nodes that
 ;; haskell-ts-mode's font-lock queries reference, which otherwise
 ;; fails with `treesit-query-error' the moment a Haskell buffer is
 ;; fontified.
 :tree-sitter-grammars
 ((haskell "https://github.com/tree-sitter/tree-sitter-haskell" "v0.23.1"))
 ;; Once the pinned grammar above is ready, prefer haskell-ts-mode over
 ;; haskell-mode — activated centrally by core/neo-treesit.el.
 :tree-sitter-modes
 ((haskell haskell-mode haskell-ts-mode))
 :repository (
	      :type "git"
	      :url "https://github.com/poly-repo/neo-extensions.git"
	      :path "extensions/neo/haskell"))
