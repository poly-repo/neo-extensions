;;; -*- lexical-binding: t -*-

(neo/extension
 :name "go"
 :title "Go where the code takes you."
 :publisher "neo"
 :description "Go editing with tree-sitter, Eglot, and gopls."
 :categories (neo)
 :keywords (programming go lsp)
 :requires ("neo:programming-foundation")
 :tree-sitter-modes
 ((go go-mode go-ts-mode))
 :repository (
              :type "git"
              :url "https://github.com/poly-repo/neo-extensions.git"
              :path "extensions/neo/go"))
