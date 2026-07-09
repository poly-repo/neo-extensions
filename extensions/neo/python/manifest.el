;;; -*- lexical-binding: t -*-
(neo/extension
 :name "Python"
 :title "Tame the snake—Emacs style."
 :publisher "neo"
 :description "Python support."
 :categories (neo)
 :keywords (programming python)
 :requires ("neo:programming-foundation")	; (require 'neo-programming-foundation-treesit)
						; needs its dir on load-path
 :depends-on ()
 :tree-sitter-grammars
 ((python "https://github.com/tree-sitter/tree-sitter-python"))
 :repository (
	      :type "git"
	      :url "https://github.com/poly-repo/neo-extensions.git"
	      :path "extensions/neo/python"))
