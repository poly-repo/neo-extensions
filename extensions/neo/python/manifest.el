;;; -*- lexical-binding: t -*-
(neo/extension
 :name "Python"
 :title "Tame the snake—Emacs style."
 :publisher "neo"
 :description "Python support."
 :categories (neo)
 :keywords (programming python)
 :requires ()
 :depends-on ("neo:programming-foundation")
 :tree-sitter-grammars
 ((python "https://github.com/tree-sitter/tree-sitter-python"))
 :repository (
	      :type "git"
	      :url "https://github.com/poly-repo/neo-extensions.git"
	      :path "extensions/neo/python"))
