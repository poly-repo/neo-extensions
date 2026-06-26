;;; -*- lexical-binding: t -*-
(neo/extension
 :name "mlody-mode"
 :title "Layout-sensitive Mlody editing without the Starlark cosplay."
 :publisher "neo"
 :description "Major-mode support for Haskell-style Mlody: syntax highlighting, layout-sensitive indentation, and buffer formatting for .mlody files."
 :categories (neo)
 :keywords (programming mlody haskell)
 :requires ()
 :repository (
	      :type "git"
	      :url "https://github.com/poly-repo/neo-extensions.git"
	      :path "extensions/neo/mlody-mode"))
