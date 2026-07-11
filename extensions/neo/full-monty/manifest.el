;;; -*- lexical-binding: t -*-
(neo/extension
 :name "full-monty"
 :title "The complete NEO experience"
 :publisher "neo"
 :description "Don't settle for less, YOLO"
 :categories (neo)
 :keywords (neo)
 :hidden t
 :requires (
	    "neo:dashboard"
	    "neo:elisp"
	    "neo:questionable-defaults"
	    "neo:session"
	    "neo:extension-manager"
	    "neo:compsel"
	    "neo:ui"
	    "neo:better-git"
	    "neo:terminal"
	    "neo:news"
	    "neo:projects"
	    "neo:programming-foundation"
	    "neo:neo-workflow"
	    "neo:build"
	    "neo:python"
	    "neo:haskell"
	    "neo:latex"
	    "neo:mlody-mode"
	    "neo:ai-buddy"
	    "neo:leetcode"
	    "mav:cocktails"
	    )
 :repository (
	      :type "git"
	      :url "https://github.com/poly-repo/neo-extensions.git"
	      :path "extensions/neo/full-monty"))
