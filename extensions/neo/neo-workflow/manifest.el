;;; -*- lexical-binding: t -*-
(neo/extension
 :name "neo-workflow"
 :title "Neo Workflow (beads)"
 :publisher "neo"
 :description "The workflow board, backed by beads and git instead of SQLite and GitHub.\n\n"
 :categories (neo) ; not sure what the intention for this was
 :keywords (programming project)

 :requires (neo:project neo:better-git neo:programming-foundation)
 :repository (
	      :type "git"
	      :url "https://github.com/poly-repo/neo-extensions.git"
	      :path "extensions/neo/neo-workflow"))
