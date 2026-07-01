;;; -*- lexical-binding: t -*-
(neo/extension
 :name "neo-workflow"
 :title "Neo Workflow (beads)"
 :publisher "neo"
 :description "The workflow board, backed by beads and git instead of SQLite and GitHub.\n\n"
 :categories (neo) ; not sure what the intention for this was
 :keywords (programming project)

 ;; NOTE: requires MUST be strings — `neo--normalize-requires' keeps a list
 ;; as-is and the topo-sort matches these against the string slug keys, so
 ;; symbol requires silently create no dependency edge (wrong load order).
 :requires ("neo:projects" "neo:better-git" "neo:programming-foundation")
 :repository (
	      :type "git"
	      :url "https://github.com/poly-repo/neo-extensions.git"
	      :path "extensions/neo/neo-workflow"))
