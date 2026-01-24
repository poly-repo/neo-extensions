(neo/extension
 :name "workflow"
 :title "Workflow Manager"
 :publisher "neo"
 :description "A smart tagline will go here\n\n"
 :categories (neo) ; not sure what the intention for this was
 :keywords (programming project)

 :requires (neo:project neo:better-git)
 :repository (
	      :type "git"
	      :url "https://github.com/poly-repo/neo-extensions.git"
	      :path "extensions/neo/workflow"))
