(neo/extension
 :name "workflow"
 :title "Workflow Manager"
 :publisher "neo"
 :description "Master the art of being in five branches at once.  Because 'one thing at a time' is just a suggestion.\n\n"
 :categories (neo) ; not sure what the intention for this was
 :keywords (programming project)

 :requires (neo:project neo:better-git)
 :repository (
	      :type "git"
	      :url "https://github.com/poly-repo/neo-extensions.git"
	      :path "extensions/neo/workflow"))
