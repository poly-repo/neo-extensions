(neo/extension
 :name "dashboard"
 :title "Emacs Dashboard"
 :publisher "neo"
 :description "A better first buffer\n\n"
 :categories (neo) ; not sure what the intention for this was
 :keywords ()

 :requires ()
 :depends-on ("neo:projects")		; TODO: this is not a depends-on in the sense of "recommend". But if that's used, we want to come after it
 :repository (
	      :type "git"
	      :url "https://github.com/poly-repo/neo-extensions.git"
	      :path "extensions/neo/dashboard"))
