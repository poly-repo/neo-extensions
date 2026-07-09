;;; -*- lexical-binding: t -*-
(neo/extension
 :name "extension-manager"
 :title "Extension Manager"
 :publisher "neo"
 :description "Your extensions, thoughtfully managed\n\n"
 :categories (neo) ; not sure what the intention for this was
 :keywords ()

 :requires ()
 :hidden t
 :repository (
	      :type "git"
	      :url "https://github.com/poly-repo/neo-extensions.git"
	      :path "extensions/neo/extension-manager"))
