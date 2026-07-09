;;; -*- lexical-binding: t -*-
(neo/extension
 :name "dashboard"
 :title "Emacs Dashboard"
 :publisher "neo"
 :description "A better first buffer\n\n"
 :categories (neo) ; not sure what the intention for this was
 :keywords ()

 :requires ("neo:projects")		; dashboard's :custom sets dashboard-projects-backend to
					; 'projectile, which neo:projects (not dashboard itself)
					; installs -- this must be a hard :requires, not just
					; :depends-on, or projectile is never installed
 :depends-on ()
 :repository (
	      :type "git"
	      :url "https://github.com/poly-repo/neo-extensions.git"
	      :path "extensions/neo/dashboard"))
