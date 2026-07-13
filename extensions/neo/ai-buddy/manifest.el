;;; -*- lexical-binding: t -*-
(neo/extension
 :name "ai-buddy"
 :title "AI at your fingertips"
 :publisher "neo"
 :description "AI: the coworker who never sleeps, and occasionally hallucinates...\n\n"
 :categories (neo) ; not sure what the intention for this was
 :keywords (tools programming)

 :requires ("neo:programming-foundation"	; ai-code's Package-Requires lists transient as a
						; dependency, but its autoloads (which call
						; transient-define-prefix) can load before Elpaca's
						; own automatic dependency resolution activates
						; transient -- programming-foundation's explicit,
						; deliberate `transient' declaration must win the
						; race (same issue documented in neo-better-git.el)
	    "neo:ui")				; neo--ai-buddy-codex-register-side-action calls
						; neo/register-side-action, defined there
 :repository (
	      :type "git"
	      :url "https://github.com/poly-repo/neo-extensions.git"
	      :path "extensions/neo/ai-buddy"))
