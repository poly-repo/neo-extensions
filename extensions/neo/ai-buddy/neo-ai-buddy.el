;;; -*- lexical-binding: t -*-

;;; This is ai-buddy, a NEO extension
;;;
;;; AI at your gingertips


(require 'cl-lib)

(require 'neo-ai-buddy-gemini)
(require 'neo-ai-buddy-claude)

;; for ai-code
(global-set-key (kbd "C-c a") #'ai-code-menu)

;;; Note, no (provide 'neo-ai-buddy) here, extensions are loaded not required.
