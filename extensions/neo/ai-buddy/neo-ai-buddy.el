;;; -*- lexical-binding: t -*-

;;; This is ai-buddy, a NEO extension
;;;
;;; AI at your gingertips


(require 'cl-lib)

(defun neo--ai-buddy-find-repo-root (start)
  "Find repo root starting from START dir."
  (locate-dominating-file start (lambda (dir)
                                  (or (file-directory-p (expand-file-name ".git" dir))
                                      (file-exists-p (expand-file-name "MODULE.bazel" dir))))))

(defun neo--ai-buddy-collect-prompt-chain (root cwd)
  "Return a list of absolute paths to all .prompt.md files from ROOT to CWD.
The list is ordered from ROOT to CWD."
  (let ((chain '())
        (cur (file-truename root))
        (end (file-truename cwd)))
    (while (and cur (string-prefix-p cur end))
      (let ((p (expand-file-name ".prompt.md" cur)))
        (when (file-exists-p p)
          (push p chain)))
      (if (equal cur end)
          (setq cur nil)  ;; stop
        ;; descend one directory toward cwd
        (let ((next (file-name-as-directory
                     (expand-file-name
                      (car (split-string (file-relative-name end cur) "[/\\\\]+" t))
                      cur))))
          (setq cur next))))
    (nreverse chain)))

(defun neo/ai-buddy-build-prompt-files (languages)
  "Build a list of absolute paths for prompt files for AI.
This is an elisp rewrite of `o-build-prompt` python script.
LANGUAGES is a list of strings."
  (let* ((cwd (file-truename default-directory))
         (root (neo--ai-buddy-find-repo-root cwd))
         (readonly '()))
    (unless root
      (error "Could not find repo root"))

    ;; Core prompts
    (setq readonly (append readonly
                           (list (expand-file-name "prompts/core/identity.md" root)
                                 (expand-file-name "prompts/core/style.md" root)
                                 (expand-file-name "prompts/core/safety.md" root))))

    ;; Language prompts
    (dolist (language languages)
      (let ((lang-file (expand-file-name (format "prompts/languages/%s.md" language) root)))
        (when (file-exists-p lang-file)
          (setq readonly (append readonly (list lang-file))))))

    ;; Directory-local prompts
    (setq readonly (append readonly (neo--ai-buddy-collect-prompt-chain root cwd)))
    readonly))


(add-hook 'aidermacs-before-run-backend-hook
	  (lambda ()
	    ;; TODO detect languages from directory tree content
	    (setq aidermacs-project-read-only-files (neo/ai-buddy-build-prompt-files '("elisp")))
	    (setenv "GEMINI_API_KEY" (auth-source-pick-first-password :host "gemini.google.com" :user "token"))))

;; (neo/use-package aidermacs
;;   :commands (aidermacs-chat)
;;   :bind (("C-c a" . aidermacs-transient-menu))
;;   :custom
;;   (aidermacs-auto-commits nil)
;;   (aidermacs-default-model "gemini/gemini-2.5-pro")
;;   (aidermacs-editor-model "gemini/gemini-flash-latest")
;;   (aidermacs-weak-model "gemini/gemini-flash-latest")
;;   (aidermacs-program "aider-ce")
;;   (aidermacs-default-chat-mode 'architect)
;;   (aidermacs-auto-accept-architect nil)
;;   (aidermacs-show-diff-after-change t)
;;   (aidermacs-backend 'comint)
;;   (aidermacs-vterm-multiline-newline-key "S-<return>")
;;   (aidermacs-exit-kills-buffer t)
;;   :hook
;;   (aidermacs-mode . 
;; 		  (lambda ()
;; 		    (visual-line-mode -1)
;; 		    (setq truncate-lines t)
;; 		    (setq word-wrap nil))))

;;; experimenting with gemini-cli, seems better
(neo/use-package ai-code
  :custom
  (ai-code-backends-infra-use-side-window nil) ; NOTE: we do want to manage the side window ourselves
  :config
  ;; use codex as backend, other options are 'gemini, 'github-copilot-cli, 'opencode, 'grok, 'claude-code-ide, 'claude-code, 'cursor
  (ai-code-set-backend 'gemini)
  ;; Enable global keybinding for the main menu
  (global-set-key (kbd "C-c a") #'ai-code-menu)
  ;; Optional: Use eat if you prefer, by default it is vterm
  (setq ai-code-backends-infra-terminal-backend 'eat) ;; for openai codex, github copilot cli, opencode, grok, cursor-cli; for claude-code-ide.el, you can check their config
  ;; Optional: Turn on auto-revert buffer, so that the AI code change automatically appears in the buffer
  (global-auto-revert-mode 1)
  (setq auto-revert-interval 1) ;; set to 1 second for faster update
  ;; Optional: Set up Magit integration for AI commands in Magit popups
  (with-eval-after-load 'magit
    (ai-code-magit-setup-transients)))


;;; Note, no (provide 'neo-ai-buddy) here, extensions are loaded not required.
