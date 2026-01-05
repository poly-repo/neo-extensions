;;; -*- lexical-binding: t -*-

;;; This is ai-buddy, a NEO extension
;;;
;;; AI at your gingertips


(add-hook 'aidermacs-before-run-backend-hook
          (lambda ()
	    (setenv "GEMINI_API_KEY" (auth-source-pick-first-password :host "gemini.google.com" :user "token"))))

(neo/use-package aidermacs
  :commands (aidermacs-chat)
  :bind (("C-c a" . aidermacs-transient-menu))
  :custom
  (aidermacs-program "aider-ce")
					;  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-chat-mode 'code)
  (aidermacs-default-model "gemini/gemini-2.5-pro")
  (aidermacs-weak-model "gemini/gemini-flash-latest")
  (aidermacs-backend 'vterm)
  (aidermacs-vterm-multiline-newline-key "S-<return>")
					;  (aidermacs-backend 'comint)
  (setq aidermacs-project-read-only-files '("CONVENTIONS.md"))
  (aidermacs-exit-kills-buffer t)
  :config
  
  ;; Default provider DeepSeek
  (setq aidermacs-default-provider 'deepseek)

  ;; Common options: deepseek/deepseek-chat, deepseek/deepseek-coder
  ;; Default model (OpenRouter)
  (setq ; aidermacs-default-model "openrouter/qwen/qwen3-235b-a22b:free"
   aidermacs-auto-commits nil
   aidermacs-use-git t)

  ;; API keys from environment
  ;; (setq aidermacs-api-keys
  ;;       `((gemini    . ,(getenv "GEMINI_API_KEY"))
  ;;         (deepseek  . ,(getenv "DEEPSEEK_API_KEY"))
  ;;         (openai    . ,(getenv "OPENAI_API_KEY"))
  ;;         (anthropic . ,(getenv "ANTHROPIC_API_KEY"))
  ;; 	  (openrouter . ,(getenv "OPENROUTER_API_KEY"))))

  ;; ;; Extra args to skip warnings
  ;; (setq aidermacs-extra-args
  ;;       '("--no-show-model-warnings"
  ;;         "--model" "deepseek/deepseek-coder"))


  ;; Disable wrapping in the Aider buffer
  (add-hook 'aidermacs-mode-hook
            (lambda ()
              (visual-line-mode -1)
              (setq truncate-lines t)
              (setq word-wrap nil))))


;;; Note, no (provide 'neo-ai-buddy) here, extensions are loaded not required.
