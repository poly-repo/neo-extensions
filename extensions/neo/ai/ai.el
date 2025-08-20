;; (neo/use-package aider
;;   :config
;;   ;; For latest claude sonnet model
;;   (setq aider-args '("--model" "gemini" )) ;; add --no-auto-commits if you don't want it
;;   (setenv "GEMINI_API_KEY" (auth-source-pick-first-password :host "gemini.google.com" :user "token"))
;;   (global-set-key (kbd "C-c a") 'aider-transient-menu) ;; for wider screen
;;   ;; or use aider-transient-menu-2cols / aider-transient-menu-1col, for narrow screen
;;   (aider-magit-setup-transients) ;; add aider magit function to magit menu
;;   ;; auto revert buffer
;;   (global-auto-revert-mode 1)
;;   (auto-revert-mode 1))
;; Make sure MELPA is enabled, then:
;; M-x package-install RET aider RET

(require 'project)

(defun neo/aider-config-args ()
  "Return (--config PATH) where PATH is the project's .aider.conf.yml if present,
else ~/.aider.conf.yml."
  (let* ((proj (project-current))                       ; nil if not in a project
         (root (if proj (project-root proj) default-directory))
         (proj-cfg (expand-file-name ".aider.conf.yml" root))
         (home-cfg (expand-file-name "~/.aider.conf.yml"))
         (cfg (if (file-exists-p proj-cfg) proj-cfg home-cfg)))
    (list "--config" cfg)))

(use-package aider
  :commands (aider-run-aider)
  :config
  (setenv "GEMINI_API_KEY" (auth-source-pick-first-password :host "gemini.google.com" :user "token"))
  :init
  ;; Always launch with your project’s config file
  (setq aider-args (neo/aider-config-args))

  ;; Optional: start in the project root (requires built-in 'project)
  (defun neo/aider ()
    "Start Aider in the current project’s root using .aider.conf.yml."
    (interactive)
    (let* ((proj (project-current t))
           (default-directory (project-root proj)))
      (aider-run-aider)))
  :bind (("C-c a" . neo/aider)))

(with-eval-after-load 'aider
  (defun neo/aider--refresh-args (&rest _)
    (setq aider-args (neo/aider-config-args)))
  (advice-add 'aider-run-aider :before #'neo/aider--refresh-args))

;; (use-package aidermacs
;;   :bind (("C-c a" . aidermacs-transient-menu))
;;   :config
;;   ; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
;;   (setenv "GEMINI_API_KEY" (auth-source-pick-first-password :host "gemini.google.com" :user "token"))
;;   :custom
;;   ; See the Configuration section below
;; ;  (aidermacs-default-chat-mode 'architect)
;;   (aidermacs-default-model "gemini"))
