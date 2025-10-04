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
(use-package transient)

(defun neo/aider-config-args ()
  "Return (--config PATH) where PATH is the project's .aider.conf.yml if present,
else ~/.aider.conf.yml."
  (let* ((proj (project-current))                       ; nil if not in a project
         (root (if proj (project-root proj) default-directory))
         (proj-cfg (expand-file-name ".aider.conf.yml" root))
         (home-cfg (expand-file-name "~/.aider.conf.yml"))
         (cfg (if (file-exists-p proj-cfg) proj-cfg home-cfg)))
    (list "--config" cfg)))


(defun neo/aider--project-default-directory-interactive (orig-fun &rest args)
  (if (called-interactively-p 'interactive)
      (let ((proj (project-current nil)))
        (if proj
            (let ((default-directory (file-name-as-directory (project-root proj))))
              (apply orig-fun args))
          (apply orig-fun args)))
    (apply orig-fun args)))

(use-package aider
  :config
  (setenv "GEMINI_API_KEY" (auth-source-pick-first-password :host "gemini.google.com" :user "token"))
  (global-set-key (kbd "C-c a") 'aider-transient-menu)
  (aider-magit-setup-transients)
  
(defun neo/aider--find-nearest-config (dir)
  "Return the nearest .aider.conf.yml path by searching ancestors of DIR.
Return nil if none found. DIR should be a directory string."
  (when dir
    (let* ((root (locate-dominating-file dir ".aider.conf.yml"))
           (proj-cfg (when root (expand-file-name ".aider.conf.yml" root))))
      (when (and proj-cfg (file-exists-p proj-cfg)) proj-cfg))))

(defun neo/aider--compute-call-directory ()
  "Return the most sensible directory for determining config when calling Aider.
Prefer `default-directory' if bound, else the directory of current buffer's file,
else `default-directory' as fallback."
  (or (and (boundp 'default-directory) default-directory)
      (and (buffer-file-name) (file-name-directory (buffer-file-name)))
      default-directory))

(defun neo/aider--advice-set-config-and-root (orig-fun &rest args)
  "Advice for `aider-run-aider' that:
- computes nearest .aider.conf.yml from the call directory,
- binds `aider-args' to use that config if present,
- binds `default-directory' to the project root (if any) discovered from the call directory,
then calls ORIG-FUN with ARGS.
Works for interactive and non-interactive callers."
  (let* ((call-dir (file-name-as-directory (or (neo/aider--compute-call-directory) default-directory)))
         ;; find nearest config relative to call-dir
         (cfg (neo/aider--find-nearest-config call-dir))
         ;; determine project root if project.el recognizes a project when default-directory is call-dir
         (proj (let ((default-directory call-dir)) (project-current nil)))
         (proj-root (when proj (file-name-as-directory (project-root proj))))
         ;; prepare local bindings
         (aider-args-binding (when cfg (list "--config" cfg)))
         (default-dir-to-use (or proj-root call-dir)))
    (let ((default-directory default-dir-to-use))
      (if aider-args-binding
          (let ((aider-args aider-args-binding))
            (apply orig-fun args))
        ;; no config found — call without binding aider-args (falls back to whatever aider does)
        (apply orig-fun args)))))

;; install advice (idempotent if re-evaluated)
(advice-add 'aider-run-aider :around #'neo/aider--advice-set-config-and-root)


  :init
  ;; Always launch with your project’s config file
  (setq aider-args (neo/aider-config-args)))


;; (with-eval-after-load 'aider
;;   (defun neo/aider--refresh-args (&rest _)
;;     (setq aider-args (neo/aider-config-args)))
;;   (advice-add 'aider-run-aider :before #'neo/aider--refresh-args))

;; (use-package aidermacs
;;   :bind (("C-c a" . aidermacs-transient-menu))
;;   :config
;;   ; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
;;   (setenv "GEMINI_API_KEY" (auth-source-pick-first-password :host "gemini.google.com" :user "token"))
;;   :custom
;;   ; See the Configuration section below
;; ;  (aidermacs-default-chat-mode 'architect)
;;   (aidermacs-default-model "gemini"))
