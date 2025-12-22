;; TODO require font Orbitron (a Google web font)

(defconst neo--hacker-image
  (expand-file-name "hacker.png" (file-name-directory (or load-file-name byte-compile-current-file))))


(neo/use-package dashboard-hackernews)

(defun neo--setup-banner ()
  (if (file-readable-p neo--hacker-image)
      (progn
        (setq dashboard-startup-banner neo--hacker-image)
        (setq dashboard-banner-logo-title "W   E      A   R   E      L   E   G   I   O   N   S")
        (set-face-attribute 'dashboard-banner-logo-title nil :font "Orbitron" :height 200 :weight 'bold :foreground "#196DB5"))
    (setq dashboard-startup-banner 'logo)
    (setq dashboard-banner-logo-title "Welcome to Emacs Neo")))
  
;; (neo/use-package dashboard
;;     :after dashboard-hackernews
;;     :delight (dashboard-mode page-break-lines-mode)
;;     :config
;;     (if (file-readable-p neo--hacker-image)
;;         (progn
;;           (setq dashboard-startup-banner neo--hacker-image)
;;           (setq dashboard-banner-logo-title "W   E      A   R   E      L   E   G   I   O   N   S")
;;           (set-face-attribute 'dashboard-banner-logo-title nil :font "Orbitron" :height 200 :weight 'bold :foreground "#196DB5"))
;;       (setq dashboard-startup-banner 'logo)
;;       (setq dashboard-banner-logo-title "Welcome to Emacs Neo"))
;;     (setq dashboard-items
;;           '((recent . 5)
;;             (agenda . 10)
;;             (hackernews . 5)))
;;     (setq initial-buffer-choice nil)
;;     (dashboard-setup-startup-hook)
;;     :hook (
;;            (after-init     . dashboard-refresh-buffer)
;;            (dashboard-mode-hook . (lambda () (message "HOOK") (setq cursor-type nil))))
;;     )

(defvar neo/dashboard-persp "Dashboard"
  "Name of the dashboard perspective.")

(defvar neo/dashboard--origin-persp nil
  "Perspective active before the dashboard was shown.")


(defun neo/dashboard-initial-buffer ()
  "Return the dashboard buffer for `initial-buffer-choice`."
  (require 'dashboard)
  (neo/dashboard--enter)
  (dashboard-open)
  (get-buffer "*dashboard*"))


(defun neo/dashboard--enter ()
  "Record originating perspective and switch to dashboard perspective."
  (when (featurep 'perspective)
    (unless neo/dashboard--origin-persp
      ;; Save current perspective (string)
      (setq neo/dashboard--origin-persp (persp-current-name)))
    ;; Switch to dashboard perspective (creates if missing)
    (persp-switch neo/dashboard-persp)))

(defun neo/dashboard--leave ()
  "Restore perspective active before dashboard."
  (when (and (featurep 'perspective)
             neo/dashboard--origin-persp)
    (persp-switch neo/dashboard--origin-persp)
    (setq neo/dashboard--origin-persp nil)))

(defun neo/dashboard ()
  (interactive)
  (neo/dashboard-initial-buffer))

(defun neo/dashboard-quit ()
  "Quit the dashboard and restore original perspective."
  (interactive)
  (kill-buffer "*dashboard*")
  (neo/dashboard--leave))

(neo/use-package dashboard
  :after dashboard-hackernews
  :config
  (neo--setup-banner)
  (setq dashboard-items '((recents . 5)
			  (projects . 5)
			  (agenda .5)
			  (hackernews . 10)))

  (dashboard-setup-startup-hook)
  :custom
  (dashboard-hide-cursor t)
  ;; (dashboard-items
  ;;          '((recent . 5)
  ;;            (agenda . 10)
  ;;            (hackernews . 5)))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-icon-type 'all-the-icons)
  (dashboard-projects-backend 'projectile)
  (dashboard-project-switch-function #'projectile-switch-project)
  (initial-buffer-choice nil)
  :bind
  (:map dashboard-mode-map
        ("q" . neo/dashboard-quit))
  :hook
  (neo/after-framework-bootstrap .  #'dashboard-insert-startupify-lists)
  (neo/after-framework-bootstrap . #'dashboard-initialize)
  )
