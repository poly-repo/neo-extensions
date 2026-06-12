;;; neo-dashboard.el --- Dashboard integration -*- lexical-binding: t; -*-

;; TODO require font Orbitron (a Google web font)

(defconst neo--hacker-image
  (expand-file-name "hacker.png" (file-name-directory (or load-file-name byte-compile-current-file))))


(neo/use-package dashboard-hackernews)

(defun neo--setup-banner ()
  (if (file-readable-p neo--hacker-image)
      (progn
        (setq dashboard-startup-banner neo--hacker-image
              dashboard-banner-logo-title "W   E      A   R   E      L   E   G   I   O   N   S")
        (set-face-attribute 'dashboard-banner-logo-title nil
                            :font "Orbitron"
                            :height 200
                            :weight 'bold
                            :foreground "#196DB5"))
    (setq dashboard-startup-banner 'logo
          dashboard-banner-logo-title "Welcome to Emacs Neo")))
  
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


(defun neo/dashboard--buffer-name ()
  "Return the dashboard buffer name."
  (if (boundp 'dashboard-buffer-name)
      dashboard-buffer-name
    "*dashboard*"))

(defun neo/dashboard--perspective-available-p ()
  "Return non-nil when perspective support can be used."
  (and (require 'perspective nil t)
       (fboundp 'persp-current-name)
       (fboundp 'persp-switch)))

(defun neo/dashboard--ensure-perspective-mode ()
  "Enable `persp-mode' when perspective is available."
  (when (and (fboundp 'persp-mode)
             (not (bound-and-true-p persp-mode)))
    (persp-mode 1)))

(defun neo/dashboard--add-buffer (buffer)
  "Add BUFFER to the current perspective when perspective is available."
  (when (and (buffer-live-p buffer)
             (fboundp 'persp-add-buffer))
    (persp-add-buffer buffer)))

(defun neo/dashboard--ensure-current-buffer-added (&optional buffer)
  "Ensure BUFFER belongs to the current perspective.
When BUFFER is nil, use the current dashboard buffer if it exists."
  (when-let ((dashboard-buffer
              (cond
               ((buffer-live-p buffer) buffer)
               ((get-buffer (neo/dashboard--buffer-name))))))
    (neo/dashboard--add-buffer dashboard-buffer)))

(defun neo/dashboard--current-perspective-dashboard-p (current-persp)
  "Return non-nil when CURRENT-PERSP is a dashboard perspective."
  (or (string= current-persp neo/dashboard-persp)
      (string= current-persp (neo/dashboard--buffer-name))))

(defun neo/dashboard-initial-buffer ()
  "Return the dashboard buffer for `initial-buffer-choice`."
  (when (require 'dashboard nil t)
    (neo/dashboard--enter)
    (dashboard-open)
    (when-let ((buffer (get-buffer (neo/dashboard--buffer-name))))
      (neo/dashboard--ensure-current-buffer-added buffer)
      buffer)))


(defun neo/dashboard--enter ()
  "Record originating perspective and switch to dashboard perspective."
  (when (neo/dashboard--perspective-available-p)
    (neo/dashboard--ensure-perspective-mode)
    (let ((current-persp (persp-current-name)))
      (unless (or neo/dashboard--origin-persp
                  (neo/dashboard--current-perspective-dashboard-p current-persp))
        (setq neo/dashboard--origin-persp current-persp))
      (unless (neo/dashboard--current-perspective-dashboard-p current-persp)
        (persp-switch neo/dashboard-persp)))))

(defun neo/dashboard--leave ()
  "Restore perspective active before dashboard."
  (when (and (featurep 'perspective)
             neo/dashboard--origin-persp)
    (persp-switch neo/dashboard--origin-persp)
    (setq neo/dashboard--origin-persp nil)))

(defun neo/dashboard ()
  "Show the dashboard in the dashboard perspective."
  (interactive)
  (when-let ((buffer (neo/dashboard-initial-buffer)))
    (switch-to-buffer buffer)
    (neo/dashboard--ensure-current-buffer-added buffer)))

(defun neo/dashboard-quit ()
  "Quit the dashboard and restore original perspective."
  (interactive)
  (when-let ((buffer (get-buffer (neo/dashboard--buffer-name))))
    (kill-buffer buffer))
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
  (advice-add 'dashboard-open :after #'neo/dashboard--ensure-current-buffer-added)
  (advice-add 'dashboard-refresh-buffer :after #'neo/dashboard--ensure-current-buffer-added)
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
  (neo/after-framework-bootstrap . #'dashboard-initialize))
