;;; neo-dashboard.el --- Dashboard integration -*- lexical-binding: t; -*-

;; TODO require font Orbitron (a Google web font)

(require 'cl-lib)
(require 'subr-x)

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

(defvar neo/dashboard-persp "App:Dashboard"
  "Name of the dashboard perspective.")

(defvar neo/dashboard--origin-persp nil
  "Perspective active before the dashboard was shown.")

;; Pushed to by `neo/application' (core/neo-application.el, loaded well
;; before this extension) right before it switches to "App:Dashboard" --
;; see `neo/dashboard--enter'.
(defvar neo--application-perspective-stack)


(defun neo/dashboard--buffer-name ()
  "Return the dashboard buffer name."
  (if (boundp 'dashboard-buffer-name)
      dashboard-buffer-name
    "*dashboard*"))

(defun neo/dashboard--framework ()
  "Return the current Neo framework instance when available."
  (when (bound-and-true-p neo--framework)
    neo--framework))

(defun neo/dashboard--config-value (key)
  "Return config KEY or nil when it is unavailable."
  (when (fboundp 'neo/get-config)
    (neo/get-config key)))

(defun neo/dashboard--format-path-status (path)
  "Return a compact status string for PATH."
  (cond
   ((not path) "unavailable")
   ((file-symlink-p path)
    (format "%s -> %s" path (file-symlink-p path)))
   ((file-exists-p path) path)
   (t (format "%s [missing]" path))))

(defun neo/dashboard--registry-state (registry-name)
  "Return a one-line status string for REGISTRY-NAME."
  (when-let* ((registry (alist-get registry-name neo/extension-registry-alist
                                   nil nil #'string=)))
    (if-let* ((override (neo--extension-registry-override registry)))
        (format "%s: local %s" registry-name override)
      (let* ((cache-dir (neo/cache-file-path (format "extensions/%s/" registry-name)))
             (manifest-path (expand-file-name "extensions-current.el" cache-dir)))
        (format "%s: cached %s"
                registry-name
                (neo/dashboard--format-path-status manifest-path))))))

(defun neo/dashboard--extension-state (slug table)
  "Return yes/no for whether SLUG exists in hash TABLE."
  (if (and (hash-table-p table) (gethash slug table))
      "yes"
    "no"))

(defun neo/dashboard--debug-lines ()
  "Return a list of environment lines for dashboard debugging."
  (let* ((framework (neo/dashboard--framework))
         (available (and framework (neo-framework-available-extensions framework)))
         (installed (and framework (neo-framework-installed-extensions framework)))
         (enabled-roots (or (neo/dashboard--config-value "enabled-extensions") "nil"))
         (extension-source
          (if (and (fboundp 'neo/use-local-extension-sources-p)
                   (neo/use-local-extension-sources-p))
              "local checkout"
            "cached release"))
         (config-db (if (fboundp 'neo/config-db-path)
                        (neo/config-db-path)
                      "unavailable")))
    (delq nil
          (list
           (format "Instance: %s" (neo/get-emacs-instance-name))
           (format "Extension source: %s" extension-source)
           (format "User dir: %s" user-emacs-directory)
           (when (boundp 'neo/cache-directory)
             (format "Cache dir: %s" neo/cache-directory))
           (when (fboundp 'neo/data-directory)
             (format "Data dir: %s" (neo/data-directory)))
           (format "Config DB: %s" (neo/dashboard--format-path-status config-db))
           (format "Enabled roots: %s" enabled-roots)
           (when framework
             (format "Framework: %d available / %d installed"
                     (hash-table-count available)
                     (hash-table-count installed)))
           (when framework
             (format "neo:full-monty available=%s installed=%s"
                     (neo/dashboard--extension-state "neo:full-monty" available)
                     (neo/dashboard--extension-state "neo:full-monty" installed)))
           (when framework
             (format "neo:haskell available=%s installed=%s"
                     (neo/dashboard--extension-state "neo:haskell" available)
                     (neo/dashboard--extension-state "neo:haskell" installed)))
           (neo/dashboard--registry-state "neo")
           (neo/dashboard--registry-state "mav")))))

(defun neo/dashboard-insert-neo-environment (_list-size)
  "Insert Neo environment diagnostics into the dashboard."
  (let ((lines (neo/dashboard--debug-lines)))
    (dashboard-insert-section
     "Neo Environment:"
     lines
     (length lines)
     'neo-environment
     nil
     `(lambda (&rest _))
     el)))

(defun neo/dashboard--application-entries ()
  "Return a list of (LABEL . COMMAND) for registered Neo applications."
  (when (fboundp 'neo/applications)
    (mapcar
     (lambda (app)
       (let* ((name (neo/application-name app))
              (binding (neo/application-binding app))
              (command (neo/application-command app))
              (label (if (and (stringp binding) (> (length binding) 0))
                         (format "%s  (M-a %s)" name binding)
                       name)))
         (cons label command)))
     (neo/applications))))

(defun neo/dashboard-insert-applications (list-size)
  "Insert the registered Neo applications into the dashboard.
LIST-SIZE bounds the number of applications shown."
  (let ((entries (neo/dashboard--application-entries)))
    (dashboard-insert-section
     "Neo Applications:"
     entries
     (or list-size (length entries))
     'neo-applications
     nil
     (lambda (&rest _)
       (when (cdr el) (call-interactively (cdr el))))
     (car el))))

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
  (when-let* ((dashboard-buffer
              (cond
               ((buffer-live-p buffer) buffer)
               ((get-buffer (neo/dashboard--buffer-name))))))
    (neo/dashboard--add-buffer dashboard-buffer)))

(defun neo/dashboard--show-buffer (buffer)
  "Show BUFFER as the sole window and keep it in the current perspective."
  (when (buffer-live-p buffer)
    (switch-to-buffer buffer)
    (neo/dashboard--ensure-current-buffer-added buffer)
    (when-let* ((window (get-buffer-window buffer t)))
      (select-window window)
      (delete-other-windows window))))

(defun neo/dashboard--current-perspective-dashboard-p (current-persp)
  "Return non-nil when CURRENT-PERSP is a dashboard perspective."
  (or (string= current-persp neo/dashboard-persp)
      (string= current-persp "Dashboard")
      (string= current-persp (neo/dashboard--buffer-name))))

(defun neo/dashboard-initial-buffer ()
  "Return the dashboard buffer for `initial-buffer-choice`."
  (when (require 'dashboard nil t)
    (neo/dashboard--enter)
    (dashboard-open)
    (when-let* ((buffer (get-buffer (neo/dashboard--buffer-name))))
      (neo/dashboard--ensure-current-buffer-added buffer)
      buffer)))


(defun neo/dashboard--enter ()
  "Record originating perspective and switch to dashboard perspective.

When entered via the `neo/application' \"Dashboard\" wrapper (`M-a d'),
that macro already switched to \"App:Dashboard\" before running this as
its :setup, so `persp-current-name' here is the dashboard's own
perspective, not the real origin. In that case the origin is instead
the top of `neo--application-perspective-stack': the wrapper pushes the
perspective it switched away from immediately before running :setup, and
nothing else can push onto the stack in between, so popping it here both
recovers the real origin and keeps the stack balanced for any further
nested application launches."
  (when (neo/dashboard--perspective-available-p)
    (neo/dashboard--ensure-perspective-mode)
    (let* ((current-persp (persp-current-name))
           (already-in-dashboard
            (neo/dashboard--current-perspective-dashboard-p current-persp))
           (origin
            (if already-in-dashboard
                (let ((pushed (pop neo--application-perspective-stack)))
                  (and pushed
                       (not (neo/dashboard--current-perspective-dashboard-p pushed))
                       pushed))
              current-persp)))
      (unless neo/dashboard--origin-persp
        (setq neo/dashboard--origin-persp origin))
      (unless already-in-dashboard
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
  (when-let* ((buffer (neo/dashboard-initial-buffer)))
    (neo/dashboard--show-buffer buffer)))

(defun neo/dashboard-quit ()
  "Quit the dashboard and restore original perspective."
  (interactive)
  (when-let* ((buffer (get-buffer (neo/dashboard--buffer-name))))
    (kill-buffer buffer))
  (neo/dashboard--leave))

(defun neo/dashboard-land-after-restore ()
  "Show the dashboard at startup, recording the displaced perspective.
Run after `persp-state-load' has restored and selected a saved
perspective.  The origin is cleared first so `neo/dashboard--enter'
records that restored perspective, letting `neo/dashboard-quit' (\"q\")
return to whatever was active before the dashboard took over."
  (setq neo/dashboard--origin-persp nil)
  (neo/dashboard))

(neo/use-package dashboard
  :after dashboard-hackernews
  :config
  (neo--setup-banner)
  (add-to-list 'dashboard-item-generators
               '(neo-environment . neo/dashboard-insert-neo-environment))
  (add-to-list 'dashboard-item-generators
               '(neo-applications . neo/dashboard-insert-applications))
  (setq dashboard-items '((neo-applications . 20)
                                  (neo-environment . 9)
                                  (recents . 5)
				  (projects . 5)
				  (agenda .5)
				  (hackernews . 10)))

  (dashboard-setup-startup-hook)
  (advice-add 'dashboard-open :after #'neo/dashboard--ensure-current-buffer-added)
  (advice-add 'dashboard-refresh-buffer :after #'neo/dashboard--ensure-current-buffer-added)
  ;; Land on the Dashboard perspective last, after perspective state has been
  ;; restored, so a restored project perspective does not win the startup race.
  (add-hook 'neo/after-perspective-restore-hook #'neo/dashboard-land-after-restore)
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
