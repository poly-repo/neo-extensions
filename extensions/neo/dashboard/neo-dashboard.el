;;; neo-dashboard.el --- Dashboard integration -*- lexical-binding: t; -*-

;; TODO require font Orbitron (a Google web font)

(require 'cl-lib)
(require 'subr-x)

(defconst neo--hacker-image
  (expand-file-name "hacker.png" (file-name-directory (or load-file-name byte-compile-current-file))))

(defface neo/dashboard-env-key-face
  '((t :foreground "sea green"))
  "Face for key labels (e.g. \"Instance:\") in the dashboard's
Neo Environment section."
  :group 'neo-ui)

(defface neo/dashboard-conflict-count-face
  '((t :inherit error :weight bold))
  "Face for a non-zero `neo/use-package' merge-conflict count in the
dashboard's Neo Environment section."
  :group 'neo-ui)

(defun neo/dashboard--fortune-line ()
  "Return a random fortune-cookie string for the dashboard's startup line.
Falls back to the stock `dashboard-init--info' startup message when the
`fortune' binary is not installed."
  (if (executable-find "fortune")
      (string-join (process-lines "fortune" "-s") "\n")
    (and (fboundp 'dashboard-init--info) (dashboard-init--info))))

(defcustom neo/dashboard-fortune-refresh-interval 30
  "Seconds between automatic fortune refreshes while the dashboard is
showing. Nil disables the automatic refresh timer."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'neo-ui)

(defvar neo/dashboard--fortune-timers (make-hash-table :test 'eq :weakness 'key)
  "Hash table mapping a dashboard buffer to its pending fortune-refresh
timer. Keyed by buffer object rather than a buffer-local variable
because `define-derived-mode' calls `kill-all-local-variables' *before*
running `dashboard-mode-hook' -- and `dashboard-refresh-buffer' always
re-runs `dashboard-mode' on every refresh -- so a buffer-local timer
variable would be silently wiped out from under
`neo/dashboard--start-fortune-timer' before it could cancel the
previous timer, leaking one extra live timer per refresh.")

(defun neo/dashboard--cancel-fortune-timer ()
  "Cancel the current buffer's pending fortune-refresh timer, if any."
  (when-let* ((timer (gethash (current-buffer) neo/dashboard--fortune-timers)))
    (cancel-timer timer))
  (remhash (current-buffer) neo/dashboard--fortune-timers))

(defun neo/dashboard--fortune-tick (buffer)
  "Refresh BUFFER's fortune, or stop once it is no longer shown."
  (if (and (buffer-live-p buffer)
           (eq buffer (window-buffer (selected-window))))
      (with-current-buffer buffer
        (dashboard-refresh-buffer))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (neo/dashboard--cancel-fortune-timer)))
    (remhash buffer neo/dashboard--fortune-timers)))

(defun neo/dashboard--start-fortune-timer ()
  "Start (or restart) the periodic dashboard fortune-refresh timer for the
current buffer. No-op when `neo/dashboard-fortune-refresh-interval' is
nil. Registers a buffer-local `kill-buffer-hook' so the timer is
cancelled once the dashboard buffer is killed."
  (neo/dashboard--cancel-fortune-timer)
  (when neo/dashboard-fortune-refresh-interval
    (puthash (current-buffer)
             (run-with-timer neo/dashboard-fortune-refresh-interval
                              neo/dashboard-fortune-refresh-interval
                              #'neo/dashboard--fortune-tick (current-buffer))
             neo/dashboard--fortune-timers))
  (add-hook 'kill-buffer-hook #'neo/dashboard--cancel-fortune-timer nil t))

(defun neo/dashboard-new-fortune ()
  "Refresh the dashboard immediately with a new fortune."
  (interactive)
  (dashboard-refresh-buffer))

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
  "Return a one-line status value (no REGISTRY-NAME prefix) for
REGISTRY-NAME, or nil when REGISTRY-NAME is unknown."
  (when-let* ((registry (alist-get registry-name neo/extension-registry-alist
                                   nil nil #'string=)))
    (if-let* ((override (neo--extension-registry-override registry)))
        (format "local %s" override)
      (let* ((cache-dir (neo/cache-file-path (format "extensions/%s/" registry-name)))
             (manifest-path (expand-file-name "extensions-current.el" cache-dir)))
        (format "cached %s" (neo/dashboard--format-path-status manifest-path))))))

(defun neo/dashboard--env-line (key value)
  "Format \"KEY: VALUE\" for the Neo Environment section, with KEY in
`neo/dashboard-env-key-face'."
  (format "%s: %s" (propertize key 'face 'neo/dashboard-env-key-face) value))

(defun neo/dashboard--extension-state (slug table)
  "Return yes/no for whether SLUG exists in hash TABLE."
  (if (and (hash-table-p table) (gethash slug table))
      "yes"
    "no"))

(defun neo/dashboard--debug-lines ()
  "Return a list of (LABEL . COMMAND) conses for the Neo Environment section.
COMMAND is nil for purely informational lines; when non-nil, activating
the line in the dashboard calls it interactively (see
`neo/dashboard-insert-neo-environment')."
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
                      "unavailable"))
         (conflict-count (if (fboundp 'neo/package-conflict-count)
                              (neo/package-conflict-count)
                            0))
         (neo-registry (neo/dashboard--registry-state "neo"))
         (mav-registry (neo/dashboard--registry-state "mav")))
    (delq nil
          (list
           (cons (neo/dashboard--env-line "Instance" (neo/get-emacs-instance-name)) nil)
           (when (fboundp 'dashboard-init--info)
             (cons (neo/dashboard--env-line "Startup" (dashboard-init--info)) nil))
           (cons (neo/dashboard--env-line "Extension source" extension-source) nil)
           (cons (neo/dashboard--env-line "User dir" user-emacs-directory) nil)
           (when (boundp 'neo/cache-directory)
             (cons (neo/dashboard--env-line "Cache dir" neo/cache-directory) nil))
           (when (fboundp 'neo/data-directory)
             (cons (neo/dashboard--env-line "Data dir" (neo/data-directory)) nil))
           (cons (neo/dashboard--env-line "Config DB" (neo/dashboard--format-path-status config-db)) nil)
           (cons (neo/dashboard--env-line "Enabled roots" enabled-roots) nil)
           (when framework
             (cons (neo/dashboard--env-line
                    "Framework"
                    (format "%d available / %d installed"
                            (hash-table-count available)
                            (hash-table-count installed)))
                   nil))
           (when framework
             (cons (format "%s available=%s installed=%s"
                           (propertize "neo:full-monty" 'face 'neo/dashboard-env-key-face)
                           (neo/dashboard--extension-state "neo:full-monty" available)
                           (neo/dashboard--extension-state "neo:full-monty" installed))
                   nil))
           (when framework
             (cons (format "%s available=%s installed=%s"
                           (propertize "neo:haskell" 'face 'neo/dashboard-env-key-face)
                           (neo/dashboard--extension-state "neo:haskell" available)
                           (neo/dashboard--extension-state "neo:haskell" installed))
                   nil))
           (when neo-registry (cons (neo/dashboard--env-line "neo" neo-registry) nil))
           (when mav-registry (cons (neo/dashboard--env-line "mav" mav-registry) nil))
           (cons (neo/dashboard--env-line
                  "Package conflicts"
                  (concat (if (> conflict-count 0)
                              (propertize (number-to-string conflict-count)
                                          'face 'neo/dashboard-conflict-count-face)
                            (number-to-string conflict-count))
                          (if (> conflict-count 0) " (RET for details)" "")))
                 (and (fboundp 'neo/package-conflicts-summary)
                      #'neo/package-conflicts-summary))))))

(defun neo/dashboard-insert-neo-environment (_list-size)
  "Insert Neo environment diagnostics into the dashboard."
  (let ((lines (neo/dashboard--debug-lines)))
    (dashboard-insert-section
     "Neo Environment:"
     lines
     (length lines)
     'neo-environment
     nil
     (lambda (&rest _)
       (when (cdr el) (call-interactively (cdr el))))
     (car el))))

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
  :config
  (neo--setup-banner)
  (add-to-list 'dashboard-item-generators
               '(neo-environment . neo/dashboard-insert-neo-environment))
  (add-to-list 'dashboard-item-generators
               '(neo-applications . neo/dashboard-insert-applications))
  (setq dashboard-items '((neo-applications . 20)
                                  (neo-environment . 9)
				  (projects . 5)
				  (agenda .5)))

  (dashboard-setup-startup-hook)
  (advice-add 'dashboard-open :after #'neo/dashboard--ensure-current-buffer-added)
  (advice-add 'dashboard-refresh-buffer :after #'neo/dashboard--ensure-current-buffer-added)
  ;; Land on the Dashboard perspective last, after perspective state has been
  ;; restored, so a restored project perspective does not win the startup race.
  (add-hook 'neo/after-perspective-restore-hook #'neo/dashboard-land-after-restore)
  :custom
  (dashboard-hide-cursor t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-icon-type 'all-the-icons)
  (dashboard-projects-backend 'projectile)
  (dashboard-project-switch-function #'projectile-switch-project)
  (initial-buffer-choice nil)
  (dashboard-init-info #'neo/dashboard--fortune-line)
  (dashboard-startupify-list
   '(dashboard-insert-banner
     dashboard-insert-newline
     dashboard-insert-banner-title
     dashboard-insert-newline
     dashboard-insert-init-info
     dashboard-insert-items
     dashboard-insert-newline))
  :bind
  (:map dashboard-mode-map
        ("q" . neo/dashboard-quit)
        ("SPC" . neo/dashboard-new-fortune))
  :hook
  (neo/after-framework-bootstrap . dashboard-insert-startupify-lists)
  (neo/after-framework-bootstrap . dashboard-initialize)
  (dashboard-mode . neo/dashboard--start-fortune-timer))
