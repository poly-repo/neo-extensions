;;; -*- lexical-binding: t -*-

(require 'neo-config)

(defconst neo/theme-config-key "theme"
  "Config DB key for the currently selected Neo theme.")

(defvar neo/current-theme nil
  "Theme currently applied by Neo, or nil for the default Emacs theme.")

(defvar neo/after-theme-load-hook nil
  "Hook run after a theme is loaded for Neo.")

(defun neo/run-after-theme-load (theme &rest _)
  "Record THEME as current and run `neo/after-theme-load-hook'."
  (setq neo/current-theme theme)
  (run-hooks 'neo/after-theme-load-hook))

(add-hook 'enable-theme-functions #'neo/run-after-theme-load)
(add-hook 'neo/after-theme-load-hook #'neo/save-initial-frame-properties)

(neo/use-package modus-themes
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-italic-constructs t))

(neo/use-package doom-themes)
(neo/use-package ef-themes)

(defun neo/theme--normalize (theme)
  "Return THEME as a symbol."
  (cond
   ((symbolp theme) theme)
   ((stringp theme) (intern theme))
   (t (error "neo: invalid theme value %S" theme))))

(defun neo/theme--available-p (theme)
  "Return non-nil when THEME is available to `load-theme'."
  (memq theme (custom-available-themes)))

(defun neo/theme--restore-disabled-themes (themes)
  "Re-enable THEMES after a failed theme switch."
  (dolist (theme themes)
    (ignore-errors
      (enable-theme theme))))

(defun neo/load-theme-internal (theme &optional persist)
  "Load THEME and disable previously enabled themes.
When PERSIST is non-nil, save THEME to the Neo config DB."
  (let* ((theme (neo/theme--normalize theme))
         (previous-themes (copy-sequence custom-enabled-themes)))
    (if (not (neo/theme--available-p theme))
        (progn
          (message "neo: theme %s is not available; keeping the current theme" theme)
          nil)
      (condition-case err
          (progn
            (mapc #'disable-theme previous-themes)
            (load-theme theme t)
            (when persist
              (neo/set-config neo/theme-config-key (symbol-name theme)))
            theme)
        (error
         (neo/theme--restore-disabled-themes previous-themes)
         (message "neo: failed to load theme %s: %s"
                  theme (error-message-string err))
         nil)))))

(defun neo/load-theme ()
  "Prompt for a theme and persist the selection in the Neo config DB."
  (interactive)
  (let ((theme
         (completing-read "Load theme: "
                          (mapcar #'symbol-name (custom-available-themes))
                          nil t)))
    (unless (neo/load-theme-internal theme t)
      (user-error "neo: failed to load theme %s" theme))))

(defun neo/restore-persisted-theme ()
  "Restore the user's persisted theme choice when available.
When no theme has been saved yet, keep the default Emacs theme."
  (when-let* ((saved-theme-name (neo/get-config neo/theme-config-key))
              ((not (equal saved-theme-name ""))))
    (neo/load-theme-internal saved-theme-name)))

(add-hook 'neo/after-framework-bootstrap-hook #'neo/restore-persisted-theme)

(provide 'neo-ui-themes)
