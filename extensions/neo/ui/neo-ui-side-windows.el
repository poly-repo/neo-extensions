(defvar neo/side-window-stack nil
  "Stack (LIFO) of side windows in order of creation.")

(defun neo/track-side-windows (frame)
  "Track side windows on FRAME after window state changes."
  (dolist (w (window-list frame 'no-mini))
    (when (and (window-live-p w)
               (window-parameter w 'window-side)
               (not (memq w neo/side-window-stack)))
      (push w neo/side-window-stack))))

;(remove-hook 'window-buffer-change-functions #'neo/track-side-window)
(add-hook 'window-state-change-functions #'neo/track-side-windows)

(defun neo/delete-last-side-window ()
  "Delete the most recently opened live side window."
  (interactive)
  (while (and neo/side-window-stack
              (not (window-live-p (car neo/side-window-stack))))
    (pop neo/side-window-stack))
  (if-let ((window (pop neo/side-window-stack)))
      (delete-window window)
    (message "No side windows to close")))

;; temp only, will probavbly be the qq chord
(global-set-key (kbd "C-c w d") #'neo/delete-last-side-window)
(with-eval-after-load 'key-chord
  (key-chord-define-global "qq" 'neo/delete-last-side-window))

;; the following uses a package vendored from
;; https://github.com/MArpogaus/auto-side-windows. Temptative only.
;; TODO: individual extensions should be self-register for side
;; windows instead of having everything centralized here.
(require 'auto-side-windows)
(use-package auto-side-windows
  :ensure nil
  :preface
  (defun my/get-header-line-icon-for-buffer (buffer)
    (with-current-buffer buffer
      (unless (boundp 'header-line-icon)
        (setq-local header-line-icon
                    (cond
                     ((buffer-match-p "Warning" buffer) '("  !  " . warning))
                     ((buffer-match-p '(or "^\\*Backtrace\\*$" ".*[Ee]rror.*") buffer) '("  !  " . error))
                     ((buffer-match-p '(or "^COMMIT_EDITMSG$" "^\\*diff-hl\\*$") buffer) '("    " . success))
                     ((buffer-match-p "^\\*Org Src.*\\*" buffer) '("     " . mode-line-emphasis))
                     ((buffer-match-p "^\\*Org Agenda\\*$" buffer) '("    " . mode-line-emphasis))
                     (t '("  ?  " . mode-line-emphasis)))))
      header-line-icon))
  (defun my/install-top-side-window-face-remaps (buffer foreground background)
    (with-current-buffer buffer
      (unless (bound-and-true-p top-side-window-face-remaps-cookies)
        (setq-local top-side-window-face-remaps-cookies
                    (list
                     (face-remap-add-relative 'header-line
                                              `(:box nil :underline nil :overline ,background))
                     (face-remap-add-relative 'fringe
                                              `(:background ,background))
                     (face-remap-add-relative 'mode-line-active
                                              `(:overline ,background :underline nil :height 0))
                     (face-remap-add-relative 'mode-line-inactive
                                              `(:overline ,background :underline nil :height 0))
                     )))))
  (defvar my/header-line-format-top
    '(:eval
      (let*
          ((buffer (current-buffer))
           (prefix-and-face (my/get-header-line-icon-for-buffer buffer))
           (prefix (car prefix-and-face))
           (background (face-foreground (cdr prefix-and-face)))
           (foreground (face-background (cdr prefix-and-face) nil 'default))
           (prefix-face (list :inherit 'bold :background background :foreground foreground))
           (buffer-face (list :inherit 'bold :foreground background)))
        (set-window-fringes nil 1 1 t)
        (my/install-top-side-window-face-remaps buffer foreground background)
        (list
         (propertize prefix 'face prefix-face 'display '(space-width 0.7))
         (propertize (format-mode-line " %b ") 'face buffer-face)
         (propertize " " 'display `(space :align-to right))
         (propertize " " 'face prefix-face 'display '(space-width 1))))))
  :custom
  (auto-side-windows-top-window-parameters `((mode-line-format . t)
                                             (header-line-format . ,my/header-line-format-top)))
  (auto-side-windows-before-display-hook '((lambda (buffer)
                                             (with-current-buffer buffer
                                               (when (bound-and-true-p top-side-window-face-remaps-cookies)
                                                 (dolist (cookie top-side-window-face-remaps-cookies)
                                                   (face-remap-remove-relative cookie))
                                                 (kill-local-variable 'top-side-window-face-remaps-cookies))))))
  (auto-side-windows-before-toggle-hook auto-side-windows-before-display-hook)


  ;; Respects display actions when switching buffers
  (switch-to-buffer-obey-display-actions t)

  ;; Top side window configurations
  (auto-side-windows-top-buffer-names
   '("^\\*Backtrace\\*$"
     "^\\*Org Agenda\\*$"
     "^\\*Org Src.*\\*"
     "^\\*Org-Babel Error Output\\*"
     "^\\*Async-native-compile-log\\*$"
     "^\\*Compile-Log\\*$"
     "^\\*Multiple Choice Help\\*$"
     "^\\*Quick Help\\*$"
     "^\\*TeX Help\\*$"
     "^\\*TeX errors\\*$"
     "^\\*Warnings\\*$"
     "^\\*Process List\\*$"))
  (auto-side-windows-top-buffer-modes
   '(flymake-diagnostics-buffer-mode
     locate-mode
     occur-mode
     grep-mode
     xref--xref-buffer-mode))

  ;; Bottom side window configurations
  (auto-side-windows-bottom-buffer-names
   '("^\\*eshell\\*$"
     "^\\*shell\\*$"
     "^\\*term\\*$"))
  (auto-side-windows-bottom-buffer-modes
   '(eshell-mode
     calc-mode
     calendar-mode
     shell-mode
     term-mode
     comint-mode
     debugger-mode))

  ;; Left side window configurations
  (auto-side-windows-left-buffer-names
   '())
  (auto-side-windows-left-buffer-modes
   '(treemacs-mode))
  
  ;; Right side window configurations
  (auto-side-windows-right-buffer-names
   '("^\\*eldoc.*\\*$"
     "^\\*info\\*$"
     "^.personal-notes.org$"
     "^\\*Metahelp\\*$"))
  (auto-side-windows-right-buffer-modes
   '(Info-mode
     TeX-output-mode
     eldoc-mode
     help-mode
     helpful-mode
     shortdoc-mode))

  ;; Example: Custom parameters for top windows (e.g., fit height to buffer)
  ;; (auto-side-windows-top-alist '((window-height . fit-window-to-buffer)))
  ;; (auto-side-windows-top-window-parameters '((mode-line-format . ...))) ;; Adjust mode-line

  ;; Maximum number of side windows on the left, top, right and bottom
  (window-sides-slots '(1 1 1 1)) ; Example: Allow one window per side

  ;; Force left and right side windows to occupy full frame height
  (window-sides-vertical t)

  ;; Make changes to tab-/header- and mode-line-format persistent when toggling windows visibility
  (window-persistent-parameters
   (append window-persistent-parameters
           '((tab-line-format . t)
             (header-line-format . t)
             (mode-line-format . t))))
  :bind ;; Example keybindings (adjust prefix as needed)
  (:map global-map ; Or your preferred keymap prefix
        ("C-c w t" . auto-side-windows-display-buffer-top)
        ("C-c w b" . auto-side-windows-display-buffer-bottom)
        ("C-c w l" . auto-side-windows-display-buffer-left)
        ("C-c w r" . auto-side-windows-display-buffer-right)
        ("C-c w w" . auto-side-windows-switch-to-buffer)
        ("C-c w t" . window-toggle-side-windows) ; Toggle all side windows
        ("C-c w T" . auto-side-windows-toggle-side-window)) ; Toggle current buffer in/out of side window
  :hook
  (after-init . auto-side-windows-mode))

(provide 'neo-ui-side-windows)
