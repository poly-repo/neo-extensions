;;; -*- lexical-binding: t -*-

;;; This is programing-foundation, a NEO extension
;;;
;;; Language agnostic software development support

(neo/use-package jsonrpc)

(require 'neo-programming-foundation-treesit)

(neo/use-package flymake)
(neo/use-package posframe)
(neo/use-package transient)
(neo/use-package vui)

(declare-function eglot-managed-p "eglot")
(declare-function eglot-format-buffer "eglot")
(declare-function eglot-hover-eldoc-function "eglot")
(declare-function posframe-hide "posframe")
(declare-function posframe-show "posframe")

(defcustom neo/eglot-hover-popup-enabled t
  "Show automatic Eglot hover docs in a popup when Emacs can do so.

The point of this toggle is to make the richer LSP payload visible
without forcing users to watch the echo area.  When disabled, or when
popup support is unavailable, Eglot falls back to the usual echo-area
display."
  :type 'boolean
  :group 'neo)

(defcustom neo/eglot-hover-popup-buffer " *neo-eglot-hover*"
  "Use this buffer name for the Eglot hover popup."
  :type 'string
  :group 'neo)

(defcustom neo/eglot-hover-popup-delay 0.35
  "Wait this many seconds before showing an Eglot hover popup."
  :type 'number
  :group 'neo)

(defcustom neo/eglot-hover-popup-poll-interval 0.05
  "Poll the mouse at this interval while Eglot hover popups are enabled."
  :type 'number
  :group 'neo)

(defcustom neo/eglot-hover-popup-exit-delay 0.12
  "Keep an Eglot hover popup alive this long after the mouse leaves it.

The delay is intentionally short.  It exists so users can move the
mouse from an identifier into the popup itself without the popup
vanishing in the gap between the two."
  :type 'number
  :group 'neo)

(defvar neo--eglot-hover-timer nil
  "Timer used to drive mouse-based Eglot hover popups.")

(when (timerp neo--eglot-hover-timer)
  ;; A previous version could install a malformed repeating timer.
  ;; Cancelling it at reload time lets `eval-buffer' repair the live
  ;; session instead of requiring a restart.
  (cancel-timer neo--eglot-hover-timer)
  (setq neo--eglot-hover-timer nil))

(defvar neo--eglot-hover-candidate nil
  "Most recent symbol-sized mouse hover candidate.")

(defvar neo--eglot-hover-candidate-since 0.0
  "Timestamp at which `neo--eglot-hover-candidate' last changed.")

(defvar neo--eglot-hover-target nil
  "Current mouse hover target for Eglot popups.

When non-nil, this is a plist containing the window, buffer, bounds,
and point currently being hovered.")

(defvar neo--eglot-hover-popup-frame nil
  "Child frame currently showing the Eglot hover popup.")

(defvar neo--eglot-hover-away-since 0.0
  "Timestamp at which the mouse last left both the hover target and popup.")

(defun neo--eglot-hover-popup-available-p ()
  "Return non-nil when Eglot hover docs can use a popup."
  (and neo/eglot-hover-popup-enabled
       (display-graphic-p)
       (fboundp 'posframe-show)
       (fboundp 'posframe-hide)
       (not (minibufferp (current-buffer)))))

(defun neo--eglot-hide-hover-popup ()
  "Dismiss the Eglot hover popup for the current frame."
  (setq neo--eglot-hover-popup-frame nil)
  (when (fboundp 'posframe-hide)
    (posframe-hide neo/eglot-hover-popup-buffer)))

(defun neo--eglot-reset-hover-popup ()
  "Forget the current hover target and dismiss any Eglot hover popup."
  (setq neo--eglot-hover-candidate nil)
  (setq neo--eglot-hover-candidate-since 0.0)
  (setq neo--eglot-hover-away-since 0.0)
  (setq neo--eglot-hover-target nil)
  (neo--eglot-hide-hover-popup))

(defun neo--eglot-format-docs-for-popup (docs)
  "Render DOCS into one string suitable for popup display.

The popup needs the same information ElDoc already computed, but laid
out vertically so type information and longer hover docs remain
readable at a glance."
  (with-temp-buffer
    (setq-local nobreak-char-display nil)
    (cl-loop
     for (doc . plist) in docs
     for thing = (plist-get plist :thing)
     for face = (plist-get plist :face)
     for index from 0
     do
     (when (> index 0)
       (insert "\n\n"))
     (when thing
       (insert (propertize (format "%s" thing)
                           'face (or face 'bold))
               "\n"))
     (insert (string-trim-right doc)))
    (string-trim-right (buffer-string))))

(defun neo--eglot-hover-target-equal-p (left right)
  "Return non-nil when LEFT and RIGHT denote the same hover target."
  (and left
       right
       (eq (plist-get left :window) (plist-get right :window))
       (eq (plist-get left :buffer) (plist-get right :buffer))
       (= (plist-get left :start) (plist-get right :start))
       (= (plist-get left :end) (plist-get right :end))))

(defun neo--eglot-bounds-at-point ()
  "Return a compact hover span at point, or nil when nothing is hoverable."
  (or (bounds-of-thing-at-point 'symbol)
      (bounds-of-thing-at-point 'word)
      (when (and (not (eobp))
                 (not (looking-at-p "[[:space:]\n]")))
        (cons (point) (1+ (point))))))

(defun neo--eglot-mouse-hover-target ()
  "Return hover target metadata for the symbol currently under the mouse."
  (pcase-let* ((`(,frame ,col . ,row) (mouse-position))
               (`(,_frame ,pixel-x . ,pixel-y) (mouse-pixel-position))
               (window
                (and frame
                     col
                     row
                     (window-at col row frame))))
    (when (and window (window-live-p window))
      (pcase-let* ((`(,left ,top . ,_) (window-inside-pixel-edges window))
                   (posn (posn-at-x-y
                          (max 0 (- pixel-x left))
                          (max 0 (- pixel-y top))
                          window))
                   (point (and posn (posn-point posn))))
        (when (integerp point)
          (with-current-buffer (window-buffer window)
            (save-excursion
              (goto-char point)
              (when-let ((bounds (neo--eglot-bounds-at-point)))
                (list :window window
                      :buffer (current-buffer)
                      :point point
                      :start (car bounds)
                      :end (cdr bounds))))))))))

(defun neo--eglot-managed-hover-target-p (target)
  "Return non-nil when TARGET is hoverable by Eglot."
  (and target
       (window-live-p (plist-get target :window))
       (buffer-live-p (plist-get target :buffer))
       (with-current-buffer (plist-get target :buffer)
         (and (bound-and-true-p eglot--managed-mode)
              (not (minibufferp (current-buffer)))))))

(defun neo--eglot-hover-popup-live-p ()
  "Return non-nil when the hover popup currently has a live child frame."
  (and (frame-live-p neo--eglot-hover-popup-frame)
       (frame-visible-p neo--eglot-hover-popup-frame)))

(defun neo--eglot-mouse-over-hover-popup-p ()
  "Return non-nil when the mouse is currently over the hover popup."
  (and (neo--eglot-hover-popup-live-p)
       (pcase-let ((`(,frame ,_column . ,_row) (mouse-position)))
         (eq frame neo--eglot-hover-popup-frame))))

(defun neo--eglot-current-mouse-target ()
  "Return the current Eglot-managed hover target under the mouse, if any."
  (let ((raw-target (neo--eglot-mouse-hover-target)))
    (and (neo--eglot-managed-hover-target-p raw-target)
         raw-target)))

(defun neo--eglot-show-hover-popup (target docs)
  "Show popup for TARGET using DOCS."
  (when (neo--eglot-hover-popup-available-p)
    (let ((text (neo--eglot-format-docs-for-popup docs))
          (buffer (plist-get target :buffer))
          (window (plist-get target :window))
          (point (plist-get target :point)))
      (unless (string-empty-p text)
        (save-selected-window
          (with-selected-window window
            (with-current-buffer buffer
              (setq neo--eglot-hover-popup-frame
                    (posframe-show
                     neo/eglot-hover-popup-buffer
                     :string text
                     :position point
                     :background-color (face-background 'tooltip nil t)
                     :foreground-color (face-foreground 'tooltip nil t)
                     :internal-border-width 1
                     :internal-border-color
                     (or (face-foreground 'shadow nil t)
                         (face-foreground 'tooltip nil t)))))))))))

(defun neo--eglot-hover-callback (target docstring &rest plist)
  "Display DOCSTRING for TARGET if the mouse is still hovering it."
  (when (neo--eglot-hover-target-equal-p target neo--eglot-hover-target)
    (if (and docstring (not (string-empty-p docstring)))
        (neo--eglot-show-hover-popup target (list (cons docstring plist)))
      (neo--eglot-hide-hover-popup))))

(defun neo--eglot-request-hover-at-target (target)
  "Request Eglot hover docs for TARGET."
  (let ((buffer (plist-get target :buffer))
        (window (plist-get target :window))
        (point (plist-get target :point)))
    (save-selected-window
      (with-selected-window window
        (with-current-buffer buffer
          (save-excursion
            (goto-char point)
            (eglot-hover-eldoc-function
             (lambda (docstring &rest plist)
               (apply #'neo--eglot-hover-callback
                      target
                      docstring
                      plist)))))))))

(defun neo--eglot-hover-track-mouse ()
  "Track the mouse and request hover docs when it rests on a symbol."
  (let* ((target (neo--eglot-current-mouse-target))
         (mouse-over-popup (neo--eglot-mouse-over-hover-popup-p))
         (now (float-time)))
    (cond
     (mouse-over-popup
      ;; Staying over the popup itself is part of the hover interaction:
      ;; it gives users a chance to click links or buttons without racing
      ;; the hide timer.
      (setq neo--eglot-hover-away-since 0.0))
     (target
      (setq neo--eglot-hover-away-since 0.0)
      (unless (neo--eglot-hover-target-equal-p target neo--eglot-hover-candidate)
        (setq neo--eglot-hover-candidate target)
        (setq neo--eglot-hover-candidate-since now)
        (unless (neo--eglot-hover-target-equal-p target neo--eglot-hover-target)
          (setq neo--eglot-hover-target nil)
          (neo--eglot-hide-hover-popup)))
      (when (and (not (neo--eglot-hover-target-equal-p
                       target
                       neo--eglot-hover-target))
                 (>= (- now neo--eglot-hover-candidate-since)
                     neo/eglot-hover-popup-delay))
        (setq neo--eglot-hover-target target)
        (neo--eglot-request-hover-at-target target)))
     ((or neo--eglot-hover-target
          neo--eglot-hover-candidate
          (neo--eglot-hover-popup-live-p))
      (when (zerop neo--eglot-hover-away-since)
        (setq neo--eglot-hover-away-since now))
      (when (>= (- now neo--eglot-hover-away-since)
                neo/eglot-hover-popup-exit-delay)
        (neo--eglot-reset-hover-popup))))))

(defun neo--eglot-preserve-hover-popup-for-command-p ()
  "Return non-nil when the current command should keep the hover popup alive.

This is what allows clickable controls inside the popup: a mouse click
that begins with the pointer already over the popup should not be
treated as an unrelated command that dismisses the popup first."
  (or (neo--eglot-mouse-over-hover-popup-p)
      (neo--eglot-hover-target-equal-p
       (neo--eglot-current-mouse-target)
       neo--eglot-hover-target)))

(defun neo--eglot-reset-hover-popup-before-command ()
  "Dismiss hover popup before unrelated commands.

When the mouse is still on the originating identifier or on the popup
itself, keep the popup alive so users can interact with it."
  (unless (neo--eglot-preserve-hover-popup-for-command-p)
    (neo--eglot-reset-hover-popup)))

(defun neo--eglot-ensure-hover-timer ()
  "Start the timer backing mouse-driven Eglot hover popups."
  (when (timerp neo--eglot-hover-timer)
    ;; Replacing any pre-existing timer matters here because an earlier
    ;; buggy version stored the poll interval where the callback should
    ;; have been.  Cancelling first lets a reload heal the live session.
    (cancel-timer neo--eglot-hover-timer))
  (setq neo--eglot-hover-timer
        (run-with-timer
         neo/eglot-hover-popup-poll-interval
         neo/eglot-hover-popup-poll-interval
         #'neo--eglot-hover-track-mouse)))

(defun neo/eglot-set-server (modes server-command)
  "Install SERVER-COMMAND for MODES in `eglot-server-programs`.

MODES is a symbol or a list of symbols.
SERVER-COMMAND is a list like (\"pyright-langserver\" \"--stdio\")."
  (let ((mode-list (if (listp modes) modes (list modes))))
    (with-eval-after-load 'eglot
      ;; Remove any existing entries that mention these modes
      (setq eglot-server-programs
            (cl-remove-if
             (lambda (cell)
               (let ((k (car-safe cell)))
                 (cond
                  ((symbolp k) (memq k mode-list))
                  ((and (consp k) (cl-intersection k mode-list))))))
             eglot-server-programs))
      ;; Add a single grouped entry so either mode hits this server
      (push (cons mode-list server-command) eglot-server-programs))))

(defun neo--eglot-format-if-supported ()
  "Format buffer only if the server exists and supports it."
  (let ((server (eglot-current-server)))
    (when (and server 
               (eglot-server-capable :documentFormattingProvider))
      (eglot-format))))

(defun neo--eglot-format-if-supported ()
  "Format the buffer if Eglot is managing it and supports formatting."
  (when (and (fboundp 'eglot-managed-p) 
             (eglot-managed-p))
    (when (eglot-server-capable :documentFormattingProvider)
      (eglot-format-buffer))))

(add-hook 'before-save-hook #'neo--eglot-format-if-supported)

;; TODO some LSP don't support formatting
(defun neo/eglot-format-on-save ()
  "Enable LSP formatting on save for the current buffer when Eglot manages it."
  (when (and (boundp 'eglot--managed-mode)
             eglot--managed-mode
	     (eglot-server-capable :documentFormattingProvider)
	     (fboundp 'eglot-format-buffer))
    (add-hook 'before-save-hook #'eglot-format-buffer nil :local)))

					;(add-hook 'before-save-hook #'eglot-format-buffer)

;; (defun neo/python-eglot-shadow-venv-setup ()
;;   "Set eglot/BasedPyright to use the shadow Bazel virtualenv."
;;   (setq eglot-workspace-configuration
;;               `((:pyright . (:python (:venvPath "~/.python"
;; 						:pythonPath "~/.python/bin/python"))))))

(neo/use-package eglot
  :config
  ;; eglot--stay-out-of-p uses cl-find on this variable, which requires a
  ;; sequence. External config sometimes sets it to `t' (meaning "all"),
  ;; which crashes eglot-client-capabilities before any connection is made.
  (unless (listp eglot-stay-out-of)
    (setq eglot-stay-out-of nil))
  :hook
  ((c++-mode c++-ts-mode) . eglot-ensure)
  (eglot-managed-mode . neo/eglot-format-on-save)
  (eglot-managed-mode . neo--eglot-configure-eldoc))

;; (before-save . (lambda ()
;; 		   (when (eglot-managed-p)
;;                    (eglot-format-buffer)))))


(defun neo/eglot-organize-includes ()
  "Trigger clangd to add missing #includes via Eglot code actions."
  (interactive)
  (eglot-code-actions nil nil "source.includeFix"))

(defun neo--eglot-specific-eldoc ()
  "Prefer richer Eglot hover docs while keeping default ElDoc sources.

The extra hover provider is what turns \"just a type blip\" into a
useful popup with documentation.  Keeping `t' in the list preserves
whatever Eglot or the major mode already registered locally."
  ;; Use custom documentation-functions (with custom priorities, given
  ;; by order):
  (setq-local eldoc-documentation-functions
	      (list
					;      #'eglot-signature-eldoc-talkative
	       #'eglot-hover-eldoc-function
	       t
	       #'flymake-eldoc-function))

  ;; Optionally, in echo-area, only show the most important
  ;; documentation:
  ;; (setq-local eldoc-documentation-strategy
  ;;   #'eldoc-documentation-enthusiast)
  )

(defun neo--eglot-configure-eldoc ()
  "Configure popup-backed ElDoc presentation for the current buffer.

This runs from `eglot-managed-mode'.  Entering Eglot management swaps
in the richer hover provider ordering and enables the mouse-hover
popup timer.  Leaving Eglot restores the buffer to normal ElDoc
behavior."
  (if (bound-and-true-p eglot--managed-mode)
      (progn
        (neo--eglot-specific-eldoc)
        (setq-local eldoc-documentation-strategy
                    #'eldoc-documentation-compose-eagerly)
        ;; The mouse-hover posframe below is our single hover surface.
        ;; Route any `help-echo' (flymake diagnostics, etc.) to the echo
        ;; area instead of a native GUI tooltip so the two popups don't
        ;; overlap on the same identifier.
        (when (fboundp 'tooltip-show-help-non-mode)
          (setq-local show-help-function #'tooltip-show-help-non-mode))
        (neo--eglot-ensure-hover-timer)
        (add-hook 'pre-command-hook #'neo--eglot-reset-hover-popup-before-command nil t))
    (remove-hook 'pre-command-hook #'neo--eglot-reset-hover-popup-before-command t)
    (when (eq (plist-get neo--eglot-hover-target :buffer) (current-buffer))
      (setq neo--eglot-hover-target nil))
    (neo--eglot-hide-hover-popup)
    (kill-local-variable 'show-help-function)
    (kill-local-variable 'eldoc-documentation-functions)
    (kill-local-variable 'eldoc-documentation-strategy)))

;; (neo/use-package eglot-signature-eldoc-talkative
;;   :after eldoc
;;   :hook
;;   (eglot-managed-mode . #'neo--eglot-specific-eldoc)
;;   )

(defun neo--bazel-clangd-path ()
  "Build `//:clangd` via Bazel and return the full path to the resulting binary."
  ;; (let* ((project-root (or (locate-dominating-file default-directory "WORKSPACE.bazel")
  ;;                          (locate-dominating-file default-directory "MODULE.bazel"))))
  ;; TODO compute project-root from project.el
  (let* ((project-root "~/Projects/uno")
	 (bazel-bin (format "%s/tools/bazel" project-root)))
    (unless project-root
      (error "Could not find WORKSPACE.bazel or MODULE.bazel"))
    (let ((default-directory project-root))
      (let* ((build-result (shell-command-to-string (format "%s build //:clangd 2>&1" bazel-bin)))
             (_ (unless (string-match "Build completed successfully" build-result)
                  (error "Failed to build //:clangd: %s" build-result)))
             (bin-dir (string-trim (shell-command-to-string (format "%s info bazel-bin  2>/dev/null" bazel-bin))))
             (clangd-path (expand-file-name "clangd" bin-dir)))
        (unless (file-executable-p clangd-path)
          (error "clangd not found at expected location: %s" clangd-path))
        clangd-path))))

;; (add-hook 'c++-mode-hook
;;           (lambda ()
;;             (setq-local eglot-server-programs
;;                         `((c++-mode . (,(neo--bazel-clangd-path)))))))


;; we could do this here, but it is better if python mode (who's
;; recommending eglot does it, like shown below
;; (with-eval-after-load 'python-mode
;;   (add-hook 'python-mode-hook #'eglot-ensure))
;; This seems better [in python.el]:
;; (with-eval-after-load 'eglot
;;   (add-hook 'python-mode-hook #'eglot-ensure))
;; this has to go in the :config piece of use-package


;; (neo/use-package eglot
;;   :after eldoc
;;   :config
;;   ;;; TODO: this is for golang, not sure if it plays well with other languages
;;   (defun neo/eglot-organize-imports() (call-interactively 'eglot-code-action-organize-imports))
;; ;  (add-hook 'before-save-hook 'neo/eglot-organize-imports nil t)
;; ;  (add-to-list 'eglot-stay-out-of 'flymake)
;;   (setq eglot-server-programs (neo/eglot-combined-server-configuration))
;;   (setq-default eglot-workspace-configuration (neo/eglot-combined-workspace-configuration))
;;   (with-eval-after-load 'eglot
;;   (add-hook 'before-save-hook
;;             (lambda ()
;;               (when (eglot-managed-p)
;;                 (eglot-format-buffer)))))
;;   :custom
;;   (eglot-autoshutdown t)
;; ;  (eglot-extend-to-xref t)
;; ;  (eglot-ignored-server-capabilities '(:hoverProvider))
;;   :bind
;;   (("C-c l c" . eglot-reconnect)
;;    ("C-c l d" . flymake-show-buffer-diagnostics)
;;    ("C-c l f f" . eglot-format)
;;    ("C-c l f b" . eglot-format-buffer)
;;    ("C-c l l" . eglot)
;;    ("C-c l r n" . eglot-rename)
;;    ("C-c l s" . eglot-shutdowmen))
;;   :hook
;;   (python-mode . eglot-ensure)d
;;   ((rust-mode rust-ts-mode) . eglot-ensure)
;; ;  (eglot-managed-mode . manually-activate-flymake)
;;   (terraform-mode . eglot-ensure)
;;   ((c++-mode c++-ts-mode) . eglot-ensure)
;;   ((go-mode go-ts-mode) . (lambda ()
;;                             (neo/setup-before-save-go)
;;                             (eglot-ensure))))

;; (use-package highlight-symbol
;;   :custom
;;   (highlight-symbol-on-navigation-p t)
;;   :hook
;;   (prog-mode . #'highlight-symbol-mode))

;; (neo/use-package flymake
;;   :ensure nil				; builtin
;;   )

(defun neo/flymake-toggle-diagnostics-buffer ()
  (interactive)
  ;; Check if we are in the diagnostics buffer.
  (if (string-search "*Flymake diagnostics" (buffer-name))
      (delete-window)
    (progn
      ;; Activate the Flymake diagnostics buffer.
      ;; and switch to it
      (flymake-show-buffer-diagnostics)
      (let ((name (flymake--diagnostics-buffer-name)))
        (if (get-buffer name)
            (switch-to-buffer-other-window name)
          (error "No Flymake diagnostics buffer found")
          )))))
(global-set-key [(f7)] #'neo/flymake-toggle-diagnostics-buffer)

;; TODO not entirely sure it belongs here
(neo/use-package imenu-list
  :custom
  (imenu-list-focus-after-activation t)
  :bind
  ("C-." . #'imenu-list-minor-mode)
  )

(with-eval-after-load 'key-chord
  (with-eval-after-load 'eglot
    (with-eval-after-load 'c++-ts-mode
      (message "DEFINING CHORD")
      (message "KEYMAP BOUND")
      (key-chord-define c++-ts-mode-map "''" #'eglot-code-action-quickfix))))

;; (defun neo--locate-coveralls (file-dir file-name)
;;   (message "**** file-dir: %s, file-name: %s" file-dir file-name)
;;   (let ((dir (locate-dominating-file file-dir "coverage-final.json")))
;;     (when dir
;;       (cons (file-truename (f-join dir "coverage-final.json")) 'coveralls))))

(defun neo/cov-mode-safe-turn-on ()
  "Turn on cov-mode only for real source files."
  (interactive)
  (when (and buffer-file-name
             (file-exists-p buffer-file-name)
             (not (string-match-p "coverage-final\\.json$" buffer-file-name)))
    (cov-mode 1)))

(neo/use-package cov
  :init
  ;; Must be set before cov-mode is enabled
  (setq cov-coverage-mode t)

  ;;  (setq cov-coverage-file-paths
  ;;        (list neo--locate-coveralls))
  )
;;:hook
;;(prog-mode . cov-mode))

(defun neo--programming-foundation-load-beads ()
  "Load the Beads module once extension packages are available."
  (remove-hook 'neo/after-framework-bootstrap-hook
               #'neo--programming-foundation-load-beads)
  (unless (featurep 'beads)
    (require 'beads)))

(if neo/framework-bootstrapped-p
    (neo--programming-foundation-load-beads)
  (add-hook 'neo/after-framework-bootstrap-hook
            #'neo--programming-foundation-load-beads))

;;; Note, no (provide 'neo-programing-foundation) here, extensions are loaded not required.
