;;; -*- lexical-binding: t -*-

;;; This is programing-foundation, a NEO extension
;;;
;;; Language agnostic software development support

(neo/use-package jsonrpc)

(require 'neo-programming-foundation-treesit)

(neo/use-package flymake)

(declare-function eglot-managed-p "eglot")
(declare-function eglot-format-buffer "eglot")

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

(defun my/eglot-format-if-supported ()
  "Format buffer only if the server supports it."
  (when (eglot-server-capable :documentFormattingProvider)
    (eglot-format)))

(add-hook 'before-save-hook #'my/eglot-format-if-supported)

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
					;  :config
					;  (neo/python-eglot-shadow-venv-setup)
  :hook
  ((c++-mode c++-ts-mode) . eglot-ensure)
  (eglot-managed-mode . neo/eglot-format-on-save))

;; (before-save . (lambda ()
;; 		   (when (eglot-managed-p)
;;                    (eglot-format-buffer)))))


(defun neo/eglot-organize-includes ()
  "Trigger clangd to add missing #includes via Eglot code actions."
  (interactive)
  (eglot-code-actions nil nil "source.includeFix"))

(defun neo--eglot-specific-eldoc ()
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
(use-package imenu-list
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


;;; Note, no (provide 'neo-programing-foundation) here, extensions are loaded not required.
