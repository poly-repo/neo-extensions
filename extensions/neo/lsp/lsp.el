;; TODO this needs to be renamed, it is not LSP anymore, it is program
;; info or something

;; TODO these should be injectable from language modes
(defcustom neo/treesit-grammar-versions
  '((python . "v0.20.4")
    (go     . "v0.23.4")
    (c      . "v0.23.3")		; newest one w/ ABI #14
    (cpp    . "v0.23.4")
    (json   . nil)
;    (yaml   . nil) ; yaml is different repo
;    (typescript . nil)			; typescript is not in src,
					; cannot us it for now
    )
  "Alist of Tree-sitter grammars and pinned versions (tags or SHAs).
Each element is of the form (LANG . REVISION), where LANG is a symbol
like `python`, and REVISION is a string Git tag, commit, or branch."
  :type '(alist :key-type symbol :value-type (choice string (const nil)))
  :group 'neo				; TODO need a better group here
  :safe t)

;; TODO for now we have no grammars not coming from the tree-sitter
;; repo, but we should allow for grammar-spec to specify one. Maybe
;; that should become a plist
(defun neo/treesit-compute-language-source-alist (grammar-spec)
  "Return a new `treesit-language-source-alist` built from VERSION-SPEC.

VERSION-SPEC is an alist of (LANG . REVISION), where LANG is a symbol
and REVISION is a string (Git tag or SHA) or nil (to use the default branch)."
  (mapcar
   (lambda (entry)
     (let* ((lang (car entry))
            (rev  (cdr entry))
            (url  (format "https://github.com/tree-sitter/tree-sitter-%s" lang)))
       (cons lang (if rev (list url rev) (list url)))))
   grammar-spec))

;; treesit is rather stubborn in where to install compiled grammars:
;;   (or (car treesit--install-language-grammar-out-dir-history)
;;       (locate-user-emacs-file "tree-sitter")))
;; either a private variable or in your directory. So we add our own
;; utility function
;; we should probably version the installation directory and add it to
;;   treesit extrapath, otherwise we wont be able to update or go to
;;   any specific version of the grammar.
;; or maybe we leave the treesit extra path as is and we record
;;   elsewhere which version it is and change the
;;   treesit-language-available-p test here, which is probably simpler.
(defun neo/treesit-install-grammar (lang)
  (unless (treesit-language-available-p lang)
    (treesit-install-language-grammar lang (neo/native-cache "treesit"))))

;; TODO grab versions from sources and allow user to configure one of
;; the last N, or latest, or a custom one
(defun neo/treesit-install-grammars (grammars)
  (dolist (grammar grammars)
    (neo/treesit-install-grammar (car grammar))))

(neo/use-package treesit
  :ensure nil
  :custom
  (treesit-extra-load-path (list (neo/native-cache "treesit")))
  (treesit-font-lock-level 4)
  (treesit-language-source-alist (neo/treesit-compute-language-source-alist neo/treesit-grammar-versions))
  :config
  (progn
    (neo/treesit-install-grammars neo/treesit-grammar-versions)))

;; (defvar neo/treesit-grammar-dir
;;   (expand-file-name "tree-sitter/" no-littering-var-directory))
;; (make-directory neo/treesit-grammar-dir t)

;; (setq treesit-extra-load-path (list neo/treesit-grammar-dir))

;; ;;; treesit for now hardcode outdir to nil which results in grammars to be placed in user-emacs-directory/tree-sitter
;; ;;; we want grammar in .litter directories, hence this piece of advice.
;; (defun neo/treesit-add-outdir (orig-fun program &rest args)
;;   (apply orig-fun neo/treesit-grammar-dir args))
;; (advice-add
;;  'treesit--install-language-grammar-1
;;  :around #'neo/treesit-add-outdir)

(neo/use-package treesit-auto
  :after treesit
  :init
  (setq treesit-auto-install t
;        treesit-auto-lang '(bash c cpp css dockerfile go gomod html javascript json latex markdown org proto python tsx typescript yaml))
        treesit-auto-lang '(c cpp python))
  (setq treesit-language-source-alist
      (mapcar (lambda (lang)
                (let ((repo-url (format "https://github.com/tree-sitter/tree-sitter-%s" lang)))
                  (cons lang (list repo-url))))
              treesit-auto-lang))
  :config
  (dolist (lang treesit-auto-lang)
    (treesit-install-language-grammar lang))
  (global-treesit-auto-mode))

;; (neo/use-package treesit
;;   :ensure nil
;;   :hook
;;   (go-mode . #'treesit-parser-create)
;;   :custom
;;   (treesit-font-lock-level 4)
;;   :config
;;   (setq treesit-extra-load-path
;;         (list (no-littering-expand-var-file-name "tree-sitter"))))

(neo/use-package flymake)

(declare-function eglot-managed-p "eglot")
(declare-function eglot-format-buffer "eglot")


(defun neo/eglot-format-on-save ()
  "Enable LSP formatting on save for the current buffer when Eglot manages it."
  (when (and (boundp 'eglot-managed-mode)
             eglot-managed-mode
             (fboundp 'eglot-format-buffer))
    (add-hook 'before-save-hook #'eglot-format-buffer nil :local)))

(neo/use-package eglot
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
  (let ((project-root "~/Projects/uno"))
    (unless project-root
      (error "Could not find WORKSPACE.bazel or MODULE.bazel"))
    (let ((default-directory project-root))
      (let* ((build-result (shell-command-to-string "bazel build //:clangd 2>&1"))
             (_ (unless (string-match "Build completed successfully" build-result)
                  (error "Failed to build //:clangd: %s" build-result)))
             (bin-dir (string-trim (shell-command-to-string "bazel info bazel-bin  2>/dev/null")))
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
