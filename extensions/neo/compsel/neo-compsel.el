;;; -*- lexical-binding: t -*-

;;; This is compsel, a NEO extension
;;;
;;; Completions and Selections

(neo/use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  ;; vertico is autoloaded, so invoking vertico-mode in :init makes
  ;; sure it is loaded before other packages potentially require
  ;; minibuf interaction
  ;; TODO: this is from the vertico documentation. Pretty sure it
  ;; doesn't work with our deferring use-package evaluation.
  (vertico-mode))

(neo/use-package posframe)

;; I had
;;  (set-face-background 'vertico-posframe "purple4") ; works with some dark themes ef-winter
;;  (set-face-background 'vertico-posframe "gainsboro") ; works with some light themes ef-spring
;; (defun neo/make-posframe-contrasting-face ()
;;   (let ((bg-fg (neo/compute-contrasting-colors)))
;;     (setq vertico-posframe-parameters
;; 	  '((left-fringe . 8)
;;             (right-fringe . 8)
;;             (internal-border-width . 2)
;;             (background-color . (car bg-fg))
;;             (foreground-color . (cdr bg-fg))
;;             ;; (background-color . "#1c1f26")
;;             ;; (foreground-color . "#ffffff")
;;             (undecorated . t)
;;             (no-accept-focus . t)
;;             (no-focus-on-map . t)
;;             (cursor-type . nil)
;;             (min-width . 80)
;;             (min-height . 10)))))

;; (defun neo/make-posframe-contrasting-face ()
;;   (let ((bg-fg (neo/compute-contrasting-colors)))
;;     (set-face-background 'vertico-posframe (car bg-fg))
;;     (set-face-foreground 'vertico-posframe (cdr bg-fg))))
(defun neo/make-posframe-contrasting-face () t)

(neo/use-package vertico-posframe
  :doc "Display vertico selections in a popup window"
  ;  :disabled ; not sure if I want it or not
  :after vertico
  :hook
  (neo/after-theme-load . #'neo/make-posframe-contrasting-face)
  :custom
  (vertico-posframe-parameters
   '((internal-border-width . 1)
     (left-fringe . 8)
     (right-fringe . 8)))
  :config
  (neo/make-posframe-contrasting-face)
;  (set-face-background 'vertico-posframe "purple4") ; works with some dark themes ef-winter
;  (set-face-background 'vertico-posframe "gainsboro") ; works with some light themes ef-spring
  (vertico-posframe-mode 1))

(neo/use-package vertico-directory
  :doc "Make navigating directories in vertico completions nicer by deleting entire components"
  :after vertico
  :ensure nil
  :bind
  (:map
   vertico-map
   ("RET" . vertico-directory-enter)
   ("DEL" . vertico-directory-delete-char)
   ("M-DEL" . vertico-directory-delete-word))
  ;; TODO: got this somewhere in the intertubes, but we don't use rfn-eshadow in Neo (yet?)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; TODO: not sure I want posframe for vertico. If so, compute
;; background and foreground
;; (neo/use-package posframe)
;; (neo/use-package vertico-posframe
;;   :doc "Display vertico selections in a popup window"
;;   ;  :disabled ; not sure if I want it or not
;;   :after vertico
;;   :custom
;;   (vertico-posframe-parameters
;;       '((left-fringe . 8)
;;         (right-fringe . 8)))
;;   :config
;;   (set-face-background 'vertico-posframe "purple4") ; works with some dark themes ef-winter
;; ;  (set-face-background 'vertico-posframe "gainsboro") ; works with some light themes ef-spring
;;   (vertico-posframe-mode 1))

(neo/use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'left)
  (marginalia-align-offset 16)
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(neo/use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(neo/use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))


(neo/use-package corfu
  :doc "Show completions near the point"
  ;; :ensure
  ;; (corfu
  ;;  :host github
  ;;  :repo "minad/corfu"
  ;;  :files (:defaults "extensions/*"))
  :hook (lsp-completion-mode . kb/corfu-setup-lsp) ; Use corfu for lsp completion
  :custom
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil) ; Always show candidates in menu

  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)

  (corfu-min-width 40)
  (corfu-max-width 120)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)

  ;; `nil' means to ignore `corfu-separator' behavior, that is, use the older
  ;; `corfu-quit-at-boundary' = nil behavior. Set this to separator if using
  ;; `corfu-auto' = `t' workflow (in that case, make sure you also set up
  ;; `corfu-separator' and a keybind for `corfu-insert-separator', which my
  ;; configuration already has pre-prepared). Necessary for manual corfu usage with
  ;; orderless, otherwise first component is ignored, unless `corfu-separator'
  ;; is inserted.
  (corfu-quit-at-boundary nil)
  (corfu-separator ?\s) ; Use space
  (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
  (corfu-preview-current 'insert) ; Preview first candidate. Insert on input if only one
  (corfu-preselect-first t) ; Preselect first candidate?

  ;; Other
  (lsp-completion-provider :none) ; Use corfu instead for lsp completions

  :config (global-corfu-mode)
  ;; (corfu-popupinfo-mode) TODO loading of extensions in :elpaca doesn't seem to work
  ;; Enable Corfu more generally for every minibuffer, as long as no other
  ;; completion UI is active. If you use Mct or Vertico as your main minibuffer
  ;; completion UI. From
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or
             (bound-and-true-p mct--active) ; Useful if I ever use MCT
             (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil) ; Ensure auto completion is disabled
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer
            1)

  ;; Setup lsp to use corfu for lsp completion
  (defun kb/corfu-setup-lsp ()
    "Use orderless completion style with lsp-capf instead of the
default lsp-passthrough."
    (setf (alist-get
           'styles
           (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))))

(use-package corfu-candidate-overlay
    :ensure (:type git
               :repo "https://code.bsdgeek.org/adam/corfu-candidate-overlay"
               :files (:defaults "*.el"))
    :after corfu
    :config
    ;; enable corfu-candidate-overlay mode globally
    ;; this relies on having corfu-auto set to nil
    (corfu-candidate-overlay-mode +1)
    ;; bind Ctrl + TAB to trigger the completion popup of corfu
    (global-set-key (kbd "C-<tab>") 'completion-at-point)
    ;; bind Ctrl + Shift + Tab to trigger completion of the first candidate
    ;; (keybing <iso-lefttab> may not work for your keyboard model)
    (global-set-key (kbd "C-<iso-lefttab>") 'corfu-candidate-overlay-complete-at-point))

(neo/use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind
  (("C-c p p" . completion-at-point) ;; capf
   ("C-c p t" . complete-tag) ;; etags
   ("C-c p d" . cape-dabbrev) ;; or dabbrev-completion
   ("C-c p h" . cape-history)
   ("C-c p f" . cape-file)
   ("C-c p k" . cape-keyword)
   ("C-c p s" . cape-symbol)
   ("C-c p a" . cape-abbrev)
   ("C-c p l" . cape-line)
   ("C-c p w" . cape-dict)
   ("C-c p \\" . cape-tex)
   ("C-c p _" . cape-tex)
   ("C-c p ^" . cape-tex)
   ("C-c p &" . cape-sgml)
   ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

;;; Note, no (provide 'neo-compsel) here, extensions are loaded not required.
