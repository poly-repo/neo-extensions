;;; -*- lexical-binding: t -*-

(require 'neo-ui-frame)
(require 'neo-ui-fonts)
(require 'neo-ui-themes)
(require 'neo-ui-modeline)
(require 'neo-ui-side-windows)

(neo/use-package winum
  :config
  (winum-mode 1))

(neo/use-package spacious-padding
  :ensure t
  :config
  ;; These are the default values, but I keep them here for visibility.
  ;; Also check `spacious-padding-subtle-frame-lines'.
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :custom-button-width 3
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8
           :fringe-width 8))
  (spacious-padding-mode 1))

(neo/use-package emacs
  ;; NOTE: (global-hl-line-mode 1) is too aggressive and make it very
  ;; hard to disable line hoghlighting in specific buffers
  :config
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'text-mode-hook #'hl-line-mode))

(neo/use-package rainbow-mode
  :custom
  (rainbow-ansi-colors nil)
  (rainbow-x-colors nil))

(neo/use-package lin
  :custom
  (lin-face 'lin-blue)
  (lin-mode-hooks
   '(dired-mode-hook
     elfeed-search-mode-hook
     git-rebase-mode-hook
     grep-mode-hook
     ibuffer-mode-hook
     ilist-mode-hook
     ledger-report-mode-hook
     log-view-mode-hook
     magit-log-mode-hook
     mu4e-headers-mode
     occur-mode-hook
     org-agenda-mode-hook
     pdf-outline-buffer-mode-hook
     proced-mode-hook
     tabulated-list-mode-hook))

  :hook
  (init-post-init-hook . lin-global-mode))
(neo/use-package ct)
