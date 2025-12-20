;;; -*- lexical-binding: t -*-

(require 'neo-ui-frame)
(require 'neo-ui-fonts)
(require 'neo-ui-themes)
; we do this in project now, too much coupling with projectile
;(require 'neo-ui-treemacs)
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
  :config
  (global-hl-line-mode 1))

(load-theme "ef-dark" t)

;; TODO: not sure what these do
;; (setq spacious-padding-subtle-frame-lines
;;       '( :mode-line-active spacious-padding-line-active
;;          :mode-line-inactive spacious-padding-line-inactive
;;          :header-line-active spacious-padding-line-active
;;          :header-line-inactive spacious-padding-line-inactive))

