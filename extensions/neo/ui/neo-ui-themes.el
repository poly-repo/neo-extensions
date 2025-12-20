;;; -*- lexical-binding: t -*-

(use-package modus-themes)
(use-package doom-themes)
(use-package ef-themes
  :config
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)
  (modus-themes-load-theme 'ef-dark))

(provide 'neo-ui-themes)
