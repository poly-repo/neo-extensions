;;; -*- lexical-binding: t -*-

(neo/use-package modus-themes)
(neo/use-package doom-themes)
(neo/use-package ef-themes
  :config
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)
  (modus-themes-load-theme 'ef-dark))

(provide 'neo-ui-themes)
