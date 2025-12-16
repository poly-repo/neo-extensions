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

