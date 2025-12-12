(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; (after! doom-modeline
;;   (remove-hook 'doom-modeline-mode-hook #'size-indication-mode) ; filesize in modeline
;;   (remove-hook 'doom-modeline-mode-hook #'column-number-mode)   ; cursor column in modeline
;;   (line-number-mode -1)
;;   (setq doom-modeline-buffer-encoding nil))

(provide 'neo-ui-modeline)
