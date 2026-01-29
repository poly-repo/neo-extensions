;;; -*- lexical-binding: t -*-

;; (use-package doom-modeline
;;  :init (doom-modeline-mode 1))

;; (after! doom-modeline
;;   (remove-hook 'doom-modeline-mode-hook #'size-indication-mode) ; filesize in modeline
;;   (remove-hook 'doom-modeline-mode-hook #'column-number-mode)   ; cursor column in modeline
;;   (line-number-mode -1)
;;   (setq doom-modeline-buffer-encoding nil))


(neo/use-package major-mode-icons
  :after all-the-icons
  )

(neo/use-package minions)
;  :ensure (:wait t))

(neo/use-package simple-modeline
  :custom
  (simple-modeline-segments '((
				    simple-modeline-segment-modified
				    (lambda () (concat "  " (major-mode-icons--major-mode-icon)))
					;				    simple-modeline-segment-major-mode
				    simple-modeline-segment-buffer-name
				    simple-modeline-segment-position)
				   (
					; simple-modeline-segment-minor-modes
					;				    simple-modeline-segment-input-method
					;				    simple-modeline-segment-eol
					;				    simple-modeline-segment-encoding
					;				    simple-modeline-segment-vc
				    simple-modeline-segment-misc-info
				    minions--mode-line-minor-modes
					;				    simple-modeline-segment-process
					;				    simple-modeline-segment-major-mode
				    )))
  :hook (after-init . simple-modeline-mode))

(provide 'neo-ui-modeline)
