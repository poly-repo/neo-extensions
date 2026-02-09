;;; -*- lexical-binding: t -*-

;; (use-package doom-modeline
;;  :init (doom-modeline-mode 1))

;; (after! doom-modeline
;;   (remove-hook 'doom-modeline-mode-hook #'size-indication-mode) ; filesize in modeline
;;   (remove-hook 'doom-modeline-mode-hook #'column-number-mode)   ; cursor column in modeline
;;   (line-number-mode -1)
;;   (setq doom-modeline-buffer-encoding nil))


(neo/use-package all-the-icons)

;(neo/use-package major-mode-icons
;  :after all-the-icons
;  )

(neo/use-package minions
  :config (minions-mode))

(neo/use-package simple-modeline
  :custom
  (simple-modeline-segments '((
			       simple-modeline-segment-modified
			       (lambda ()
                                 (let ((icon (condition-case nil
                                                 (all-the-icons-icon-for-mode major-mode)
                                               (error nil))))
                                   (if (stringp icon)
                                       (concat "  " icon)
                                     "")))
			       simple-modeline-segment-buffer-name
			       simple-modeline-segment-position)
			      (
			       simple-modeline-segment-misc-info
			       minions--mode-line-minor-modes)))
  :hook (emacs-startup . simple-modeline-mode))

(provide 'neo-ui-modeline)
