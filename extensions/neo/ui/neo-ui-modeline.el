;;; -*- lexical-binding: t -*-

(neo/use-package all-the-icons
  :config
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

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
