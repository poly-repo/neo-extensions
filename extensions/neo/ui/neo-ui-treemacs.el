(neo/use-package treemacs
  :init
  (setq treemacs-follow-after-init t
	treemacs-width 0.20
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc)
;        treemacs-persist-file (concat doom-cache-dir "treemacs-persist")
;        treemacs-last-error-persist-file (concat doom-cache-dir "treemacs-last-error-persist"))
  :config
  (treemacs-follow-mode -1))

(defun neo/treemacs-resize-to-fraction ()
  "Keep Treemacs width equal to `treemacs-width` fraction of the frame."
  (when (and (treemacs-current-visibility)
             (floatp treemacs-width))
    (let* ((fw (frame-width))
           (tw (max 20 (floor (* fw treemacs-width)))))
      (treemacs-set-width tw))))

(add-hook 'window-size-change-functions
          (lambda (&rest _) (neo/treemacs-resize-to-fraction)))

;; (neo/use-package treemacs-nerd-icons
;;   :after treemacs
;;   :config (treemacs-load-theme "nerd-icons"))

(neo/use-package treemacs-all-the-icons)

(neo/use-package treemacs-magit
  :after treemacs magit)

(neo/use-package treemacs-persp
  :after treemacs
  :config (treemacs-set-scope-type 'Perspectives))

(neo/use-package project-treemacs
  :after treemacs
  :config
  (project-treemacs-mode))

(provide 'neo-ui-treemacs)
