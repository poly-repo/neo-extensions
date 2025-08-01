(defun neo/desktop-restore ()
  (desktop-read)
  (desktop-save-mode +1)
  (savehist-mode 1))

(neo/use-package desktop
  :ensure nil				; built-in
  :init
  (progn
    (defvar neo/desktop-path (expand-file-name "desktop/" no-littering-var-directory))
    (unless (file-directory-p neo/desktop-path)
      (make-directory neo/desktop-path t)))
  :config
  ;; TODO: neo/use-package could support dept in :hook
  (add-hook 'after-init-hook 'neo/desktop-restore 99) ; :hook doesn't support depth
 
  :custom
  (desktop-dirname neo/desktop-path)
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)

  (desktop-save t)
  (desktop-path `(,neo/desktop-path))
  (desktop-restore-eager 5)
  (desktop-load-locked-desktop 'check-pid)
  (desktop-save-buffer-predicate 'neo/desktop-exclude-gpg)
  )
