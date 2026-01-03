;;; -*- lexical-binding: t -*-

;;; This is session, a NEO extension
;;;
;;; Session management for Emacs

;; (defun neo/desktop-restore ()
;;   (desktop-read)
;;   (desktop-save-mode +1)
;;   (savehist-mode 1))

;; (neo/use-package desktop
;;   :builtin
;;   :init
;;   (progn
;;     (defvar neo/desktop-path (expand-file-name "desktop/" no-littering-var-directory))
;;     (unless (file-directory-p neo/desktop-path)
;;       (make-directory neo/desktop-path t)))
;;   :config
;;   ;; TODO: neo/use-package could support depth in :hook
;;   (add-hook 'after-init-hook 'neo/desktop-restore 99) ; :hook doesn't support depth

;;   :custom
;;   (desktop-dirname neo/desktop-path)
;;   (backup-by-copying t)
;;   (delete-old-versions t)
;;   (kept-new-versions 6)
;;   (kept-old-versions 2)
;;   (version-control t)

;;   (desktop-save t)
;;   (desktop-path `(,neo/desktop-path))
;;   (desktop-restore-eager 5)
;;   (desktop-load-locked-desktop 'check-pid)
;;   ;; we don't want to save encrypted files as they will force us to
;;   ;; give a passphrase every time Emacs starts.
;;   (desktop-files-not-to-save
;;    (concat
;;     ;; Exclude TRAMP/Ange-FTP buffers (e.g. /ssh:user@host:/path or (ftp))
;;     "\\(\\`/[^/:]+:[^/]*\\|"          ; TRAMP and Ange-FTP style paths
;;     "(ftp)\\'\\|"                     ; FTP-based buffers
;;     ;; Exclude encrypted files
;;     "\\.gpg\\'\\)"                    ; GPG encrypted files
;;     ))
;;   )

;; This is recommended in the vertico documentation
;; TODO: surface this fact in the configuration wizards
(neo/use-package savehist
  :builtin
  :init
  (savehist-mode))

;; TODO don't really remember what this was about
(defun neo/sync-neo ()
  (message "We try to sync Emacs before restarting"))

(defun neo/restart-emacs-or-exit (arg)
  (interactive "p")
  (if (>= arg 16)
      (neo/sync-neo))
  (if (>= arg 4)
      (restart-emacs)
    (save-buffers-kill-emacs)))

(global-set-key (kbd "C-x C-c") 'neo/restart-emacs-or-exit)

;;; Note, no (provide 'neo-session) here, extensions are loaded not required.
