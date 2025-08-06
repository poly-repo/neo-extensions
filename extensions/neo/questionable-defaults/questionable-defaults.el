;;; Defaults for Neo

(defgroup neo-questionable-defaults nil
  "Customization options for the questionable defaults extension."
  :group 'neo-extensions)

(defcustom neo/nuke-echo-area-message t
  "If non-nil, disables the self-promoting message Emacs put in the echo area.")

(neo/use-package emacs
  :doc "Setup questionable defaults"
  :config
  (fset 'yes-or-no-p 'y-or-n-p)
  :custom
  ;; I'd be ok w/ a warning, but cannot stand this in the echo area
  (ad-redefinition-action 'accept)
  (inhibit-splash-screen t)
  (custom-file (expand-file-name (format "%s-custom.el" (neo/get-emacs-instance-name)) user-emacs-directory))
  ;; The following is mainly for avoiding the
  ;; 'Symbolic link to Git-controlled source file; follow link? (y or n)'
  ;; question every time I get to a package source via find-library or
  ;; find-function. The question could be avoided with
  ;; (setq vc-follow-symlinks t)
  ;; but since we don't use VC at all, I take the nuclear option.
  ( vc-handled-backends nil)
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(neo/use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define-global "``" 'toggle-menu-bar-mode-from-frame)
  (key-chord-define-global ".." 'comment-or-uncomment-region)
  (key-chord-define-global ",," 'sort-lines)) ; not to useful these
					; days of autoformat

;; ;;; TODO this will go in the 'Questionable Defaults' extension
;; ;;; also, for now we run it only on the develop emacs (the final copy
;; ;;; would be otherwise wrong). 
;; (if (string= (neo/get-emacs-instance-name) "emacs")
;;     (progn
;;       (setq neo/nuke-echo-area-message t)
;;       (neo/hacking-write-early-init-config!
;;        inhibit-splash-screen
;;        frame-inhibit-implied-resize
;;        frame-resize-pixelwise
;;        neo/nuke-echo-area-message

;;        ;; the following will be called as functions, with a default getter
;;        ;; as they have a variable with the same name giving the current
;;        ;; setting.
;;        ;; When this is not possible, a form can be passed as
;;        ;; (FUNC . VALUE-GETTER) 
;;        (menu-bar-mode)
;;        (scroll-bar-mode)
;;        (tool-bar-mode)
;;        (tooltip-mode)
;;        )
;;       (let* ((instance-name (neo/get-emacs-instance-name))
;;              (file-name (format "%s-early-init-config.el" instance-name))
;;              (target-dir (expand-file-name "neo" (or (getenv "XDG_CONFIG_HOME") "~/.config")))
;;              (target-file (expand-file-name file-name target-dir)))
;; 	(copy-file target-file (expand-file-name "emacs-neo-devel-early-init-config.el" target-dir) t))))



;; probably need to ensure it is run early
(neo/use-package no-littering
  :init
  ;; We define these in early-init.el so everything can be kept out of the way
  ;; in particular elpaca and eln-cache
  ;(setq no-littering-etc-directory (expand-file-name ".litter/config" user-emacs-directory))
  ;(setq no-littering-var-directory (expand-file-name ".litter/data" user-emacs-directory))
  (setq custom-file
        (expand-file-name "custom.el" no-littering-var-directory))
  :config
  (no-littering-theme-backups)
  (setq auto-save-file-name-transforms
        `((".*"
           ,(no-littering-expand-var-file-name "auto-save/")
           t))))
