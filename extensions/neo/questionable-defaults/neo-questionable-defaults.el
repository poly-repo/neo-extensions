;;; Defaults for Neo

(defgroup neo-questionable-defaults nil
  "Customization options for the questionable defaults extension."
  :group 'neo-extensions)

(defcustom neo/y-or-n-ret-default 'no
  "Global default for what pressing RET should mean in `y-or-n-p'.
Possible values: 'yes or 'no."
  :type '(choice (const :tag "Yes" yes)
                 (const :tag "No" no))
  :group 'convenience)

;; per-buffer override (nil means "use global")
(defvar-local neo/y-or-n-ret-default-local nil
  "Buffer-local override for `neo/y-or-n-ret-default'.
Set to 'yes or 'no to override; set to nil to fall back to the global value.")

;; per-command override: attach with (put 'some-command 'neo/y-or-n-ret-default 'yes)
;; We'll consult (get this-command 'neo/y-or-n-ret-default).
;;
;; Resolve order:
;;   1. buffer-local override (neo/y-or-n-ret-default-local, if non-nil)
;;   2. command-local override (get this-command 'neo/y-or-n-ret-default)
;;   3. global neo/y-or-n-ret-default
;;
(defun neo/y-or-n--effective-ret-default ()
  "Return effective RET default symbol 'yes or 'no for the current context."
  (or
   ;; 1) buffer-local override if non-nil
   (and (boundp 'neo/y-or-n-ret-default-local)
        neo/y-or-n-ret-default-local)
   ;; 2) command-local override (attached to command symbol)
   (and (boundp 'this-command)
        (get this-command 'neo/y-or-n-ret-default))
   ;; 3) global fallback
   neo/y-or-n-ret-default))


;; Helpers for convenience:
(defun neo/set-y-or-n-ret-default-for-command (command value)
  "Set per-command RET default for COMMAND (symbol) to VALUE ('yes or 'no).
Example: (neo/set-y-or-n-ret-default-for-command 'magit-commit 'no)"
  (put (intern (if (symbolp command) (symbol-name command) (format "%s" command)))
       'neo/y-or-n-ret-default
       value))

(defun neo/set-y-or-n-ret-default-for-buffer (value)
  "Set buffer-local RET default to VALUE ('yes or 'no) for current buffer."
  (setq-local neo/y-or-n-ret-default-local value))

(neo/use-package emacs
  :doc "Setup questionable defaults"
  :config
  (fset 'yes-or-no-p 'y-or-n-p)
  (advice-add 'y-or-n-p :around
              (lambda (orig-func prompt &rest args)
		(let* ((prompt (or prompt ""))
                       (default (neo/y-or-n--effective-ret-default))
                       ;; insert an explicit suffix to show RET and case
                       (suffix (format " (y or n, RET=%s) " (if (eq default 'yes) "yes" "no")))
                       (full-prompt (concat prompt suffix)))
                  (catch 'answer
                    (while t
                      (let ((k (read-key full-prompt)))
			(cond
			 ((or (eq k ?y) (eq k ?Y)) (throw 'answer t))
			 ((or (eq k ?n) (eq k ?N)) (throw 'answer nil))
			 ((or (eq k ?\r) (eq k ?\n))
                          (if (eq default 'yes)
                              (throw 'answer t)
                            (throw 'answer nil)))
			 ((eq k ?\C-g) (signal 'quit nil))
			 (t (message "Please answer y or n (RET = %s).  Press y or n." (if (eq default 'yes) "yes" "no"))))))))))

  ;; Set Coding System
  (set-charset-priority 'unicode)
  (prefer-coding-system 'utf-8)
  (setopt locale-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)

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

  (image-animate-loop t)
  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; Better Scrolling
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-interpolate-page t)
  (scroll-conservatively 101) ;; must be greater than or equal to 101
  (scroll-step 1)

  ;; Backup
  (setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))
  
  :hook
  ((prog-mode text-mode conf-mode help-mode)
   . visual-wrap-prefix-mode)
  )

(neo/use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define-global "``" 'toggle-menu-bar-mode-from-frame)
  (key-chord-define-global ".." 'comment-or-uncomment-region)
  (key-chord-define-global ",," 'sort-lines)) ; not to useful these
					; days of autoformat

(neo/use-package which-key
  :custom
  (which-key-add-column-padding 2)
  (which-key-allow-multiple-replacements t)
  (which-key-idle-delay 0.8)
  (which-key-min-display-lines 6)
  (which-key-mode t)
  (which-key-side-window-slot -10))
