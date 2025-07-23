;;; Defaults for Neo

(defgroup neo-questionable-defaults nil
  "Customization options for the questionable defaults extension."
  :group 'neo)

(defcustom neo/nuke-echo-area-message t
  "If non-nil, disables the self-promoting message Emacs put in the echo area.")

(neo/use-package emacs
  :doc "Setup questionable defaults"
  :custom
  (inhibit-splash-screen t)
  (custom-file (expand-file-name (format "%s-custom.el" (neo/get-emacs-instance-name)) user-emacs-directory))
 ; (neo/nuke-echo-area-message nil)
  )


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



