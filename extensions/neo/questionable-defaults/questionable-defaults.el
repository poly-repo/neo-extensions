;;; Defaults for Neo

(neo/use-package emacs
  :doc "Setup questionable defaults"
  :custom
  (inhibit-splash-screen t)
  (custom-file (expand-file-name (format "%s-custom.el" (neo/get-emacs-instance-name)) user-emacs-directory))
  )


