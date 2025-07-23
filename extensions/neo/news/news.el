(defgroup neo-news nil
  "Customization options for getting news in Neo."
  :group 'neo)

(neo/use-package gnus
  :ensure nil  ;; Gnus is built into Emacs, so we don't need to install it separately
  :custom
  ;; Set gnus-init-file to dynamically load from user's Emacs directory
  (setq gnus-init-file (expand-file-name "gnus.el" user-emacs-directory)))

(neo/use-package all-the-icons-gnus
  :config
  (all-the-icons-gnus-setup))
