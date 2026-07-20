;;; -*- lexical-binding: t -*-

(require 'neo-org-core)

(defcustom neo/org-enable-roam nil
  "When non-nil, enable Org Roam support."
  :type 'boolean
  :group 'neo-org)

(defcustom neo/org-roam-directory nil
  "Directory for Org Roam notes.
When nil, use a `roam/' subdirectory under `neo/org-directory'."
  :type '(choice (const :tag "Use base-directory roam/" nil)
                 string)
  :group 'neo-org)

(defcustom neo/org-roam-dailies-directory "Dailies/"
  "Directory for Org Roam dailies, relative to `org-roam-directory'."
  :type 'string
  :group 'neo-org)

(defcustom neo/org-roam-completion-everywhere t
  "When non-nil, enable Org Roam completion outside Roam buffers."
  :type 'boolean
  :group 'neo-org)

(defcustom neo/org-roam-project-template-file nil
  "Optional Org Roam project template file.
Relative paths are resolved under `neo/org-directory'."
  :type '(choice (const :tag "No project template" nil)
                 string)
  :group 'neo-org)

(defcustom neo/org-enable-roam-ui nil
  "When non-nil, enable Org Roam UI integration."
  :type 'boolean
  :group 'neo-org)

(defcustom neo/org-roam-ui-follow t
  "When non-nil, keep Org Roam UI focused on the current node."
  :type 'boolean
  :group 'neo-org)

(defcustom neo/org-roam-ui-update-on-save t
  "When non-nil, refresh Org Roam UI after saving notes."
  :type 'boolean
  :group 'neo-org)

(defcustom neo/org-roam-ui-node-display '("title" "tags")
  "Fields that Org Roam UI should show on each node."
  :type '(repeat string)
  :group 'neo-org)

(defun neo--org-roam-directory ()
  "Return the absolute Org Roam directory."
  (file-name-as-directory
   (neo--org-normalize-path
    (or (and (neo--org-string-present-p neo/org-roam-directory)
             neo/org-roam-directory)
        "roam/")
    (neo--org-base-directory))))

(defun neo--org-roam-db-location ()
  "Return the instance-local Org Roam database path."
  (neo/data-file-path "org-roam.db"))

(defun neo--org-roam-project-template-file ()
  "Return the absolute path for `neo/org-roam-project-template-file'."
  (when (neo--org-string-present-p neo/org-roam-project-template-file)
    (neo--org-normalize-path neo/org-roam-project-template-file
                             (neo--org-base-directory))))

(defun neo--org-roam-project-capture-template ()
  "Return the optional project capture template, or nil."
  (when-let ((template-file (neo--org-roam-project-template-file)))
    `("p" "project" plain (file ,template-file)
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)))

(defun neo--org-roam-capture-templates ()
  "Return Org Roam capture templates for the extension."
  (delq nil
        `(("d" "default" plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n")
           :unnarrowed t)
          ,(neo--org-roam-project-capture-template)
          ("b" "book notes" plain
           "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t))))

(defun neo--org-roam-dailies-capture-templates ()
  "Return Org Roam dailies capture templates."
  '(("d" "default" entry "* TODO %?"
     :target (file+head "%<%Y-%m>.org" "#+TITLE: %<%Y-%m>\n"))))

(defun neo--org-enable-roam-ui-mode ()
  "Enable `org-roam-ui-mode' once Org Roam UI is ready."
  (if after-init-time
      (org-roam-ui-mode 1)
    (add-hook 'after-init-hook #'org-roam-ui-mode)))

(defun neo--org-roam-ui-force-label-opacity ()
  "Force Org Roam UI node labels to remain visible."
  (when (fboundp 'org-roam-ui--eval)
    (org-roam-ui--eval
     "document.querySelectorAll('.node-label').forEach((node) => { node.style.opacity = '1'; });")))

(defun neo/org-roam-ui-show-labels ()
  "Ensure Org Roam UI keeps node labels visible."
  (interactive)
  (when (fboundp 'org-roam-ui-mode)
    (org-roam-ui-mode 1))
  (run-at-time "1 sec" nil #'neo--org-roam-ui-force-label-opacity))

(neo/use-package org-roam
  :after org
  :if neo/org-enable-roam
  :custom
  (org-roam-directory (neo--org-roam-directory))
  (org-roam-db-location (neo--org-roam-db-location))
  (org-roam-dailies-directory neo/org-roam-dailies-directory)
  (org-roam-completion-everywhere neo/org-roam-completion-everywhere)
  (org-roam-dailies-capture-templates (neo--org-roam-dailies-capture-templates))
  (org-roam-capture-templates (neo--org-roam-capture-templates))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   :map org-mode-map
   ("C-M-i" . completion-at-point)
   ("C-c r t" . org-roam-tag-add)
   :map org-roam-dailies-map
   ("Y" . org-roam-dailies-capture-yesterday)
   ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))

(neo/use-package org-roam-ui
  :after org-roam
  :if (and neo/org-enable-roam neo/org-enable-roam-ui)
  :hook
  (org-roam-ui-mode . neo/org-roam-ui-show-labels)
  :custom
  (org-roam-ui-node-title t)
  (org-roam-ui-node-display neo/org-roam-ui-node-display)
  (org-roam-ui-follow neo/org-roam-ui-follow)
  (org-roam-ui-update-on-save neo/org-roam-ui-update-on-save)
  :config
  (neo--org-enable-roam-ui-mode))

(provide 'neo-org-roam)
;;; neo-org-roam.el ends here
