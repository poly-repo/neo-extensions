;;; Setting of fonts (in the UI section) seems to prevent the
;;; fixed-pitch setting here, so we defer this rather than having it
;;; in the :config section of the org package.
;;; TODO: there must be a better way.
;;; And this doesn't seem to work, we have this for now:
;; (add-hook 'after-init-hook (lambda ()
;;                              (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
;;                              (set-face-attribute 'org-block nil :inherit 'fixed-pitch))
;;                              100)


;; (defun org-protocol-store-link (o)
;;   (message "org protocol store link: %s" (prin1-to-string o)))

;; (defun org-protocol-capture (o)
;;   (message "org protocol capture: %s" (prin1-to-string o)))

;; Kill the frame if one was created for the capture
(defvar neo/delete-frame-after-capture 0 "Whether to delete the last frame after the current capture")

(defun neo/delete-frame-if-neccessary (&rest r)
  (cond
   ((= neo/delete-frame-after-capture 0) nil)
   ((> neo/delete-frame-after-capture 1)
    (setq neo/delete-frame-after-capture (- neo/delete-frame-after-capture 1)))
   (t
    (setq neo/delete-frame-after-capture 0)
    (delete-frame))))

(advice-add 'org-capture-finalize :after 'neo/delete-frame-if-neccessary)
(advice-add 'org-capture-kill :after 'neo/delete-frame-if-neccessary)
(advice-add 'org-capture-refile :after 'neo/delete-frame-if-neccessary)

(neo/use-package org
  :ensure nil
  :config
  (setq org-default-notes-file (concat org-directory "/default-notes.org"))
  (setq org-protocol-default-template-key "l")
  ;; (setq org-capture-templates
  ;;       `(("L" "org-protocol-capture" entry
  ;;          (file "~/org/inbox.org")
  ;;          "* TODO %a %i [[%:link][%:description]]\n\n %:initial"
  ;;       :immediate-finish t)))
  (setq org-capture-templates `(
        ("p" "org-protocol-capture" entry (file+headline ,(concat org-directory "/notes.org") "Inbox")
        "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?" :immediate-finish t)
        ("L" "org-protocol-store-link" entry (file+headline ,(concat org-directory "/notes.org") "Inbox")
        "* %? [[%:link][%:description]] \nCaptured On: %U" :immediate-finish t)
        ))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)   ;; Enable Emacs Lisp (default)
     (python . t)       ;; Enable Python
     (shell . t)        ;; Enable Shell (Bash)
     (C . t)))            ;; Enable C++
;      (go . t)))         ;; Enable Go
  ;; Set Clang as the compiler and enable C++23
  (setq org-babel-C-compiler "clang++")
  (setq org-babel-C++-flags "-std=c++23")
  (setq org-babel-default-header-args:C++
        '((:compiler . "clang++")
          (:flags . "-std=c++23")))

  :hook
  (org-mode .  (lambda () (electric-pair-mode -1))))

(neo/use-package org-tempo
  :ensure nil                             ; part of org
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("b" . "src bash"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
;  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("cpp" . "src C++"))
  (add-to-list 'org-structure-template-alist '("c++" . "src C++"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json")))

(neo/use-package org-crypt
  :ensure nil		; part of org-mode
  :after org
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))
  (setq org-crypt-key "mrz.vtl@gmail.com")
  (setq org-crypt-disable-auto-save 'encrypt)
  )

;(neo/use-package ob-go)
(neo/use-package ob-python :ensure nil)
(neo/use-package ob-shell :ensure nil)
(neo/use-package ob-C :ensure nil)


(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Roam/Notes")
  (org-roam-dailies-directory "~/Roam/Dailies")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
      '(("d" "default" entry "* TODO %?"
         :target (file+head "%<%Y-%m>.org" "#+TITLE: %<%Y-%m>\n"))))
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ("p" "project" plain (file "~/RoamNotes/Templates/project.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)
     ("b" "book notes" plain "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point)
	 ("C-c r t" . org-roam-tag-add)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-enable)
  )

(defun neo/org-roam-ui-show-labels ()
  "Force org-roam-ui to always show node labels."
  (interactive)
  (org-roam-ui-mode 1) ;; Ensure org-roam-ui is running
  (run-at-time "1 sec" nil
               (lambda ()
                 (let ((js "document.querySelectorAll('.node-label').forEach(e => e.style.opacity = '1');"))
                   (org-roam-ui--eval js)))))

(neo/use-package org-roam-ui
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  (org-roam-ui-mode . neo/org-roam-ui-show-labels)
  :config
  (setq org-roam-ui-node-title t   ;; Show titles
      org-roam-ui-node-display '("title" "tags")  ;; Display extra info
      org-roam-ui-follow t  ;; Sync selected node with Emacs
      org-roam-ui-update-on-save t)
  )

(neo/use-package org-download
  :config
  (setq org-download-method 'directory)
  (setq org-download-image-dir "~/org-images")
  (setq org-download-image-org-width 600)
  (setq org-download-link-format "[[file:%s]]\n"
        org-download-abbreviate-filename-function #'file-relative-name)
  (setq org-download-link-format-function #'org-download-link-format-function-default))


