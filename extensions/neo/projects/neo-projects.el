;; (defun neo/project-try-bazel (dir)
;;   "Return a Bazel project instance if DIR contains a MODULE.bazel file.
;; Otherwise return nil. DIR is expected to be an absolute directory name."
;;   (let* ((root (locate-dominating-file dir "MODULE.bazel")))
;;     (when root
;;       (cons 'neo-bazel-project root))))

;; (cl-defmethod project-root ((project (head neo-bazel-project)))
;;   (cdr project))

;; (neo/use-package project
;;   :builtin
;;   :custom
;;   (project-switch-commands '((project-find-file "Find file" "f")
;;                              (project-find-dir "Find dir" "d")
;;                              (project-dired "Dired" "D")
;;                              (consult-ripgrep "ripgrep" "g")
;;                              (magit-project-status "Magit" "m")))
;;   :init
;;   (add-hook 'project-find-functions #'neo/project-try-bazel))


(neo/use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  ("C-x b" . persp-switch-to-buffer*)
  ("C-x k" . persp-kill-buffer*)
  :custom
  (persp-mode-prefix-key (kbd "C-c C-p"))  ; pick your own prefix key here
  (persp-modestring-dividers '("⟪" "⟫" "•"))
  (persp-modestring-short t)
  :init
  (persp-mode))

(defun neo/persp-projectile-init-at-startup ()
  "Initialize persp-projectile from Projectile's current project."
  (when-let ((root (projectile-project-root)))
    (let ((persp-name (funcall projectile-project-name-function root)))
      (persp-switch persp-name))))

(neo/use-package persp-projectile
  :after (projectile perspective)
  :config
  (projectile-discover-projects-in-search-path)
;;  :hook
;;  (emacs-startup . #'neo/persp-projectile-init-at-startup)
)

;; NOTE: this cannot easily go inside a use package, would trigger
;; loading of persp-projectile-mode that is not defined.
(add-hook 'emacs-startup-hook
          #'neo/persp-projectile-init-at-startup)
;; TODO probably too muxh, we only need the treemacs part
(add-hook 'projectile-after-switch-project-hook #'neo/persp-projectile-init-at-startup)

(defvar neo/project-last-switched-times (make-hash-table :test 'equal)
  "Hash table mapping project root -> last switch time (float seconds since epoch).")

(defcustom neo/projectile-notes-open-threshold
  ;; default: 24 hours = 24 * 60 * 60
  (* 24 60 60)
  "Number of seconds that must have passed since the last switch to a project
before opening that project's .personal-notes.org file again.
Set to nil or 0 to always open the notes file when switching."
  :type '(choice (const :tag "Always" 0)
                 (number :tag "Seconds"))
  :group 'neo)

(defun neo--project-root-or-default ()
  "Return the projectile project root if available, otherwise `default-directory'."
  (if (fboundp 'projectile-project-root)
      (or (projectile-project-root) default-directory)
    default-directory))

(defun neo--should-open-notes-p (project-root)
  "Return non-nil if `.personal-notes.org' should be opened for PROJECT-ROOT.
Decision is based on `neo/project-last-switched-times' and
`neo/projectile-notes-open-threshold'."
  (let* ((key project-root)
         (last (gethash key neo/project-last-switched-times))
         (threshold neo/projectile-notes-open-threshold))
    (cond
     ;; user set threshold to 0 or nil: always open
     ((not threshold) t)
     ((<= (or threshold 0) 0) t)
     ;; if we've never switched to this project before, open it
     ((not last) t)
     ;; otherwise open only if enough time has passed
     (t
      (let ((elapsed (float-time (time-subtract (current-time) (seconds-to-time last)))))
        (>= elapsed threshold))))))

(defun neo/projectile-switch-project-action ()
  "Switch to a projectile project and optionally open that project's .personal-notes.org.

.opens .personal-notes.org from the project root only if the project hasn't
been switched to in more than `neo/projectile-notes-open-threshold' seconds."
  (interactive)
  (message "AYE")
  (ignore-errors
    (let* ((project-root (neo--project-root-or-default))
           (notes-file-name ".personal-notes.org")
           (notes-path (expand-file-name notes-file-name project-root)))
      (when (and (neo--should-open-notes-p project-root)
                 (file-exists-p notes-path))
        ;; Open notes in other window to avoid stealing current buffer
        (find-file-other-window notes-path))
      ;; record that we've switched to this project now
      (puthash project-root (float-time (current-time)) neo/project-last-switched-times)))
  ;; now run magit if the project is a git repo
  (when (vc-git-responsible-p default-directory)
    (magit-status)))

(defun neo/project-name-function (project)
  (let ((name (file-name-nondirectory
	       (directory-file-name project))))
    (string-remove-prefix "mav-" (string-remove-prefix "omega_" name))))

;; TODO when vertico+marginalia are available, put the directory as a
;; marginalia annotation. Keep a mapping to use the path when switching project.
;; Not sure about conflicts, we really shouldn't have any in Omega.
(defun neo/projectile-switch-project-by-name ()
  (interactive)
  (let* ((alist
          (mapcar (lambda (root)
                    (cons
                     (format "%s  [%s]"
                             (funcall projectile-project-name-function root)
                             (abbreviate-file-name root))
                     root))
                  projectile-known-projects))
         (choice (completing-read "Switch to project: " alist nil t)))
    (projectile-persp-switch-project (cdr (assoc choice alist)))))
    ;; (projectile-switch-project-by-name
    ;;  (cdr (assoc choice alist)))))

;; (define-key projectile-mode-map
;;   (kbd "C-c p p")
;;   #'neo/projectile-switch-project-by-name)

;; TODO: make persp-* change project obey projectile-auto-discover (advise them here?)
(use-package projectile
  :custom
  (projectile-project-search-path '("~/Projects/" "~/.local/share/wtrees")) 
  (projectile-switch-project-action #'neo/projectile-switch-project-action)
  (projectile-indexing-method 'alien)
  (projectile-auto-discover t)		; not very useful as we go through persp-*
  (projectile-project-name nil)
  (projectile-project-name-function #'neo/project-name-function)
  ;; TODO: we probably want 'remove here, but for now intergation w/ perspective is not fully working and we have to force the current project
  (projectile-current-project-on-switch 'move-to-end)
  :init
  (projectile-mode +1)
  (projectile-register-project-type 'omega-bazel '("MODULE.bazel"))
  (add-to-list 'projectile-project-root-files "MODULE.bazel")
 :bind
  (:map projectile-mode-map
        ("C-x p" . projectile-command-map))
  ("C-x p p" . neo/projectile-switch-project-by-name))
  ;; :bind (:map projectile-mode-map
  ;;             ("C-x p" . projectile-command-map)))

;; (defcustom projectile-after-switch-project-hook nil
;;   "Hooks run right after project is switched."
;;   :group 'projectile
;;   :type 'hook)

;; (defcustom projectile-before-switch-project-hook nil
;;   "Hooks run when right before project is switched."
;;   :group 'projectile
;;   :type 'hook)


(neo/use-package treemacs-projectile)

