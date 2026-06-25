;;; -*- lexical-binding: t -*-

(defun neo/project-name-function (project)
  (let ((name (file-name-nondirectory
	       (directory-file-name project))))
    (string-remove-prefix "mav-" (string-remove-prefix "omega_" name))))

;; TODO when vertico+marginalia are available, put the directory as a
;; marginalia annotation. Keep a mapping to use the path when switching project.
;; Not sure about conflicts, we really shouldn't have any in Omega.
(defun neo/projectile-switch-project-by-name ()
  "Switch to a Projectile project using short names."
  (interactive)
  (let* ((alist
          (mapcar (lambda (root)
                    (cons
                     (funcall projectile-project-name-function root)
                     root))
                  projectile-known-projects))
         ;; list of names for completing-read
         (names (mapcar #'car alist))
         (choice (completing-read "Switch to project: " names nil t))
         (root (cdr (assoc choice alist))))
    (unless root
      (error "Project not found!"))
    (message "(projectile-switch-project %s)" root)
    (projectile-switch-project-by-name root)))


(defvar neo/project-last-switched-times (make-hash-table :test 'equal)
  "Hash table mapping project root -> last switch time (float seconds since epoch).")

(defcustom neo/projectile-notes-open-threshold
  ;; default: 24 hours = 24 * 60 * 60
  30 ;(* 24 60 60)
  "Number of seconds that must have passed since the last switch to a project
before opening that project's .personal-notes.org file again.
Set to nil or 0 to always open the notes file when switching."
  :type '(choice (const :tag "Always" 0)
                 (number :tag "Seconds"))
  :group 'neo)

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

(require 'vc-git)

(defun neo/bury-other-project-buffers ()
  "Bury all buffers not belonging to the current Projectile project."
  (interactive)
  (let ((current-project (projectile-project-root)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (unless (or (not (buffer-file-name buf)) ; ignore non-file buffers
                    (string-prefix-p current-project (file-truename (buffer-file-name buf))))
          (bury-buffer buf))))))

(defun neo--persp-exists-p (name)
  "Return non-nil if a perspective named NAME already exists."
  (and (featurep 'perspective)
       (member name (persp-names))))

(defun neo--projects-ensure-project-magit-status (project-root &optional force)
  "Prefer a project Magit status buffer for PROJECT-ROOT.

When FORCE is non-nil and `neo-better-git' is unavailable, fall back
to `magit-status'."
  (cond
   ((fboundp 'neo--better-git-ensure-project-magit-status)
    (neo--better-git-ensure-project-magit-status project-root force))
   ((and force
         (vc-git-responsible-p project-root)
         (fboundp 'magit-status))
    (magit-status))))

(defun neo/projectile-switch-project-action ()
  "Switch to a projectile project.

On the first switch to a project (new perspective), open
`.personal-notes.org' if present and recent enough, otherwise
prefer `magit-status'. On revisits, let perspective.el restore the
saved window configuration while still allowing Magit to reclaim
low-priority buffers such as `*scratch*'."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (notes-file-name ".personal-notes.org")
         (notes-path (expand-file-name notes-file-name project-root))
         (new-persp (neo/projectile-update-treemacs))
         (opened-notes nil))
    (when new-persp
      (neo/bury-other-project-buffers))
    (message "neo/projectile-switch-project-action in %s <%s> (new=%s)"
             project-root
             (persp-name (persp-curr))
             new-persp)
    (when (and new-persp
               (neo--should-open-notes-p project-root)
               (file-exists-p notes-path))
      (find-file-other-window notes-path)
      (setq opened-notes t))
    (neo--projects-ensure-project-magit-status
     project-root
     (and new-persp (not opened-notes)))
    ;; record that we've switched to this project now
    (puthash project-root (float-time (current-time))
             neo/project-last-switched-times)))

(neo/use-package projectile
  :demand t
  :custom
  (projectile-project-search-path '("~/Projects/" "~/.local/share/wtrees" "~/Projects/worktrees/omega--m-vitale")) ; TODO the last is too hardcoded
  (projectile-auto-discover t)
  (projectile-current-project-on-switch 'move-to-end) ; TODO: once stable, remove this
  (projectile-project-name nil)
  (projectile-project-name-function #'neo/project-name-function)
  (projectile-switch-project-action #'neo/projectile-switch-project-action)
  :config
  (projectile-mode 1)
  (projectile-discover-projects-in-search-path)
  (with-eval-after-load 'project
    (define-key project-prefix-map (kbd "p") #'neo/projectile-switch-project-by-name))
  :bind
  (:map projectile-command-map
        ("p" . neo/projectile-switch-project-by-name))
  )

(defun neo/persp-ensure-messages ()
  "Ensure *Messages* is part of the current perspective."
  (when (and (featurep 'perspective)
             (get-buffer "*Messages*"))
    (persp-add-buffer (get-buffer "*Messages*"))))


(neo/use-package perspective
  :demand t
  :custom
  (persp-mode-prefix-key (kbd "C-c C-p"))
  (persp-modestring-dividers '("⟪" "⟫" "•"))
  (persp-modestring-short t)
  (persp-state-default-file (expand-file-name "persp-state.el" no-littering-var-directory))
  :config
  (persp-mode 1)
  ;; use-package adds NAME-mode as an autoload trigger for every :hook entry;
  ;; moving kill-emacs here avoids perspective-mode landing in kill-emacs-hook.
  (remove-hook 'kill-emacs-hook #'perspective-mode)
  (add-hook 'kill-emacs-hook #'persp-state-save)
  ;; NOTE we give time for magit/projectile/etc... to settle. There're
  ;; races I've not been able to solve.
  (run-with-idle-timer 1 nil (lambda ()
			       (when (file-exists-p persp-state-default-file)
				 (persp-state-load persp-state-default-file))))
  :hook
  ((persp-switch persp-created) . neo/persp-ensure-messages)
  :bind
  ("C-x b" . persp-switch-to-buffer*)
  ("C-x k" . persp-kill-buffer*))

(neo/use-package treemacs
  :custom
  (treemacs-persist-file nil) ; Projectile is authoritative
  (treemacs-auto-add-fallback-projects nil)
  (treemacs-follow-after-init nil)	;TODO once stable try to flip
  (treemacs-project-follow-cleanup nil)
  (treemacs-width 30)
  (treemacs-width-is-initially-locked t) ; nil ; we'd like it locked, but treemacs-set-width doesn't seem to have any (sane) effect
  (treemacs-lock-width t)
  (treemacs-display-in-side-window nil)	; we manage side windows ourselves
  ; treemacs-display-in-side-window t we'll try to handle this
  (treemacs-is-never-other-window t)
  (treemacs-use-all-the-icons-theme t)
  (treemacs-sorting 'alphabetic-case-insensitive-asc)

  :demand t
  :custom-face
;  (treemacs-root-face ((t (:font "Orbitron"))))
  (treemacs-root-face
   ((t (
	:inherit font-lock-keyword-face
        :font "Orbitron"
        :underline (:style line :thickness 2)
        :weight bold
        :extend t))))
  :config
  ;; not sure about this guy
  (treemacs-follow-mode 1)
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)
  )

(neo/use-package treemacs-all-the-icons
  :demand t)

;; not sure TODO wha does this do? seems to color different magit states differently
(neo/use-package treemacs-magit
  :after treemacs magit
  :demand t)

(defun neo/treemacs-visible-p ()
  (window-live-p
   (treemacs-get-local-window)))

(defun neo/treemacs-show-only-project (root name)
  (when (treemacs-current-workspace)
    (let* ((ws (treemacs-current-workspace))
           (projects (treemacs-workspace->projects ws)))

      (dolist (proj projects)
        (treemacs-do-remove-project-from-workspace proj t nil))

      (treemacs-do-add-project-to-workspace root name))))

(defun neo/projectile-update-treemacs ()
  "Update treemacs to show only the current project; switch perspective.
Return non-nil if a new perspective was created for this project."
  (when (fboundp 'projectile-project-root)
    (when-let* ((root (projectile-project-root))
	       (name (projectile-project-name)))
      (let ((new-persp (not (neo--persp-exists-p name))))
        (persp-switch name)
        (neo/treemacs-show-only-project
         root
         (projectile-project-name root))
        new-persp))))

(add-hook 'emacs-startup-hook #'neo/projectile-update-treemacs)
;(add-hook 'projectile-after-switch-project-hook
;          #'neo/projectile-update-treemacs)
