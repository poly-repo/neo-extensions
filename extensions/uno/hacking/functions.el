(defgroup neo/hacking nil
  "Settings related to live hacking and development utilities."
  :group 'neo
  :prefix "neo/hacking-")

(defcustom neo/hacking-devex-root (expand-file-name "~/Projects/uno/devex/editors/emacs/")
  "Root directory containing source Emacs Lisp files."
  :type 'directory
  :group 'neo/hacking)

(defcustom neo/hacking-target-root (expand-file-name "~/neo-devel/")
  "Target root directory where files will be copied."
  :type 'directory
  :group 'neo/hacking)

;; (defun neo/hacking-maybe-copy-to-neo-devel ()
;;   "If the current buffer file is under `neo/hacking-devex-root`, copy it to `neo/hacking-target-root`, preserving hierarchy."
;;   (let ((file (buffer-file-name)))
;;     (when (and file (string-prefix-p neo/hacking-devex-root file))
;;       (let* ((relative-path (file-relative-name file neo/hacking-devex-root))
;;              (target-path (expand-file-name relative-path neo/hacking-target-root))
;;              (target-dir (file-name-directory target-path)))
;;         (make-directory target-dir t)
;;         (copy-file file target-path t)
;;         (message "neo/hacking: copied to %s" target-path)))))

(defun neo/hacking-maybe-copy-to-neo-devel ()
  "If the current buffer file is under `neo/hacking-devex-root`, copy it to `neo/hacking-target-root`, preserving hierarchy.
Special case: if the path starts with extensions/extensions/, collapse one level."
  (let ((file (buffer-file-name)))
    (when (and file (string-prefix-p neo/hacking-devex-root file))
      (let* ((relative-path (file-relative-name file neo/hacking-devex-root))
             ;; Handle the extensions/extensions/ case specially
             (fixed-relative-path
              (if (string-prefix-p "extensions/extensions/" relative-path)
                  (string-remove-prefix "extensions/" relative-path)
                relative-path))
             (target-path (expand-file-name fixed-relative-path neo/hacking-target-root))
             (target-dir (file-name-directory target-path)))
        (make-directory target-dir t)
        (copy-file file target-path t)
        (message "neo/hacking: copied to %s" target-path)))))

(defun neo/hacking-sync-all (&optional cleanup)
  "Copy all files from `neo/hacking-devex-root` to `neo/hacking-target-root`.
If CLEANUP is non-nil, delete everything in the target root before syncing.
Skips Emacs lockfiles like .#filename.el."
  (interactive "P")
  (when cleanup
    (when (file-exists-p neo/hacking-target-root)
      (delete-directory neo/hacking-target-root t)
      (message "neo/hacking: cleaned %s" neo/hacking-target-root)))

  (let ((default-directory neo/hacking-devex-root))
    (dolist (file (directory-files-recursively default-directory "\\.el\\'"))
      ;; Skip temp or lockfiles
      (when (and (file-exists-p file)
                 (not (string-match-p "/\\.?#" file))) ;; skips .# and .#
        (let* ((relative (file-relative-name file neo/hacking-devex-root))
               (fixed-relative
                (if (string-prefix-p "extensions/extensions/" relative)
                    (string-remove-prefix "extensions/" relative)
                  relative))
               (target (expand-file-name fixed-relative neo/hacking-target-root))
               (target-dir (file-name-directory target)))
          (make-directory target-dir t)
          (copy-file file target t)
          (message "neo/hacking: copied %s → %s" relative fixed-relative))))))

(add-hook 'after-save-hook #'neo/hacking-maybe-copy-to-neo-devel)

;;;--------------------------------------------------------------------------------------

(defvar neo/hacking-neo-pidfile
  (expand-file-name "neo-emacs.pid" (or (getenv "XDG_RUNTIME_DIR") "/tmp"))
  "Path to the PID file used to track the neo-devel Emacs instance.")

(defun neo/hacking--emacs-process-alive-p (pid)
  "Return non-nil if PID belongs to a live Emacs process with neo-devel in its cmdline."
  (when (and pid (integerp pid) (> pid 0))
    (let ((cmdline-file (format "/proc/%s/cmdline" pid)))
      (and (file-exists-p cmdline-file)
           (string-match-p "neo-devel" (with-temp-buffer
                                         (insert-file-contents cmdline-file)
                                         (buffer-string)))))))

(defun neo/hacking--kill-neo-instance ()
  "Kill any running Emacs associated with neo-devel config."
  (when (file-exists-p neo/hacking-neo-pidfile)
    (let ((pid (ignore-errors
                 (string-to-number
                  (with-temp-buffer
                    (insert-file-contents neo/hacking-neo-pidfile)
                    (buffer-string))))))
      (when (neo/hacking--emacs-process-alive-p pid)
        (ignore-errors (signal-process pid 'SIGTERM))
        (message "neo/hacking: killed existing neo-devel Emacs (PID %s)" pid)))))

(defun neo/hacking-launch-neo-emacs ()
  "Launch a new Emacs instance with the neo-devel config.
Kills any previous one using the neo PID file."
  (interactive)
  (neo/hacking--kill-neo-instance)
  (let* ((proc (start-process
                "neo-emacs" nil
                (car command-line-args) "--init-directory" (expand-file-name "~/neo-devel")))
         (pid (process-id proc)))
    (when pid
      (with-temp-file neo/hacking-neo-pidfile
        (insert (number-to-string pid)))
      (message "neo/hacking: launched new neo-devel Emacs (PID %s)" pid))))




