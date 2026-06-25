;;; -*- lexical-binding: t -*-

(defconst neo--ai-buddy-vterm-install-buffer-name " *Install vterm* "
  "Name of the buffer used to compile `vterm-module'.")

(defconst neo--ai-buddy-direnv-buffer-name " *Direnv allow* "
  "Name of the buffer used to run `direnv allow`.")

(defconst neo--ai-buddy-direnv-rc-filenames '(".envrc" ".env")
  "Direnv authorization files to search for before starting Codex.")

(defvar neo--ai-buddy-codex-direnv-guard nil
  "Prevent nested Codex startup commands from re-running `direnv allow`.")

(defun neo--ai-buddy-vterm-build-command ()
  "Return the shell command used to compile `vterm-module'."
  (mapconcat #'identity
             '("mkdir -p build"
               "cd build"
               "cmake -G 'Unix Makefiles' .."
               "make")
             " && "))

(defun neo--ai-buddy-vterm-build-directory ()
  "Return the Elpaca build directory for `vterm'."
  (expand-file-name "vterm" elpaca-builds-directory))

(defun neo--ai-buddy-vterm-module-filename ()
  "Return the platform-specific filename for `vterm-module'."
  (concat "vterm-module" module-file-suffix))

(defun neo--ai-buddy-vterm-module-path-in-directory (directory)
  "Return the `vterm-module' path rooted at DIRECTORY."
  (expand-file-name (neo--ai-buddy-vterm-module-filename) directory))

(defun neo--ai-buddy-vterm-module-path ()
  "Return the compiled `vterm-module' path for the active platform."
  (neo--ai-buddy-vterm-module-path-in-directory
   (neo--ai-buddy-vterm-build-directory)))

(defun neo--ai-buddy-vterm-installed-module-path ()
  "Return the prebuilt `vterm-module' path installed by Neo setup."
  (let ((xdg-data-home
         (or (getenv "XDG_DATA_HOME")
             (expand-file-name ".local/share" (or (getenv "HOME") "~")))))
    (expand-file-name
     (neo--ai-buddy-vterm-module-filename)
     (expand-file-name "emacs/site-lisp/vterm" xdg-data-home))))

(defun neo--ai-buddy-vterm-stage-installed-module (directory)
  "Copy the prebuilt `vterm-module' into DIRECTORY when available."
  (let ((installed-module (neo--ai-buddy-vterm-installed-module-path))
        (destination (neo--ai-buddy-vterm-module-path-in-directory directory)))
    (when (file-exists-p installed-module)
      (copy-file installed-module destination t)
      t)))

(defun neo--ai-buddy-direnv-rc-path (&optional directory)
  "Return the nearest direnv authorization file for DIRECTORY."
  (let ((directory (file-name-as-directory
                    (expand-file-name (or directory default-directory)))))
    (catch 'path
      (dolist (filename neo--ai-buddy-direnv-rc-filenames)
        (let ((root (locate-dominating-file directory filename)))
          (when root
            (throw 'path (expand-file-name filename root))))))))

(defun neo--ai-buddy-direnv-allow (&optional directory)
  "Allow the nearest direnv authorization file for DIRECTORY when present."
  (let ((rc-path (neo--ai-buddy-direnv-rc-path directory)))
    (when (and rc-path (executable-find "direnv"))
      (let ((default-directory (file-name-directory rc-path))
            (buffer (get-buffer-create neo--ai-buddy-direnv-buffer-name)))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)))
        (if (zerop (process-file "direnv" nil buffer nil "allow" rc-path))
            (kill-buffer buffer)
          (error "direnv allow failed; see buffer %s" (buffer-name buffer)))))))

(defun neo--ai-buddy-codex-run-with-direnv (fn &rest args)
  "Allow direnv for the active worktree before invoking FN with ARGS."
  (if neo--ai-buddy-codex-direnv-guard
      (apply fn args)
    (let ((neo--ai-buddy-codex-direnv-guard t))
      (neo--ai-buddy-direnv-allow)
      (apply fn args))))

(defun neo--ai-buddy-codex-enable-direnv-allow ()
  "Allow the active direnv file before running Codex startup commands."
  (dolist (command '(codex-cli-start codex-cli-toggle codex-cli-toggle-all))
    (unless (advice-member-p #'neo--ai-buddy-codex-run-with-direnv command)
      (advice-add command :around #'neo--ai-buddy-codex-run-with-direnv))))

(defun neo--ai-buddy-vterm-build-in-directory (directory)
  "Compile `vterm-module' in DIRECTORY."
  (let ((default-directory directory)
        (buffer (get-buffer-create neo--ai-buddy-vterm-install-buffer-name)))
    (unless (executable-find "cmake")
      (error "vterm requires cmake to build vterm-module"))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (compilation-mode))
    (if (zerop (call-process "sh" nil buffer t "-c"
                             (neo--ai-buddy-vterm-build-command)))
        (kill-buffer buffer)
      (error "vterm-module build failed; see buffer %s" (buffer-name buffer)))))

(defun neo--ai-buddy-vterm-build-module (elpaca)
  "Populate `vterm-module' for ELPACA in its build directory."
  (condition-case err
      (let* ((build-directory (elpaca<-build-dir elpaca))
             (module-path (neo--ai-buddy-vterm-module-path-in-directory build-directory)))
        (unless (file-exists-p module-path)
          (unless (neo--ai-buddy-vterm-stage-installed-module build-directory)
            (neo--ai-buddy-vterm-build-in-directory build-directory)))
        (elpaca-continue elpaca))
    (error
     (signal 'elpaca-build-error (list elpaca (error-message-string err))))))

(defun neo--ai-buddy-vterm-ensure-module ()
  "Populate `vterm-module' in the active Elpaca build directory when missing."
  (let ((build-directory (neo--ai-buddy-vterm-build-directory)))
    (when (and module-file-suffix
               (file-directory-p build-directory)
               (not (file-exists-p (neo--ai-buddy-vterm-module-path))))
      (unless (neo--ai-buddy-vterm-stage-installed-module build-directory)
        (neo--ai-buddy-vterm-build-in-directory build-directory)))))

(neo/use-package vterm
  :ensure (vterm
           :build ((:after elpaca-build-compile
                           neo--ai-buddy-vterm-build-module)))
  :init
  (neo--ai-buddy-vterm-ensure-module))

(neo/use-package codex-cli
  :after vterm
  :bind (("C-c c t" . codex-cli-toggle)
	 ("C-c c s" . codex-cli-start)
	 ("C-c c q" . codex-cli-stop)
	 ("C-c c Q" . codex-cli-stop-all)
	 ("C-c c p" . codex-cli-send-prompt)
	 ("C-c c r" . codex-cli-send-region)
	 ("C-c c f" . codex-cli-send-file)
	 ;; Show-all layout + paging
	 ("C-c c a" . codex-cli-toggle-all)
	 ("C-c c n" . codex-cli-toggle-all-next-page)
	 ("C-c c b" . codex-cli-toggle-all-prev-page))
  :init
  (setq codex-cli-executable "codex"
	codex-cli-terminal-backend 'vterm
	codex-cli-side 'right
	codex-cli-width 90)
  (setq codex-cli-toggle-all-min-width 60)
  :config
  (neo--ai-buddy-codex-enable-direnv-allow))


(provide 'neo-ai-buddy-codex)
