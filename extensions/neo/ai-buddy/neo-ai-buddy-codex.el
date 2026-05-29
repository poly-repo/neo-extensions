;;; -*- lexical-binding: t -*-

(defconst neo--ai-buddy-vterm-install-buffer-name " *Install vterm* "
  "Name of the buffer used to compile `vterm-module'.")

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

(defun neo--ai-buddy-vterm-module-path ()
  "Return the compiled `vterm-module' path for the active platform."
  (expand-file-name (concat "vterm-module" module-file-suffix)
                    (neo--ai-buddy-vterm-build-directory)))

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
  "Compile `vterm-module' for ELPACA in its build directory."
  (condition-case err
      (progn
        (neo--ai-buddy-vterm-build-in-directory (elpaca<-build-dir elpaca))
        (elpaca-continue elpaca))
    (error
     (signal 'elpaca-build-error (list elpaca (error-message-string err))))))

(defun neo--ai-buddy-vterm-ensure-module ()
  "Compile `vterm-module' in the active Elpaca build directory when missing."
  (when (and module-file-suffix
             (file-directory-p (neo--ai-buddy-vterm-build-directory))
             (not (file-exists-p (neo--ai-buddy-vterm-module-path))))
    (neo--ai-buddy-vterm-build-in-directory
     (neo--ai-buddy-vterm-build-directory))))

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
  )


(provide 'neo-ai-buddy-codex)
