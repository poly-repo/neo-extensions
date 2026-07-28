;;; -*- lexical-binding: t -*-

;; TODO requires
;; python -m pip install pyflakes

;; Pulls in neo:programming-foundation's treesit setup (font-lock
;; level, folding); python's own grammar is declared separately via
;; :tree-sitter-grammars in this extension's manifest.el.
(require 'neo-programming-foundation-treesit)

(defun neo--python-project-marker-p (directory)
  "Return non-nil when DIRECTORY contains a Python project marker."
  (or (file-exists-p (expand-file-name "pyproject.toml" directory))
      (file-exists-p (expand-file-name "pyrightconfig.json" directory))))

(defun neo--python-eglot-project (directory)
  "Return the nearest Python project rooted above DIRECTORY for Eglot.

Return nil outside Python Eglot project discovery so ordinary project
backends retain ownership of Bazel, VC, and other project operations."
  (when (and (bound-and-true-p eglot-lsp-context)
             (derived-mode-p 'python-base-mode))
    (when-let* ((root (locate-dominating-file
                       directory
                       #'neo--python-project-marker-p)))
      (cons 'transient (expand-file-name root)))))

(defun neo/python-eglot-ensure ()
  "Install Python project discovery before starting or reusing Eglot."
  (require 'project)
  (add-hook 'project-find-functions #'neo--python-eglot-project)
  (eglot-ensure))

;; Handy command to fix a session that started at the wrong root
(defun neo/python-eglot-reconnect ()
  "Restart Eglot for the nearest Python project root."
  (interactive)
  (when (eglot-current-server)
    (eglot-shutdown (eglot-current-server)))
  (neo/python-eglot-ensure))

(defun neo--python-dape-breakpoint-load ()
  "Load saved Dape breakpoints without breaking startup."
  (when (fboundp 'dape-breakpoint-load)
    (condition-case err
        (dape-breakpoint-load)
      (file-missing
       (message "neo: skipping Dape breakpoint restore: %s"
                (error-message-string err))))))

(defvar eglot-watch-files-outside-project-root)

(with-eval-after-load 'eglot
  (defclass neo/python-eglot-server (eglot-lsp-server) ()
    :documentation
    "Eglot server for Python that ignores file watches outside its project.")

  (cl-defmethod eglot-register-capability
    ((_server neo/python-eglot-server)
     (_method (eql workspace/didChangeWatchedFiles))
     _id &rest _params)
    "Register Python file watches without traversing outside the project root."
    (let ((eglot-watch-files-outside-project-root nil))
      (cl-call-next-method))))


;(neo/eglot-set-server '(python-mode python-ts-mode) '("pyright-langserver" "--stdio"))
;(neo/eglot-set-server '(python-mode python-ts-mode) '("pylsp"))
;(neo/eglot-set-server '(python-mode python-ts-mode) '("pyright-langserver"))
;(neo/eglot-set-server '(python-mode python-ts-mode) '("basedpyright-langserver"))


;; Make pylsp use flake8 and disable pycodestyle/mccabe.
;; (setq-default eglot-workspace-configuration
;;   `((:pylsp
;;      . (:configurationSources ["flake8"]
;;         :plugins (:pycodestyle (:enabled ,json-false)
;;                   :mccabe      (:enabled ,json-false)
;;                   :flake8      (:enabled t))))))

(neo/use-package python
  :builtin
  :hook
  (python-base-mode . neo/python-eglot-ensure)
  :custom
  (python-shell-interpreter "python3")
  :config
  (neo/eglot-set-server
   '(python-mode python-ts-mode)
   '(neo/python-eglot-server
     "rass"
     "--"
     "basedpyright-langserver"
     "--stdio"
     "--"
     "ruff"
     "server"))
  ;; (neo/eglot-set-server '(python-mode python-ts-mode) '("basedpyright-langserver"
  ;; 							"--stdio"
  ;; 							))
  ;; (setq-default eglot-workspace-configuration
  ;;     '((:pylsp . (:plugins (:pyflakes (:enabled t)
  ;;                            :flake8 (:enabled t
  ;;                                     :builtins ["_", "ngettext"]
  ;;                                     )
  ;;                            )
  ;;                 )
  ;;      ))
  ;;   )
  )


(neo/use-package dape
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  (setq dape-key-prefix "\C-x\C-a")

  :hook
  ;; Load breakpoints once Elpaca has activated queued packages.
  (elpaca-after-init . neo--python-dape-breakpoint-load)

  :custom
  ;; Turn on global bindings for setting breakpoints with mouse
  ;; TODO should this be really global
  (dape-breakpoint-global-mode +1)

  ;; Info buffers to the right
  (dape-buffer-window-arrangement 'right)
  ;; Info buffers like gud (gdb-mi)
  (dape-buffer-window-arrangement 'gud)
  (dape-info-hide-mode-line nil)

  ;; Projectile users
;;  (dape-cwd-function #'project-root)

  :config
  ;; Save breakpoints on quit (moved from :hook to avoid dape-mode autoload in kill-emacs-hook)
  (add-hook 'kill-emacs-hook #'dape-breakpoint-save)
  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-hook #'kill-buffer)
  (add-to-list 'dape-configs
	       `(debugpy-omega
		 modes (python-mode python-ts-mode)
		 command "bazel run //repo/smoketest/python/fibonacci:fibonacci_test.debug"
		 ;;	       command-args ["-m" "debugpy.adapter" "--host" "0.0.0.0" "--port" :autoport ]
		 port 5678
		 :program ,(lambda ()
                             (let ((file (buffer-file-name)))
                               (unless file (user-error "No file associated with buffer"))
                               ;; Try project.el then fall back to vc root
                               (let ((proj-root
                                      (cond
                                       ((and (fboundp 'project-current) (project-current))
					(file-truename (project-root (project-current))))
                                       ((fboundp 'vc-root-dir)
					(file-truename (vc-root-dir)))
                                       (t nil))))
				 (unless proj-root (user-error "No project root found"))
				 ;; return path relative to project root
				 (file-relative-name (file-truename file) proj-root))))
		 :type "python"
		 :request "launch"
		 :console "integratedTerminal"
		 :showReturnValue t
		 :justMyCode nil
		 :cwd dape-cwd-fn))

  (add-to-list 'dape-configs
               `(omega
                 modes (python-ts-mode python-mode)
                 command "/home/mav/.local/share/wtrees/omega_mav-27-python-debug-support/.repo+smoketest+python+fibonacci+fibonacci_test.venv/bin/python"
		 command-args ("-m" "debugpy.adapter" "--host" "0.0.0.0" "--port" :autoport )
		 port :autoport
                 :type "executable"
                 :request "launch"
                 :cwd (lambda () "/home/mav/.local/share/wtrees/omega_mav-27-python-debug-support/repo/smoketest/python/fibonacci")
		 :stopOnEntry t
		 :program "/home/mav/.local/share/wtrees/omega_mav-27-python-debug-support/bazel-out/k8-fastbuild/bin/repo/smoketest/python/fibonacci/fibonacci_test.pytest_main.py"))
					;                           :program "fibonacci_test.py"))
  )

;; TODO: anaconda has a pythonic-activate thingy. Would be cool if it could be made to use one of the bazel venvs.

;; NOTE: this used to gate on the Elisp `unless' above out of distrust of
;; `use-package''s own :if/:unless/:disabled, believed broken. That belief
;; was stale: the breakage was in `neo/use-package''s cross-extension merge
;; (fixed in omega-11sv.13.5, see `neo--merge-use-package-condition-section'
;; in core/neo-use-package.el), not in single-extension use, which already
;; worked correctly. The Elisp `unless' wrapper here is left as-is since
;; it's equally valid and unrelated to that bug.
(unless (neo/extensionp "neo:programming-foundation")
  (neo/use-package anaconda-mode
    :hook
    (python-mode . (lambda ()
		     (anaconda-mode 1)
		     (anaconda-eldoc-mode 1)))))

;; ;; For a more ergonomic Emacs and `dape' experience
;; (use-package repeat
;;   :custom
;;   (repeat-mode +1))

;; Left and right side windows occupy full frame height
;; TODO a bit too invasive
;;(use-package emacs
;;  :custom
;;  (window-sides-vertical t))
