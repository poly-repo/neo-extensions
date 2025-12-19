;;; -*- lexical-binding: t -*-

;; TODO requires
;; python -m pip install pyflakes

;; NOTE: this will end up in neo:programming-foundation. In
;; preparation, we call it neo-programming-foundation-treesit because
;; we're lazy this way. TODO: actually do this once the
;; neo:programming-foundation is done. Move the file and the (require
;; ...) there.
(require 'neo-programming-foundation-treesit)

;; not really needed .git should be good enough
(with-eval-after-load 'project
  (add-to-list 'project-vc-extra-root-markers "pyproject.toml")
  (add-to-list 'project-vc-extra-root-markers "pyrightconfig.json"))

(defun neo/project-root ()
  "Return the current project's root, or a sensible fallback."
  (or (when-let ((p (project-current nil)))
        (expand-file-name (project-root p)))
      (locate-dominating-file default-directory ".git")
      (locate-dominating-file default-directory "pyproject.toml")
      default-directory))

(defun neo/eglot-ensure-at-root ()
  "Start/reuse Eglot with the project root as workspace."
  (let ((root (neo/project-root)))
    (let ((default-directory root))
      (eglot-ensure))))

;; Enable for Python (use python-base-mode to cover python-mode & python-ts-mode)
(when (neo/extensionp "neo:programming-foundation")
  (add-hook 'python-base-mode-hook #'neo/eglot-ensure-at-root))

;; Handy command to fix a session that started at the wrong root
(defun neo/eglot-reconnect-at-root ()
  "Restart Eglot for this buffer using the project root."
  (interactive)
  (when (eglot-current-server)
    (eglot-shutdown (eglot-current-server)))
  (neo/eglot-ensure-at-root))


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

;(defun neo/python-eglot-shadow-venv-setup ()
  ;"Set eglot/BasedPyright to use the shadow Bazel virtualenv."
  ;(setq eglot-workspace-configuration
        ;`((:pyright . (:python (
				;:venvPath "~/.local/share/lsp-venv"
					  ;:pythonPath "~/.local/share/lsp-venv/bin/python"
					  ;:extraPaths ["~/.local/share/lsp-venv-typings"]))))))

(defun neo/python-eglot-shadow-venv-setup ()
  "Set eglot/BasedPyright to use the shadow Bazel virtualenv."
  (setq-default eglot-workspace-configuration nil))

  ;(setq-default eglot-workspace-configuration
	      ;'(:basedpyright (
			       ;:python (
					;:venvPath "~/.local/share/lsp-venv"
						  ;:pythonPath "~/.local/share/lsp-venv/bin/python"
			


		  ;:extraPaths ["~/.local/share/lsp-venv-typings"])
			       ;:typeCheckingMode "recommended"
						 ;)
			      ;:basedpyright.analysis (
						      ;:diagnosticSeverityOverrides (
										    ;:reportUnusedCallResult "none"
													    ;)
           ;:inlayHints (
             ;:callArgumentNames :json-false
           ;)
           ;))))

;;; TODO testing. Working with eglot, but we must be able to load without
(when (neo/extensionp "neo:programming-foundation")
  (add-hook 'python-mode-hook #'neo/python-eglot-shadow-venv-setup))

(neo/use-package python
  :builtin
  :after eglot				;TODO why?
  :custom
  (python-shell-interpreter "python3")
  :config
  (neo/eglot-set-server '(python-mode python-ts-mode) '("basedpyright-langserver"
							"--stdio"
							))
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
  ;; Save breakpoints on quit
  (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  (after-init . dape-breakpoint-load)

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

;; NOTE: :if/:unless/:disable don't seem to work. Could be related to
;; the fact that neo/use-package does expand the use-package itself at
;; some potentially unexpected time.
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
