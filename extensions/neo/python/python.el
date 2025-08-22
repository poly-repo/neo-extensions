;; TODO requires
;; python -m pip install pyflakes

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
    (let ((default-directory root)) ;; <- critical: set workspace root
      (eglot-ensure))))

;; Enable for Python (use python-base-mode to cover python-mode & python-ts-mode)
(add-hook 'python-base-mode-hook #'neo/eglot-ensure-at-root)

;; Handy command to fix a session that started at the wrong root
(defun neo/eglot-reconnect-at-root ()
  "Restart Eglot for this buffer using the project root."
  (interactive)
  (when (eglot-current-server)
    (eglot-shutdown (eglot-current-server)))
  (neo/eglot-ensure-at-root))


;(neo/eglot-set-server '(python-mode python-ts-mode) '("pyright-langserver" "--stdio"))
(neo/eglot-set-server '(python-mode python-ts-mode) '("pylsp"))

;; Make pylsp use flake8 and disable pycodestyle/mccabe.
(setq-default eglot-workspace-configuration
  `((:pylsp
     . (:configurationSources ["flake8"]
        :plugins (:pycodestyle (:enabled ,json-false)
                  :mccabe      (:enabled ,json-false)
                  :flake8      (:enabled t))))))

(use-package python
  :ensure nil  ; builtin
  :hook
  (python-mode . #'eglot-ensure)
;  (python-mode . ws-butler-mode)
;  (python-mode . hs-minor-mode)
  :custom
  (python-shell-interpreter "python3")
  :config
(setq-default eglot-workspace-configuration
    '((:pylsp . (:plugins (:pyflakes (:enabled t)
                           :flake8 (:enabled t
                                    :builtins ["_", "ngettext"]
                                    )
                           )
                )
     ))
  )
)
