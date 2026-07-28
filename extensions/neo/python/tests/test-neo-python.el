;;; test-neo-python.el --- Tests for neo-python -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)
(require 'eglot)
(require 'python)

(defvar neo--python-test-package-declarations nil
  "Package declarations recorded while loading `neo-python.el'.")

(defconst neo--python-test-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing `test-neo-python.el'.")

(defvar python-base-mode-hook nil
  "Python base-mode hook isolated for this test.")

(defmacro neo/use-package (name &rest arguments)
  "Record package NAME and ARGUMENTS without configuring it."
  `(push (cons ',name ',arguments)
         neo--python-test-package-declarations))

(defun neo/extensionp (_slug)
  "Return non-nil so the test follows the programming-foundation path."
  t)

(provide 'neo-programming-foundation-treesit)

(load-file
 (expand-file-name "../neo-python.el"
                   neo--python-test-directory))

(describe "neo-python"
  (it "defers automatic Eglot startup with the Python declaration"
    (let* ((arguments
            (cdr (assq 'python neo--python-test-package-declarations)))
           (hook (cadr (memq :hook arguments))))
      (expect hook
              :to-equal
              '(python-base-mode . neo/python-eglot-ensure))
      (expect python-base-mode-hook :to-be nil)))

  (it "uses an explicit basedpyright and Ruff Rass configuration"
    (let* ((arguments
            (cdr (assq 'python neo--python-test-package-declarations)))
           (config (cdr (memq :config arguments))))
      (expect config
              :to-contain
              '(neo/eglot-set-server
                '(python-mode python-ts-mode)
                '(neo/python-eglot-server
                  "rass"
                  "--"
                  "basedpyright-langserver"
                  "--stdio"
                  "--"
                  "ruff"
                  "server")))))

  (it "installs the tested Python language-server tools for every NEO user"
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "../../../../../requirements.txt"
                         neo--python-test-directory))
      (expect (buffer-string) :to-match "^basedpyright$")
      (expect (buffer-string) :to-match "^rassumfrassum==0\\.3\\.4$")
      (expect (buffer-string) :to-match "^ruff$")))

  (describe "Eglot project discovery"
    (it "selects the nearest Python marker"
      (let* ((root (make-temp-file "neo-python-project-" t))
             (outer (expand-file-name "outer/" root))
             (inner (expand-file-name "outer/inner/" root))
             (source (expand-file-name "outer/inner/pkg/app.py" root)))
        (unwind-protect
            (progn
              (make-directory (file-name-directory source) t)
              (with-temp-file (expand-file-name "pyrightconfig.json" outer))
              (with-temp-file (expand-file-name "pyproject.toml" inner))
              (with-temp-buffer
                (python-mode)
                (let ((eglot-lsp-context t))
                  (expect
                   (project-root
                    (neo--python-eglot-project
                     (file-name-directory source)))
                   :to-equal
                   (file-name-as-directory inner)))))
          (delete-directory root t))))

    (it "falls through outside Python Eglot discovery"
      (let* ((root (make-temp-file "neo-python-project-" t))
             (source (expand-file-name "pkg/app.py" root)))
        (unwind-protect
            (progn
              (make-directory (file-name-directory source) t)
              (with-temp-file (expand-file-name "pyproject.toml" root))
              (with-temp-buffer
                (python-mode)
                (let ((eglot-lsp-context nil))
                  (expect
                   (neo--python-eglot-project
                    (file-name-directory source))
                   :to-be nil)))
              (with-temp-buffer
                (emacs-lisp-mode)
                (let ((eglot-lsp-context t))
                  (expect
                   (neo--python-eglot-project
                    (file-name-directory source))
                   :to-be nil))))
          (delete-directory root t))))

    (it "preserves the ordinary outer project for Bazel and VC operations"
      (let* ((root (make-temp-file "neo-python-project-" t))
             (python-root (expand-file-name "smoke/python/" root))
             (source-directory (expand-file-name "pkg/" python-root))
             (outer-project (cons 'transient (file-name-as-directory root)))
             (fallback (lambda (_directory) outer-project)))
        (unwind-protect
            (progn
              (make-directory source-directory t)
              (with-temp-file (expand-file-name "pyproject.toml" python-root))
              (with-temp-buffer
                (python-mode)
                (let ((project-find-functions
                       (list #'neo--python-eglot-project fallback)))
                  (let ((eglot-lsp-context nil))
                    (expect
                     (project-root (project-current nil source-directory))
                     :to-equal
                     (file-name-as-directory root)))
                  (let ((eglot-lsp-context t))
                    (expect
                     (project-root (project-current nil source-directory))
                     :to-equal
                     (file-name-as-directory python-root))))))
          (delete-directory root t))))

    (it "falls through when no Python marker exists"
      (let ((root (make-temp-file "neo-python-project-" t)))
        (unwind-protect
            (with-temp-buffer
              (python-mode)
              (let ((eglot-lsp-context t))
                (expect (neo--python-eglot-project root) :to-be nil)))
          (delete-directory root t))))

    (it "installs the Python backend before ensuring Eglot"
      (let ((project-find-functions nil)
            backend-at-ensure)
        (cl-letf (((symbol-function 'eglot-ensure)
                   (lambda ()
                     (setq backend-at-ensure
                           (car project-find-functions)))))
          (neo/python-eglot-ensure)
          (neo/python-eglot-ensure))
        (expect backend-at-ensure :to-equal #'neo--python-eglot-project)
        (expect
         (cl-count #'neo--python-eglot-project project-find-functions)
         :to-equal 1))))

  (it "filters only outside-root watches for the Python Eglot server"
    (let* ((root (file-name-as-directory
                  (make-temp-file "neo-python-watch-root-" t)))
           (process (make-process
                     :name "neo-python-watch-test"
                     :command '("cat")
                     :noquery t))
           (server (make-instance 'neo/python-eglot-server
                                  :name "neo-python-watch-test"
                                  :process process))
           outside-watches-during-registration)
      (unwind-protect
          (progn
            (setf (eglot--project server) (cons 'transient root))
            (let ((eglot-watch-files-outside-project-root t))
              (cl-letf (((symbol-function 'eglot-unregister-capability)
                         (lambda (&rest _arguments)
                           (setq outside-watches-during-registration
                                 (symbol-value
                                  'eglot-watch-files-outside-project-root)))))
                (eglot-register-capability
                 server
                 'workspace/didChangeWatchedFiles
                 "python-watches"
                 :watchers []))
              (expect eglot-watch-files-outside-project-root :to-be t))
            (expect outside-watches-during-registration :to-be nil))
        (when (process-live-p process)
          (delete-process process))
        (delete-directory root t)))))

;;; test-neo-python.el ends here
