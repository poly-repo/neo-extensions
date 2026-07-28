;;; test-neo-compsel.el --- Tests for neo-compsel -*- lexical-binding: t; -*-

(require 'buttercup)

(defvar neo--compsel-test-package-declarations nil)

(defmacro neo/use-package (name &rest args)
  "Capture the package declaration for NAME with ARGS."
  `(push (cons ',name ',args) neo--compsel-test-package-declarations))

(load-file
 (expand-file-name "../neo-compsel.el"
                   (file-name-directory (or load-file-name buffer-file-name))))

(describe "neo:compsel Corfu activation"
  (it "loads Corfu eagerly so Eglot buffers receive the global frontend"
    (let ((args (cdr (assq 'corfu neo--compsel-test-package-declarations))))
      (expect (cadr (memq :demand args)) :to-be t)
      (expect (cdr (memq :config args)) :to-contain '(global-corfu-mode))
      (expect args
              :to-contain '(lsp-completion-mode . kb/corfu-setup-lsp)))))

;;; test-neo-compsel.el ends here
