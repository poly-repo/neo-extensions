;;; test-neo-go.el --- Tests for neo-go -*- lexical-binding: t; -*-

(require 'buttercup)

(defvar neo--go-test-package-declarations nil
  "Package declarations recorded while loading `neo-go.el'.")

(defconst neo--go-test-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing `test-neo-go.el'.")

(defmacro neo/use-package (name &rest arguments)
  "Record package NAME and ARGUMENTS without configuring it."
  `(push (cons ',name ',arguments)
         neo--go-test-package-declarations))

(load-file
 (expand-file-name "../neo-go.el"
                   neo--go-test-directory))

(describe "neo-go"
  (it "provisions the Go toolchain without overriding PATH"
    (let* ((arguments
            (cdr (assq 'eglot neo--go-test-package-declarations)))
           (requirements
            (cadr (memq :ensure-system-package arguments))))
      (expect requirements
              :to-equal
              '((go . golang-go)
                (gopls . gopls)))))

  (it "starts Eglot in classic and tree-sitter Go buffers"
    (let* ((arguments
            (cdr (assq 'eglot neo--go-test-package-declarations)))
           (hook (cadr (memq :hook arguments))))
      (expect hook
              :to-equal
              '((go-mode go-ts-mode) . eglot-ensure))))

  (it "uses gopls for both supported Go modes"
    (let* ((arguments
            (cdr (assq 'eglot neo--go-test-package-declarations)))
           (config (cdr (memq :config arguments))))
      (expect config
              :to-contain
              '(neo/eglot-set-server
                '(go-mode go-ts-mode)
                '("gopls")))))

  (it "declares the Go tree-sitter mode preference"
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "../manifest.el" neo--go-test-directory))
      (expect (buffer-string)
              :to-match
              ":tree-sitter-modes[[:space:]]+((go go-mode go-ts-mode))")))

  (it "ships in full-monty"
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "../../full-monty/manifest.el"
                         neo--go-test-directory))
      (expect (buffer-string) :to-match "\"neo:go\""))))

;;; test-neo-go.el ends here
