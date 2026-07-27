;;; tests/test-neo-ai-buddy-claude.el --- Claude package tests -*- lexical-binding: t; -*-

(require 'buttercup)

(defvar neo--ai-buddy-claude-test-package-declarations nil
  "Package declarations recorded while loading `neo-ai-buddy-claude.el'.")

(defmacro neo/use-package (name &rest arguments)
  "Record package NAME and ARGUMENTS without configuring it."
  `(push (cons ',name ',arguments)
         neo--ai-buddy-claude-test-package-declarations))

(load-file (expand-file-name "../neo-ai-buddy-claude.el"
                             (file-name-directory
                              (or load-file-name buffer-file-name))))

(describe "neo-ai-buddy Claude package ordering"
  (it "queues and demands web-server before claude-code-ide"
    (let* ((declarations
            (reverse neo--ai-buddy-claude-test-package-declarations))
           (names (mapcar #'car declarations))
           (web-server-arguments
            (cdr (assq 'web-server declarations)))
           (claude-arguments
            (cdr (assq 'claude-code-ide declarations))))
      (expect (seq-position names 'web-server)
              :to-be-less-than
              (seq-position names 'claude-code-ide))
      (expect (cadr (memq :demand web-server-arguments)) :to-be t)
      (expect (cadr (memq :after claude-arguments))
              :to-be
              'web-server))))

;;; test-neo-ai-buddy-claude.el ends here
