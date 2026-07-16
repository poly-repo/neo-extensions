;;; tests/test-neo-terminal.el --- Tests for neo-terminal -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)

(defmacro neo/use-package (&rest _args)
  "Ignore package declarations while loading extension code in tests."
  nil)

(load-file (expand-file-name "../neo-terminal.el"
                             (file-name-directory (or load-file-name buffer-file-name))))

(describe "neo-terminal Codex vterm links"
  (it "extracts wrapped Markdown references across vterm line wraps"
    (with-temp-buffer
      (let* ((prefix "[neo-terminal.el](/tmp/project/devex/editors/emacs/extensions/")
             (suffix "extensions/neo/terminal/neo-terminal.el:17)")
             (wrap-start nil)
             (click-position nil)
             captured)
        (insert prefix)
        (setq wrap-start (point))
        (insert "\n")
        (add-text-properties wrap-start (point)
                             '(vterm-line-wrap t rear-nonsticky t))
        (setq click-position (point))
        (insert suffix)
        (cl-letf (((symbol-function 'neo--terminal-vterm-resolve-reference)
                   (lambda (reference)
                     (setq captured reference)
                     'wrapped-markdown)))
          (expect (neo--terminal-vterm-reference-at-position (+ click-position 10))
                  :to-equal 'wrapped-markdown)
          (expect captured
                  :to-equal
                  "/tmp/project/devex/editors/emacs/extensions/extensions/neo/terminal/neo-terminal.el:17")))))

  (it "extracts wrapped plain references across vterm line wraps"
    (with-temp-buffer
      (let* ((prefix "See /tmp/project/devex/editors/emacs/extensions/")
             (suffix "extensions/neo/terminal/neo-terminal.el:23 for details")
             (wrap-start nil)
             (click-position nil)
             captured)
        (insert prefix)
        (setq wrap-start (point))
        (insert "\n")
        (add-text-properties wrap-start (point)
                             '(vterm-line-wrap t rear-nonsticky t))
        (setq click-position (point))
        (insert suffix)
        (cl-letf (((symbol-function 'neo--terminal-vterm-resolve-reference)
                   (lambda (reference)
                     (setq captured reference)
                     'wrapped-plain)))
          (expect (neo--terminal-vterm-reference-at-position (+ click-position 10))
                  :to-equal 'wrapped-plain)
          (expect captured
                  :to-equal
                  "/tmp/project/devex/editors/emacs/extensions/extensions/neo/terminal/neo-terminal.el:23"))))))

(provide 'test-neo-terminal)
;;; test-neo-terminal.el ends here
