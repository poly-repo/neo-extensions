;;; tests/test-neo-haskell.el --- Tests for neo-haskell -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)

(defmacro neo/use-package (&rest _args)
  "Ignore package declarations while loading extension code in tests."
  nil)

(load-file (expand-file-name "../neo-haskell.el"
                             (file-name-directory (or load-file-name buffer-file-name))))

(describe "neo-haskell"
  (describe "neo--haskell-prefer-ts-mode"
    (before-each
      (setq major-mode-remap-alist nil))

    (after-each
      (setq major-mode-remap-alist nil))

    (it "remaps haskell-mode to haskell-ts-mode when tree-sitter is ready"
      (cl-letf (((symbol-function 'locate-library)
                 (lambda (library &optional _path _nosuffix)
                   (when (equal library "haskell-ts-mode")
                     "/tmp/haskell-ts-mode.el")))
                ((symbol-function 'treesit-ready-p)
                 (lambda (language &optional _quiet)
                   (eq language 'haskell))))
        (neo--haskell-prefer-ts-mode)
        (expect (alist-get 'haskell-mode major-mode-remap-alist)
                :to-equal 'haskell-ts-mode)))

    (it "removes the remap when the grammar is unavailable"
      (setq major-mode-remap-alist '((haskell-mode . haskell-ts-mode)))
      (cl-letf (((symbol-function 'locate-library)
                 (lambda (&rest _args) nil))
                ((symbol-function 'treesit-ready-p)
                 (lambda (&rest _args) nil)))
        (neo--haskell-prefer-ts-mode)
        (expect (alist-get 'haskell-mode major-mode-remap-alist)
                :to-equal nil))))

  (describe "neo/haskell-switch-to-repl"
    (it "saves, loads, and switches to the REPL"
      (let ((calls nil)
            (buffer-file-name "/tmp/neo-haskell-test.hs"))
        (cl-letf (((symbol-function 'interactive-haskell-mode)
                   (lambda (&optional arg)
                     (push (list 'enable arg) calls)))
                  ((symbol-function 'save-buffer)
                   (lambda ()
                     (push 'save calls)))
                  ((symbol-function 'haskell-process-load-file)
                   (lambda ()
                     (push 'load calls)))
                  ((symbol-function 'haskell-interactive-switch)
                   (lambda ()
                     (interactive)
                     (push 'switch calls))))
          (neo/haskell-switch-to-repl))
        (expect (nreverse calls)
                :to-equal '((enable 1) save load switch))))))

(provide 'test-neo-haskell)
;;; test-neo-haskell.el ends here
