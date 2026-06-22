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
                :to-equal '((enable 1) save load switch))))

    (it "uses the standalone repl path for direct-file workspaces"
      (let ((calls nil)
            (buffer-file-name "/tmp/codelabs/haskell/monads.hs"))
        (cl-letf (((symbol-function 'neo--haskell-standalone-workspace-p)
                   (lambda () t))
                  ((symbol-function 'neo--haskell-load-buffer-into-standalone-repl)
                   (lambda ()
                     (push 'standalone-load calls)
                     'ghci-buffer))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer)
                     (push (list 'pop buffer) calls)))
                  ((symbol-function 'interactive-haskell-mode)
                   (lambda (&optional _arg)
                     (push 'interactive-haskell-mode calls))))
          (neo/haskell-switch-to-repl))
        (expect (member 'interactive-haskell-mode calls) :to-be nil)
        (expect (nreverse calls)
                :to-equal '(standalone-load (pop ghci-buffer))))))

  (describe "neo--haskell-ensure-standalone-repl"
    (it "starts ghci with the direct-cradle flags"
      (let ((calls nil))
        (cl-letf (((symbol-function 'neo--haskell-project-root)
                   (lambda () "/tmp/codelabs/haskell/"))
                  ((symbol-function 'neo--haskell-find-executable)
                   (lambda (_program) nil))
                  ((symbol-function 'require)
                   (lambda (&rest _args) t))
                  ((symbol-function 'get-buffer-create)
                   (lambda (name)
                     (push (list 'buffer name) calls)
                     'ghci-buffer))
                  ((symbol-function 'comint-check-proc)
                   (lambda (_buffer) nil))
                  ((symbol-function 'neo--haskell-configure-standalone-repl)
                   (lambda (buffer)
                     (push (list 'configure buffer) calls)))
                  ((symbol-function 'make-comint-in-buffer)
                   (lambda (&rest args)
                     (push (cons 'make-comint args) calls)
                     'ghci-buffer)))
          (expect (neo--haskell-ensure-standalone-repl) :to-equal 'ghci-buffer))
        (expect (nreverse calls)
                :to-equal
                '((buffer "*neo-haskell:haskell*")
                  (make-comint
                   "neo-haskell-ghci:*neo-haskell:haskell*"
                   ghci-buffer
                   "ghci"
                   nil
                   "-ignore-dot-ghci"
                   "-i."
                   "-XGHC2024")
                  (configure ghci-buffer)))))

  (describe "neo--haskell-configure-standalone-repl"
    (it "enables inferior-haskell-mode and runs its hook"
      (let ((calls nil)
            (repl-buffer (generate-new-buffer " *neo-haskell-ghci*"))
            (had-inferior-haskell-buffer (boundp 'inferior-haskell-buffer))
            (saved-inferior-haskell-buffer
             (and (boundp 'inferior-haskell-buffer)
                  (symbol-value 'inferior-haskell-buffer))))
        (unwind-protect
            (progn
              (when had-inferior-haskell-buffer
                (makunbound 'inferior-haskell-buffer))
              (cl-letf (((symbol-function 'require)
                         (lambda (feature &optional _filename _noerror)
                           (push (list 'require feature) calls)
                           (eq feature 'inf-haskell)))
                        ((symbol-function 'derived-mode-p)
                         (lambda (&rest _modes) nil))
                        ((symbol-function 'inferior-haskell-mode)
                         (lambda ()
                           (push 'inferior-mode calls)))
                        ((symbol-function 'run-hooks)
                         (lambda (hook)
                           (push (list 'run-hooks hook) calls))))
                (neo--haskell-configure-standalone-repl repl-buffer)
                (expect (symbol-value 'inferior-haskell-buffer)
                        :to-equal repl-buffer)))
          (if had-inferior-haskell-buffer
              (setq inferior-haskell-buffer saved-inferior-haskell-buffer)
            (makunbound 'inferior-haskell-buffer))
          (kill-buffer repl-buffer))
        (expect (nreverse calls)
                :to-equal
                '((require inf-haskell)
                  inferior-mode
                  (run-hooks inferior-haskell-hook))))))))

(provide 'test-neo-haskell)
;;; test-neo-haskell.el ends here
