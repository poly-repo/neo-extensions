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
      (let ((calls nil)
            (repl-buffer (generate-new-buffer " *neo-haskell-ghci*")))
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'neo--haskell-project-root)
                         (lambda () "/tmp/codelabs/haskell/"))
                        ((symbol-function 'neo--haskell-find-executable)
                         (lambda (_program) nil))
                        ((symbol-function 'require)
                         (lambda (&rest _args) t))
                        ((symbol-function 'get-buffer-create)
                         (lambda (name &optional _inhibit-buffer-hooks)
                           (when (equal name "*neo-haskell:haskell*")
                             (push (list 'buffer name) calls))
                           repl-buffer))
                        ((symbol-function 'comint-check-proc)
                         (lambda (_buffer) nil))
                        ((symbol-function 'neo--haskell-configure-standalone-repl)
                         (lambda (buffer)
                           (push (list 'configure buffer) calls)))
                        ((symbol-function 'make-comint-in-buffer)
                         (lambda (&rest args)
                           (push (cons 'make-comint args) calls)
                           repl-buffer)))
                (expect (neo--haskell-ensure-standalone-repl) :to-equal repl-buffer))
              (expect (cl-subseq (nreverse calls) 0 3)
                      :to-equal
                      (list
                       (list 'buffer "*neo-haskell:haskell*")
                       (cons 'make-comint
                             (list
                              "neo-haskell-ghci:*neo-haskell:haskell*"
                              repl-buffer
                              "ghci"
                              nil
                              "-ignore-dot-ghci"
                              "-i."
                              "-XGHC2024"))
                       (list 'configure repl-buffer))))
          (kill-buffer repl-buffer)))))

  (describe "neo--haskell-load-buffer-into-standalone-repl"
    (it "tracks the source buffer for standalone repl switching"
      (let ((calls nil)
            (source-buffer (generate-new-buffer " *neo-haskell-source*"))
            (repl-buffer (generate-new-buffer " *neo-haskell-ghci*")))
        (unwind-protect
            (progn
              (with-current-buffer source-buffer
                (setq buffer-file-name "/tmp/codelabs/haskell/monads.hs")
                (cl-letf (((symbol-function 'save-buffer)
                           (lambda ()
                             (push 'save calls)))
                          ((symbol-function 'neo--haskell-ensure-standalone-repl)
                           (lambda ()
                             repl-buffer))
                          ((symbol-function 'get-buffer-process)
                           (lambda (buffer)
                             (expect buffer :to-equal repl-buffer)
                             'ghci-process))
                          ((symbol-function 'comint-send-string)
                           (lambda (process string)
                             (push (list 'send process string) calls))))
                  (expect (neo--haskell-load-buffer-into-standalone-repl)
                          :to-equal repl-buffer)))
              (with-current-buffer repl-buffer
                (expect neo--haskell-standalone-repl-source-buffer
                        :to-equal source-buffer))
              (expect (nreverse calls)
                      :to-equal
                      (list
                       'save
                       (list 'send
                             'ghci-process
                             ":load \"/tmp/codelabs/haskell/monads.hs\"\n"))))
          (kill-buffer source-buffer)
          (kill-buffer repl-buffer)))))

  (describe "neo--haskell-standalone-repl-parse-completions"
    (it "parses GHCi `:complete repl` responses"
      (cl-letf (((symbol-function 'haskell-string-literal-decode)
                 (lambda (string)
                   (read string))))
        (expect
         (neo--haskell-standalone-repl-parse-completions
          "2 2 \"import \"\n\"Data.List\"\n\"Data.List.NonEmpty\"\n")
         :to-equal
         '("import " "Data.List" "Data.List.NonEmpty")))))

  (describe "neo--haskell-standalone-repl-completion-at-point"
    (it "offers completions for the current REPL input"
      (with-temp-buffer
        (insert "Prelude> import Data.Lis")
        (goto-char (point-max))
        (let ((prompt-end (copy-marker (+ (point-min) (length "Prelude> ")))))
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (&optional _buffer)
                       'ghci-process))
                    ((symbol-function 'process-mark)
                     (lambda (_process)
                       prompt-end))
                    ((symbol-function 'comint-after-pmark-p)
                     (lambda ()
                       t))
                    ((symbol-function 'neo--haskell-standalone-repl-completions)
                     (lambda (input)
                       (expect input :to-equal "import Data.Lis")
                       '("import " "Data.List" "Data.List.NonEmpty"))))
            (expect (neo--haskell-standalone-repl-completion-at-point)
                    :to-equal
                    (list (+ prompt-end (length "import "))
                          (point-max)
                          '("Data.List" "Data.List.NonEmpty"))))))))

  (describe "neo--haskell-configure-standalone-repl"
    (it "enables inferior-haskell-mode, installs completion, and runs its hook"
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
                        :to-equal repl-buffer)
                (with-current-buffer repl-buffer
                  (expect (member #'neo--haskell-standalone-repl-completion-at-point
                                  completion-at-point-functions)
                          :not :to-be nil)
                  (expect (local-key-binding (kbd "TAB"))
                          :to-equal #'neo--haskell-standalone-repl-tab)
                  (expect (local-key-binding (kbd "C-c C-z"))
                          :to-equal #'neo--haskell-standalone-repl-switch-back))))
          (if had-inferior-haskell-buffer
              (setq inferior-haskell-buffer saved-inferior-haskell-buffer)
            (makunbound 'inferior-haskell-buffer))
          (kill-buffer repl-buffer))
        (expect (nreverse calls)
                :to-equal
                '((require inf-haskell)
                  inferior-mode
                  (run-hooks inferior-haskell-hook))))))

  (describe "neo--haskell-standalone-repl-switch-back"
    (it "returns to the most recent standalone source buffer"
      (let ((calls nil)
            (source-buffer (generate-new-buffer " *neo-haskell-source*"))
            (repl-buffer (generate-new-buffer " *neo-haskell-ghci*")))
        (unwind-protect
            (progn
              (with-current-buffer repl-buffer
                (setq neo--haskell-standalone-repl-source-buffer source-buffer)
                (cl-letf (((symbol-function 'pop-to-buffer)
                           (lambda (buffer)
                             (push buffer calls))))
                  (neo--haskell-standalone-repl-switch-back)))
              (expect calls :to-equal (list source-buffer)))
          (kill-buffer source-buffer)
          (kill-buffer repl-buffer))))))

(provide 'test-neo-haskell)
;;; test-neo-haskell.el ends here
