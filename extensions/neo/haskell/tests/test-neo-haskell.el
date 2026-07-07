;;; tests/test-neo-haskell.el --- Tests for neo-haskell -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)

(defmacro neo/use-package (&rest _args)
  "Ignore package declarations while loading extension code in tests."
  nil)

(load-file (expand-file-name "../neo-haskell.el"
                             (file-name-directory (or load-file-name buffer-file-name))))

(describe "neo-haskell"
  (describe "neo--haskell-setup-keymap"
    (it "binds the shared C-c h command set on the given keymap"
      (let ((map (make-sparse-keymap)))
        (neo--haskell-setup-keymap map)
        (expect (lookup-key map (kbd "C-c h h")) :to-equal #'neo/haskell-hoogle)
        (expect (lookup-key map (kbd "C-c h i")) :to-equal #'haskell-navigate-imports)
        (expect (lookup-key map (kbd "C-c h I")) :to-equal #'neo/haskell-format-imports)
        (expect (lookup-key map (kbd "C-c h m")) :to-equal #'haskell-auto-insert-module-template)
        (expect (lookup-key map (kbd "C-c h z")) :to-equal #'haskell-interactive-switch)
        (expect (lookup-key map (kbd "C-c h l")) :to-equal #'haskell-process-load-file)
        (expect (lookup-key map (kbd "C-c h b")) :to-equal #'haskell-process-cabal-build)
        (expect (lookup-key map (kbd "C-c h c")) :to-equal #'haskell-compile))))

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
          (kill-buffer repl-buffer)))))

  (describe "neo--haskell-run-eglot-quickfix"
    (it "uses the Flymake diagnostic span when offering HLS quick fixes"
      (let ((calls nil))
        (with-temp-buffer
          (setq-local eglot--managed-mode t)
          (insert "abcdefghij")
          (goto-char 7)
          (cl-letf (((symbol-function 'flymake-diagnostics)
                     (lambda (position &optional _end)
                       (expect position :to-equal 7)
                       '(diag-a diag-b)))
                    ((symbol-function 'flymake-diagnostic-beg)
                     (lambda (diagnostic)
                       (pcase diagnostic
                         ('diag-a 5)
                         (_ 6))))
                    ((symbol-function 'flymake-diagnostic-end)
                     (lambda (diagnostic)
                       (pcase diagnostic
                         ('diag-a 7)
                         (_ 9))))
                    ((symbol-function 'eglot-code-actions)
                     (lambda (beg end kind &optional interactive)
                       (push (list beg end kind interactive) calls)
                       (unless interactive '(quickfix-action)))))
            (expect (neo--haskell-run-eglot-quickfix)
                    :to-equal
                    '(quickfix-action))))
        (expect (nreverse calls)
                :to-equal
                '((5 9 "quickfix" nil)
                  (5 9 "quickfix" t))))))

  (describe "neo--haskell-enable-eglot-ui"
    (it "makes Eglot-backed HLS diagnostics clickable"
      (let ((inlay-hints-enabled nil))
        (with-temp-buffer
          (setq-local eglot--managed-mode t)
          (cl-letf (((symbol-function 'derived-mode-p)
                     (lambda (&rest _modes) t))
                    ((symbol-function 'eglot-inlay-hints-mode)
                     (lambda (arg)
                       (setq inlay-hints-enabled arg))))
            (neo--haskell-enable-eglot-ui))
          (let ((warning-props
                 (alist-get :warning
                            (buffer-local-value 'flymake-diagnostic-types-alist
                                                (current-buffer))
                            nil nil #'eq)))
            (expect (local-variable-p 'flymake-diagnostic-types-alist)
                    :to-be-truthy)
            (expect inlay-hints-enabled :to-equal 1)
            (expect (alist-get 'keymap warning-props)
                    :to-equal neo--haskell-eglot-quickfix-map)
            (expect (alist-get 'help-echo warning-props)
                    :to-equal #'neo--haskell-eglot-diagnostic-help)
            (expect (alist-get 'pointer warning-props)
                    :to-equal 'hand))))))

  (describe "neo--haskell-eglot-inlay-hint-action"
    (it "keeps top-level text edits from HLS import hints"
      (let* ((hint '(:label "( nub, sort )"
                     :textEdits ((:newText "import Data.List ( nub, sort )"))))
             (edits (plist-get hint :textEdits)))
        (expect (neo--haskell-eglot-inlay-hint-action
                 hint)
                :to-equal
                (list :hint hint
                      :text-edits edits))))

    (it "prefers whole-hint text edits over label-part commands"
      (let* ((command '(:title "Apply" :command "hls.applyOneHint"))
             (hint '(:label ["nub" ", " "sort"]
                     :textEdits ((:newText "import Data.List ( nub, sort )"))))
             (edits (plist-get hint :textEdits)))
        (expect (neo--haskell-eglot-inlay-hint-action
                 hint
                 (list :value "nub" :command command))
                :to-equal
                (list :hint hint
                      :text-edits edits)))))

  (describe "neo--haskell-propertize-eglot-inlay-hint-text"
    (it "adds clickable properties when the hint carries an action"
      (let* ((action '(:text-edits ((:newText "import Data.List ( nub )"))))
             (text (neo--haskell-propertize-eglot-inlay-hint-text
                    "( nub )" nil action "Make this import explicit" nil)))
        (expect (get-text-property 0 'keymap text)
                :to-equal neo--haskell-eglot-inlay-hint-map)
        (expect (get-text-property 0 'neo--haskell-eglot-inlay-action text)
                :to-equal action)
        (expect (string-match-p "Make this import explicit"
                                (get-text-property 0 'help-echo text))
                :not :to-be nil))))

  (describe "neo--haskell-eglot-inlay-hint-action-in-string"
    (it "extracts the action from the clicked inlay-hint segment"
      (let* ((action '(:text-edits ((:newText "import Data.List ( nub )"))))
             (text (neo--haskell-propertize-eglot-inlay-hint-text
                    "( nub )" nil action "Make this import explicit" nil)))
        (expect (neo--haskell-eglot-inlay-hint-action-in-string text 2)
                :to-equal action))))

  (describe "neo--haskell-refresh-eglot-inlay-hints"
    (it "clears and repaints visible Eglot inlay hints"
      (let (removed updated)
        (with-temp-buffer
          (setq-local eglot--managed-mode t)
          (setq-local eglot-inlay-hints-mode t)
          (cl-letf (((symbol-function 'remove-overlays)
                     (lambda (&rest args)
                       (setq removed args)))
                    ((symbol-function 'window-start)
                     (lambda (&optional _window)
                       11))
                    ((symbol-function 'window-end)
                     (lambda (&optional _window _update)
                       29))
                    ((symbol-function 'eglot--update-hints-1)
                     (lambda (from to)
                       (setq updated (list from to)))))
            (neo--haskell-refresh-eglot-inlay-hints)))
        (expect removed
                :to-equal
                '(nil nil eglot--inlay-hint t))
        (expect updated
                :to-equal
                '(11 29)))))

  (describe "neo--haskell-paint-eglot-inlay-hint"
    (it "ignores hints outside the requested region without signaling"
      (with-temp-buffer
        (insert "import Data.List\n")
        (cl-letf (((symbol-function 'eglot--lsp-position-to-point)
                   (lambda (_position)
                     (point-max))))
          (expect (condition-case nil
                      (progn
                        (neo--haskell-paint-eglot-inlay-hint
                         '(:label "( nub )"
                           :position (:line 0 :character 0))
                         1
                         1)
                        'ok)
                    (error 'failed))
                  :to-equal
                  'ok)))))

  (describe "neo--haskell-apply-eglot-inlay-hint-at-point"
    (it "applies HLS import-hint text edits from inlay overlays"
      (let ((applied nil)
            (refreshed nil))
        (with-temp-buffer
          (insert "import Data.List\n")
          (goto-char (line-end-position))
          (setq-local eglot--managed-mode t)
          (let ((overlay (make-overlay (point) (point) nil t)))
            (overlay-put overlay 'eglot--inlay-hint t)
            (overlay-put overlay 'neo--haskell-eglot-inlay-action
                         '(:text-edits ((:newText "import Data.List ( nub, sort )"))))
            (cl-letf (((symbol-function 'eglot--apply-text-edits)
                       (lambda (edits &optional _version _silent)
                         (setq applied edits)))
                      ((symbol-function 'neo--haskell-refresh-eglot-inlay-hints)
                       (lambda ()
                         (setq refreshed t))))
              (neo--haskell-apply-eglot-inlay-hint-at-point)))
          (expect applied
                  :to-equal
                  '((:newText "import Data.List ( nub, sort )")))
          (expect refreshed :to-be-truthy))))

    (it "resolves HLS hints before falling back to raw commands"
      (let ((applied nil)
            (refreshed nil)
            (executed nil)
            (command '(:title "Apply" :command "hls.applyOneHint"))
            (hint '(:label "nub")))
        (cl-letf (((symbol-function 'eglot-server-capable)
                   (lambda (&rest _args) t))
                  ((symbol-function 'eglot--current-server-or-lose)
                   (lambda () 'server))
                  ((symbol-function 'jsonrpc-request)
                   (lambda (_server method arg)
                     (expect method :to-equal :inlayHint/resolve)
                     (expect arg :to-equal hint)
                     '(:textEdits ((:newText "import Data.List ( nub )")))))
                  ((symbol-function 'eglot--apply-text-edits)
                   (lambda (edits &optional _version _silent)
                     (setq applied edits)))
                  ((symbol-function 'neo--haskell-refresh-eglot-inlay-hints)
                   (lambda ()
                     (setq refreshed t)))
                  ((symbol-function 'eglot-execute)
                   (lambda (&rest args)
                     (setq executed args))))
          (neo--haskell-eglot-apply-inlay-hint-action
           (list :hint hint :command command))
          (expect applied
                  :to-equal
                  '((:newText "import Data.List ( nub )")))
          (expect refreshed :to-be-truthy)
          (expect executed :to-be nil))))))

(provide 'test-neo-haskell)
;;; test-neo-haskell.el ends here
