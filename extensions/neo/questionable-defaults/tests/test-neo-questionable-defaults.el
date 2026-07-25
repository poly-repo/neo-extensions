;;; tests/test-neo-questionable-defaults.el --- Tests for neo-questionable-defaults -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)
(require 'comint)
(require 'org)

(defconst neo--questionable-defaults-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing `test-neo-questionable-defaults.el'.")

(defvar neo--questionable-defaults-test-package-declarations nil
  "Recorded package declarations from `neo-questionable-defaults.el'.")

(defvar key-chord-mode nil)

(define-derived-mode neo--questionable-defaults-test-terminal-mode comint-mode
  "NeoChordTerminal")

(define-derived-mode neo--questionable-defaults-test-notebook-mode org-mode
  "NeoChordNotebook")

(defmacro neo/use-package (name &rest arguments)
  "Record the package NAME and ARGUMENTS without configuring it."
  `(push (cons ',name ',arguments)
         neo--questionable-defaults-test-package-declarations))

(load-file
 (expand-file-name "../neo-questionable-defaults.el"
                   neo--questionable-defaults-test-dir))

(defun neo--questionable-defaults-test-package-arguments (package)
  "Return the `neo/use-package' arguments for PACKAGE in the extension."
  (cdr (assq package neo--questionable-defaults-test-package-declarations)))

(describe "neo-questionable-defaults"
  (it "enables global auto-revert instead of a buffer-local mode"
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "../neo-questionable-defaults.el"
                         neo--questionable-defaults-test-dir))
      (goto-char (point-min))
      (expect (re-search-forward
               "^  (global-auto-revert-mode 1)$"
               nil
               t)
              :not :to-be nil)
      (goto-char (point-min))
      (expect (re-search-forward
               "^  (auto-revert-mode 1)$"
               nil
               t)
              :to-be nil)))

  (describe "key-chord activation scope"
    (it "clears stale key-chord state when the input method is unavailable"
      (with-temp-buffer
        (setq-local input-method-function #'key-chord-input-method)
        (neo--questionable-defaults-enable-key-chords)
        (expect (local-variable-p 'input-method-function)
                :to-be nil)
        (expect input-method-function
                :not :to-be #'key-chord-input-method)))

    (it "enables key chords from editing-mode hooks"
      (let* ((arguments
              (neo--questionable-defaults-test-package-arguments 'key-chord))
             (hooks (cadr (memq :hook arguments))))
        (expect (cadr (memq :demand arguments)) :to-be t)
        (expect (member
                 '(after-change-major-mode
                   . neo--questionable-defaults-refresh-key-chords)
                 hooks)
                :not :to-be nil)
        (expect (member
                 '(minibuffer-setup
                   . neo--questionable-defaults-refresh-key-chords)
                 hooks)
                :not :to-be nil)
        (cl-letf (((symbol-function 'key-chord-input-method)
                   #'ignore)
                  ((symbol-function 'key-chord-reset-typing-detection)
                   #'ignore))
          (dolist (mode '(emacs-lisp-mode
                          text-mode
                          neo--questionable-defaults-test-notebook-mode))
            (with-temp-buffer
              (let ((after-change-major-mode-hook
                     '(neo--questionable-defaults-refresh-key-chords)))
                (funcall mode)
                (expect (local-variable-p 'input-method-function)
                        :to-be-truthy)
                (expect input-method-function
                        :to-be #'key-chord-input-method)))))))

    (it "leaves terminal buffers and minibuffer interaction without key-chord processing"
      (let* ((arguments
              (neo--questionable-defaults-test-package-arguments 'key-chord))
             (hooks (cadr (memq :hook arguments)))
             (minibuffer-buffer (window-buffer (minibuffer-window)))
             (had-local nil)
             (original-local nil))
        (expect (member '(key-chord-mode 1) arguments) :to-be nil)
        (expect (member '(global-key-chord-mode 1) arguments) :to-be nil)
        (expect (member
                 '(minibuffer-setup
                   . neo--questionable-defaults-refresh-key-chords)
                 hooks)
                :not :to-be nil)
        (with-temp-buffer
          (let ((after-change-major-mode-hook
                 '(neo--questionable-defaults-refresh-key-chords)))
            (neo--questionable-defaults-test-terminal-mode)
            (expect input-method-function
                    :not :to-be #'key-chord-input-method)))
        (with-current-buffer minibuffer-buffer
          (setq had-local (local-variable-p 'input-method-function))
          (when had-local
            (setq original-local input-method-function))
          (unwind-protect
              (progn
                (setq-local input-method-function #'key-chord-input-method)
                (let ((minibuffer-setup-hook
                       '(neo--questionable-defaults-refresh-key-chords)))
                  (run-hooks 'minibuffer-setup-hook))
                (expect input-method-function
                        :not :to-be #'key-chord-input-method))
            (if had-local
                (setq-local input-method-function original-local)
              (kill-local-variable 'input-method-function))))))

    (it "cleans stale global state when the extension is reloaded"
      (let ((arguments
             (neo--questionable-defaults-test-package-arguments 'key-chord))
            (original-default (default-value 'input-method-function))
            (minibuffer-buffer (window-buffer (minibuffer-window)))
            (minibuffer-had-local nil)
            (minibuffer-original-local nil)
            (key-chord-mode t)
            buffers)
        (expect (member
                 '(neo--questionable-defaults-scope-key-chords)
                 arguments)
                :not :to-be nil)
        (unwind-protect
            (progn
              (dolist (mode '(emacs-lisp-mode
                              text-mode
                              neo--questionable-defaults-test-terminal-mode))
                (let ((buffer (generate-new-buffer " key-chord-scope-test")))
                  (push buffer buffers)
                  (with-current-buffer buffer
                    (let ((after-change-major-mode-hook nil))
                      (funcall mode)))))
              (push minibuffer-buffer buffers)
              (with-current-buffer minibuffer-buffer
                (setq minibuffer-had-local
                      (local-variable-p 'input-method-function))
                (when minibuffer-had-local
                  (setq minibuffer-original-local input-method-function)))
              (set-default 'input-method-function #'key-chord-input-method)
              (dolist (buffer buffers)
                (with-current-buffer buffer
                  (setq-local input-method-function #'key-chord-input-method)))
              (cl-letf (((symbol-function 'buffer-list)
                         (lambda () buffers))
                        ((symbol-function 'key-chord-input-method)
                         #'ignore)
                        ((symbol-function 'key-chord-reset-typing-detection)
                         #'ignore))
                (neo--questionable-defaults-scope-key-chords))
              (expect (default-value 'input-method-function) :to-be nil)
              (expect key-chord-mode :to-be nil)
              (dolist (buffer buffers)
                (with-current-buffer buffer
                  (if (neo--questionable-defaults-key-chords-disabled-p)
                      (progn
                        (expect (local-variable-p 'input-method-function)
                                :to-be nil)
                        (expect input-method-function :to-be nil))
                    (expect (local-variable-p 'input-method-function)
                            :to-be-truthy)
                    (expect input-method-function
                            :to-be #'key-chord-input-method)))))
          (set-default 'input-method-function original-default)
          (with-current-buffer minibuffer-buffer
            (if minibuffer-had-local
                (setq-local input-method-function minibuffer-original-local)
              (kill-local-variable 'input-method-function)))
          (dolist (buffer buffers)
            (unless (eq buffer minibuffer-buffer)
              (kill-buffer buffer))))))

    (it "preserves the configured global chord bindings"
      (let ((arguments
             (neo--questionable-defaults-test-package-arguments 'key-chord)))
        (dolist (binding
                 '((key-chord-define-global
                    "``" 'toggle-menu-bar-mode-from-frame)
                   (key-chord-define-global
                    ".." 'comment-or-uncomment-region)
                   (key-chord-define-global ",," 'sort-lines)))
          (expect (member binding arguments) :not :to-be nil))))

    (it "scrubs stale key-chord state before `save-buffers-kill-emacs'"
      (let ((original-default (default-value 'input-method-function))
            (buffer (generate-new-buffer " *key-chord-exit* "))
            original-value)
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (setq-local input-method-function #'key-chord-input-method)
                (setq original-value input-method-function))
              (set-default 'input-method-function #'key-chord-input-method)
              (cl-letf (((symbol-function 'buffer-list)
                         (lambda () (list buffer))))
                (neo--questionable-defaults-cleanup-before-exit
                 (lambda ()
                   (expect (default-value 'input-method-function) :to-be nil)
                   (with-current-buffer buffer
                     (expect (local-variable-p 'input-method-function)
                             :to-be nil)
                     (expect input-method-function :to-be nil))))))
          (set-default 'input-method-function original-default)
          (with-current-buffer buffer
            (if original-value
                (setq-local input-method-function original-value)
              (kill-local-variable 'input-method-function)))
          (kill-buffer buffer))))))

(provide 'test-neo-questionable-defaults)
;;; test-neo-questionable-defaults.el ends here
