;;; tests/test-neo-questionable-defaults.el --- Tests for neo-questionable-defaults -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)

(defconst neo--questionable-defaults-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing `test-neo-questionable-defaults.el'.")

(defvar neo--questionable-defaults-test-package-declarations nil
  "Recorded package declarations from `neo-questionable-defaults.el'.")

(defvar key-chord-mode nil)

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
    (it "enables key chords from prog-mode-hook"
      (let* ((arguments
              (neo--questionable-defaults-test-package-arguments 'key-chord))
             (hook (cadr (memq :hook arguments)))
             (resets 0))
        (expect hook
                :to-equal
                '(prog-mode
                  . neo--questionable-defaults-enable-key-chords))
        (cl-letf (((symbol-function 'key-chord-reset-typing-detection)
                   (lambda () (setq resets (1+ resets)))))
          (with-temp-buffer
            (let ((prog-mode-hook (list (cdr hook))))
              (emacs-lisp-mode)
              (expect resets :to-equal 1)
              (expect (local-variable-p 'input-method-function)
                      :to-be-truthy)
              (expect input-method-function
                      :to-be #'key-chord-input-method))))))

    (it "leaves text and minibuffer input without key-chord processing"
      (let* ((arguments
              (neo--questionable-defaults-test-package-arguments 'key-chord))
             (hook (cadr (memq :hook arguments))))
        (expect (member '(key-chord-mode 1) arguments) :to-be nil)
        (expect (member '(global-key-chord-mode 1) arguments) :to-be nil)
        (dolist (mode '(text-mode minibuffer-mode))
          (with-temp-buffer
            (let ((prog-mode-hook (list (cdr hook)))
                  (text-mode-hook nil))
              (funcall mode)
              (expect input-method-function
                      :not :to-be #'key-chord-input-method))))))

    (it "cleans stale global state when the extension is reloaded"
      (let ((arguments
             (neo--questionable-defaults-test-package-arguments 'key-chord))
            (original-default (default-value 'input-method-function))
            (key-chord-mode t)
            buffers)
        (expect (member
                 '(neo--questionable-defaults-restrict-key-chords-to-prog-mode)
                 arguments)
                :not :to-be nil)
        (unwind-protect
            (progn
              (dolist (mode '(emacs-lisp-mode text-mode minibuffer-mode))
                (let ((buffer (generate-new-buffer " key-chord-scope-test")))
                  (push buffer buffers)
                  (with-current-buffer buffer
                    (let ((prog-mode-hook nil)
                          (text-mode-hook nil))
                      (funcall mode)))))
              (set-default 'input-method-function #'key-chord-input-method)
              (dolist (buffer (cdr buffers))
                (with-current-buffer buffer
                  (setq-local input-method-function #'key-chord-input-method)))
              (cl-letf (((symbol-function 'buffer-list)
                         (lambda () buffers))
                        ((symbol-function 'key-chord-reset-typing-detection)
                         #'ignore))
                (neo--questionable-defaults-restrict-key-chords-to-prog-mode))
              (expect (default-value 'input-method-function) :to-be nil)
              (expect key-chord-mode :to-be nil)
              (dolist (buffer buffers)
                (with-current-buffer buffer
                  (if (derived-mode-p 'prog-mode)
                      (progn
                        (expect (local-variable-p 'input-method-function)
                                :to-be-truthy)
                        (expect input-method-function
                                :to-be #'key-chord-input-method))
                    (expect (local-variable-p 'input-method-function)
                            :to-be nil)
                    (expect input-method-function :to-be nil)))))
          (set-default 'input-method-function original-default)
          (mapc #'kill-buffer buffers))))

    (it "preserves the configured global chord bindings"
      (let ((arguments
             (neo--questionable-defaults-test-package-arguments 'key-chord)))
        (dolist (binding
                 '((key-chord-define-global
                    "``" 'toggle-menu-bar-mode-from-frame)
                   (key-chord-define-global
                    ".." 'comment-or-uncomment-region)
                   (key-chord-define-global ",," 'sort-lines)))
          (expect (member binding arguments) :not :to-be nil))))))

(provide 'test-neo-questionable-defaults)
;;; test-neo-questionable-defaults.el ends here
