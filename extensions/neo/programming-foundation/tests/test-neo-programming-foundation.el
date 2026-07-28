;;; test-neo-programming-foundation.el --- Tests for programming-foundation -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)
(require 'eglot)
(require 'tooltip)

(defmacro neo/use-package (_name &rest _arguments)
  "Ignore package declarations while loading the extension under test."
  nil)

(provide 'neo-programming-foundation-treesit)
(provide 'neo-eglot-info)

(defvar neo/framework-bootstrapped-p nil)
(defvar neo/after-framework-bootstrap-hook nil)

(defconst neo--programming-foundation-test-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing `test-neo-programming-foundation.el'.")

(load-file
 (expand-file-name "../neo-programming-foundation.el"
                   neo--programming-foundation-test-directory))

(describe "neo-programming-foundation"
  (describe "Eglot hover presentation"
    (it "preserves popup tooltip routing when Eglot starts"
      (with-temp-buffer
        (let ((show-help-function #'tooltip-show-help)
              (eglot--managed-mode t))
          (cl-letf (((symbol-function 'neo--eglot-ensure-hover-timer)
                     #'ignore))
            (neo--eglot-configure-eldoc))
          (expect show-help-function :to-be #'tooltip-show-help)
          (expect (local-variable-p 'show-help-function) :to-be nil))))

    (it "preserves a buffer-local tooltip presenter across Eglot state changes"
      (with-temp-buffer
        (setq-local show-help-function #'tooltip-show-help)
        (let ((eglot--managed-mode t))
          (cl-letf (((symbol-function 'neo--eglot-ensure-hover-timer)
                     #'ignore))
            (neo--eglot-configure-eldoc)))
        (let ((eglot--managed-mode nil))
          (neo--eglot-configure-eldoc))
        (expect show-help-function :to-be #'tooltip-show-help)
        (expect (local-variable-p 'show-help-function) :to-be t)))

    (it "pads popup text using the tooltip background"
      (with-temp-buffer
        (let ((target (list :buffer (current-buffer)
                            :window (selected-window)
                            :point (point)))
              captured-arguments)
          (cl-letf (((symbol-function 'neo--eglot-hover-popup-available-p)
                     (lambda () t))
                    ((symbol-function 'posframe-show)
                     (lambda (_buffer &rest arguments)
                       (setq captured-arguments arguments)
                       nil)))
            (neo--eglot-show-hover-popup
             target
             '(("function documentation" :thing "function"))))
          (expect (plist-get captured-arguments :internal-border-width)
                  :to-equal neo/eglot-hover-popup-padding)
          (expect (plist-get captured-arguments :internal-border-color)
                  :to-equal
                  (plist-get captured-arguments :background-color)))))))

;;; test-neo-programming-foundation.el ends here
