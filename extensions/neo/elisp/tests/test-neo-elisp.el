;;; tests/test-neo-elisp.el --- Tests for neo-elisp -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'button)

(defconst neo--elisp-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing `test-neo-elisp.el'.")

(defmacro neo/use-package (&rest _arguments)
  "Ignore package declarations while loading the extension under test."
  nil)

(load-file
 (expand-file-name "../neo-elisp.el" neo--elisp-test-dir))

(defun neo--elisp-test-fontify (source)
  "Insert and fontify SOURCE in the current temporary buffer."
  (emacs-lisp-mode)
  (neo--elisp-enable-use-package-buttons)
  (insert source)
  (font-lock-ensure))

(describe "neo-elisp use-package buttons"
  (it "browses the package symbol through standard button activation"
    (with-temp-buffer
      (neo--elisp-test-fontify "(neo/use-package helpful)\n")
      (search-backward "helpful")
      (let ((button (button-at (point)))
            browsed)
        (expect button :not :to-be nil)
        (expect (button-get button 'follow-link) :to-be t)
        (cl-letf (((symbol-function 'elpaca-browse)
                   (lambda (package)
                     (setq browsed package))))
          (button-activate button))
        (expect browsed :to-be 'helpful))))

  (it "ignores lookalikes in comments and strings"
    (with-temp-buffer
      (neo--elisp-test-fontify
       (concat ";; (neo/use-package commented)\n"
               "\"(neo/use-package string-package)\"\n"
               "(neo/use-package real-package)\n"))
      (goto-char (point-min))
      (search-forward "commented")
      (expect (button-at (1- (point))) :to-be nil)
      (search-forward "string-package")
      (expect (button-at (1- (point))) :to-be nil)
      (search-forward "real-package")
      (expect (button-at (1- (point))) :not :to-be nil)))

  (it "does not advance beyond an incremental Font Lock limit"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(neo/use-package ;; package comment\n helpful)\n")
      (goto-char (point-min))
      (let ((limit (save-excursion
                     (search-forward "package comment")
                     (1- (point)))))
        (expect (neo--elisp-use-package-button-matcher limit)
                :to-be nil)
        (expect (<= (point) limit) :to-be t)
        (expect (button-at (point)) :to-be nil))))

  (it "removes button properties when Font Lock unfontifies the form"
    (with-temp-buffer
      (neo--elisp-test-fontify "(neo/use-package helpful)\n")
      (search-backward "helpful")
      (let ((package-position (point)))
        (expect (button-at package-position) :not :to-be nil)
        (font-lock-unfontify-region (point-min) (point-max))
        (expect (button-at package-position) :to-be nil)
        (dolist (property neo--elisp-use-package-button-properties)
          (expect (get-text-property package-position property)
                  :to-be nil))))))

(provide 'test-neo-elisp)
;;; test-neo-elisp.el ends here
