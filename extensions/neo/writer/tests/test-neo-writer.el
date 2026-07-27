;;; test-neo-writer.el --- Tests for neo-writer -*- lexical-binding: t; -*-

(require 'buttercup)

(defvar neo--writer-test-package-declarations nil
  "Package declarations recorded while loading `neo-writer.el'.")

(defmacro neo/use-package (name &rest arguments)
  "Record package NAME and ARGUMENTS without configuring it."
  `(push (cons ',name ',arguments)
         neo--writer-test-package-declarations))

(load-file
 (expand-file-name "../neo-writer.el"
                   (file-name-directory (or load-file-name buffer-file-name))))

(describe "neo-writer"
  (it "declares Jinx system prerequisites in dependency order"
    (let* ((arguments
            (cdr (assq 'jinx neo--writer-test-package-declarations)))
           (requirements
            (cadr (memq :ensure-system-package arguments))))
      (expect requirements
              :to-equal
              '((pkgconf . pkgconf)
                (("pkgconf" "--exists" "enchant-2")
                 . libenchant-2-dev))))))

;;; test-neo-writer.el ends here
