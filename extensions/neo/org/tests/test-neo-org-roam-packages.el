;;; tests/test-neo-org-roam-packages.el --- Org Roam package tests -*- lexical-binding: t; -*-

(require 'buttercup)

(defvar neo--org-roam-test-package-declarations nil
  "Package declarations recorded while loading `neo-org-roam.el'.")

(defmacro neo/use-package (name &rest arguments)
  "Record package NAME and ARGUMENTS without configuring it."
  `(push (cons ',name ',arguments)
         neo--org-roam-test-package-declarations))

(load-file (expand-file-name "../neo-org-roam.el"
                             (file-name-directory
                              (or load-file-name buffer-file-name))))

(describe "neo-org-roam package ordering"
  (it "queues and demands simple-httpd before org-roam-ui"
    (let* ((declarations
            (reverse neo--org-roam-test-package-declarations))
           (names (mapcar #'car declarations))
           (simple-httpd-arguments
            (cdr (assq 'simple-httpd declarations)))
           (org-roam-ui-arguments
            (cdr (assq 'org-roam-ui declarations))))
      (expect (seq-position names 'simple-httpd)
              :to-be-less-than
              (seq-position names 'org-roam-ui))
      (expect (cadr (memq :demand simple-httpd-arguments)) :to-be t)
      (expect (cadr (memq :after org-roam-ui-arguments))
              :to-equal
              '(org-roam simple-httpd)))))

;;; test-neo-org-roam-packages.el ends here
