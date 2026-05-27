;;; beads-client-test.el --- Tests for beads-client -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'beads-client)

(defun beads-client-test--without-beads-env ()
  "Return `process-environment' without Beads workspace overrides."
  (seq-remove
   (lambda (entry)
     (or (string-prefix-p "BEADS_DIR=" entry)
         (string-prefix-p "BEADS_DB=" entry)))
   process-environment))

(ert-deftest beads-client/find-database-uses-cli-workspace-for-beads-dir ()
  (let* ((workspace-root (make-temp-file "beads-workspace" t))
         (beads-dir (expand-file-name ".beads" workspace-root))
         (db-path (expand-file-name "embeddeddolt" beads-dir))
         (process-environment
          (cons (format "BEADS_DIR=%s" beads-dir)
                (beads-client-test--without-beads-env))))
    (make-directory db-path t)
    (unwind-protect
        (progn
          (beads-client-clear-cache)
          (cl-letf (((symbol-function 'beads-client--workspace-info-from-cli)
                     (lambda (&optional _directory)
                       `((path . ,beads-dir)
                         (database_path . ,db-path))))
                    ((symbol-function 'beads-backend-socket-name-for-project)
                     (lambda ()
                       "bd.sock")))
            (should (equal (beads-client--find-database) db-path))
            (should (equal (beads-client--project-root)
                           (file-name-as-directory workspace-root)))
            (should (equal (beads-client--socket-path)
                           (expand-file-name "bd.sock" beads-dir)))))
      (beads-client-clear-cache)
      (delete-directory workspace-root t))))

(ert-deftest beads-client/find-database-falls-back-to-legacy-beads-db ()
  (let* ((workspace-root (make-temp-file "beads-workspace" t))
         (default-directory (expand-file-name "src" workspace-root))
         (beads-dir (expand-file-name ".beads" workspace-root))
         (db-path (expand-file-name "beads.db" beads-dir))
         (process-environment (beads-client-test--without-beads-env)))
    (make-directory default-directory t)
    (make-directory beads-dir t)
    (write-region "" nil db-path nil 'silent)
    (unwind-protect
        (progn
          (beads-client-clear-cache)
          (cl-letf (((symbol-function 'beads-client--workspace-info-from-cli)
                     (lambda (&optional _directory)
                       nil)))
            (should (equal (beads-client--find-database) db-path))
            (should (equal (beads-client--project-root)
                           (file-name-as-directory workspace-root)))))
      (beads-client-clear-cache)
      (delete-directory workspace-root t))))

(provide 'beads-client-test)
;;; beads-client-test.el ends here
