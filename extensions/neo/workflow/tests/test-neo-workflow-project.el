;;; tests/test-neo-workflow-project.el --- Tests for neo-workflow-project -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'neo-workflow-project)
(require 'neo-workflow-db)
(require 'project)

(describe "neo-workflow-project"

  (defvar neo--test-project-db-file nil)

  (before-each
    (setq neo--test-project-db-file (make-temp-file "neo-test-db-project" nil ".sqlite"))
    (setq neo--workflow-db-file neo--test-project-db-file)
    (setq neo--db-conn (sqlite-open neo--workflow-db-file))
    (neo--ensure-schema neo--db-conn))

  (after-each
    (when neo--db-conn (sqlite-close neo--db-conn))
    (setq neo--db-conn nil)
    (when (and neo--test-project-db-file (file-exists-p neo--test-project-db-file))
      (delete-file neo--test-project-db-file)))

  (describe "neo-db-get-all-project-paths"
    (it "returns paths from projects table"
      (neo-db-insert-project "p1" "repo1" "git" nil "/path/to/p1")
      (neo-db-insert-project "p2" "repo2" "git" nil "/path/to/p2")
      (expect (neo-db-get-all-project-paths) :to-contain "/path/to/p1")
      (expect (neo-db-get-all-project-paths) :to-contain "/path/to/p2")
      (expect (length (neo-db-get-all-project-paths)) :to-equal 2))

    (it "returns paths from branches table"
      (neo-db-insert-branch "b1" 1 nil nil nil nil nil "/path/to/b1")
      (neo-db-insert-branch "b2" 1 nil nil nil nil nil "/path/to/b2")
      (expect (neo-db-get-all-project-paths) :to-contain "/path/to/b1")
      (expect (neo-db-get-all-project-paths) :to-contain "/path/to/b2")
      (expect (length (neo-db-get-all-project-paths)) :to-equal 2))

    (it "merges and deduplicates paths"
      (neo-db-insert-project "p1" "repo1" "git" nil "/common/path")
      (neo-db-insert-branch "b1" 1 nil nil nil nil nil "/common/path")
      (neo-db-insert-branch "b2" 1 nil nil nil nil nil "/unique/path")
      (let ((paths (neo-db-get-all-project-paths)))
        (expect paths :to-contain "/common/path")
        (expect paths :to-contain "/unique/path")
        (expect (length paths) :to-equal 2))))

  (describe "neo-workflow-project-backend"
    (it "identifies a project root"
      (neo-db-insert-project "p1" "repo1" "git" nil "/path/to/p1")
      ;; Mocking expand-file-name to behave predictably in test environment if needed
      ;; But here we use absolute paths so it should be fine.
      
      (let ((backend-result (neo-workflow-project-backend "/path/to/p1/subdir/file.el")))
        (expect backend-result :to-be-truthy)
        (expect (car backend-result) :to-equal 'neo-workflow)
        (expect (cdr backend-result) :to-equal "/path/to/p1")))

    (it "returns nil for unknown paths"
      (neo-db-insert-project "p1" "repo1" "git" nil "/path/to/p1")
      (let ((backend-result (neo-workflow-project-backend "/other/path/file.el")))
        (expect backend-result :to-equal nil)))))
