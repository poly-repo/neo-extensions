;;; neo-workflow-db-tests.el --- Tests for Neo Workflow DB layer -*- lexical-binding: t; -*-

(require 'ert)
(require 'sqlite)
(require 'neo-workflow-db)

(defvar neo--db-conn nil
  "In-memory test DB connection.")

;; TODO for now ok, but we shouldn't destroy an existing connection
(defun neo--setup-test-db ()
  "Set up a fresh in-memory test DB with the Neo Workflow schema."
  (setq neo--db-conn (sqlite-open))
  (neo--ensure-schema neo--db-conn))

(defun neo--teardown-test-db ()
  "Close test DB connection."
  (when neo--db-conn
    (sqlite-close neo--db-conn))
  (setq neo--db-conn nil))

;;;###autoload
(defun neo--insert-stack (name branch issue)
  "Insert stack NAME and return its ID."
  (sqlite-execute neo--db-conn "INSERT INTO stacks (name, branch_name, issue_id) VALUES (?, ?, ?)" (list name branch issue)) ;TODO hack need real values
  (caar (sqlite-select neo--db-conn "SELECT last_insert_rowid()")))


;;;###autoload
(defun neo--insert-branch (stack-id name)
  "Insert branch NAME for STACK-ID and return branch ID."
  (sqlite-execute neo--db-conn "INSERT INTO branches (stack_id, name) VALUES (?, ?)" (list stack-id name))
  (caar (sqlite-select neo--db-conn "SELECT id FROM branches WHERE name = ?" (list name))))

;;;###autoload
(defun neo--insert-pr (branch-id number status)
  "Insert PR info for BRANCH-ID."
  (sqlite-execute neo--db-conn "INSERT INTO prs (branch_id, number, status) VALUES (?, ?, ?)"
                  (list branch-id number status)))

;;;###autoload
(defun neo--insert-ci (branch-id status)
  "Insert CI status for BRANCH-ID."
  (sqlite-execute neo--db-conn "INSERT INTO ci (branch_id, status) VALUES (?, ?)"
                  (list branch-id status)))

;;;###autoload
(defun neo--insert-worktree (branch-id path)
  "Insert worktree PATH for BRANCH-ID."
  (sqlite-execute neo--db-conn "INSERT INTO worktrees (branch_id, path) VALUES (?, ?)"
                  (list branch-id path)))

;; -------------------------------
;; Tests
;; -------------------------------

(ert-deftest neo-db-smoke-test ()
  "Smoke test."
  (neo--setup-test-db)
  (neo--teardown-test-db))

(ert-deftest neo-db-get-all-stacks-test ()
  "Test fetching all stacks."
  (neo--setup-test-db)
  (let ((id1 (neo--insert-stack "stack-a" "branch-a" "#1"))
        (id2 (neo--insert-stack "stack-b" "branch-b" "#2")))
    (should (= (length (neo-db-get-all-stacks)) 2))
    (should (= id1 1))
    (should (= id2 2))
    (should (member "stack-a" (mapcar (lambda (s) (plist-get s :name)) (neo-db-get-all-stacks))))
    (should (member "stack-b" (mapcar (lambda (s) (plist-get s :name)) (neo-db-get-all-stacks)))))
  (neo--teardown-test-db))

(ert-deftest neo-db-get-branch-for-stack-test ()
  "Test fetching branch for a stack."
  (neo--setup-test-db)
  (sqlite-execute neo--db-conn "INSERT INTO branches (name) VALUES ('branch-a')")
  (let ((stack-id (neo--insert-stack "stack-a" "branch-a" "#1")))
    (should (string= "branch-a" (neo-db-get-branch-for-stack stack-id))))
  (neo--teardown-test-db))

;; (ert-deftest neo-db-get-pr-test ()
;;   "Test fetching PR info for a branch."
;;   (neo--setup-test-db)
;;   (let* ((stack-id (neo--insert-stack "stack-a"))
;;          (branch-id (neo--insert-branch stack-id "branch-1")))
;;     (neo--insert-pr branch-id 42 "open")
;;     (let ((pr (neo-db-get-pr branch-id)))
;;       (should (= (plist-get pr :number) 42))
;;       (should (string= (plist-get pr :status) "open"))))
;;   (neo--teardown-test-db))

;; (ert-deftest neo-db-get-ci-test ()
;;   "Test fetching CI info for a branch."
;;   (neo--setup-test-db)
;;   (let* ((stack-id (neo--insert-stack "stack-a"))
;;          (branch-id (neo--insert-branch stack-id "branch-1")))
;;     (neo--insert-ci branch-id "success")
;;     (let ((ci (neo-db-get-ci branch-id)))
;;       (should (string= ci "success"))))
;;   (neo--teardown-test-db))

;; (ert-deftest neo-db-get-worktree-test ()
;;   "Test fetching worktree path for a branch."
;;   (neo--setup-test-db)
;;   (let* ((stack-id (neo--insert-stack "stack-a"))
;;          (branch-id (neo--insert-branch stack-id "branch-1")))
;;     (neo--insert-worktree branch-id "/tmp/wt1")
;;     (let ((wt (neo-db-get-worktree branch-id)))
;;       (should (string= wt "/tmp/wt1"))))
;;   (neo--teardown-test-db))

;; (ert-deftest neo-stack-summary-test ()
;;   "Test stack-level summary aggregation."
;;   (neo--setup-test-db)
;;   (let* ((stack-id (neo--insert-stack "stack-a"))
;;          (b1 (neo--insert-branch stack-id "b1"))
;;          (b2 (neo--insert-branch stack-id "b2")))
;;     (neo--insert-pr b1 1 "open")
;;     (neo--insert-pr b2 2 "merged")
;;     (neo--insert-ci b1 "success")
;;     (neo--insert-ci b2 "failure")
;;     (let ((summary (neo-stack-summary stack-id)))
;;       (should (= (plist-get summary :branches) 2))
;;       (should (= (plist-get summary :open-prs) 1))
;;       (should (= (plist-get summary :merged) 1))
;;       (should (= (plist-get summary :ci-passed) 1))
;;       (should (= (plist-get summary :ci-failed) 1))))
;;   (neo--teardown-test-db))

(provide 'neo-workflow-db-tests)
