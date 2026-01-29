;;; -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'sqlite)

(require 'neo-utils)
(require 'neo-early-init-utils)
(require 'neo-workflow-db)
(require 'neo-workflow-models)
(require 'neo-workflow-slug)
(require 'neo-workflow-status)

(defconst neo--prs-schema
  '((prs [(number integer :primary-key)
          (title text)
          (author text)
          (base text)
          (head text)
          (mergeable integer)
          (status text)
          (ci text)
          (created_at text)
          (updated_at text)
          (url text)
          (issue_id text)])))

(defun neo--db-load-schemas-from-file (db file)
  "Load schemas from FILE and initialize them in DB."
  (let ((schemas (with-temp-buffer
                   (insert-file-contents file)
                   (eval (read (current-buffer)) t))))
    (neo/workflow-db-init db schemas)))

(defconst neo--simple-repo-issues
  '((:number 101 :title "First issue" :labels ("bug" "priority: high"))
    (:number 102 :title "Second issue" :labels ("feature" "docs"))
    (:number 103 :title "Third issue" :labels ("bug"))))

(defun neo--db-fixture-simple-repo (db)
  "Populate DB with the 'simple-owner/simple-repo' fixture."
  ;; 1. Insert repo and get ID
  (sqlite-execute db "INSERT INTO repositories (full_name, fork) VALUES (?, ?)" '("simple-owner/simple-repo" 0))
  (let ((repo-id (caar (sqlite-select db "SELECT id FROM repositories WHERE full_name = ?" '("simple-owner/simple-repo")))))
    ;; 2. Iterate through issues
    (dolist (issue-data neo--simple-repo-issues)
      (let ((issue-number (plist-get issue-data :number))
            (issue-title (plist-get issue-data :title))
            (labels (plist-get issue-data :labels)))
        ;; Insert issue and get ID
        (sqlite-execute db "INSERT INTO issues (repository_id, number, title, type, state, draft, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                        (list repo-id issue-number issue-title "Issue" "open" 0 "2024-01-01T00:00:00Z" "2024-01-01T00:00:00Z"))
        (let ((issue-id (caar (sqlite-select db "SELECT id FROM issues WHERE repository_id = ? AND number = ?" (list repo-id issue-number)))))
          ;; Handle labels
          (dolist (label-name labels)
            ;; Insert label if not exists
            (sqlite-execute db "INSERT OR IGNORE INTO labels (repository_id, name) VALUES (?, ?)" (list repo-id label-name))
            (let ((label-id (caar (sqlite-select db "SELECT id FROM labels WHERE repository_id = ? AND name = ?" (list repo-id label-name)))))
              ;; Link issue and label
              (sqlite-execute db "INSERT INTO issue_labels (issue_id, label_id) VALUES (?, ?)" (list issue-id label-id)))))))))

(defvar neo--db-conn nil
  "In-memory test DB connection.")

(defun neo--insert-stack (name branch-name issue-id)
  "Insert a branch and a stack, and return the stack's ID."
  (sqlite-execute neo--db-conn "INSERT OR IGNORE INTO branches (name) VALUES (?)" (list branch-name))
  (sqlite-execute neo--db-conn "INSERT INTO stacks (name, branch_name, issue_id) VALUES (?, ?, ?)" (list name branch-name issue-id))
  (caar (sqlite-select neo--db-conn "SELECT last_insert_rowid()")))

(defun neo--insert-pr (branch-name number status &optional ci)
  "Insert a PR for BRANCH-NAME.
This is a helper that only inserts a few columns into the 'prs' table,
assuming others are nullable."
  (sqlite-execute neo--db-conn "INSERT INTO prs (head, number, status, ci) VALUES (?, ?, ?, ?)"
                  (list branch-name number status ci)))

(describe "neo-workflow-db"
  (before-each
    ;; Set up a fresh in-memory test DB with the Neo Workflow schema.
    (setq neo--db-conn (sqlite-open))
    (neo--db-load-schemas-from-file neo--db-conn (expand-file-name "../github-schemas.el" default-directory))
    (neo--ensure-schema neo--db-conn)
    (neo/workflow-db-init neo--db-conn neo--prs-schema))

  (after-each
    ;; Close test DB connection.
    (when neo--db-conn
      (sqlite-close neo--db-conn))
    (setq neo--db-conn nil))

  (it "creates all necessary tables"
    (let* ((tables-result (sqlite-select neo--db-conn "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name ASC"))
           (tables (mapcar #'car tables-result))
           (expected-tables '("branches"
                              "contexts"
                              "issue_labels"
                              "issue_ui_state"
                              "issues"
                              "labels"
                              "prs"
                              "project_stacks"
                              "projects"
                              "repositories"
                              "repo_ui_state"
                              "stacks"
                              "sync_metadata")))
      (expect tables :to-have-same-items-as expected-tables)))

  (it "fetches all stacks"
    (neo--insert-stack "stack-a" "branch-a" "#1")
    (neo--insert-stack "stack-b" "branch-b" "#2")
    (let ((stacks (neo-db-get-all-stacks)))
      (expect (length stacks) :to-equal 2)
      (let ((stack-names (mapcar (lambda (s) (plist-get s :name)) stacks)))
        (expect stack-names :to-contain "stack-a")
        (expect stack-names :to-contain "stack-b"))))

  (it "fetches branch for a stack"
    (let ((stack-id (neo--insert-stack "stack-a" "branch-for-stack-a" "#1")))
      (expect (neo-db-get-branch-for-stack stack-id) :to-equal "branch-for-stack-a")))

  (it "fetches PR info for a branch"
    (neo--insert-stack "stack-a" "branch-1" "#1")
    (neo--insert-pr "branch-1" 42 "open")
    (let ((pr (neo-db-get-pr "branch-1")))
      (expect (plist-get pr :number) :to-equal 42)
      (expect (plist-get pr :status) :to-equal 'open)))

  (it "populates simple repo fixture"
    (neo--db-fixture-simple-repo neo--db-conn)
    (expect (caar (sqlite-select neo--db-conn "SELECT COUNT(*) FROM repositories")) :to-equal 1)
    (expect (caar (sqlite-select neo--db-conn "SELECT COUNT(*) FROM issues")) :to-equal (length neo--simple-repo-issues)))

  (it "starts work on an issue using neo--hack"
    ;; Setup: create a repo and an issue
    (neo--db-fixture-simple-repo neo--db-conn)

    ;; Get the issue to work on from the fixture
    (let* ((repo-id (caar (sqlite-select neo--db-conn "SELECT id FROM repositories WHERE full_name = ?" '("simple-owner/simple-repo"))))
           (issue-number 101)
           (issue-title "First issue")
           (issue-id (caar (sqlite-select neo--db-conn "SELECT id FROM issues WHERE repository_id = ? AND number = ?" (list repo-id issue-number))))
           (issue (neo-load-issue issue-id)))

      (expect issue :to-be-truthy)
      (expect (neo-issue-stack issue) :to-equal nil)

      ;; Mock git run to avoid file system errors
      (cl-letf (((symbol-function 'neo--workflow-git-run) (lambda (&rest args) t))
                ((symbol-function 'neo--workflow-git-current-branch-uncached) (lambda () "main"))
                ((symbol-function 'neo/workflow-git-branch-exists) (lambda (&rest args) nil))
                ((symbol-function 'neo/workflow-git-create-branch) (lambda (&rest args) t))
                ((symbol-function 'neo--workflow-choose-workspace-strategy) (lambda () 'repo)) ; use repo strategy to avoid worktree creation
                ((symbol-function 'make-directory) (lambda (&rest args) t))
                ((symbol-function 'persp-switch) (lambda (&rest args) t))
                ((symbol-function 'neo--ensure-stack-scratch) (lambda (&rest args) t))
                ((symbol-function 'neo--get-current-username) (lambda () "testuser"))
                ((symbol-function 'neo/workflow-refresh) (lambda (&rest args) t)))

        ;; Action: call neo--hack to create a stack and branch for the issue
        (neo--hack issue)

        ;; Assertions: verify the database changes
        (let* ((base-slug (neo-issue-title-to-slug issue-number issue-title))
               (expected-stack-name (format "testuser/%s" base-slug))
               (stack-row (car (sqlite-select neo--db-conn "SELECT id, name, title FROM stacks WHERE name = ?" (list expected-stack-name))))
               (stack-id (and stack-row (nth 0 stack-row))))

          ;; 1. Check that the stack was created correctly
          (expect stack-id :to-be-truthy)
          (expect (nth 1 stack-row) :to-equal expected-stack-name)
          (expect (nth 2 stack-row) :to-equal issue-title)

          ;; 2. Check that the branch was created and linked to the issue
          (let ((branch-row (car (sqlite-select neo--db-conn "SELECT name, issue_id FROM branches WHERE name = ?" (list expected-stack-name)))))
            (expect branch-row :to-be-truthy)
            (expect (nth 0 branch-row) :to-equal expected-stack-name)
            (expect (nth 1 branch-row) :to-equal issue-id))

          ;; 3. Check that the issue is now linked to the new stack
          (let ((updated-issue-stack-id (caar (sqlite-select neo--db-conn "SELECT stack_id FROM issues WHERE id = ?" (list issue-id)))))
            (expect updated-issue-stack-id :to-equal stack-id))))))

  (it "reuses existing stack when neo--hack is called twice"
    ;; Setup: create a repo and an issue
    (neo--db-fixture-simple-repo neo--db-conn)

    ;; Get the issue to work on from the fixture
    (let* ((repo-id (caar (sqlite-select neo--db-conn "SELECT id FROM repositories WHERE full_name = ?" '("simple-owner/simple-repo"))))
           (issue-number 101)
           (issue-title "First issue")
           (issue-id (caar (sqlite-select neo--db-conn "SELECT id FROM issues WHERE repository_id = ? AND number = ?" (list repo-id issue-number))))
           (issue (neo-load-issue issue-id)))

      (expect (neo-issue-stack issue) :to-equal nil)

      ;; Mock git run to avoid file system errors
      (cl-letf (((symbol-function 'neo--workflow-git-run) (lambda (&rest args) t))
                ((symbol-function 'neo--workflow-git-current-branch-uncached) (lambda () "main"))
                ((symbol-function 'neo/workflow-git-branch-exists) (lambda (&rest args) nil))
                ((symbol-function 'neo/workflow-git-create-branch) (lambda (&rest args) t))
                ((symbol-function 'neo--workflow-choose-workspace-strategy) (lambda () 'repo))
                ((symbol-function 'make-directory) (lambda (&rest args) t))
                ((symbol-function 'persp-switch) (lambda (&rest args) t))
                ((symbol-function 'neo--ensure-stack-scratch) (lambda (&rest args) t))
                ((symbol-function 'neo--get-current-username) (lambda () "testuser"))
                ((symbol-function 'neo/workflow-refresh) (lambda (&rest args) t)))

        ;; Action 1: Call neo--hack for the first time.
        (neo--hack issue)

        ;; Get the reloaded issue, which is now associated with a stack.
        (let* ((reloaded-issue (neo-load-issue issue-id))
               (base-slug (neo-issue-title-to-slug issue-number issue-title))
               (expected-stack-name (format "testuser/%s" base-slug)))
          (expect (neo-issue-stack reloaded-issue) :to-be-truthy)

          ;; Action 2: Call neo--hack for the second time on the reloaded issue.
          (neo--hack reloaded-issue)

          ;; Assertions: Verify that no new stack was created.
          (let ((stack-count (caar (sqlite-select neo--db-conn "SELECT COUNT(*) FROM stacks WHERE name = ?" (list expected-stack-name)))))
            (expect stack-count :to-equal 1))))))

  (it "inserts and replaces a full PR"
    (neo-db-insert-pr 1 "title" "author" "base" "head" 1 "open" "success" "t1" "t2" "url" "101")
    (let ((row (car (sqlite-select neo--db-conn "SELECT * FROM prs WHERE number = 1"))))
      (expect (nth 1 row) :to-equal "title"))
    ;; Test replace
    (neo-db-insert-pr 1 "new-title" "author" "base" "head" 1 "open" "success" "t1" "t2" "url" "101")
    (let ((row (car (sqlite-select neo--db-conn "SELECT * FROM prs WHERE number = 1"))))
      (expect (nth 1 row) :to-equal "new-title")))

(it "inserts and replaces a branch"
  (neo-db-insert-branch "b1" 1 101 nil "open" "success" "abc" "/tmp/w")
  (let ((row (car (sqlite-select neo--db-conn "SELECT * FROM branches WHERE name = 'b1'"))))
    (expect (nth 2 row) :to-equal 101))
  ;; Test replace
  (neo-db-insert-branch "b1" 1 102 nil "closed" "failure" "def" "/tmp/x")
  (let ((row (car (sqlite-select neo--db-conn "SELECT * FROM branches WHERE name = 'b1'"))))
    (expect (nth 2 row) :to-equal 102)))

(it "inserts and updates a stack on conflict"
  (let ((repo-id 1))
    ;; Insert dummy repo to satisfy foreign key constraint
    (sqlite-execute neo--db-conn "INSERT INTO repositories (id, full_name, fork) VALUES (?, ?, ?)" (list repo-id "test/repo" 0))
    (sqlite-execute neo--db-conn "INSERT INTO branches (name, repository_id) VALUES (?, ?)" (list "b1" repo-id))
    (neo-db-insert-stack "stack1" "b1" "101" repo-id "title1")
    (let ((row (car (sqlite-select neo--db-conn "SELECT name, title FROM stacks WHERE branch_name = ? AND repository_id = ?" (list "b1" repo-id)))))
      (expect (nth 0 row) :to-equal "stack1")
      (expect (nth 1 row) :to-equal "title1"))
    ;; Test update on conflict
    (neo-db-insert-stack "stack-updated" "b1" "102" repo-id "title2")
    (let ((row (car (sqlite-select neo--db-conn "SELECT name, title FROM stacks WHERE branch_name = ? AND repository_id = ?" (list "b1" repo-id)))))
      (expect (nth 0 row) :to-equal "stack-updated")
      (expect (nth 1 row) :to-equal "title2"))))

(it "inserts a project and associates a stack"
  (neo-db-insert-project "proj1" "repo1" "repo" nil nil)
  (expect (caar (sqlite-select neo--db-conn "SELECT COUNT(*) FROM projects")) :to-equal 1)
  (neo-db-insert-project-stack "proj1" "stack1")
  (let ((row (car (sqlite-select neo--db-conn "SELECT * FROM project_stacks"))))
    (expect (nth 0 row) :to-equal "proj1")
    (expect (nth 1 row) :to-equal "stack1")))

(it "sets and gets repository UI state"
  (neo--db-fixture-simple-repo neo--db-conn)
  (let ((repo-id (caar (sqlite-select neo--db-conn "SELECT id FROM repositories"))))
    (expect (neo-db-get-repo-ui-state repo-id) :to-equal nil)
    (neo-db-set-repo-ui-state repo-id "expanded")
    (let ((state (neo-db-get-repo-ui-state repo-id)))
      (expect (plist-get state :state) :to-equal "expanded"))
    (neo-db-set-repo-ui-state repo-id "collapsed")
    (let ((state (neo-db-get-repo-ui-state repo-id)))
      (expect (plist-get state :state) :to-equal "collapsed"))))

(it "sets and gets issue UI state"
  (neo--db-fixture-simple-repo neo--db-conn)
  (let ((issue-id (caar (sqlite-select neo--db-conn "SELECT id FROM issues"))))
    (expect (neo-db-get-issue-ui-state issue-id) :to-equal nil)
    (neo-db-set-issue-ui-state issue-id "expanded")
    (expect (neo-db-get-issue-ui-state issue-id) :to-equal "expanded")
    (neo-db-set-issue-ui-state issue-id "collapsed")
    (expect (neo-db-get-issue-ui-state issue-id) :to-equal "collapsed")))

(it "fetches stacks for a specific repository"
  (neo--db-fixture-simple-repo neo--db-conn)
  (let ((repo-id (caar (sqlite-select neo--db-conn "SELECT id FROM repositories"))))
    (sqlite-execute neo--db-conn "INSERT INTO repositories (id, full_name, fork) VALUES (99, 'other/repo', 0)")
    (sqlite-execute neo--db-conn "INSERT INTO branches (name) VALUES ('b1'), ('b2'), ('b3')")
    (neo-db-insert-stack "s1" "b1" "1" repo-id "t1")
    (neo-db-insert-stack "s2" "b2" "2" repo-id "t2")
    (neo-db-insert-stack "s3" "b3" "3" 99 "t3")
    (let* ((stacks (neo-db-get-stacks-for-repo repo-id))
           (stack-names (mapcar #'neo-stack-name stacks)))
      (expect (length stacks) :to-equal 2)
      (expect stack-names :to-have-same-items-as '("s1" "s2")))))

(it "calculates a stack summary"
  (let* ((stack-id (neo--insert-stack "summary-stack" "summary-branch" "issue-123")))
    ;; Case 1: Open PR, CI success
    (neo--insert-pr "summary-branch" 1 "open" "success")
    (let ((summary (neo-stack-summary stack-id)))
      (expect (plist-get summary :branches) :to-equal 1)
      (expect (plist-get summary :open-prs) :to-equal 1)
      (expect (plist-get summary :merged) :to-equal 0)
      (expect (plist-get summary :ci-passed) :to-equal 1))
    ;; Case 2: Merged PR, CI failure
    (sqlite-execute neo--db-conn "UPDATE prs SET status='merged', ci='failure' WHERE head='summary-branch'")
    (let ((summary (neo-stack-summary stack-id)))
      (expect (plist-get summary :open-prs) :to-equal 0)
      (expect (plist-get summary :merged) :to-equal 1)
      (expect (plist-get summary :ci-passed) :to-equal 0)
      (expect (plist-get summary :ci-failed) :to-equal 1))))

(it "fetches an issue for a branch"
  (neo--db-fixture-simple-repo neo--db-conn)
  (let* ((repo-id (caar (sqlite-select neo--db-conn "SELECT id FROM repositories")))
         (issue-number 101)
         (issue-id (caar (sqlite-select neo--db-conn "SELECT id FROM issues WHERE repository_id = ? AND number = ?" (list repo-id issue-number))))
         (branch-name "branch-for-issue-101"))
    (neo-db-insert-branch branch-name repo-id nil issue-id nil nil nil nil)
    (expect (neo-db-get-issue branch-name) :to-equal "#101"))))
