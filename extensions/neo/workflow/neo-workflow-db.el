;;; neo-workflow-db.el --- Neo Workflow SQLite DB layer -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'sqlite)
(require 'github-models)
;(require 'neo-workflow-models)

;;;###autoload
(defvar neo--workflow-db-file (neo/data-file-path "workflow.sqlite")
  "Path to the Neo Workflow SQLite database file.")

(defvar neo--db-conn nil
  "SQLite connection to the Neo Workflow SQLite database.")

(defun neo-open-db ()
  "Open the Neo Workflow database, creating it if necessary."
  (unless (and neo--db-conn (sqlitep neo--db-conn))
    (setq neo--db-conn (sqlite-open neo--workflow-db-file)))
  (neo--ensure-schema neo--db-conn)
  neo--db-conn)

(defconst neo--workflow-db-schemas
  '(
    (branches [(name text)
               (repository_id integer)
	       (pr_number integer)
	       (issue_id integer)
	       (status text)
	       (ci_status text)
	       (last_commit text)
	       (worktree_path text)]
            (:primary-key name repository_id)
            (:foreign-key (issue_id) (issues id))
            (:foreign-key (repository_id) (repositories id)))

    (prs [(number integer)
          (title text)
          (author text)
          (base text)
          (head text)
          (mergeable text)
          (status text)
          (ci text)
          (created text)
          (updated text)
          (url text)
          (issue_id integer)]
         (:primary-key issue_id)
         (:foreign-key (issue_id) (issues id)))

    (stacks [(id integer :primary-key)
	     (name text)
             (title text)
	     (branch_name text)
	     (repository_id integer)
	     (issue_id text)
             (parent_stack_id integer)]
            (:foreign-key (repository_id) (repositories id))
            (:foreign-key (parent_stack_id) (stacks id))
            (:unique branch_name repository_id))

    (contexts [(repository_id integer)
               (stack_id integer)
               (perspective text)]
              (:primary-key repository_id stack_id)
              (:foreign-key (repository_id) (repositories id))
              (:foreign-key (stack_id) (stacks id)))
    
    (projects
     [
      (id text :primary-key)
      (repo text)
      (type text)
      (pr_number integer)
      (worktree_path text)])
    
    (project_stacks
     [
      (project_id text)
      (stack_name text)])

    (repo_ui_state
     [
      (repo_id integer :primary-key)
      (state text :not-null)
      (filter text)
      (order text)
     ]
     (:foreign-key (repo_id) (repositories id)))

    (issue_ui_state
     [
      (issue_id integer :primary-key)
      (state text :not-null)
     ]
     (:foreign-key (issue_id) (issues id)))

    (workflow_ui_state
     [
      (key text :primary-key)
      (value text)
     ])
    )
  "Workflow-related tables.")

;; TODO naming init should call ensure schemas, names are swapped
(defun neo--ensure-schema (db)
  "Ensure that the database DB has all required tables."
  (let ((tables (mapcar #'car (sqlite-select db "SELECT name FROM sqlite_master WHERE type='table'"))))
    (unless (member "repo_ui_state" tables)
      (neo/workflow-db-init db neo--workflow-db-schemas)) ; other tables are from ghsync

    (unless (member "workflow_ui_state" tables)
      (neo/workflow-db-init db (list (assoc 'workflow_ui_state neo--workflow-db-schemas))))

    (unless (member "prs" tables)
      (neo/workflow-db-init db (list (assoc 'prs neo--workflow-db-schemas))))

    (unless (member "projects" tables)
      (neo/workflow-db-init db (list (assoc 'projects neo--workflow-db-schemas))))

    (unless (member "contexts" tables)
      (neo/workflow-db-init db (list (assoc 'contexts neo--workflow-db-schemas))))

    (when (member "repo_ui_state" tables)
      (let* ((cols (sqlite-select db "PRAGMA table_info(repo_ui_state)"))
             (col-names (mapcar (lambda (row) (nth 1 row)) cols)))
        (unless (member "filter" col-names)
          (sqlite-execute db "ALTER TABLE repo_ui_state ADD COLUMN filter TEXT"))
        (unless (member "order" col-names)
          (sqlite-execute db "ALTER TABLE repo_ui_state ADD COLUMN `order` TEXT"))))

    ;; poor man's migrations for ghsync tables
    (when (member "issues" tables)
      (let* ((cols (sqlite-select db "PRAGMA table_info(issues)"))
             (col-names (mapcar (lambda (row) (nth 1 row)) cols)))
        (unless (member "stack_id" col-names)
          (sqlite-execute db "ALTER TABLE issues ADD COLUMN stack_id INTEGER REFERENCES stacks(id)"))))

    ;; HACK: poor man's migrations for stacks table
    (when (member "stacks" tables)
      (let* ((cols (sqlite-select db "PRAGMA table_info(stacks)"))
             (col-names (mapcar (lambda (row) (nth 1 row)) cols))
             (needs-recreate nil))

        ;; Check 1: Missing repository_id (New migration check)
        (unless (member "repository_id" col-names)
          (setq needs-recreate t)
          (message "Recreating 'stacks' table due to outdated schema (missing repository_id)..."))

        (when needs-recreate
          (sqlite-execute db "DROP TABLE stacks")
          (neo/workflow-db-init db (list (assoc 'stacks neo--workflow-db-schemas))))

        ;; If we didn't recreate, run existing ALTER TABLE migrations
        (unless needs-recreate
          (unless (member "title" col-names)
            (sqlite-execute db "ALTER TABLE stacks ADD COLUMN title TEXT"))
          (unless (member "parent_stack_id" col-names)
            (sqlite-execute db "ALTER TABLE stacks ADD COLUMN parent_stack_id INTEGER REFERENCES stacks(id)")))))

    ;; HACK: poor man's migration for branches table
    (when (member "branches" tables)
      (let* ((cols (sqlite-select db "PRAGMA table_info(branches)"))
             (col-names (mapcar (lambda (row) (nth 1 row)) cols))
             (issue-id-col (seq-find (lambda (row) (string= (nth 1 row) "issue_id")) cols))
             (needs-recreate nil))

        ;; Check 1: Missing repository_id
        (unless (member "repository_id" col-names)
          (setq needs-recreate t)
          (message "Recreating 'branches' table due to outdated schema (missing repository_id)..."))

        ;; Check 2: issue_id type mismatch (only if not already flagged for recreate)
        (when (and (not needs-recreate) issue-id-col (string-equal (nth 2 issue-id-col) "TEXT"))
          (setq needs-recreate t)
          (message "Recreating 'branches' table due to outdated schema (issue_id TEXT -> INTEGER)..."))

        (when needs-recreate
          (sqlite-execute db "DROP TABLE branches")
          (neo/workflow-db-init db (list (assoc 'branches neo--workflow-db-schemas))))))))


(defun neo--column-spec-to-sql (spec)
  "Convert a column spec list to an SQL string."
  (let* ((name (symbol-name (car spec)))
         (type (symbol-name (cadr spec)))
         (constraints (copy-sequence (cddr spec)))
         (sql-constraints '()))
    (while constraints
      (let ((c (pop constraints)))
        (pcase c
          (:primary-key (push "PRIMARY KEY" sql-constraints))
          (:not-null (push "NOT NULL" sql-constraints))
          (:unique (push "UNIQUE" sql-constraints))
          (:default (push (format "DEFAULT %s" (pop constraints)) sql-constraints)))))
    (string-join (cons (format "`%s` %s" name (upcase type)) (nreverse sql-constraints)) " ")))

(defun neo--table-constraint-to-sql (constraint)
  "Convert a table constraint list to an SQL string."
  (pcase constraint
    (`(:primary-key . ,cols)
     (format "PRIMARY KEY (%s)" (string-join (mapcar #'symbol-name cols) ", ")))
    (`(:foreign-key ,from-cols (,ref-table . ,ref-cols))
     (format "FOREIGN KEY (%s) REFERENCES `%s`(%s)"
             (string-join (mapcar #'symbol-name from-cols) ", ")
             (symbol-name ref-table)
             (string-join (mapcar #'symbol-name ref-cols) ", ")))
    (`(:unique . ,cols)
     (format "UNIQUE (%s)" (string-join (mapcar #'symbol-name cols) ", ")))
    (_ nil)))

(defun neo--schema-to-sql (table-name schema constraints)
  "Convert an emacsql-style schema to a CREATE TABLE SQL string."
  (let ((columns (mapcar #'neo--column-spec-to-sql schema))
        (table-constraints (delq nil (mapcar #'neo--table-constraint-to-sql constraints))))
    (format "CREATE TABLE IF NOT EXISTS `%s` (%s);"
            (symbol-name table-name)
            (string-join (append columns table-constraints) ", "))))

(defun neo/workflow-db-init (db schemas)
  "Initialize database DB with the correct schema."
  (with-sqlite-transaction db
    (dolist (schema-def schemas)
      (let* ((table-name (car schema-def))
	     (schema (cadr schema-def))
	     (constraints (cddr schema-def))
	     (sql (neo--schema-to-sql table-name schema constraints)))
	(sqlite-execute db sql)))))

;;;###autoload
(defun neo-db-upsert-issue (issue)
  "Upsert a `neo-issue' struct into the database, including its labels and stack association."
  (let ((db (neo-open-db)))
    (with-sqlite-transaction db
      (let* ((stack (neo-issue-stack issue))
             (issue-id (neo-issue-id issue))
             (repo-id (neo-issue-repository-id issue))
             (stack-id nil))

        ;; 1. Handle Stack Association
        (when stack
          (let ((stack-name (neo-stack-name stack)))
            ;; Ensure stack exists/is updated using the new function
            (neo-db-insert-stack stack-name stack-name issue-id repo-id (neo-stack-title stack))
            
            ;; Ensure corresponding branch exists/is updated
            (neo-db-insert-branch stack-name repo-id nil issue-id nil nil nil nil)
            
            ;; Retrieve stack ID using composite key
            (setq stack-id
                  (caar (sqlite-select db "SELECT id FROM stacks WHERE name = ? AND repository_id = ?" (list stack-name repo-id))))))

        ;; 2. Upsert the issue's core data
        (sqlite-execute db
                        "INSERT INTO issues (id, number, title, type, state, draft, created_at, updated_at, closed_at, merged_at, repository_id, stack_id)
                         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                         ON CONFLICT(id) DO UPDATE SET
                           number=excluded.number, title=excluded.title, type=excluded.type,
                           state=excluded.state, draft=excluded.draft, created_at=excluded.created_at,
                           updated_at=excluded.updated_at, closed_at=excluded.closed_at,
                           merged_at=excluded.merged_at, repository_id=excluded.repository_id,
                           stack_id=excluded.stack_id"
                        (list issue-id
                              (neo-issue-number issue)
                              (neo-issue-title issue)
                              (neo-issue-type issue)
                              (symbol-name (neo-issue-state issue))
                              (neo-issue-draft issue)
                              (neo-issue-created-at issue)
                              (neo-issue-updated-at issue)
                              (neo-issue-closed-at issue)
                              (neo-issue-merged-at issue)
                              repo-id
                              stack-id))

        ;; 3. Update Labels
        (when issue-id
          ;; a. Delete old label associations
          (sqlite-execute db "DELETE FROM issue_labels WHERE issue_id = ?" (list issue-id))

          ;; b. Loop through labels, upsert label, and create new association
          (dolist (label (neo-issue-labels issue))
            (let ((label-id (neo-label-id label))
                  (label-name (neo-label-name label))
                  (label-color (neo-label-color label))
                  (label-desc (neo-label-description label))
                  (label-repo-id (neo-label-repository-id label)))

              ;; i. Upsert the label (using ID as primary key)
              (sqlite-execute db
                              "INSERT INTO labels (id, name, color, description, repository_id)
                               VALUES (?, ?, ?, ?, ?)
                               ON CONFLICT(id) DO UPDATE SET
                                 name=excluded.name, color=excluded.color,
                                 description=excluded.description, repository_id=excluded.repository_id"
                              (list label-id label-name label-color label-desc label-repo-id))

              ;; ii. Create the new association
              (sqlite-execute db "INSERT INTO issue_labels (issue_id, label_id) VALUES (?, ?)"
                              (list issue-id label-id)))))))))

;;;###autoload
(defun neo-db-insert-pr (number title author base head mergeable status ci created updated url issue-id)
  "Insert or replace a PR."
  (sqlite-execute (neo-open-db)
                  "INSERT OR REPLACE INTO prs VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                  (list number title author base head mergeable status ci created updated url issue-id)))

;;;###autoload
(defun neo-db-insert-branch (name repository-id pr-number issue-id status ci last-commit worktree)
  "Insert or replace a branch."
  (sqlite-execute (neo-open-db)
                  "INSERT OR REPLACE INTO branches VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                  (list name repository-id pr-number issue-id status ci last-commit worktree)))

;;;###autoload
(defun neo-db-insert-stack (name branch-name issue-id repository-id &optional title parent-stack-id)
  "Insert or replace a stack."
  (sqlite-execute (neo-open-db)
                  "INSERT INTO stacks (name, branch_name, issue_id, repository_id, title, parent_stack_id) VALUES (?, ?, ?, ?, ?, ?)
ON CONFLICT(branch_name, repository_id) DO UPDATE SET name=excluded.name, issue_id=excluded.issue_id, title=excluded.title, parent_stack_id=excluded.parent_stack_id"
                  (list name branch-name issue-id repository-id title parent-stack-id)))

;;;###autoload
(defun neo-db-insert-project (id repo type &optional pr-number worktree)
  "Insert or replace a project."
  (sqlite-execute (neo-open-db)
                  "INSERT OR REPLACE INTO projects VALUES (?, ?, ?, ?, ?)"
                  (list id repo type pr-number worktree)))

;;;###autoload
(defun neo-db-insert-project-stack (project stack)
  "Associate a stack with a project."
  (sqlite-execute (neo-open-db)
                  "INSERT OR REPLACE INTO project_stacks VALUES (?, ?)"
                  (list project stack)))


;; -------------------------------------------------------------------
;; UI State API
;; -------------------------------------------------------------------

;;;###autoload
(defun neo-db-set-repo-ui-state (repo-id state &optional filter order)
  "Set/update the UI state for a given repository."
  (sqlite-execute (neo-open-db)
                  "INSERT INTO repo_ui_state (repo_id, state, filter, `order`) VALUES (?, ?, ?, ?)
ON CONFLICT(repo_id) DO UPDATE SET state=excluded.state, filter=excluded.filter, `order`=excluded.`order`"
                  (list repo-id state filter order)))

;;;###autoload
(defun neo-db-get-repo-ui-state (repo-id)
  "Get the UI state for a given repository.
Returns a plist (:state 'collapsed|'expanded :filter 'open|'closed|... :order 'priority|...)."
  (let ((row (car (sqlite-select (neo-open-db)
                                 "SELECT state, filter, `order` FROM repo_ui_state WHERE repo_id = ?"
                                 (list repo-id)))))
    (when row
      (list :state (nth 0 row)
            :filter (nth 1 row)
            :order (nth 2 row)))))

;;;###autoload
(defun neo-db-set-issue-ui-state (issue-id state)
  "Set/update the UI state for a given issue."
  (sqlite-execute (neo-open-db)
                  "INSERT INTO issue_ui_state (issue_id, state) VALUES (?, ?)
ON CONFLICT(issue_id) DO UPDATE SET state=excluded.state"
                  (list issue-id state)))

;;;###autoload
(defun neo-db-get-issue-ui-state (issue-id)
  "Get the UI state for a given issue. Returns state string or nil."
  (caar (sqlite-select (neo-open-db)
                       "SELECT state FROM issue_ui_state WHERE issue_id = ?"
                       (list issue-id))))

;;;###autoload
(defun neo-db-set-workflow-filter (filter)
  "Set/update the global workflow filter."
  (sqlite-execute (neo-open-db)
                  "INSERT INTO workflow_ui_state (key, value) VALUES ('filter', ?)
ON CONFLICT(key) DO UPDATE SET value=excluded.value"
                  (list filter)))

;;;###autoload
(defun neo-db-get-workflow-filter ()
  "Get the global workflow filter. Returns filter string or nil."
  (caar (sqlite-select (neo-open-db)
                       "SELECT value FROM workflow_ui_state WHERE key = 'filter'")))


;;;###autoload
(defun neo/workflow-db-upsert-context (repo-id stack-id perspective)
  "Upsert a context mapping for REPO-ID and STACK-ID.
Schema: (repository_id, stack_id, perspective) PK(repository_id, stack_id)"
  (sqlite-execute (neo-open-db)
                  "INSERT INTO contexts (repository_id, stack_id, perspective) VALUES (?, ?, ?)
                   ON CONFLICT(repository_id, stack_id) DO UPDATE SET perspective=excluded.perspective"
                  (list repo-id stack-id perspective)))

;;;###autoload
(defun neo/workflow-db-get-context (repo-id)
  "Get the workflow context for REPO-ID.
Returns a plist (:repository-id ... :stack-id ... :perspective ...)."
  (let ((row (car (sqlite-select (neo-open-db)
                                 "SELECT repository_id, stack_id, perspective FROM contexts WHERE repository_id = ?"
                                 (list repo-id)))))
    (when row
      (list :repository-id (nth 0 row)
            :stack-id (nth 1 row)
            :perspective (nth 2 row)))))

;;;###autoload
(defun neo/workflow-db-get-context-by-stack (repo-id stack-id)
  "Get the workflow context for REPO-ID and STACK-ID.
Returns a plist (:repository-id ... :stack-id ... :perspective ...)."
  (let ((row (car (sqlite-select (neo-open-db)
                                 "SELECT repository_id, stack_id, perspective FROM contexts WHERE repository_id = ? AND stack_id = ?"
                                 (list repo-id stack-id)))))
    (when row
      (list :repository-id (nth 0 row)
            :stack-id (nth 1 row)
            :perspective (nth 2 row)))))

;; -------------------------------------------------------------------
;; Stack API
;; -------------------------------------------------------------------

(defun neo-db-get-all-stacks ()
  "Return a list of stack plists with their IDs, names, repository_ids and titles."
  (mapcar (lambda (row)
            (list :id (nth 0 row) :name (nth 1 row) :repository-id (nth 2 row) :title (nth 3 row)))
          (sqlite-select (neo-open-db) "SELECT id, name, repository_id, title FROM stacks")))

(defun neo-db-get-stacks-for-repo (repo-id)
  "Return a list of `neo-stack' structs for a given REPO-ID."
  (mapcar
   (lambda (row)
     (make-neo-stack :id (nth 0 row) :name (nth 1 row)))
   (sqlite-select (neo-open-db)
                  "SELECT id, name FROM stacks WHERE repository_id = ? ORDER BY name"
                  (list repo-id))))

(defun neo-stack-summary (stack-id)
  "Return aggregated summary for STACK-ID.
Returns plist: (:branches n :open-prs n :merged n :ci-passed n :ci-failed n :ci-skipped n)"
  (let* ((branch-name (neo-db-get-branch-for-stack stack-id))
         (n-branches (if branch-name 1 0))
         (n-open-prs 0)
         (n-merged 0)
         (n-ci-passed 0)
         (n-ci-failed 0)
         (n-ci-skipped 0))
    (when branch-name
      (let ((pr (neo-db-get-pr branch-name)))
        (when pr
          (if (eq (plist-get pr :status) 'merged)
              (cl-incf n-merged)
            (cl-incf n-open-prs))
          (pcase (plist-get pr :ci)
            ('success (cl-incf n-ci-passed))
            ('failure (cl-incf n-ci-failed))
            ('skipped (cl-incf n-ci-skipped))))))
    (list :branches n-branches
          :open-prs n-open-prs
          :merged n-merged
          :ci-passed n-ci-passed
          :ci-failed n-ci-failed
          :ci-skipped n-ci-skipped)))

;; -------------------------------------------------------------------
;; Branch-level API
;; -------------------------------------------------------------------

(defun neo-db-get-branch-for-stack (stack-id)
  "Return branch name for STACK-ID."
  (when stack-id
    (caar (sqlite-select (neo-open-db)
                         "SELECT branch_name FROM stacks WHERE id = ?"
                         (list stack-id)))))

(defun neo-db-get-pr (branch-name)
  "Return PR info for BRANCH as plist (:number ... :status ... :ci ...)."
  (let ((row (car (sqlite-select (neo-open-db)
                                 "SELECT number, status, ci FROM prs WHERE head = ?"
                                 (list branch-name)))))
    (when row
      (list :number (nth 0 row)
            :status (let ((val (nth 1 row))) (when val (intern val)))
            :ci (let ((val (nth 2 row))) (when val (intern val)))))))

(defun neo-db-get-issue (branch-name)
  "Return issue associated with BRANCH as a formatted string."
  (let* ((db (neo-open-db))
         (issue-id (caar (sqlite-select db
                                        "SELECT issue_id FROM branches WHERE name = ?"
                                        (list branch-name)))))
    (when (and issue-id (> issue-id 0))
      (let ((issue (neo-load-issue issue-id)))
        (when issue
          (format "#%d" (neo-issue-number issue)))))))


(defun neo-db-upsert-labels (labels repository-id)
  "Upsert a list of LABELS for REPOSITORY-ID.
LABELS is a list of `neo-label' structs."
  (let ((db (neo-open-db)))
    (with-sqlite-transaction db
      (dolist (label labels)
        (sqlite-execute db
                        "INSERT INTO labels (id, name, color, description, repository_id)
                         VALUES (?, ?, ?, ?, ?)
                         ON CONFLICT(id) DO UPDATE SET
                           name=excluded.name, color=excluded.color,
                           description=excluded.description, repository_id=excluded.repository_id"
                        (list (neo-label-id label)
                              (neo-label-name label)
                              (neo-label-color label)
                              (neo-label-description label)
                              repository-id))))))

(defun neo-db-get-labels-for-repo (repo-id)
  "Return list of label names for REPO-ID."
  (mapcar #'car
          (sqlite-select (neo-open-db)
                         "SELECT name FROM labels WHERE repository_id = ? ORDER BY name"
                         (list repo-id))))

(defun neo-db-get-repository-id (full-name)
  "Get repository ID from its full name."
  (caar (sqlite-select (neo-open-db)
                       "SELECT id FROM repositories WHERE full_name = ?"
                       (list full-name))))

(defun neo-db-get-sync-metadata (user repository endpoint)
  "Fetch sync metadata (etag, last_sync_timestamp) from the database."
  (let ((row (car (sqlite-select (neo-open-db)
                                 "SELECT etag, last_sync_timestamp FROM sync_metadata WHERE user = ? AND repository = ? AND endpoint = ?"
                                 (list user repository endpoint)))))
    (if row
        (list :etag (nth 0 row) :last-sync-timestamp (nth 1 row))
      (list :etag nil :last-sync-timestamp nil))))

(defun neo-db-update-sync-metadata (user repository endpoint etag)
  "Update or insert sync metadata into the database."
  (let ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t))
        (db (neo-open-db)))
    (sqlite-execute db
                    "INSERT INTO sync_metadata (user, repository, endpoint, etag, last_sync_timestamp)
                     VALUES (?, ?, ?, ?, ?)
                     ON CONFLICT(user, repository, endpoint) DO UPDATE SET
                     etag=excluded.etag,
                     last_sync_timestamp=excluded.last_sync_timestamp"
                    (list user repository endpoint etag timestamp))))

(defun neo-db-get-open-prs-for-repo (repo-id)
  "Return list of `neo-pr' structs for all open PRs in REPO-ID."
  (let ((rows (sqlite-select (neo-open-db)
                             "SELECT * FROM prs WHERE status = 'open' AND issue_id IN (SELECT id FROM issues WHERE repository_id = ?)"
                             (list repo-id))))
    (mapcar (lambda (row)
              (make-neo-pr :number (nth 0 row)
                           :title (nth 1 row)
                           :author (nth 2 row)
                           :base (nth 3 row)
                           :head (nth 4 row)
                           :mergeable (nth 5 row)
                           :status (nth 6 row)
                           :ci (nth 7 row)
                           :created-at (nth 8 row)
                           :updated-at (nth 9 row)
                           :url (nth 10 row)
                           :issue-id (nth 11 row)))
            rows)))

(defun neo-db-set-pr-ci-status (issue-id ci-status)
  "Update CI status for a PR identified by ISSUE-ID."
  (sqlite-execute (neo-open-db)
                  "UPDATE prs SET ci = ? WHERE issue_id = ?"
                  (list ci-status issue-id)))

;;;###autoload
(defun neo-db-insert-repository (id full-name fork created-at pushed-at updated-at visibility forks default-branch)
  "Insert or replace a repository."
  (sqlite-execute (neo-open-db)
                  "INSERT OR REPLACE INTO repositories (id, full_name, fork, created_at, pushed_at, updated_at, visibility, forks, default_branch) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
                  (list id full-name fork created-at pushed-at updated-at visibility forks default-branch)))

(defun neo-db-get-all-project-paths ()
  "Return a list of all project paths.
Includes:
1. 'worktree_path' from 'projects' table.
2. 'worktree_path' from 'branches' table."
  (let ((project-paths (mapcar #'car (sqlite-select (neo-open-db) "SELECT worktree_path FROM projects WHERE worktree_path IS NOT NULL")))
        (branch-paths (mapcar #'car (sqlite-select (neo-open-db) "SELECT worktree_path FROM branches WHERE worktree_path IS NOT NULL"))))
    (delete-dups (append project-paths branch-paths))))

(provide 'neo-workflow-db)
;;; neo-workflow-db.el ends here
