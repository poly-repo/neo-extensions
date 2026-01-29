;;; neo-workflow-models.el --- Neo Workflow core data structures -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'neo-workflow-db)
(require 'github-models)

;; ============================================================
;; Core CL structs
;; ============================================================

(cl-defstruct neo-pr
  number
  title
  author
  base
  head
  mergeable
  status
  ci
  created-at
  updated-at
  url
  issue-id)

(cl-defstruct neo-branch
  name              ;; string
  pr-number         ;; integer
  issue-id          ;; integer
  status            ;; string
  ci-status         ;; string
  last-commit       ;; string SHA
  worktree-path)    ;; string path to local worktree

(cl-defstruct neo-stack
  id                ;; integer
  name              ;; string
  title             ;; string or nil
  prefix            ;; string or nil
  issue-id          ;; string
  branch            ;; a neo-branch object
  children-stacks)  ;; list of neo-stack objects

(cl-defstruct neo-project
  id                ;; string, unique project id (repo or PR-based)
  repo              ;; string, repo name
  type              ;; string: "repo" or "pr"
  pr-number         ;; integer, optional
  worktree-path     ;; string, optional
  stacks)           ;; list of neo-stack objects

(cl-defstruct neo-repo-ui-state
  repo-id       ;; integer: repository id
  state)        ;; string: "hidden", "summary", "expanded"

(cl-defstruct neo-issue-ui-state
  issue-id      ;; integer: issue id
  state)        ;; string: "expanded", "collapsed"

;; facts
(cl-defstruct neo-context
  repository				; neo-repository
  stack					; the top neo-stack object for this context
  perspective				; a perspective.el perspective
  )

;; diff contect to context + plan
(cl-defstruct neo-transition
  )

(cl-defstruct neo-workflow
  name
  target-context
  steps
  )

;; 
;; ============================================================
;; Helpers to load from DB into structs
;; ============================================================

(defun neo/workflow-load-context (repository-id)
  "Load the workflow context for REPOSITORY-ID."
  (let ((data (neo/workflow-db-get-context repository-id)))
    (when data
      (let* ((repo (neo-load-repository repository-id))
             (stack (when (plist-get data :stack-id)
                      (let ((stack-row (car (sqlite-select (neo-open-db)
                                                           "SELECT name FROM stacks WHERE id = ?"
                                                           (list (plist-get data :stack-id))))))
                        (when stack-row
                          (neo-load-stack (car stack-row) repository-id))))))
        (make-neo-context
         :repository repo
         :stack stack
         :perspective (plist-get data :perspective))))))

(defun neo/workflow-load-context-for-stack (repository-id stack-id)
  "Load the workflow context for REPOSITORY-ID and STACK-ID."
  (let ((data (neo/workflow-db-get-context-by-stack repository-id stack-id)))
    (when data
      (let* ((repo (neo-load-repository repository-id))
             (stack (when stack-id
                      (let ((stack-row (car (sqlite-select (neo-open-db)
                                                           "SELECT name FROM stacks WHERE id = ?"
                                                           (list stack-id)))))
                        (when stack-row
                          (neo-load-stack (car stack-row) repository-id))))))
        (make-neo-context
         :repository repo
         :stack stack
         :perspective (plist-get data :perspective))))))

(defun neo/workflow-save-context (context)
  "Save CONTEXT to the database."
  (let ((repo-id (neo-repository-id (neo-context-repository context)))
        (stack-id (when (neo-context-stack context)
                    (neo-stack-id (neo-context-stack context))))
        (perspective (neo-context-perspective context)))
    (neo/workflow-db-upsert-context repo-id stack-id perspective)))

(defun neo-load-repository (id)
  "Load a repository by ID from the DB."
  (let ((row (car (sqlite-select (neo-open-db)
                                 "SELECT id, full_name, fork, created_at, pushed_at, updated_at, visibility, forks, default_branch FROM repositories WHERE id = ?"
                                 (list id)))))
    (when row
      (make-neo-repository
       :id (nth 0 row)
       :full-name (nth 1 row)
       :fork (nth 2 row)
       :created-at (nth 3 row)
       :pushed-at (nth 4 row)
       :updated-at (nth 5 row)
       :visibility (nth 6 row)
       :forks (nth 7 row)
       :default-branch (nth 8 row)))))

(defun neo-load-issue (id)
  "Load an issue by ID from the DB."
  (let ((row (car (sqlite-select (neo-open-db)
                                 "SELECT T1.id, T1.number, T1.title, T1.type, T1.state, T1.draft, T1.created_at, T1.updated_at, T1.closed_at, T1.merged_at, T1.repository_id, T2.name, T3.state FROM issues AS T1 LEFT JOIN stacks AS T2 ON T1.stack_id = T2.id LEFT JOIN issue_ui_state AS T3 ON T1.id = T3.issue_id WHERE T1.id = ?"
                                 (list id)))))
    (when row
      (let ((issue-id (nth 0 row)))
        (make-neo-issue
         :id issue-id
         :number (nth 1 row)
         :title (nth 2 row)
         :type (nth 3 row)
         :labels (neo-db-get-labels-for-issue issue-id)
         :state (let ((val (nth 4 row))) (and val (intern val)))
         :draft (nth 5 row)
         :created-at (nth 6 row)
         :updated-at (nth 7 row)
         :closed-at (nth 8 row)
         :merged-at (nth 9 row)
         :repository-id (nth 10 row)
         :stack (let ((stack-name (nth 11 row))
                      (repo-id (nth 10 row)))
                  (when stack-name (neo-load-stack stack-name repo-id)))
         :ui-state (nth 12 row))))))

(defun neo-load-pr (number)
  "Load a PR by NUMBER from the DB."
  (let ((row (car (sqlite-select (neo-open-db)
                                 "SELECT * FROM prs WHERE number = ?"
                                 (list number)))))
    (when row
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
                   :issue-id (nth 11 row)))))

(defun neo-load-branch (name repository-id)
  "Load a branch by NAME and REPOSITORY-ID from the DB."
  (let ((row (car (sqlite-select (neo-open-db)
                                 "SELECT * FROM branches WHERE name = ? AND repository_id = ?"
                                 (list name repository-id)))))
    (when row
      (make-neo-branch :name (nth 0 row)
                       :pr-number (nth 2 row)
                       :issue-id (nth 3 row)
                       :status (nth 4 row)
                       :ci-status (nth 5 row)
                       :last-commit (nth 6 row)
                       :worktree-path (nth 7 row)))))

(defun neo-load-stack (name repository-id)
  "Load a stack by NAME and REPOSITORY-ID, including its branch and child stacks."
  (let* ((db (neo-open-db))
         (row (car (sqlite-select db
                                  "SELECT id, name, issue_id, title FROM stacks WHERE name = ? AND repository_id = ?"
                                  (list name repository-id)))))
    (when row
      (let* ((stack-id (nth 0 row))
             (stack-name (nth 1 row))
             (stack (make-neo-stack :id stack-id
                                    :name stack-name
                                    :issue-id (nth 2 row)
                                    :title (nth 3 row)))
             (child-stack-rows (sqlite-select db
                                              "SELECT name FROM stacks WHERE parent_stack_id = ? AND repository_id = ?"
                                              (list stack-id repository-id))))
        (setf (neo-stack-branch stack) (neo-load-branch stack-name repository-id))
        (setf (neo-stack-children-stacks stack)
              (mapcar (lambda (row) (neo-load-stack (car row) repository-id)) child-stack-rows))
        stack))))

(defun neo-load-project (id)
  "Load a project by ID, including its stacks."
  (let* ((row (car (sqlite-select (neo-open-db)
                                  "SELECT * FROM projects WHERE id = ?"
                                  (list id))))
         (project (when row
                    (make-neo-project :id (nth 0 row)
                                      :repo (nth 1 row)
                                      :type (nth 2 row)
                                      :pr-number (nth 3 row)
                                      :worktree-path (nth 4 row))))
         (stack-rows (sqlite-select (neo-open-db)
                                    "SELECT stack_name FROM project_stacks WHERE project_id = ?"
                                    (list id))))
    (when project
      (setf (neo-project-stacks project)
            (mapcar #'neo-load-stack (mapcar #'car stack-rows)))
      project)))

(defun neo-load-project-by-repo (repo-name)
  "Load a project by REPO-NAME, including its stacks."
  (let* ((row (car (sqlite-select (neo-open-db)
                                  "SELECT * FROM projects WHERE repo = ?"
                                  (list repo-name))))
         (project (when row
                    (make-neo-project :id (nth 0 row)
                                      :repo (nth 1 row)
                                      :type (nth 2 row)
                                      :pr-number (nth 3 row)
                                      :worktree-path (nth 4 row))))
         (id (when project (neo-project-id project)))
         (stack-rows (when id
                       (sqlite-select (neo-open-db)
                                      "SELECT stack_name FROM project_stacks WHERE project_id = ?"
                                      (list id)))))
    (when project
      (setf (neo-project-stacks project)
            (mapcar #'neo-load-stack (mapcar #'car stack-rows)))
      project)))

(defun neo--ghub-result-to-neo-issue (result repository-id)
  "Convert a ghub API result alist for an issue to a `neo-issue' struct."
  (when (listp result)
    (let* ((labels-data (cdr (assoc 'labels result)))
           (labels (when (listp labels-data)
                     (mapcar (lambda (label-alist)
                               (make-neo-label :id (cdr (assoc 'id label-alist))
                                               :name (cdr (assoc 'name label-alist))
                                               :color (cdr (assoc 'color label-alist))
                                               :description (cdr (assoc 'description label-alist))
                                               :repository-id repository-id))
                             labels-data)))
           (state-str (cdr (assoc 'state result)))
           (pr-info (cdr (assoc 'pull_request result))))
      (make-neo-issue
       :id (cdr (assoc 'id result))
       :number (cdr (assoc 'number result))
       :title (cdr (assoc 'title result))
       :type (if pr-info "PR" "Issue")
       :labels labels
       :state (when state-str (intern state-str))
       :draft (if (cdr (assoc 'draft result)) 1 0)
       :created-at (cdr (assoc 'created_at result))
       :updated-at (cdr (assoc 'updated_at result))
       :closed-at (cdr (assoc 'closed_at result))
       :merged-at (when (consp pr-info) (cdr (assoc 'merged_at pr-info)))
       :repository-id repository-id))))

(defun neo-db-get-labels-for-issue (issue-id)
  (mapcar (lambda (label-row)
	    (make-neo-label
             :id (nth 0 label-row)
             :name (nth 1 label-row)
             :color (nth 2 label-row)
             :description (nth 3 label-row)
             :repository-id (nth 4 label-row)))
	  (sqlite-select (neo-open-db)
			 "SELECT T2.id, T2.name, T2.color, T2.description, T2.repository_id FROM issue_labels AS T1 JOIN labels AS T2 ON T1.label_id = T2.id WHERE T1.issue_id = ?"
			 (list issue-id))))

(defun neo-db-get-issues-for-repo (repo-id)
  "Return issues for REPO-ID as `neo-issue' structs."
  (let ((rows (sqlite-select (neo-open-db)
                             "SELECT T1.id, T1.number, T1.title, T1.type, T1.state, T1.draft, T1.created_at, T1.updated_at, T1.closed_at, T1.merged_at, T1.repository_id, T2.name, T3.state FROM issues AS T1 LEFT JOIN stacks AS T2 ON T1.stack_id = T2.id LEFT JOIN issue_ui_state AS T3 ON T1.id = T3.issue_id WHERE T1.repository_id = ? ORDER BY T1.created_at DESC"
                             (list repo-id))))
    (mapcar (lambda (row)
              (let ((issue-id (nth 0 row)))
		(make-neo-issue
		 :id issue-id
		 :number (nth 1 row)
		 :title (nth 2 row)
		 :type (nth 3 row)
		 :labels (neo-db-get-labels-for-issue issue-id)
		 :state (let ((val (nth 4 row))) (and val (intern val)))
		 :draft (nth 5 row)
		 :created-at (nth 6 row)
		 :updated-at (nth 7 row)
		 :closed-at (nth 8 row)
		 :merged-at (nth 9 row)
		 :repository-id (nth 10 row)
		 :stack (let ((stack-name (nth 11 row))
                              (repo-id (nth 10 row)))
                          (when stack-name (neo-load-stack stack-name repo-id)))
                 :ui-state (nth 12 row))))
	    rows)))

(defun neo-db-get-issue-by-number (repo-id issue-number)
  "Return issue for REPO-ID and ISSUE-NUMBER as a `neo-issue' struct."
  (let ((row (car (sqlite-select (neo-open-db)
                                 "SELECT T1.id, T1.number, T1.title, T1.type, T1.state, T1.draft, T1.created_at, T1.updated_at, T1.closed_at, T1.merged_at, T1.repository_id, T2.name, T3.state FROM issues AS T1 LEFT JOIN stacks AS T2 ON T1.stack_id = T2.id LEFT JOIN issue_ui_state AS T3 ON T1.id = T3.issue_id WHERE T1.repository_id = ? AND T1.number = ?"
                                 (list repo-id issue-number)))))
    (when row
      (let ((issue-id (nth 0 row)))
        (make-neo-issue
         :id issue-id
         :number (nth 1 row)
         :title (nth 2 row)
         :type (nth 3 row)
         :labels (neo-db-get-labels-for-issue issue-id)
         :state (let ((val (nth 4 row))) (and val (intern val)))
         :draft (nth 5 row)
         :created-at (nth 6 row)
         :updated-at (nth 7 row)
         :closed-at (nth 8 row)
         :merged-at (nth 9 row)
         :repository-id (nth 10 row)
         :stack (let ((stack-name (nth 11 row))
                      (repo-id (nth 10 row)))
                  (when stack-name (neo-load-stack stack-name repo-id)))
         :ui-state (nth 12 row))))))

(defun neo-load-all-repositories ()
  "Load all repositories from the DB."
  (let ((rows (sqlite-select (neo-open-db)
                             "SELECT id, full_name, fork, created_at, pushed_at, updated_at, visibility, forks, default_branch FROM repositories ORDER BY fork ASC, updated_at DESC")))
    (mapcar (lambda (row)
              (make-neo-repository
               :id (nth 0 row)
               :full-name (nth 1 row)
               :fork (nth 2 row)
               :created-at (nth 3 row)
               :pushed-at (nth 4 row)
               :updated-at (nth 5 row)
               :visibility (nth 6 row)
               :forks (nth 7 row)
               :default-branch (nth 8 row)))
            rows)))

(provide 'neo-workflow-models)
;;; neo-workflow-models.el ends here
