;;; neo-workflow-models.el --- Neo Workflow core data structures -*- lexical-binding: t; -*-

;; Data structs for the Neo Workflow board backed by beads + git.
;; No SQLite, no GitHub.  Issue reads go through `beads-client'; branch
;; reads go through `neo-workflow-git'.

(require 'cl-lib)
(require 'seq)
(require 'beads-client)
(require 'neo-workflow-git)

;; ============================================================
;; Core CL structs (shape-compatible with old workflow/)
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
  pr-number         ;; integer or nil
  issue-id          ;; string or nil
  status            ;; string or nil
  ci-status         ;; string or nil
  last-commit       ;; string SHA or nil
  worktree-path)    ;; string path to local worktree or nil

(cl-defstruct neo-stack
  id                ;; string (beads issue id for epics; synthetic otherwise)
  name              ;; string
  title             ;; string or nil
  prefix            ;; string or nil (set during rendering)
  issue-id          ;; string (beads issue id)
  branch            ;; neo-branch or nil
  children-stacks)  ;; list of neo-stack objects

(cl-defstruct neo-project
  id                ;; string, unique project id
  repo              ;; string, project/workspace name
  type              ;; string: "beads"
  pr-number         ;; integer or nil (unused, kept for compat)
  worktree-path     ;; string, project root directory
  stacks)           ;; list of neo-stack objects

(cl-defstruct neo-repo-ui-state
  repo-id           ;; string
  state)            ;; string: "hidden", "summary", "expanded"

(cl-defstruct neo-issue-ui-state
  issue-id          ;; string
  state)            ;; string: "expanded", "collapsed"

;; Relocated from github-models.el (minus the GitHub-specific bits).
(cl-defstruct neo-repository
  id                ;; string (workspace path used as stable key)
  full-name         ;; string (display name / workspace name)
  fork              ;; integer (always 0 — not applicable)
  created-at        ;; string or nil
  pushed-at         ;; string or nil
  updated-at        ;; string or nil
  visibility        ;; string or nil (always "private")
  forks             ;; integer or nil (always 0)
  default-branch)   ;; string or nil

(cl-defstruct neo-label
  id                ;; integer or nil
  name              ;; string
  color             ;; string or nil
  description       ;; string or nil
  repository-id)    ;; string or nil

(cl-defstruct neo-issue
  id                ;; string (beads issue id)
  number            ;; integer (beads sequence number, converted from id)
  title             ;; string or nil
  type              ;; string (beads issue type)
  labels            ;; list of neo-label
  state             ;; symbol: 'open or 'closed
  draft             ;; integer: 0 (unused, kept for compat)
  created-at        ;; string or nil
  updated-at        ;; string or nil
  closed-at         ;; string or nil
  merged-at         ;; string or nil (unused)
  repository-id     ;; string (workspace path)
  stack             ;; neo-stack or nil (set when issue has an active branch)
  ui-state)         ;; string ("expanded" or "collapsed")

(cl-defstruct neo-context
  repository        ;; neo-repository
  stack             ;; neo-stack or nil
  perspective)      ;; string perspective name or nil

(cl-defstruct neo-transition)

(cl-defstruct neo-workflow
  name
  target-context
  steps)

;; ============================================================
;; Beads issue alist -> neo-issue mapper (the core of Phase 2)
;; ============================================================

(defun neo--beads-issue-number (id)
  "Convert beads issue ID string to an integer sequence number.
Beads IDs are strings like \"omega-123\".  We extract the numeric suffix.
Returns 0 if the ID cannot be parsed."
  (if (and id (string-match "\\([0-9]+\\)$" id))
      (string-to-number (match-string 1 id))
    0))

(defun neo--beads-labels-to-neo-labels (labels-data repository-id)
  "Convert LABELS-DATA (list of strings or alists) to a list of `neo-label' structs.
REPOSITORY-ID is the workspace path used as the label's repository key."
  (mapcar (lambda (label)
            (let ((name (if (stringp label)
                            label
                          (or (alist-get 'name label)
                              (cdr (assoc "name" label))
                              (format "%s" label)))))
              (make-neo-label
               :id nil
               :name name
               :color nil
               :description nil
               :repository-id repository-id)))
          (if (arrayp labels-data)
              (append labels-data nil)
            (or labels-data nil))))

(defun neo--beads-state-to-symbol (status)
  "Convert beads STATUS string to a symbol.
\"open\", \"in_progress\", \"blocked\", \"ready\" -> 'open
\"closed\", \"done\", \"cancelled\", \"wont-fix\" -> 'closed"
  (let ((s (if (stringp status) status (format "%s" status))))
    (cond
     ((member s '("open" "in_progress" "blocked" "ready" "in-progress")) 'open)
     ((member s '("closed" "done" "cancelled" "wont-fix" "wont_fix")) 'closed)
     (t 'open))))

(defun neo--beads-alist-to-neo-issue (alist repository-id)
  "Convert a beads issue ALIST to a `neo-issue' struct.
REPOSITORY-ID is the workspace path (used as the stable project key)."
  (let* ((id (or (alist-get 'id alist) (cdr (assoc "id" alist))))
         (title (or (alist-get 'title alist) (cdr (assoc "title" alist))))
         (issue-type (or (alist-get 'issue_type alist)
                         (alist-get 'type alist)
                         (cdr (assoc "issue_type" alist))
                         (cdr (assoc "type" alist))
                         "task"))
         (status (or (alist-get 'status alist) (cdr (assoc "status" alist)) "open"))
         (labels-raw (or (alist-get 'labels alist) (cdr (assoc "labels" alist))))
         (created-at (or (alist-get 'created_at alist) (cdr (assoc "created_at" alist))))
         (updated-at (or (alist-get 'updated_at alist) (cdr (assoc "updated_at" alist))))
         (closed-at (or (alist-get 'closed_at alist) (cdr (assoc "closed_at" alist)))))
    (make-neo-issue
     :id id
     :number (neo--beads-issue-number id)
     :title (or title "")
     :type (format "%s" issue-type)
     :labels (neo--beads-labels-to-neo-labels labels-raw repository-id)
     :state (neo--beads-state-to-symbol status)
     :draft 0
     :created-at created-at
     :updated-at updated-at
     :closed-at closed-at
     :merged-at nil
     :repository-id repository-id
     :stack nil
     :ui-state nil)))

;; ============================================================
;; Branch loading from git
;; ============================================================

(defun neo--workflow-git-branch-to-neo-branch (git-branch-info)
  "Convert a `neo-workflow-git-branch-info' to a `neo-branch'.
GIT-BRANCH-INFO is a struct returned by `neo/workflow-git-branches-snapshot'."
  (let* ((name (or (neo-workflow-git-branch-info-local-name git-branch-info)
                   (neo-workflow-git-branch-info-remote-name git-branch-info)))
         (worktree-path (when name
                          (neo/workflow-git-worktree-path-for-branch name))))
    (make-neo-branch
     :name name
     :pr-number nil
     :issue-id nil
     :status (symbol-name (neo-workflow-git-branch-info-sync-status git-branch-info))
     :ci-status nil
     :last-commit (neo-workflow-git-branch-info-local-sha git-branch-info)
     :worktree-path worktree-path)))

(defun neo-load-branch-from-git (name)
  "Load a branch by NAME from live git state.
Returns a `neo-branch' struct, or nil if the branch does not exist."
  (let ((snapshot (condition-case nil
                      (neo/workflow-git-branches-snapshot)
                    (error nil))))
    (when snapshot
      (let ((found (seq-find (lambda (b)
                               (or (string= (neo-workflow-git-branch-info-local-name b) name)
                                   (string= (neo-workflow-git-branch-info-remote-name b) name)))
                             (neo-workflow-git-branches-snapshot-branches snapshot))))
        (when found
          (neo--workflow-git-branch-to-neo-branch found))))))

;; ============================================================
;; Stack loading (Phase 2: flat — stacks-as-epics is Phase 3)
;; ============================================================

(defun neo-load-stack (_name _repository-id)
  "Return nil — stack-loading from beads epics is implemented in Phase 3.
This stub preserves the function signature expected by the UI."
  ;; Phase 3 will map beads epics to neo-stack here.
  nil)

(defun neo-db-get-stacks-for-repo (_repo-id)
  "Return nil — stacks-as-epics is Phase 3.
REPO-ID is ignored."
  nil)

;; ============================================================
;; Project discovery from BEADS_DIR workspace
;; ============================================================

(defun neo--workflow-beads-workspace-as-project ()
  "Return a `neo-project' struct for the current beads workspace, or nil."
  (when-let* ((info (beads-client--workspace-info))
              (beads-dir-path (alist-get 'path info))
              (root (beads-client--project-root)))
    (let* ((name (file-name-nondirectory (directory-file-name root)))
           (id (expand-file-name root)))
      (make-neo-project
       :id id
       :repo name
       :type "beads"
       :pr-number nil
       :worktree-path root
       :stacks nil))))

(defun neo-load-all-repositories ()
  "Return the list of beads workspaces as `neo-repository' structs.
Each beads workspace becomes one board row."
  (let ((project (neo--workflow-beads-workspace-as-project)))
    (if project
        (list (neo--project-to-repository project))
      nil)))

(defun neo--project-to-repository (project)
  "Convert a `neo-project' struct to a `neo-repository' struct."
  (make-neo-repository
   :id (neo-project-id project)
   :full-name (neo-project-repo project)
   :fork 0
   :created-at nil
   :pushed-at nil
   :updated-at nil
   :visibility "private"
   :forks 0
   :default-branch "main"))

(defun neo-load-project-by-repo (repo-name)
  "Return a `neo-project' for REPO-NAME, or nil if not found.
Looks in the current beads workspace."
  (when-let* ((project (neo--workflow-beads-workspace-as-project)))
    (when (string= (neo-project-repo project) repo-name)
      project)))

;; ============================================================
;; Issue loading from beads
;; ============================================================

(defun neo-db-get-issues-for-repo (repository-id)
  "Return issues for REPOSITORY-ID as `neo-issue' structs.
Fetches all non-epic issues from beads in one call."
  (condition-case err
      (let ((raw (beads-client-list)))
        (seq-keep (lambda (alist)
                    (let* ((issue-type (or (alist-get 'issue_type alist)
                                           (alist-get 'type alist)
                                           "task"))
                           (type-str (format "%s" issue-type)))
                      ;; Epics become stacks (Phase 3); exclude them here.
                      (unless (string= type-str "epic")
                        (neo--beads-alist-to-neo-issue alist repository-id))))
                  (append raw nil)))
    (error
     (message "neo-workflow: beads fetch failed: %s" (error-message-string err))
     nil)))

(defun neo-load-issue (id)
  "Load an issue by beads ID from beads.
Returns a `neo-issue' struct or nil."
  (when id
    (condition-case err
        (let* ((alist (beads-client-show id))
               (repo-id (or (when-let* ((project (neo--workflow-beads-workspace-as-project)))
                              (neo-project-id project))
                            "")))
          (neo--beads-alist-to-neo-issue alist repo-id))
      (error
       (message "neo-workflow: beads show %s failed: %s" id (error-message-string err))
       nil))))

(defun neo-load-repository (id)
  "Return the `neo-repository' whose :id equals ID, or nil."
  (seq-find (lambda (r) (equal (neo-repository-id r) id))
            (neo-load-all-repositories)))

;; ============================================================
;; Context loading (in-memory for Phase 2)
;; ============================================================

(defun neo/workflow-load-context (_repository-id)
  "Return nil — in-memory context persistence is Phase 5.
REPOSITORY-ID is ignored."
  nil)

(defun neo/workflow-load-context-for-stack (_repository-id _stack-id)
  "Return nil — in-memory context persistence is Phase 5."
  nil)

(defun neo/workflow-save-context (_context)
  "No-op — in-memory context persistence is Phase 5.
CONTEXT is ignored."
  nil)

(provide 'neo-workflow-models)
;;; neo-workflow-models.el ends here
