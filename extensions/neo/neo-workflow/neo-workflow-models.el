;;; neo-workflow-models.el --- Neo Workflow core data structures -*- lexical-binding: t; -*-

;; Data structs for the Neo Workflow board backed by beads + git.
;; No SQLite, no GitHub.  Issue reads go through `beads-client'; branch
;; reads go through `neo-workflow-git'.

(require 'cl-lib)
(require 'seq)
(require 'beads-client)
(require 'neo-workflow-git)
(require 'neo-workflow-slug)

;; ============================================================
;; Core CL structs (shape-compatible with old workflow/)
;; ============================================================

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
  short-id          ;; string (beads id minus workspace prefix, e.g. "11sv")
  priority          ;; integer 0-4 (beads priority) or nil
  title             ;; string or nil
  type              ;; string (beads issue type)
  labels            ;; list of neo-label
  state             ;; symbol: 'open or 'closed (coarse, for filtering)
  status            ;; string: raw beads status (open/in_progress/blocked/closed/deferred/...)
  draft             ;; integer: 0 (unused, kept for compat)
  created-at        ;; string or nil
  updated-at        ;; string or nil
  closed-at         ;; string or nil
  merged-at         ;; string or nil (unused)
  repository-id     ;; string (workspace path)
  stack             ;; neo-stack or nil (parent epic when this issue has one)
  prefix            ;; string tree-indent prefix (set during board rendering)
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

(defun neo--beads-issue-id (alist)
  "Return the issue ID string from a beads issue ALIST, or nil."
  (or (alist-get 'id alist) (cdr (assoc "id" alist))))

(defun neo--beads-issue-title (alist)
  "Return the title string from a beads issue ALIST, or nil."
  (or (alist-get 'title alist) (cdr (assoc "title" alist))))

(defun neo--beads-issue-type-string (alist)
  "Return the issue type of a beads ALIST as a string (default \"task\")."
  (format "%s" (or (alist-get 'issue_type alist)
                   (alist-get 'type alist)
                   (cdr (assoc "issue_type" alist))
                   (cdr (assoc "type" alist))
                   "task")))

(defun neo--beads-issue-epic-p (alist)
  "Return non-nil when the beads issue ALIST is an epic."
  (string= (neo--beads-issue-type-string alist) "epic"))

(defun neo--beads-issue-parent (alist)
  "Return the parent issue ID from a beads ALIST, or nil when there is none."
  (let ((parent (or (alist-get 'parent alist) (cdr (assoc "parent" alist)))))
    (and (stringp parent) (not (string-empty-p parent)) parent)))

(defun neo--beads-issue-priority (alist)
  "Return the integer priority (0-4) from a beads ALIST, or nil when absent."
  (let ((p (or (alist-get 'priority alist) (cdr (assoc "priority" alist)))))
    (cond ((integerp p) p)
          ((and (stringp p) (string-match-p "\\`[0-9]+\\'" p)) (string-to-number p))
          (t nil))))

(defun neo--beads-short-id (id)
  "Return ID without its workspace prefix, e.g. \"omega-11sv\" -> \"11sv\".
Falls back to ID itself when there is no prefix."
  (if (and id (string-match "-\\([^-]+\\)\\'" id))
      (match-string 1 id)
    (or id "")))

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
     :short-id (neo--beads-short-id id)
     :priority (neo--beads-issue-priority alist)
     :title (or title "")
     :type (format "%s" issue-type)
     :labels (neo--beads-labels-to-neo-labels labels-raw repository-id)
     :state (neo--beads-state-to-symbol status)
     :status status
     :draft 0
     :created-at created-at
     :updated-at updated-at
     :closed-at closed-at
     :merged-at nil
     :repository-id repository-id
     :stack nil
     :prefix nil
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
;; Stack loading (Phase 3: stacks ARE beads epics)
;;
;; A stack maps to a beads epic.  Its name/branch follow the
;; "<epic-number>-<slug>" convention (`neo-issue-title-to-slug'), so the
;; status board can match a stack to its parent issue by number and to its
;; live git branch by name.  An epic whose `parent' is another epic becomes a
;; nested child stack; non-epic children are ordinary issues that carry the
;; stack on their `stack' slot (set in `neo-db-get-issues-for-repo').
;; ============================================================

(defun neo--workflow-slug-prefix (id)
  "Return the stack/branch-name prefix for a beads ID.
Uses the numeric sequence number when ID has a numeric suffix (e.g.
\"omega-123\" -> 123); otherwise falls back to the full ID (e.g. hash-style
\"omega-11sv\"), so every stack name stays unique and stable."
  (let ((number (neo--beads-issue-number id)))
    (if (> number 0) number id)))

(defun neo--workflow-stack-name (id title)
  "Return the \"<prefix>-<slug>\" stack/branch name for beads ID and TITLE."
  (neo-issue-title-to-slug (neo--workflow-slug-prefix id) (or title "")))

(defun neo--workflow-epic-stack-name (epic-alist)
  "Return the \"<prefix>-<slug>\" stack/branch name for EPIC-ALIST."
  (neo--workflow-stack-name (neo--beads-issue-id epic-alist)
                            (neo--beads-issue-title epic-alist)))

(defun neo--workflow-epic-to-stack (epic-alist children-by-parent repository-id)
  "Build a `neo-stack' from EPIC-ALIST.
CHILDREN-BY-PARENT maps an epic ID to the list of its child-epic alists.
REPOSITORY-ID is the workspace key.  The branch is read live from git (nil
when no matching branch exists); child epics become nested stacks."
  (let* ((id (neo--beads-issue-id epic-alist))
         (name (neo--workflow-epic-stack-name epic-alist))
         (stack (make-neo-stack
                 :id id
                 :name name
                 :title (neo--beads-issue-title epic-alist)
                 :prefix nil
                 :issue-id id
                 :branch (neo-load-branch-from-git name)
                 :children-stacks nil)))
    (setf (neo-stack-children-stacks stack)
          (mapcar (lambda (child)
                    (neo--workflow-epic-to-stack child children-by-parent repository-id))
                  (reverse (gethash id children-by-parent))))
    stack))

(defun neo--workflow-stacks-from-issues (raw repository-id)
  "Build the list of top-level `neo-stack' structs from RAW beads issues.
RAW is the list of issue alists returned by `beads-client-list'.  Epics
become stacks; an epic whose `parent' is another epic in RAW becomes a
nested child stack instead of a top-level entry."
  (let* ((epics (seq-filter #'neo--beads-issue-epic-p raw))
         (epic-ids (mapcar #'neo--beads-issue-id epics))
         (children-by-parent (make-hash-table :test #'equal)))
    (dolist (epic epics)
      (let ((parent (neo--beads-issue-parent epic)))
        (when (and parent (member parent epic-ids))
          (push epic (gethash parent children-by-parent)))))
    (let ((top-level (seq-remove
                      (lambda (epic)
                        (let ((parent (neo--beads-issue-parent epic)))
                          (and parent (member parent epic-ids))))
                      epics)))
      (mapcar (lambda (epic)
                (neo--workflow-epic-to-stack epic children-by-parent repository-id))
              top-level))))

(defun neo--workflow-flatten-stacks (stacks)
  "Return STACKS together with all of their descendant child stacks, flattened."
  (mapcan (lambda (stack)
            (cons stack (neo--workflow-flatten-stacks
                         (neo-stack-children-stacks stack))))
          stacks))

(defun neo--workflow-all-stacks (repository-id)
  "Return the top-level `neo-stack' tree for REPOSITORY-ID from beads.
Performs a single `beads-client-list' and assembles the epic tree
client-side.  Returns nil on error."
  (condition-case err
      (neo--workflow-stacks-from-issues
       (append (beads-client-list) nil) repository-id)
    (error
     (message "neo-workflow: stack fetch failed: %s" (error-message-string err))
     nil)))

(defun neo-load-stack (name repository-id)
  "Load the stack named NAME for REPOSITORY-ID, or nil when none matches.
A stack is a beads epic and NAME is its \"<number>-<slug>\" branch name.
The returned `neo-stack' includes its live git branch and nested child
stacks.  Child stacks are searched too, not just top-level ones."
  (when name
    (seq-find (lambda (stack) (string= (neo-stack-name stack) name))
              (neo--workflow-flatten-stacks
               (neo--workflow-all-stacks repository-id)))))

(defun neo-load-stack-by-id (stack-id repository-id)
  "Load the stack whose id is STACK-ID for REPOSITORY-ID, or nil.
Searches nested child stacks as well as top-level ones."
  (when stack-id
    (seq-find (lambda (stack) (equal (neo-stack-id stack) stack-id))
              (neo--workflow-flatten-stacks
               (neo--workflow-all-stacks repository-id)))))

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

(defun neo--workflow-current-repository-id ()
  "Return the repository id (workspace path) for the current beads workspace.
Returns an empty string when no workspace can be resolved."
  (or (when-let* ((project (neo--workflow-beads-workspace-as-project)))
        (neo-project-id project))
      ""))

;; ============================================================
;; Issue loading from beads
;; ============================================================

(defun neo-db-get-issues-for-repo (repository-id)
  "Return non-epic issues for REPOSITORY-ID as `neo-issue' structs.
Performs a single `beads-client-list'.  Epics are surfaced as stacks (see
`neo-db-get-stacks-for-repo') and excluded here.  Each issue whose beads
`parent' is an epic gets that epic's `neo-stack' attached on its `stack'
slot, so the board can nest issues under their stack."
  (condition-case err
      (let* ((raw (append (beads-client-list) nil))
             (stacks (neo--workflow-stacks-from-issues raw repository-id))
             (stack-by-id (make-hash-table :test #'equal)))
        (dolist (stack (neo--workflow-flatten-stacks stacks))
          (puthash (neo-stack-id stack) stack stack-by-id))
        (seq-keep
         (lambda (alist)
           ;; Epics become stacks (above); exclude them from the issue list.
           (unless (neo--beads-issue-epic-p alist)
             (let ((issue (neo--beads-alist-to-neo-issue alist repository-id))
                   (parent (neo--beads-issue-parent alist)))
               (when parent
                 (setf (neo-issue-stack issue) (gethash parent stack-by-id)))
               issue)))
         raw))
    (error
     (message "neo-workflow: beads fetch failed: %s" (error-message-string err))
     nil)))

(defun neo-load-issue (id)
  "Load an issue by beads ID from beads.
Returns a `neo-issue' struct or nil."
  (when id
    (condition-case err
        (let ((alist (beads-client-show id)))
          (neo--beads-alist-to-neo-issue alist (neo--workflow-current-repository-id)))
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

(defun neo/workflow-load-context (repository-id)
  "Return the current `neo-context' for REPOSITORY-ID from the in-memory store.
Returns nil when no context has been recorded for the repo."
  (when-let* ((data (neo/workflow-db-get-context repository-id)))
    (make-neo-context
     :repository (neo-load-repository repository-id)
     :stack (neo-load-stack-by-id (plist-get data :stack-id) repository-id)
     :perspective (plist-get data :perspective))))

(defun neo/workflow-load-context-for-stack (repository-id stack-id)
  "Return the `neo-context' for REPOSITORY-ID and STACK-ID, or nil."
  (when-let* ((data (neo/workflow-db-get-context-by-stack repository-id stack-id)))
    (make-neo-context
     :repository (neo-load-repository repository-id)
     :stack (neo-load-stack-by-id stack-id repository-id)
     :perspective (plist-get data :perspective))))

(defun neo/workflow-save-context (context)
  "Persist CONTEXT in the in-memory context store."
  (neo/workflow-db-upsert-context
   (neo-repository-id (neo-context-repository context))
   (when (neo-context-stack context) (neo-stack-id (neo-context-stack context)))
   (neo-context-perspective context)))

(provide 'neo-workflow-models)
;;; neo-workflow-models.el ends here
