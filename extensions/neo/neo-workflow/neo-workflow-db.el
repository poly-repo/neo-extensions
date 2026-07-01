;;; neo-workflow-db.el --- Neo Workflow beads/git/in-memory data layer -*- lexical-binding: t; -*-

;; Reimplementation of the old SQLite-backed `neo-workflow-db.el'.
;; Public function names are preserved so that status/ui code requires
;; no changes.  All data comes from beads-client or git; UI state is
;; kept in-memory (not persisted — Phase 5 adds persistence).

(require 'cl-lib)
(require 'seq)
(require 'beads-client)
(require 'neo-workflow-models)

;; ============================================================
;; In-memory state (replaces workflow_ui_state, repo_ui_state, etc.)
;; ============================================================

(defvar neo--workflow-global-filter nil
  "Current global workflow filter symbol: 'open, 'active, 'all, or nil (= all).")

(defvar neo--workflow-repo-ui-states (make-hash-table :test 'equal)
  "Hash table: repo-id -> plist (:state STRING :filter STRING :order STRING).")

(defvar neo--workflow-issue-ui-states (make-hash-table :test 'equal)
  "Hash table: issue-id -> STATE string (\"expanded\" or \"collapsed\").")

;; ============================================================
;; UI-state API (replaces the SQLite-backed equivalents)
;; ============================================================

(defun neo-db-set-repo-ui-state (repo-id state &optional filter order)
  "Set/update the UI state for REPO-ID.
STATE, FILTER, and ORDER are strings."
  (puthash repo-id (list :state state :filter filter :order order)
           neo--workflow-repo-ui-states))

(defun neo-db-get-repo-ui-state (repo-id)
  "Get the UI state for REPO-ID.
Returns a plist (:state STRING :filter STRING :order STRING) or nil."
  (gethash repo-id neo--workflow-repo-ui-states))

(defun neo-db-set-issue-ui-state (issue-id state)
  "Set the UI state for ISSUE-ID to STATE."
  (puthash issue-id state neo--workflow-issue-ui-states))

(defun neo-db-get-issue-ui-state (issue-id)
  "Get the UI state for ISSUE-ID.  Returns STATE string or nil."
  (gethash issue-id neo--workflow-issue-ui-states))

(defun neo-db-set-workflow-filter (filter)
  "Set the global workflow FILTER (string or nil)."
  (setq neo--workflow-global-filter (when filter (intern filter))))

(defun neo-db-get-workflow-filter ()
  "Get the global workflow filter as a string, or nil."
  (when neo--workflow-global-filter
    (symbol-name neo--workflow-global-filter)))

;; ============================================================
;; Project / repository API
;; ============================================================

(defun neo-db-get-all-project-paths ()
  "Return a list of project root paths from the beads workspace."
  (when-let* ((project (neo--workflow-beads-workspace-as-project)))
    (list (neo-project-worktree-path project))))

(defun neo-workflow-project-backend (dir)
  "Return a project instance if DIR is inside a known Neo Workflow project root."
  (let ((canonical-dir (expand-file-name dir))
        (known-roots (neo-db-get-all-project-paths)))
    (when-let* ((root (seq-find (lambda (r)
                                  (string-prefix-p (expand-file-name r) canonical-dir))
                                known-roots)))
      (cons 'neo-workflow root))))

;; ============================================================
;; Repository lookup helpers (kept for status.el call sites)
;; ============================================================

(defun neo--get-repo-id-by-full-name (repo-full-name)
  "Return the repository id for REPO-FULL-NAME.
Matches against the beads workspace name."
  (when-let* ((repo (seq-find (lambda (r)
                                (string= (neo-repository-full-name r) repo-full-name))
                              (neo-load-all-repositories))))
    (neo-repository-id repo)))

(defun neo--workflow-get-repo-full-name-by-id (repo-id)
  "Return the full name for REPO-ID."
  (when-let* ((repo (seq-find (lambda (r)
                                (equal (neo-repository-id r) repo-id))
                              (neo-load-all-repositories))))
    (neo-repository-full-name repo)))

;; ============================================================
;; Stack API (Phase 3 — stacks ARE beads epics)
;;
;; Compat boundary: the real epic->stack mapping lives in
;; `neo-workflow-models' (`neo--workflow-all-stacks' & friends).  These
;; public `neo-db-*' names are what the board/summary UIs call.
;; ============================================================

(defun neo-db-get-all-stacks ()
  "Return every stack as a plist (:id :name :repository-id :title).
Flattens nested child stacks; consumed by the summary UI in
`neo-workflow-ui'.  See `neo-db-get-stacks-for-repo' for the struct view."
  (let ((repo-id (neo--workflow-current-repository-id)))
    (mapcar (lambda (stack)
              (list :id (neo-stack-id stack)
                    :name (neo-stack-name stack)
                    :repository-id repo-id
                    :title (neo-stack-title stack)))
            (neo--workflow-flatten-stacks (neo--workflow-all-stacks repo-id)))))

(defun neo-db-get-stacks-for-repo (repo-id)
  "Return the top-level `neo-stack' tree for REPO-ID (beads epics)."
  (neo--workflow-all-stacks repo-id))

(defun neo-db-get-branch-for-stack (stack-id)
  "Return the live `neo-branch' for STACK-ID's epic, or nil when none exists."
  (when-let* ((stack (seq-find
                      (lambda (s) (equal (neo-stack-id s) stack-id))
                      (neo--workflow-flatten-stacks
                       (neo--workflow-all-stacks
                        (neo--workflow-current-repository-id))))))
    (neo-stack-branch stack)))

;; ============================================================
;; Branch API (live git reads)
;; ============================================================

(defun neo-load-branch (name &optional _repository-id)
  "Load a branch by NAME from live git.
REPOSITORY-ID is accepted for signature compat but ignored."
  (neo-load-branch-from-git name))

;; ============================================================
;; Context API (in-memory stubs for Phase 2)
;; ============================================================

(defun neo/workflow-db-upsert-context (_repo-id _stack-id _perspective)
  "No-op — in-memory context persistence is Phase 5."
  nil)

(defun neo/workflow-db-get-context (_repo-id)
  "Return nil — in-memory context persistence is Phase 5."
  nil)

(defun neo/workflow-db-get-context-by-stack (_repo-id _stack-id)
  "Return nil — in-memory context persistence is Phase 5."
  nil)

;; ============================================================
;; Issue write-path stubs (Phase 4)
;; ============================================================

(defun neo-db-upsert-issue (_issue)
  "No-op stub — issue writes via beads are Phase 4.
ISSUE is ignored; the board always re-reads from beads on refresh."
  nil)

(defun neo-db-insert-branch (_name _repository-id &rest _args)
  "No-op stub — branch writes are Phase 4.
All args are ignored."
  nil)

(defun neo-db-insert-stack (_name _branch-name _issue-id _repository-id &rest _args)
  "No-op stub — stack writes (beads epic creation) are Phase 4.
All args are ignored."
  nil)

(defun neo-db-insert-project (_id _repo _type &rest _args)
  "No-op stub — projects are discovered from the beads workspace in Phase 2.
All args are ignored."
  nil)

(defun neo-db-insert-project-stack (_project _stack)
  "No-op stub — project/stack association is Phase 4."
  nil)

;; ============================================================
;; Label helpers (beads labels come through the issue, no separate table)
;; ============================================================

(defun neo-db-get-labels-for-repo (_repo-id)
  "Return empty list — label management is not separately tracked in Phase 2.
Labels flow through `beads-client-list' attached to issues."
  nil)

(defun neo-db-get-labels-for-issue (_issue-id)
  "Return empty list — labels are inlined in the issue alist from beads."
  nil)

;; ============================================================
;; Sync-metadata and PR helpers — no-ops (no GitHub in Phase 2)
;; ============================================================

(defun neo-db-get-sync-metadata (_user _repository _endpoint)
  "Return empty metadata — no GitHub sync in Phase 2."
  (list :etag nil :last-sync-timestamp nil))

(defun neo-db-update-sync-metadata (_user _repository _endpoint _etag)
  "No-op — no GitHub sync in Phase 2."
  nil)

(defun neo-db-get-open-prs-for-repo (_repo-id)
  "Return empty list — no GitHub PRs in Phase 2."
  nil)

(defun neo-db-get-pr (_branch-name)
  "Return nil — no GitHub PRs in Phase 2."
  nil)

(defun neo-db-get-issue (_branch-name)
  "Return nil — branch-to-issue mapping is Phase 4."
  nil)

(defun neo-db-get-repository-id (_full-name)
  "Return nil — repository lookup uses beads workspace in Phase 2."
  nil)

(defun neo-stack-summary (_stack-id)
  "Return empty summary — stacks-as-epics is Phase 3.
STACK-ID is ignored."
  (list :branches 0 :open-prs 0 :merged 0 :ci-passed 0 :ci-failed 0 :ci-skipped 0))

(provide 'neo-workflow-db)
;;; neo-workflow-db.el ends here
