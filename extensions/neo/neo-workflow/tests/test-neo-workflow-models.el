;;; tests/test-neo-workflow-models.el --- Tests for neo-workflow-models -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)

;; Mock beads-client so we can test without the sibling extension on the load-path.
(unless (featurep 'beads-client)
  (defun beads-client-list () nil)
  (defun beads-client-show (_id) nil)
  (defun beads-client-create (_title &rest _args) nil)
  (defun beads-client-update (_id &rest _args) nil)
  (defun beads-client-close (_id) nil)
  (defun beads-client--workspace-info () nil)
  (defun beads-client--project-root () nil)
  (defun beads-client-clear-cache () nil)
  (provide 'beads-client))

;; Mock neo-workflow-git so models.el can load without git subprocess calls.
(unless (featurep 'neo-workflow-git)
  (provide 'neo-workflow-git))

(require 'neo-workflow-models)

;; ============================================================
;; Test helpers
;; ============================================================

(defun neo--test-make-label (name)
  "Return a minimal neo-label with NAME."
  (make-neo-label :id nil :name name :color nil :description nil :repository-id "test"))

(defun neo--test-make-issue (&rest label-names)
  "Return a minimal neo-issue with the given LABEL-NAMES."
  (make-neo-issue
   :id "omega-1"
   :number 1
   :title "Test"
   :type "task"
   :labels (mapcar #'neo--test-make-label label-names)
   :state 'open
   :draft 0
   :created-at nil
   :updated-at nil
   :closed-at nil
   :merged-at nil
   :repository-id "test"
   :stack nil
   :ui-state nil))

;; ============================================================
;; neo--beads-issue-number
;; ============================================================

(describe "neo--beads-issue-number"
  (it "extracts the numeric suffix from an omega ID"
    (expect (neo--beads-issue-number "omega-42") :to-equal 42))

  (it "handles a single-digit suffix"
    (expect (neo--beads-issue-number "omega-1") :to-equal 1))

  (it "handles a large number"
    (expect (neo--beads-issue-number "omega-9999") :to-equal 9999))

  (it "returns 0 for nil"
    (expect (neo--beads-issue-number nil) :to-equal 0))

  (it "returns 0 for a string with no digits"
    (expect (neo--beads-issue-number "no-digits") :to-equal 0))

  (it "works with other prefix formats"
    (expect (neo--beads-issue-number "proj-123") :to-equal 123)))

;; ============================================================
;; neo--beads-state-to-symbol
;; ============================================================

(describe "neo--beads-state-to-symbol"
  (it "maps open states to 'open"
    (expect (neo--beads-state-to-symbol "open") :to-equal 'open)
    (expect (neo--beads-state-to-symbol "in_progress") :to-equal 'open)
    (expect (neo--beads-state-to-symbol "blocked") :to-equal 'open)
    (expect (neo--beads-state-to-symbol "ready") :to-equal 'open)
    (expect (neo--beads-state-to-symbol "in-progress") :to-equal 'open))

  (it "maps closed states to 'closed"
    (expect (neo--beads-state-to-symbol "closed") :to-equal 'closed)
    (expect (neo--beads-state-to-symbol "done") :to-equal 'closed)
    (expect (neo--beads-state-to-symbol "cancelled") :to-equal 'closed)
    (expect (neo--beads-state-to-symbol "wont-fix") :to-equal 'closed)
    (expect (neo--beads-state-to-symbol "wont_fix") :to-equal 'closed))

  (it "defaults unknown states to 'open"
    (expect (neo--beads-state-to-symbol "weirdstate") :to-equal 'open)))

;; ============================================================
;; neo--beads-labels-to-neo-labels
;; ============================================================

(describe "neo--beads-labels-to-neo-labels"
  (it "returns empty list for nil"
    (expect (neo--beads-labels-to-neo-labels nil "repo-id") :to-equal nil))

  (it "converts a list of strings"
    (let ((result (neo--beads-labels-to-neo-labels '("bug" "high") "repo-id")))
      (expect (length result) :to-equal 2)
      (expect (neo-label-name (car result)) :to-equal "bug")
      (expect (neo-label-name (cadr result)) :to-equal "high")
      (expect (neo-label-repository-id (car result)) :to-equal "repo-id")))

  (it "converts a vector of strings (JSON array)"
    (let ((result (neo--beads-labels-to-neo-labels ["bug" "low"] "wksp")))
      (expect (length result) :to-equal 2)
      (expect (neo-label-name (car result)) :to-equal "bug")))

  (it "converts a list of alists with a name key"
    (let* ((alist-labels (list '((name . "feature")) '((name . "high"))))
           (result (neo--beads-labels-to-neo-labels alist-labels "wksp")))
      (expect (length result) :to-equal 2)
      (expect (neo-label-name (car result)) :to-equal "feature"))))

;; ============================================================
;; neo--beads-alist-to-neo-issue
;; ============================================================

(describe "neo--beads-alist-to-neo-issue"
  (it "converts a minimal beads issue alist"
    (let* ((alist '((id . "omega-7")
                    (title . "My issue")
                    (issue_type . "task")
                    (status . "open")
                    (labels . nil)))
           (issue (neo--beads-alist-to-neo-issue alist "workspace-id")))
      (expect (neo-issue-id issue) :to-equal "omega-7")
      (expect (neo-issue-number issue) :to-equal 7)
      (expect (neo-issue-title issue) :to-equal "My issue")
      (expect (neo-issue-type issue) :to-equal "task")
      (expect (neo-issue-state issue) :to-equal 'open)
      (expect (neo-issue-repository-id issue) :to-equal "workspace-id")
      (expect (neo-issue-labels issue) :to-equal nil)
      (expect (neo-issue-stack issue) :to-equal nil)
      (expect (neo-issue-draft issue) :to-equal 0)))

  (it "maps closed status correctly"
    (let* ((alist '((id . "omega-9") (title . "Done") (status . "done") (labels . nil)))
           (issue (neo--beads-alist-to-neo-issue alist "wksp")))
      (expect (neo-issue-state issue) :to-equal 'closed)))

  (it "defaults empty title to empty string"
    (let* ((alist '((id . "omega-3") (status . "open") (labels . nil)))
           (issue (neo--beads-alist-to-neo-issue alist "wksp")))
      (expect (neo-issue-title issue) :to-equal "")))

  (it "attaches labels"
    (let* ((alist '((id . "omega-5")
                    (title . "Labelled")
                    (status . "open")
                    (labels . ("bug" "high"))))
           (issue (neo--beads-alist-to-neo-issue alist "wksp")))
      (expect (length (neo-issue-labels issue)) :to-equal 2)
      (expect (neo-label-name (car (neo-issue-labels issue))) :to-equal "bug")))

  (it "converts string-keyed alists (from JSON parse)"
    (let* ((alist (list (cons "id" "omega-11")
                        (cons "title" "JSON style")
                        (cons "status" "blocked")
                        (cons "labels" nil)))
           (issue (neo--beads-alist-to-neo-issue alist "wksp")))
      (expect (neo-issue-id issue) :to-equal "omega-11")
      (expect (neo-issue-title issue) :to-equal "JSON style")
      (expect (neo-issue-state issue) :to-equal 'open))))

;; ============================================================
;; neo--project-to-repository
;; ============================================================

(describe "neo--project-to-repository"
  (it "converts a neo-project to a neo-repository"
    (let* ((project (make-neo-project
                     :id "/home/user/projects/myrepo"
                     :repo "myrepo"
                     :type "beads"
                     :pr-number nil
                     :worktree-path "/home/user/projects/myrepo"
                     :stacks nil))
           (repo (neo--project-to-repository project)))
      (expect (neo-repository-id repo) :to-equal "/home/user/projects/myrepo")
      (expect (neo-repository-full-name repo) :to-equal "myrepo")
      (expect (neo-repository-fork repo) :to-equal 0)
      (expect (neo-repository-visibility repo) :to-equal "private")
      (expect (neo-repository-default-branch repo) :to-equal "main"))))

;; ============================================================
;; Phase 3: stacks are beads epics
;; ============================================================

;; A small board fixture: one top-level epic with a child epic and a child
;; task, plus an orphan task with no parent.
(defvar neo--test-board-issues
  '(((id . "omega-100") (title . "Build the thing") (issue_type . "epic") (status . "open"))
    ((id . "omega-101") (title . "Sub effort") (issue_type . "epic") (status . "open") (parent . "omega-100"))
    ((id . "omega-102") (title . "Do a task") (issue_type . "task") (status . "open") (parent . "omega-100"))
    ((id . "omega-103") (title . "Orphan task") (issue_type . "task") (status . "open")))
  "Fixture issue list mimicking `beads-client-list' output.")

(describe "neo--beads-issue-parent / -epic-p / -type-string"
  (it "returns the parent id when present"
    (expect (neo--beads-issue-parent '((id . "x") (parent . "omega-100")))
            :to-equal "omega-100"))
  (it "returns nil for a missing or empty parent"
    (expect (neo--beads-issue-parent '((id . "x"))) :to-be nil)
    (expect (neo--beads-issue-parent '((id . "x") (parent . ""))) :to-be nil))
  (it "recognises epics"
    (expect (neo--beads-issue-epic-p '((issue_type . "epic"))) :to-be-truthy)
    (expect (neo--beads-issue-epic-p '((issue_type . "task"))) :to-be nil))
  (it "defaults the type to task"
    (expect (neo--beads-issue-type-string '((id . "x"))) :to-equal "task")))

(describe "neo--workflow-epic-stack-name"
  (it "builds a <number>-<slug> name for a numeric beads id"
    (expect (neo--workflow-epic-stack-name
             '((id . "omega-100") (title . "Build the thing")))
            :to-equal "100-build-the-thing"))
  (it "falls back to the full id for a hash-style beads id"
    (expect (neo--workflow-epic-stack-name
             '((id . "omega-11sv") (title . "Sub effort")))
            :to-equal "omega-11sv-sub-effort")))

(describe "neo--workflow-stacks-from-issues"
  (before-each
    ;; No git branches in the unit environment; branch loading degrades to nil.
    (spy-on 'neo-load-branch-from-git :and-return-value nil))

  (it "returns one top-level stack per top-level epic"
    (let ((stacks (neo--workflow-stacks-from-issues neo--test-board-issues "wksp")))
      (expect (length stacks) :to-equal 1)
      (expect (neo-stack-id (car stacks)) :to-equal "omega-100")
      (expect (neo-stack-name (car stacks)) :to-equal "100-build-the-thing")
      (expect (neo-stack-title (car stacks)) :to-equal "Build the thing")
      (expect (neo-stack-issue-id (car stacks)) :to-equal "omega-100")))

  (it "nests a child epic under its parent stack, not at top level"
    (let* ((stacks (neo--workflow-stacks-from-issues neo--test-board-issues "wksp"))
           (children (neo-stack-children-stacks (car stacks))))
      (expect (length children) :to-equal 1)
      (expect (neo-stack-id (car children)) :to-equal "omega-101")
      (expect (neo-stack-name (car children)) :to-equal "101-sub-effort")))

  (it "ignores non-epic issues when building stacks"
    (let ((stacks (neo--workflow-stacks-from-issues neo--test-board-issues "wksp")))
      (expect (neo--workflow-flatten-stacks stacks) :to-have-same-items-as
              (neo--workflow-flatten-stacks stacks))
      (expect (length (neo--workflow-flatten-stacks stacks)) :to-equal 2)))

  (it "returns nil when there are no epics"
    (expect (neo--workflow-stacks-from-issues
             '(((id . "omega-1") (issue_type . "task"))) "wksp")
            :to-be nil)))

(describe "neo-db-get-issues-for-repo (stack attachment)"
  (before-each
    (spy-on 'beads-client-list :and-return-value neo--test-board-issues)
    (spy-on 'neo-load-branch-from-git :and-return-value nil))

  (it "excludes epics and returns the remaining issues"
    (let ((issues (neo-db-get-issues-for-repo "wksp")))
      (expect (length issues) :to-equal 2)
      (expect (mapcar #'neo-issue-id issues) :to-have-same-items-as
              '("omega-102" "omega-103"))))

  (it "attaches the parent epic's stack to a child issue"
    (let* ((issues (neo-db-get-issues-for-repo "wksp"))
           (child (seq-find (lambda (i) (equal (neo-issue-id i) "omega-102")) issues))
           (orphan (seq-find (lambda (i) (equal (neo-issue-id i) "omega-103")) issues)))
      (expect (neo-issue-stack child) :not :to-be nil)
      (expect (neo-stack-id (neo-issue-stack child)) :to-equal "omega-100")
      (expect (neo-issue-stack orphan) :to-be nil))))

(describe "neo-load-stack"
  (before-each
    (spy-on 'beads-client-list :and-return-value neo--test-board-issues)
    (spy-on 'neo-load-branch-from-git :and-return-value nil))

  (it "loads a top-level stack by name"
    (let ((stack (neo-load-stack "100-build-the-thing" "wksp")))
      (expect (neo-stack-id stack) :to-equal "omega-100")))

  (it "finds a nested child stack by name"
    (let ((stack (neo-load-stack "101-sub-effort" "wksp")))
      (expect (neo-stack-id stack) :to-equal "omega-101")))

  (it "returns nil for an unknown stack name"
    (expect (neo-load-stack "999-nope" "wksp") :to-be nil)))
