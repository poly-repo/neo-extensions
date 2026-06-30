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
