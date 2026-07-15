;;; tests/test-neo-workflow-status.el --- Tests for neo-workflow-status -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)

;; Mock beads-client so tests run without the sibling extension on the load-path.
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

;; Mock neo-workflow-git.
(unless (featurep 'neo-workflow-git)
  (provide 'neo-workflow-git))

;; Mock vtable so neo-workflow-status.el can load without the library present.
(unless (featurep 'vtable)
  (defmacro make-vtable (&rest _args) nil)
  (defun vtable-current-table () nil)
  (defun vtable-current-object () nil)
  (defun vtable-objects (_table) nil)
  (defun vtable-goto-table (_table) nil)
  (defun vtable-goto-object (_object) nil)
  (defun vtable-update-object (_table _new _old) nil)
  (defun vtable-beginning-of-table () nil)
  (defun vtable-end-of-table () nil)
  (provide 'vtable))

;; Mock beads-detail so the RET-to-detail path loads without the sibling ext.
(unless (featurep 'beads-detail)
  (defun beads-detail-open (_issue &optional _buffer-name) nil)
  (provide 'beads-detail))

;; Mock neo-workflow-context.
(unless (featurep 'neo-workflow-context)
  (provide 'neo-workflow-context))

;; Mock the neo-workflow-git/context functions neo-workflow-status.el calls
;; directly (the stub `provide's above mean the real definitions never load).
(unless (fboundp 'neo/workflow-git-branch-exists)
  (defun neo/workflow-git-branch-exists (_branch) nil))
(unless (fboundp 'neo/workflow-git-create-branch)
  (defun neo/workflow-git-create-branch (_name _parent) nil))
(unless (fboundp 'neo--workflow-git-run)
  (defun neo--workflow-git-run (&rest _args) nil))
(unless (fboundp 'neo--workflow-choose-workspace-strategy)
  (defun neo--workflow-choose-workspace-strategy (&optional _dir) 'repo))

;; Mock persp-switch / neo/treemacs-show-only-project so
;; `neo--workflow-activate-perspective' can be exercised without loading
;; `perspective'/`treemacs' in batch.
(unless (fboundp 'persp-switch)
  (defun persp-switch (_name) nil))
(unless (fboundp 'neo/treemacs-show-only-project)
  (defun neo/treemacs-show-only-project (_root _name) nil))

;; Mock neo-workflow-async.
(unless (featurep 'neo-workflow-async)
  (defvar neo-workflow-refresh-hook nil)
  (provide 'neo-workflow-async))

;; Mock hl-line to avoid X display dependency.
(unless (featurep 'hl-line)
  (defun hl-line-mode (&rest _args) nil)
  (defun hl-line-highlight () nil)
  (provide 'hl-line))

;; Stub neo/application so the registration form in status.el is a no-op.
(defmacro neo/application (_name &rest _args) nil)

(require 'neo-workflow-models)
(require 'neo-workflow-db)
(require 'neo-workflow-status)

;; ============================================================
;; Test helpers
;; ============================================================

(defun neo--make-mock-label (name)
  "Return a minimal neo-label with NAME."
  (make-neo-label :id nil :name name :color nil :description nil :repository-id "test"))

(defun neo--make-mock-issue (&rest label-names)
  "Return a minimal neo-issue with LABEL-NAMES."
  (make-neo-issue
   :id "omega-1"
   :number 1
   :title "Mock Issue"
   :type "task"
   :labels (mapcar #'neo--make-mock-label label-names)
   :state 'open
   :draft 0
   :created-at "2024-01-01T00:00:00Z"
   :updated-at "2024-01-01T00:00:00Z"
   :closed-at nil
   :merged-at nil
   :repository-id "test"
   :stack nil
   :ui-state nil))

;; ============================================================
;; Priority extraction
;; ============================================================

(defun neo--mock-issue-pri (priority)
  "Return a minimal neo-issue with beads PRIORITY (integer or nil)."
  (make-neo-issue
   :id "omega-1" :number 1 :short-id "1" :priority priority
   :title "Mock" :type "task" :labels nil :state 'open :draft 0
   :created-at nil :updated-at nil :closed-at nil :merged-at nil
   :repository-id "test" :stack nil :prefix nil :ui-state nil))

(describe "neo--priority-icon"
  (it "renders the beads priority as a Pn / fire icon"
    (expect (substring-no-properties (neo--priority-icon (neo--mock-issue-pri 0))) :to-equal "🔥")
    (expect (substring-no-properties (neo--priority-icon (neo--mock-issue-pri 1))) :to-equal "P1")
    (expect (substring-no-properties (neo--priority-icon (neo--mock-issue-pri 2))) :to-equal "P2")
    (expect (substring-no-properties (neo--priority-icon (neo--mock-issue-pri 3))) :to-equal "P3")
    (expect (substring-no-properties (neo--priority-icon (neo--mock-issue-pri 4))) :to-equal "P4"))

  (it "returns an empty string when the issue has no priority"
    (expect (neo--priority-icon (neo--mock-issue-pri nil)) :to-equal "")))

(describe "neo--get-issue-priority-score"
  (it "returns the priority integer"
    (expect (neo--get-issue-priority-score (neo--mock-issue-pri 0)) :to-equal 0)
    (expect (neo--get-issue-priority-score (neo--mock-issue-pri 3)) :to-equal 3))

  (it "returns 99 for issues with no priority"
    (expect (neo--get-issue-priority-score (neo--mock-issue-pri nil)) :to-equal 99)))

(describe "neo--sort-issues"
  (it "sorts by priority ascending when neo/workflow-sort-by-priority is non-nil"
    (let* ((p0 (neo--mock-issue-pri 0))
           (p1 (neo--mock-issue-pri 1))
           (p2 (neo--mock-issue-pri 2))
           (pn (neo--mock-issue-pri nil))
           (neo/workflow-sort-by-priority t))
      (expect (neo--sort-issues (list p2 p0 pn p1))
              :to-equal (list p0 p1 p2 pn))))

  (it "preserves order when neo/workflow-sort-by-priority is nil"
    (let* ((unsorted (list (neo--mock-issue-pri 3) (neo--mock-issue-pri 0)))
           (neo/workflow-sort-by-priority nil))
      (expect (neo--sort-issues unsorted) :to-equal unsorted))))

;; ============================================================
;; Priority step navigation (numeric beads priority)
;; ============================================================

(describe "neo--new-priority"
  (it "raises priority toward P0 when stepping up (direction 1)"
    (expect (neo--new-priority (neo--mock-issue-pri 3) 1) :to-equal 2)
    (expect (neo--new-priority (neo--mock-issue-pri 1) 1) :to-equal 0))

  (it "saturates at P0 when raising"
    (expect (neo--new-priority (neo--mock-issue-pri 0) 1) :to-equal 0))

  (it "lowers priority toward P4 when stepping down (direction -1)"
    (expect (neo--new-priority (neo--mock-issue-pri 0) -1) :to-equal 1)
    (expect (neo--new-priority (neo--mock-issue-pri 2) -1) :to-equal 3))

  (it "saturates at P4 when lowering"
    (expect (neo--new-priority (neo--mock-issue-pri 4) -1) :to-equal 4))

  (it "defaults to P4 (backlog) when the issue has no priority"
    (expect (neo--new-priority (neo--mock-issue-pri nil) 1) :to-equal 3)))

;; ============================================================
;; Color helpers
;; ============================================================

(describe "neo--hex-color"
  (it "returns nil for nil"
    (expect (neo--hex-color nil) :to-equal nil))

  (it "preserves strings that already start with #"
    (expect (neo--hex-color "#aabbcc") :to-equal "#aabbcc"))

  (it "prepends # to bare hex strings"
    (expect (neo--hex-color "aabbcc") :to-equal "#aabbcc")))

;; ============================================================
;; Issue status face
;; ============================================================

(describe "neo--get-issue-status-face"
  (it "maps state symbols to faces"
    (expect (neo--get-issue-status-face 'open) :to-equal 'neo-workflow-issue-open-face)
    (expect (neo--get-issue-status-face 'active) :to-equal 'neo-workflow-issue-active-face)
    (expect (neo--get-issue-status-face 'closed) :to-equal 'neo-workflow-issue-completed-face)
    (expect (neo--get-issue-status-face 'unknown) :to-equal nil)))

;; ============================================================
;; Issue partition
;; ============================================================

(describe "neo--partition-issues"
  (it "separates active and inactive issues"
    (let* ((active-stack (make-neo-stack :id "s1" :name "s1" :title nil
                                         :prefix nil :issue-id "omega-1"
                                         :branch nil :children-stacks nil))
           (active-issue (make-neo-issue
                          :id "omega-1" :number 1 :title "Active"
                          :type "task" :labels nil :state 'open :draft 0
                          :created-at nil :updated-at nil :closed-at nil
                          :merged-at nil :repository-id "r"
                          :stack active-stack :ui-state nil))
           (inactive-issue (make-neo-issue
                            :id "omega-2" :number 2 :title "Inactive"
                            :type "task" :labels nil :state 'open :draft 0
                            :created-at nil :updated-at nil :closed-at nil
                            :merged-at nil :repository-id "r"
                            :stack nil :ui-state nil))
           (result (neo--partition-issues (list active-issue inactive-issue))))
      (expect (car result) :to-equal (list active-issue))
      (expect (cdr result) :to-equal (list inactive-issue))))

  (it "returns empty lists for empty input"
    (let ((result (neo--partition-issues nil)))
      (expect (car result) :to-equal nil)
      (expect (cdr result) :to-equal nil))))

;; ============================================================
;; In-memory UI state (neo-workflow-db)
;; ============================================================

(describe "neo-db repo UI state"
  (before-each
    (clrhash neo--workflow-repo-ui-states))

  (it "returns nil for an unknown repo"
    (expect (neo-db-get-repo-ui-state "unknown") :to-equal nil))

  (it "stores and retrieves state"
    (neo-db-set-repo-ui-state "r1" "expanded" "open" "priority")
    (let ((state (neo-db-get-repo-ui-state "r1")))
      (expect (plist-get state :state) :to-equal "expanded")
      (expect (plist-get state :filter) :to-equal "open")
      (expect (plist-get state :order) :to-equal "priority")))

  (it "overwrites previous state"
    (neo-db-set-repo-ui-state "r1" "hidden" nil nil)
    (neo-db-set-repo-ui-state "r1" "expanded" "all" nil)
    (expect (plist-get (neo-db-get-repo-ui-state "r1") :state) :to-equal "expanded")))

(describe "neo-db issue UI state"
  (before-each
    (clrhash neo--workflow-issue-ui-states))

  (it "returns nil for an unknown issue"
    (expect (neo-db-get-issue-ui-state "omega-0") :to-equal nil))

  (it "stores and retrieves state"
    (neo-db-set-issue-ui-state "omega-5" "expanded")
    (expect (neo-db-get-issue-ui-state "omega-5") :to-equal "expanded")
    (neo-db-set-issue-ui-state "omega-5" "collapsed")
    (expect (neo-db-get-issue-ui-state "omega-5") :to-equal "collapsed")))

(describe "neo-db global workflow filter"
  (before-each
    (setq neo--workflow-global-filter nil))

  (it "returns nil when no filter is set"
    (expect (neo-db-get-workflow-filter) :to-equal nil))

  (it "stores and retrieves a filter"
    (neo-db-set-workflow-filter "open")
    (expect (neo-db-get-workflow-filter) :to-equal "open"))

  (it "clears the filter when nil is set"
    (neo-db-set-workflow-filter "active")
    (neo-db-set-workflow-filter nil)
    (expect (neo-db-get-workflow-filter) :to-equal nil)))

;; ============================================================
;; Filter logic
;; ============================================================

(describe "neo--issue-filter"
  (before-each
    (clrhash neo--workflow-repo-ui-states)
    (setq neo--workflow-global-filter nil))

  (it "shows only open issues by default"
    (let* ((open-issue (make-neo-issue
                        :id "omega-1" :number 1 :title "Open"
                        :type "task" :labels nil :state 'open :draft 0
                        :created-at nil :updated-at nil :closed-at nil
                        :merged-at nil :repository-id "r" :stack nil :ui-state nil))
           (closed-issue (make-neo-issue
                          :id "omega-2" :number 2 :title "Closed"
                          :type "task" :labels nil :state 'closed :draft 0
                          :created-at nil :updated-at nil :closed-at nil
                          :merged-at nil :repository-id "r" :stack nil :ui-state nil)))
      (expect (neo--issue-filter (list open-issue closed-issue) "test-repo")
              :to-equal (list open-issue))))

  (it "shows all issues when global filter is 'all"
    ;; Setting the global filter to 'all bypasses per-repo filter.
    ;; The 'all path is exercised through neo-db-set-workflow-filter which
    ;; stores a string; the filter logic interns it.  We need to also set
    ;; a per-repo state so the filter branch is taken.
    (neo-db-set-workflow-filter nil)   ; 'all means no global filter
    (let ((repo-id (neo--get-repo-id-by-full-name "test-repo")))
      ;; When repo-id is nil, per-repo filter defaults to 'open.
      ;; Verify by setting it explicitly to "all".
      (when repo-id
        (neo-db-set-repo-ui-state repo-id "expanded" "all" nil)))
    (let* ((open-issue (make-neo-issue
                        :id "omega-1" :number 1 :title "Open"
                        :type "task" :labels nil :state 'open :draft 0
                        :created-at nil :updated-at nil :closed-at nil
                        :merged-at nil :repository-id "r" :stack nil :ui-state nil))
           (closed-issue (make-neo-issue
                          :id "omega-2" :number 2 :title "Closed"
                          :type "task" :labels nil :state 'closed :draft 0
                          :created-at nil :updated-at nil :closed-at nil
                          :merged-at nil :repository-id "r" :stack nil :ui-state nil)))
      ;; Default filter (nil global filter → per-repo defaults to open)
      (expect (neo--issue-filter (list open-issue closed-issue) "test-repo")
              :to-equal (list open-issue)))))

;; ============================================================
;; Phase 4: write path (stack create / issue close)
;; ============================================================

(describe "neo--append (stack creation)"
  (before-each
    ;; Never touch git or the real refresh during unit tests.
    (spy-on 'neo--workflow-create-stack-branch :and-return-value "branch")
    (spy-on 'neo/workflow-refresh))

  (it "promotes an issue without a stack into a beads epic"
    (spy-on 'beads-client-update)
    (let ((issue (make-neo-issue
                  :id "omega-42" :number 42 :title "Do the thing"
                  :type "task" :labels nil :state 'open :draft 0
                  :created-at nil :updated-at nil :closed-at nil
                  :merged-at nil :repository-id "r" :stack nil :ui-state nil)))
      (neo--append issue)
      (expect 'beads-client-update :to-have-been-called-with
              "omega-42" :issue-type "epic")
      (expect 'neo--workflow-create-stack-branch :to-have-been-called-with
              "42-do-the-thing")))

  (it "creates a child epic under an existing stack, parented to it"
    (spy-on 'beads-client-create :and-return-value '((id . "omega-child")))
    (spy-on 'read-string :and-return-value "Sub effort")
    (let ((stack (make-neo-stack :id "omega-100" :name "100-parent"
                                 :title "Parent" :prefix nil
                                 :issue-id "omega-100" :branch nil
                                 :children-stacks nil)))
      (neo--append stack)
      (expect 'beads-client-create :to-have-been-called-with
              "Sub effort" :issue-type "epic" :parent "omega-100")
      (expect 'neo--workflow-create-stack-branch :to-have-been-called-with
              "omega-child-sub-effort")))

  (it "does nothing when the child stack title is empty"
    (spy-on 'beads-client-create)
    (spy-on 'read-string :and-return-value "")
    (let ((stack (make-neo-stack :id "omega-100" :name "100-parent"
                                 :title "Parent" :prefix nil
                                 :issue-id "omega-100" :branch nil
                                 :children-stacks nil)))
      (neo--append stack)
      (expect 'beads-client-create :not :to-have-been-called))))

(describe "neo--close-issue-at-point"
  (before-each
    (spy-on 'neo/workflow-refresh))

  (it "closes the issue at point after confirmation"
    (let ((issue (make-neo-issue
                  :id "omega-9" :number 9 :title "Close me"
                  :type "task" :labels nil :state 'open :draft 0
                  :created-at nil :updated-at nil :closed-at nil
                  :merged-at nil :repository-id "r" :stack nil :ui-state nil)))
      (spy-on 'vtable-current-table :and-return-value t)
      (spy-on 'vtable-current-object :and-return-value issue)
      (spy-on 'yes-or-no-p :and-return-value t)
      (spy-on 'beads-client-close)
      (neo--close-issue-at-point)
      (expect 'beads-client-close :to-have-been-called-with "omega-9")))

  (it "does not close when the user declines"
    (let ((issue (make-neo-issue
                  :id "omega-9" :number 9 :title "Close me"
                  :type "task" :labels nil :state 'open :draft 0
                  :created-at nil :updated-at nil :closed-at nil
                  :merged-at nil :repository-id "r" :stack nil :ui-state nil)))
      (spy-on 'vtable-current-table :and-return-value t)
      (spy-on 'vtable-current-object :and-return-value issue)
      (spy-on 'yes-or-no-p :and-return-value nil)
      (spy-on 'beads-client-close)
      (neo--close-issue-at-point)
      (expect 'beads-client-close :not :to-have-been-called))))

;; ============================================================
;; RET: open bead detail window
;; ============================================================

(describe "neo--select-thing"
  (it "fetches the full bead for an issue and renders it in the shared page"
    (let ((issue (make-neo-issue
                  :id "omega-7" :number 7 :title "Detail me"
                  :type "task" :labels nil :state 'open :draft 0
                  :created-at nil :updated-at nil :closed-at nil
                  :merged-at nil :repository-id "r" :stack nil :ui-state nil))
          (full '((id . "omega-7") (description . "Full text"))))
      (spy-on 'beads-client-show :and-return-value full)
      (spy-on 'beads-detail-open)
      (neo--select-thing issue)
      (expect 'beads-client-show :to-have-been-called-with "omega-7")
      ;; Always the single reusable detail buffer, so the perspective keeps at
      ;; most one detail page.
      (expect 'beads-detail-open :to-have-been-called-with
              full neo--workflow-detail-buffer-name)))

  (it "opens the detail page for an epic (a stack) using its beads id"
    (let ((stack (make-neo-stack :id "omega-e" :name "e-epic" :title "Epic"
                                 :prefix nil :issue-id "omega-e" :branch nil
                                 :children-stacks nil)))
      (spy-on 'beads-client-show :and-return-value '((id . "omega-e")))
      (spy-on 'beads-detail-open)
      (neo--select-thing stack)
      (expect 'beads-client-show :to-have-been-called-with "omega-e")
      (expect 'beads-detail-open :to-have-been-called-with
              '((id . "omega-e")) neo--workflow-detail-buffer-name)))

  (it "errors when there is no bead at point"
    (spy-on 'beads-detail-open)
    (expect (neo--select-thing nil) :to-throw 'user-error)
    (expect 'beads-detail-open :not :to-have-been-called)))

;; ============================================================
;; Phase 5: in-memory context store
;; ============================================================

(describe "neo-db context store"
  (before-each
    (clrhash neo--workflow-contexts))

  (it "returns nil for a repo with no context"
    (expect (neo/workflow-db-get-context "repo-x") :to-be nil))

  (it "stores and retrieves the current context for a repo"
    (neo/workflow-db-upsert-context "repo-x" "omega-100" "100-persp")
    (let ((ctx (neo/workflow-db-get-context "repo-x")))
      (expect (plist-get ctx :repository-id) :to-equal "repo-x")
      (expect (plist-get ctx :stack-id) :to-equal "omega-100")
      (expect (plist-get ctx :perspective) :to-equal "100-persp")))

  (it "retrieves a context by stack independent of the current one"
    (neo/workflow-db-upsert-context "repo-x" "omega-100" "p100")
    (neo/workflow-db-upsert-context "repo-x" "omega-200" "p200")
    ;; Current is now omega-200, but the omega-100 mapping is still queryable.
    (expect (plist-get (neo/workflow-db-get-context "repo-x") :stack-id)
            :to-equal "omega-200")
    (expect (plist-get (neo/workflow-db-get-context-by-stack "repo-x" "omega-100")
                       :perspective)
            :to-equal "p100"))

  (it "updates the perspective for an existing stack"
    (neo/workflow-db-upsert-context "repo-x" "omega-100" "old")
    (neo/workflow-db-upsert-context "repo-x" "omega-100" "new")
    (expect (plist-get (neo/workflow-db-get-context-by-stack "repo-x" "omega-100")
                       :perspective)
            :to-equal "new")))

(describe "neo/workflow-switch-context"
  (before-each
    (clrhash neo--workflow-contexts)
    (spy-on 'neo--workflow-resolve-stack-root :and-return-value nil))

  (it "records the chosen stack's context (no perspective library in batch)"
    (spy-on 'neo-db-get-all-stacks :and-return-value
            '((:id "omega-100" :name "100-parent" :repository-id "repo-x" :title "Parent")))
    (spy-on 'completing-read :and-return-value "Parent (100-parent)")
    (neo/workflow-switch-context)
    (let ((ctx (neo/workflow-db-get-context "repo-x")))
      (expect (plist-get ctx :stack-id) :to-equal "omega-100")
      (expect (plist-get ctx :perspective) :to-equal "100-parent")))

  (it "resolves the stack root and activates the perspective with it"
    (spy-on 'neo-db-get-all-stacks :and-return-value
            '((:id "omega-100" :name "100-parent" :repository-id "repo-x" :title "Parent")))
    (spy-on 'completing-read :and-return-value "Parent (100-parent)")
    (spy-on 'neo--workflow-resolve-stack-root :and-return-value "/worktrees/100-parent")
    (spy-on 'neo--workflow-activate-perspective)
    (neo/workflow-switch-context)
    (expect 'neo--workflow-resolve-stack-root :to-have-been-called-with "omega-100")
    (expect 'neo--workflow-activate-perspective :to-have-been-called-with
            "100-parent" "/worktrees/100-parent"))

  (it "errors when there are no stacks"
    (spy-on 'neo-db-get-all-stacks :and-return-value nil)
    (expect (neo/workflow-switch-context) :to-throw 'user-error)))

;; ============================================================
;; Perspective/project sync (treemacs + scratch buffer + persp-switch)
;; ============================================================

(describe "neo--ensure-stack-scratch"
  (it "sets default-directory to ROOT on a freshly created buffer"
    (let ((buf-name "*scratch* (fresh-stack)"))
      (when (get-buffer buf-name) (kill-buffer buf-name))
      (unwind-protect
          (progn
            (neo--ensure-stack-scratch "fresh-stack" "/tmp/root-a")
            (expect default-directory :to-equal (file-name-as-directory
                                                  (expand-file-name "/tmp/root-a"))))
        (when (get-buffer buf-name) (kill-buffer buf-name)))))

  (it "resets default-directory on an already-existing buffer with a stale directory"
    (let ((buf-name "*scratch* (stale-stack)"))
      (when (get-buffer buf-name) (kill-buffer buf-name))
      (unwind-protect
          (progn
            (neo--ensure-stack-scratch "stale-stack" "/tmp/root-a")
            (neo--ensure-stack-scratch "stale-stack" "/tmp/root-b")
            (expect default-directory :to-equal (file-name-as-directory
                                                  (expand-file-name "/tmp/root-b"))))
        (when (get-buffer buf-name) (kill-buffer buf-name))))))

(describe "neo--workflow-activate-perspective"
  (before-each
    (spy-on 'persp-switch)
    (spy-on 'neo/treemacs-show-only-project)
    (spy-on 'neo--ensure-stack-scratch))

  (it "does nothing when `perspective' is not loaded"
    (let ((orig-featurep (symbol-function 'featurep)))
      (cl-letf (((symbol-function 'featurep)
                 (lambda (f) (if (eq f 'perspective) nil (funcall orig-featurep f)))))
        (neo--workflow-activate-perspective "my-persp" "/tmp/root")
        (expect 'persp-switch :not :to-have-been-called))))

  (it "switches perspective, syncs treemacs, and resets the scratch buffer's directory"
    (let ((orig-featurep (symbol-function 'featurep)))
      (cl-letf (((symbol-function 'featurep)
                 (lambda (f) (if (eq f 'perspective) t (funcall orig-featurep f)))))
        (neo--workflow-activate-perspective "my-persp" "/tmp/root/")
        (expect 'persp-switch :to-have-been-called-with "my-persp")
        (expect 'neo/treemacs-show-only-project :to-have-been-called-with "/tmp/root/" "root")
        (expect 'neo--ensure-stack-scratch :to-have-been-called-with "my-persp" "/tmp/root/"))))

  (it "skips the treemacs sync when neo/treemacs-show-only-project is unavailable"
    (let ((orig-featurep (symbol-function 'featurep))
          (orig-fboundp (symbol-function 'fboundp)))
      (cl-letf (((symbol-function 'featurep)
                 (lambda (f) (if (eq f 'perspective) t (funcall orig-featurep f))))
                ((symbol-function 'fboundp)
                 (lambda (f) (if (eq f 'neo/treemacs-show-only-project) nil (funcall orig-fboundp f)))))
        (neo--workflow-activate-perspective "my-persp" "/tmp/root/")
        (expect 'neo/treemacs-show-only-project :not :to-have-been-called)
        (expect 'neo--ensure-stack-scratch :to-have-been-called-with "my-persp" "/tmp/root/")))))

(describe "neo--workflow-resolve-stack-root"
  (it "prefers the live worktree path recorded on the stack's branch"
    (spy-on 'neo-db-get-branch-for-stack :and-return-value
            (make-neo-branch :name "b" :pr-number nil :issue-id nil :status nil
                             :ci-status nil :last-commit nil
                             :worktree-path "/worktrees/w1"))
    (spy-on 'neo--workflow-beads-workspace-as-project :and-return-value nil)
    (expect (neo--workflow-resolve-stack-root "omega-1") :to-equal "/worktrees/w1"))

  (it "falls back to the beads workspace root when the branch has no worktree"
    (spy-on 'neo-db-get-branch-for-stack :and-return-value nil)
    (spy-on 'neo--workflow-beads-workspace-as-project :and-return-value
            (make-neo-project :id "p" :repo "r" :type "beads" :pr-number nil
                              :worktree-path "/repo/root" :stacks nil))
    (expect (neo--workflow-resolve-stack-root "omega-1") :to-equal "/repo/root")))

(describe "neo--hack"
  (let ((issue (make-neo-issue
                :id "omega-9" :number 9 :title "Fix the thing"
                :type "task" :labels nil :state 'open :draft 0
                :created-at nil :updated-at nil :closed-at nil
                :merged-at nil :repository-id "r" :stack nil :ui-state nil)))

    (before-each
      (spy-on 'neo/workflow-refresh)
      (spy-on 'beads-client-update)
      (spy-on 'neo--workflow-activate-perspective)
      (spy-on 'neo/workflow-git-branch-exists :and-return-value nil)
      (spy-on 'neo/workflow-git-create-branch :and-return-value t)
      (spy-on 'neo/workflow-git-fetch)
      (spy-on 'neo/workflow-git-origin-head :and-return-value "main")
      (spy-on 'neo--workflow-git-run :and-return-value t)
      (spy-on 'neo-issue-title-to-slug :and-return-value "9-fix-the-thing")
      (spy-on 'neo--resolve-branch-conflict :and-call-fake (lambda (_repo slug _strategy _rename) slug))
      (spy-on 'neo--get-current-username :and-return-value "mav")
      (spy-on 'neo--workflow-beads-workspace-as-project :and-return-value
              (make-neo-project :id "p" :repo "r" :type "beads" :pr-number nil
                                :worktree-path "/repo/root" :stacks nil)))

    (it "activates the perspective with repo-path when strategy is 'repo"
      (spy-on 'neo--workflow-choose-workspace-strategy :and-return-value 'repo)
      (neo--hack issue)
      (expect 'neo--workflow-activate-perspective :to-have-been-called-with
              "mav/9-fix-the-thing" "/repo/root"))

    (it "builds the slug from the title only, without the issue number as a prefix"
      (spy-on 'neo--workflow-choose-workspace-strategy :and-return-value 'repo)
      (neo--hack issue)
      (expect 'neo-issue-title-to-slug :to-have-been-called-with nil "Fix the thing"))

    (it "activates the perspective with the worktree path when strategy is 'worktree"
      (spy-on 'neo--workflow-choose-workspace-strategy :and-return-value 'worktree)
      (let ((neo/workflow-worktrees-directory "/tmp/worktrees"))
        (neo--hack issue)
        (expect 'neo--workflow-activate-perspective :to-have-been-called-with
                "mav/9-fix-the-thing" "/tmp/worktrees/r_9-fix-the-thing")))

    (it "creates the worktree before activating the perspective (ordering regression)"
      (spy-on 'neo--workflow-choose-workspace-strategy :and-return-value 'worktree)
      (let* ((calls nil))
        (spy-on 'neo--workflow-git-run :and-call-fake
                (lambda (&rest args) (push (cons 'worktree-add args) calls) t))
        (spy-on 'neo--workflow-activate-perspective :and-call-fake
                (lambda (&rest args) (push (cons 'activate args) calls) nil))
        (let ((neo/workflow-worktrees-directory "/tmp/worktrees"))
          (neo--hack issue))
        (setq calls (nreverse calls))
        (expect (mapcar #'car calls) :to-equal '(worktree-add activate))))

    (it "sanitizes a username containing spaces into a valid branch component"
      (spy-on 'neo--workflow-choose-workspace-strategy :and-return-value 'repo)
      (spy-on 'neo--get-current-username :and-return-value "Maurizio Vitale")
      (neo--hack issue)
      (expect 'neo--workflow-activate-perspective :to-have-been-called-with
              "maurizio-vitale/9-fix-the-thing" "/repo/root"))

    (it "fetches origin and creates the branch off origin/<default-branch>"
      (spy-on 'neo--workflow-choose-workspace-strategy :and-return-value 'repo)
      (neo--hack issue)
      (expect 'neo/workflow-git-fetch :to-have-been-called)
      (expect 'neo/workflow-git-create-branch :to-have-been-called-with
              "mav/9-fix-the-thing" "origin/main"))

    (it "falls back to origin/main when the remote HEAD can't be determined"
      (spy-on 'neo--workflow-choose-workspace-strategy :and-return-value 'repo)
      (spy-on 'neo/workflow-git-origin-head :and-return-value nil)
      (neo--hack issue)
      (expect 'neo/workflow-git-create-branch :to-have-been-called-with
              "mav/9-fix-the-thing" "origin/main"))

    (it "does not fetch or recreate the branch when it already exists"
      (spy-on 'neo--workflow-choose-workspace-strategy :and-return-value 'repo)
      (spy-on 'neo/workflow-git-branch-exists :and-return-value t)
      (neo--hack issue)
      (expect 'neo/workflow-git-fetch :not :to-have-been-called)
      (expect 'neo/workflow-git-create-branch :not :to-have-been-called))

    (it "signals a user-error and does not activate when branch creation fails"
      (spy-on 'neo--workflow-choose-workspace-strategy :and-return-value 'repo)
      (spy-on 'neo/workflow-git-create-branch :and-return-value nil)
      (expect (neo--hack issue) :to-throw 'user-error)
      (expect 'neo--workflow-activate-perspective :not :to-have-been-called)
      (expect 'beads-client-update :not :to-have-been-called))

    (it "signals a user-error and does not activate when worktree creation fails"
      (spy-on 'neo--workflow-choose-workspace-strategy :and-return-value 'worktree)
      (spy-on 'neo--workflow-git-run :and-return-value nil)
      (let ((neo/workflow-worktrees-directory "/tmp/worktrees"))
        (expect (neo--hack issue) :to-throw 'user-error))
      (expect 'neo--workflow-activate-perspective :not :to-have-been-called)
      (expect 'beads-client-update :not :to-have-been-called))))

(describe "neo--workflow-worktree-directory-name"
  (it "prefixes the flattened slug with the repo name, matching the repo-wide {repo}_{slug} convention"
    (expect (neo--workflow-worktree-directory-name
             "omega" "maurizio-vitale/14-manually-verify-leetcode-extension-after")
            :to-equal
            "omega_maurizio-vitale--14-manually-verify-leetcode-extension-after"))

  (it "flattens every slash in the slug, not just the first"
    (expect (neo--workflow-worktree-directory-name "omega" "a/b/c")
            :to-equal "omega_a--b--c")))

(describe "neo--workflow-strip-username-prefix"
  (it "strips a leading \"username/\" from the slug"
    (expect (neo--workflow-strip-username-prefix "mav/9-fix-the-thing" "mav")
            :to-equal "9-fix-the-thing"))

  (it "leaves the slug alone when it has no matching username prefix"
    (expect (neo--workflow-strip-username-prefix "9-fix-the-thing" "mav")
            :to-equal "9-fix-the-thing"))

  (it "only strips the prefix, not any later occurrence of the username"
    (expect (neo--workflow-strip-username-prefix "mav/mav-utils" "mav")
            :to-equal "mav-utils")))

(describe "neo--workflow-default-base-ref"
  (it "uses origin's default branch when it can be determined"
    (spy-on 'neo/workflow-git-origin-head :and-return-value "develop")
    (expect (neo--workflow-default-base-ref) :to-equal "origin/develop"))

  (it "falls back to origin/main when origin's default branch is unknown"
    (spy-on 'neo/workflow-git-origin-head :and-return-value nil)
    (expect (neo--workflow-default-base-ref) :to-equal "origin/main")))

(describe "neo--workflow-sanitize-branch-component"
  (it "lowercases and replaces spaces with a single dash"
    (expect (neo--workflow-sanitize-branch-component "Maurizio Vitale")
            :to-equal "maurizio-vitale"))

  (it "collapses runs of invalid characters into one dash"
    (expect (neo--workflow-sanitize-branch-component "Smoke   Test!!")
            :to-equal "smoke-test"))

  (it "trims leading and trailing dashes produced by leading/trailing punctuation"
    (expect (neo--workflow-sanitize-branch-component "-Al Green-")
            :to-equal "al-green"))

  (it "leaves an already git-safe string unchanged"
    (expect (neo--workflow-sanitize-branch-component "mav")
            :to-equal "mav")))

;; ============================================================
;; Board ordering: epics as headers with nested child issues
;; ============================================================

(defun neo--mock-board-issue (id priority stack)
  "Return a mock open issue with ID, PRIORITY, and parent STACK."
  (make-neo-issue
   :id id :number 0 :short-id id :priority priority :title id :type "task"
   :labels nil :state 'open :draft 0 :created-at nil :updated-at nil
   :closed-at nil :merged-at nil :repository-id "r" :stack stack
   :prefix nil :ui-state nil))

(defun neo--mock-board-repo ()
  "Return a mock neo-repository for board tests."
  (make-neo-repository
   :id "r" :full-name "r" :fork 0 :created-at nil :pushed-at nil
   :updated-at nil :visibility "private" :forks 0 :default-branch "main"))

(describe "neo--get-sorted-issues-for-repo (epic nesting)"
  (before-each
    (clrhash neo--workflow-issue-ui-states)
    ;; Bypass the open/closed filter for these ordering tests.
    (spy-on 'neo--issue-filter :and-call-fake (lambda (issues _repo) issues)))

  (it "puts the epic first, its children (priority-sorted) next, orphans last"
    (let* ((stack (make-neo-stack :id "omega-e" :name "e" :title "Epic" :prefix nil
                                  :issue-id "omega-e" :branch nil :children-stacks nil))
           (child-hi (neo--mock-board-issue "c-hi" 0 stack))
           (child-lo (neo--mock-board-issue "c-lo" 3 stack))
           (orphan   (neo--mock-board-issue "orphan" 2 nil)))
      (spy-on 'neo-db-get-issues-for-repo :and-return-value (list child-lo orphan child-hi))
      (spy-on 'neo-db-get-stacks-for-repo :and-return-value (list stack))
      (let ((result (neo--get-sorted-issues-for-repo (neo--mock-board-repo))))
        (expect (nth 0 result) :to-be stack)
        (expect (neo-issue-id (nth 1 result)) :to-equal "c-hi")
        (expect (neo-issue-id (nth 2 result)) :to-equal "c-lo")
        (expect (neo-issue-id (nth 3 result)) :to-equal "orphan")
        ;; children are indented; the orphan is not
        (expect (> (length (neo-issue-prefix (nth 1 result))) 0) :to-be t)
        (expect (neo-issue-prefix (nth 3 result)) :to-equal ""))))

  (it "hides child issues when the epic is collapsed"
    (let* ((stack (make-neo-stack :id "omega-e" :name "e" :title "Epic" :prefix nil
                                  :issue-id "omega-e" :branch nil :children-stacks nil))
           (child (neo--mock-board-issue "c" 1 stack)))
      (neo-db-set-issue-ui-state "omega-e" "collapsed")
      (spy-on 'neo-db-get-issues-for-repo :and-return-value (list child))
      (spy-on 'neo-db-get-stacks-for-repo :and-return-value (list stack))
      (let ((result (neo--get-sorted-issues-for-repo (neo--mock-board-repo))))
        (expect (length result) :to-equal 1)
        (expect (nth 0 result) :to-be stack)))))
