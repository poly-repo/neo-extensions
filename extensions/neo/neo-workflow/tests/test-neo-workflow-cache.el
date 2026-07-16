;;; tests/test-neo-workflow-cache.el --- Tests for neo-workflow-cache -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)

;; Mock beads-client so we can test without the sibling extension on the load-path.
;; (Same stub set as test-neo-workflow-models.el / test-neo-workflow-status.el --
;; only the first-loaded test file's block actually runs, since it's guarded by
;; `featurep', so all three must agree on the same mocked surface.)
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

;; Mock neo-workflow-git so sibling test files can load it without git
;; subprocess calls.
(unless (featurep 'neo-workflow-git)
  (provide 'neo-workflow-git))

(require 'neo-workflow-cache)

;; ============================================================
;; Test helpers
;; ============================================================

(defun neo--test-cache-alist (id &rest overrides)
  "Return a minimal beads issue alist for ID, with OVERRIDES merged in."
  (append overrides
          `((id . ,id)
            (title . "Test issue")
            (issue_type . "task")
            (status . "open")
            (priority . 2)
            (labels . ("foo" "bar"))
            (parent . nil)
            (created_at . "2026-01-01")
            (updated_at . "2026-01-02")
            (closed_at . nil))))

(defun neo--test-cache-reset ()
  "Reset all cache + watch state between tests.
Resets watch/timer bookkeeping directly rather than via
`neo-workflow-cache-stop-watching', since tests spy on
`file-notify-add-watch'/`run-with-timer' with fake return values that
real `file-notify-rm-watch'/`cancel-timer' calls would choke on."
  (neo-workflow-cache-clear)
  (clrhash neo-workflow-cache--watch-table)
  (setq neo-workflow-cache--watched-root nil)
  (setq neo-workflow-cache--debounce-timer nil)
  (setq neo-workflow-cache--warned-degraded-watch nil))

(defun neo--test-cache-refresh-spy ()
  "No-op function spied on to observe `neo-workflow-refresh-hook' runs.")

;; ============================================================
;; neo--beads-alist-to-bead / neo--bead-to-alist
;; ============================================================

(describe "neo--beads-alist-to-bead"
  (it "maps every field from a full alist"
    (let ((bead (neo--beads-alist-to-bead
                 (neo--test-cache-alist "omega-42" '(parent . "omega-1")))))
      (expect (neo-bead-id bead) :to-equal "omega-42")
      (expect (neo-bead-number bead) :to-equal 42)
      (expect (neo-bead-short-id bead) :to-equal "42")
      (expect (neo-bead-title bead) :to-equal "Test issue")
      (expect (neo-bead-type bead) :to-equal "task")
      (expect (neo-bead-status bead) :to-equal "open")
      (expect (neo-bead-priority bead) :to-equal 2)
      (expect (neo-bead-labels bead) :to-equal '("foo" "bar"))
      (expect (neo-bead-parent bead) :to-equal "omega-1")
      (expect (neo-bead-created-at bead) :to-equal "2026-01-01")
      (expect (neo-bead-updated-at bead) :to-equal "2026-01-02")
      (expect (neo-bead-closed-at bead) :to-equal nil)))

  (it "defaults missing priority/parent/labels/type/status"
    (let ((bead (neo--beads-alist-to-bead '((id . "omega-9") (title . "Bare")))))
      (expect (neo-bead-priority bead) :to-equal nil)
      (expect (neo-bead-parent bead) :to-equal nil)
      (expect (neo-bead-labels bead) :to-equal nil)
      (expect (neo-bead-type bead) :to-equal "task")
      (expect (neo-bead-status bead) :to-equal "open")))

  (it "identifies epics via neo-bead-epic-p"
    (let ((epic (neo--beads-alist-to-bead (neo--test-cache-alist "omega-1" '(issue_type . "epic"))))
          (task (neo--beads-alist-to-bead (neo--test-cache-alist "omega-2"))))
      (expect (neo-bead-epic-p epic) :to-be-truthy)
      (expect (neo-bead-epic-p task) :not :to-be-truthy))))

(describe "neo--bead-to-alist"
  (it "round-trips the fields a bead was built from"
    (let* ((raw (neo--test-cache-alist "omega-7" '(parent . "omega-1")))
           (alist (neo--bead-to-alist (neo--beads-alist-to-bead raw))))
      (expect (alist-get 'id alist) :to-equal "omega-7")
      (expect (alist-get 'title alist) :to-equal "Test issue")
      (expect (alist-get 'issue_type alist) :to-equal "task")
      (expect (alist-get 'status alist) :to-equal "open")
      (expect (alist-get 'priority alist) :to-equal 2)
      (expect (alist-get 'labels alist) :to-equal '("foo" "bar"))
      (expect (alist-get 'parent alist) :to-equal "omega-1"))))

;; ============================================================
;; neo-workflow-cache-get-beads
;; ============================================================

(describe "neo-workflow-cache-get-beads"
  (before-each (neo--test-cache-reset))
  (after-each (neo--test-cache-reset))

  (it "fetches once on a cache miss and caches the result"
    (spy-on 'beads-client-list :and-return-value
            (vector (neo--test-cache-alist "omega-1")))
    (let ((beads (neo-workflow-cache-get-beads "repo-a")))
      (expect (length beads) :to-equal 1)
      (expect (neo-bead-p (car beads)) :to-be-truthy))
    (expect 'beads-client-list :to-have-been-called-times 1))

  (it "serves a second call for the same repository-id from the cache"
    (spy-on 'beads-client-list :and-return-value
            (vector (neo--test-cache-alist "omega-1")))
    (neo-workflow-cache-get-beads "repo-a")
    (neo-workflow-cache-get-beads "repo-a")
    (expect 'beads-client-list :to-have-been-called-times 1))

  (it "re-fetches when the repository-id changes"
    (spy-on 'beads-client-list :and-return-value
            (vector (neo--test-cache-alist "omega-1")))
    (neo-workflow-cache-get-beads "repo-a")
    (neo-workflow-cache-get-beads "repo-b")
    (expect 'beads-client-list :to-have-been-called-times 2))

  (it "re-fetches once the cache has been marked stale"
    (spy-on 'beads-client-list :and-return-value
            (vector (neo--test-cache-alist "omega-1")))
    (neo-workflow-cache-get-beads "repo-a")
    (setq neo-workflow-cache--valid nil)
    (neo-workflow-cache-get-beads "repo-a")
    (expect 'beads-client-list :to-have-been-called-times 2))

  (it "never caches an errored fetch as a valid result"
    (spy-on 'beads-client-list :and-throw-error 'error)
    (expect (neo-workflow-cache-get-beads "repo-a") :to-equal nil)
    (expect (neo-workflow-cache-get-beads "repo-a") :to-equal nil)
    (expect 'beads-client-list :to-have-been-called-times 2)))

;; ============================================================
;; neo-workflow-cache-clear
;; ============================================================

(describe "neo-workflow-cache-clear"
  (before-each (neo--test-cache-reset))
  (after-each (neo--test-cache-reset))

  (it "discards the last-known bead list too"
    (spy-on 'beads-client-list :and-return-value
            (vector (neo--test-cache-alist "omega-1")))
    (neo-workflow-cache-get-beads "repo-a")
    (neo-workflow-cache-clear)
    (expect neo-workflow-cache--beads :to-equal nil)
    (expect neo-workflow-cache--repository-id :to-equal nil)))

;; ============================================================
;; Debounce logic (neo-workflow-cache--handle-event / --debounced-invalidate)
;; ============================================================

(describe "neo-workflow-cache--handle-event"
  (before-each (neo--test-cache-reset))
  (after-each (neo--test-cache-reset))

  (it "schedules exactly one timer for a single event"
    (spy-on 'run-with-timer :and-return-value 'fake-timer)
    (neo-workflow-cache--handle-event 'ignored)
    (expect 'run-with-timer :to-have-been-called-times 1)
    (expect 'run-with-timer :to-have-been-called-with
            neo-workflow-cache-debounce-delay nil #'neo-workflow-cache--debounced-invalidate))

  (it "cancels and reschedules on a second event within the window"
    (spy-on 'run-with-timer :and-call-fake (lambda (&rest _) 'fake-timer))
    (spy-on 'cancel-timer)
    (neo-workflow-cache--handle-event 'ignored)
    (neo-workflow-cache--handle-event 'ignored)
    (expect 'cancel-timer :to-have-been-called-times 1)
    (expect 'run-with-timer :to-have-been-called-times 2)))

(describe "neo-workflow-cache--debounced-invalidate"
  (before-each (neo--test-cache-reset))
  (after-each
    (remove-hook 'neo-workflow-refresh-hook #'neo--test-cache-refresh-spy)
    (neo--test-cache-reset))

  (it "does not run the hook or touch the cache when the fetched beads are unchanged"
    (spy-on 'beads-client-list :and-return-value
            (vector (neo--test-cache-alist "omega-1")))
    (neo-workflow-cache-get-beads "repo-a")
    (spy-on 'neo--test-cache-refresh-spy)
    (add-hook 'neo-workflow-refresh-hook #'neo--test-cache-refresh-spy)
    (neo-workflow-cache--debounced-invalidate)
    (expect 'neo--test-cache-refresh-spy :not :to-have-been-called)
    (expect neo-workflow-cache--valid :to-be-truthy)
    ;; Cache is still valid and unchanged, so a subsequent get does not refetch.
    (neo-workflow-cache-get-beads "repo-a")
    (expect 'beads-client-list :to-have-been-called-times 2))

  (it "ignores element order when comparing -- reordered-but-identical beads do not redraw"
    (spy-on 'beads-client-list :and-return-value
            (vector (neo--test-cache-alist "omega-1") (neo--test-cache-alist "omega-2")))
    (neo-workflow-cache-get-beads "repo-a")
    (spy-on 'beads-client-list :and-return-value
            (vector (neo--test-cache-alist "omega-2") (neo--test-cache-alist "omega-1")))
    (spy-on 'neo--test-cache-refresh-spy)
    (add-hook 'neo-workflow-refresh-hook #'neo--test-cache-refresh-spy)
    (neo-workflow-cache--debounced-invalidate)
    (expect 'neo--test-cache-refresh-spy :not :to-have-been-called))

  (it "updates the cache in place and runs the hook once when beads actually changed"
    (let ((call-count 0))
      (spy-on 'beads-client-list :and-call-fake
              (lambda (&rest _)
                (cl-incf call-count)
                (if (= call-count 1)
                    (vector (neo--test-cache-alist "omega-1"))
                  (vector (neo--test-cache-alist "omega-1" '(status . "closed")))))))
    (neo-workflow-cache-get-beads "repo-a")
    (spy-on 'neo--test-cache-refresh-spy)
    (add-hook 'neo-workflow-refresh-hook #'neo--test-cache-refresh-spy)
    (neo-workflow-cache--debounced-invalidate)
    (expect 'neo--test-cache-refresh-spy :to-have-been-called-times 1)
    (expect (neo-bead-status (car neo-workflow-cache--beads)) :to-equal "closed")
    ;; The cache was updated in place (still valid), so a subsequent get
    ;; does not need to refetch again.
    (neo-workflow-cache-get-beads "repo-a")
    (expect 'beads-client-list :to-have-been-called-times 2))

  (it "always runs the hook when the cache was not valid to begin with"
    (spy-on 'beads-client-list :and-return-value
            (vector (neo--test-cache-alist "omega-1")))
    (spy-on 'neo--test-cache-refresh-spy)
    (add-hook 'neo-workflow-refresh-hook #'neo--test-cache-refresh-spy)
    (neo-workflow-cache--debounced-invalidate)
    (expect 'neo--test-cache-refresh-spy :to-have-been-called-times 1)
    (expect 'beads-client-list :not :to-have-been-called))

  (it "logs and does not run the hook when the comparison fetch errors"
    (spy-on 'beads-client-list :and-return-value
            (vector (neo--test-cache-alist "omega-1")))
    (neo-workflow-cache-get-beads "repo-a")
    (spy-on 'beads-client-list :and-throw-error 'error)
    (spy-on 'message)
    (spy-on 'neo--test-cache-refresh-spy)
    (add-hook 'neo-workflow-refresh-hook #'neo--test-cache-refresh-spy)
    (neo-workflow-cache--debounced-invalidate)
    (expect 'neo--test-cache-refresh-spy :not :to-have-been-called)
    (expect 'message :to-have-been-called)))

;; ============================================================
;; Recursive watch registration / teardown
;; ============================================================

(describe "neo-workflow-cache--watch-directory-recursively"
  (let (root)
    (before-each
      (neo--test-cache-reset)
      (setq root (make-temp-file "neo-workflow-cache-test-" t))
      (make-directory (expand-file-name "a/b" root) t)
      (make-directory (expand-file-name "c" root) t))
    (after-each
      (neo--test-cache-reset)
      (when (and root (file-directory-p root))
        (delete-directory root t)))

    (it "watches the root and every nested subdirectory exactly once"
      (let ((counter 0))
        (spy-on 'file-notify-add-watch :and-call-fake
                (lambda (&rest _) (cl-incf counter)))
        (neo-workflow-cache--watch-directory-recursively root)
        (expect 'file-notify-add-watch :to-have-been-called-times 4) ; root, a, a/b, c
        (expect (hash-table-count neo-workflow-cache--watch-table) :to-equal 4)))

    (it "does not double-watch a directory already in the table"
      (spy-on 'file-notify-add-watch :and-return-value 'descriptor)
      (neo-workflow-cache--watch-directory-recursively root)
      (neo-workflow-cache--watch-directory-recursively root)
      (expect 'file-notify-add-watch :to-have-been-called-times 4))

    (it "stop-watching removes every descriptor and empties the table"
      (spy-on 'file-notify-add-watch :and-return-value 'descriptor)
      (spy-on 'file-notify-rm-watch)
      (neo-workflow-cache--watch-directory-recursively root)
      (neo-workflow-cache-stop-watching)
      (expect 'file-notify-rm-watch :to-have-been-called-times 4)
      (expect (hash-table-count neo-workflow-cache--watch-table) :to-equal 0))))
