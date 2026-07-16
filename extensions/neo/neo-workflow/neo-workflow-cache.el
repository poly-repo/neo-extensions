;;; neo-workflow-cache.el --- In-memory beads cache for Neo Workflow -*- lexical-binding: t; -*-

;; A single-workspace, in-memory cache of `beads-client-list' results,
;; invalidated by watching the underlying Dolt store with
;; `file-notify-add-watch'.  Avoids repeated full-workspace RPC round-trips
;; on every board redraw/interaction (see `neo--workflow-all-stacks' and
;; `neo-db-get-issues-for-repo' in neo-workflow-models.el, both of which
;; read from this cache instead of calling `beads-client-list' directly).
;;
;; Deliberately does NOT require `neo-workflow-models' (which requires this
;; file), so it defines its own small `beads-client-list' alist-extraction
;; helpers rather than reusing `neo--beads-issue-*' from that file.  This
;; duplication is intentional and is exactly what the deferred-cleanup bead
;; (folding `neo-issue' into `neo-bead') should eliminate.
;;
;; The full `bd show' detail (description, design, acceptance criteria) is
;; NOT cached here -- it stays on-demand via `beads-client-show', as today.

(require 'cl-lib)
(require 'seq)
(require 'beads-client)
(require 'filenotify)
(require 'neo-workflow-async)

;; ============================================================
;; The `neo-bead' struct
;; ============================================================

(cl-defstruct neo-bead
  id           ;; string (beads issue id, e.g. "omega-123")
  number       ;; integer (numeric suffix of id, 0 if unparseable)
  short-id     ;; string (id minus workspace prefix, e.g. "123")
  title        ;; string (never nil; "" when beads returns none)
  type         ;; string (beads issue_type/type, e.g. "task"/"epic"; defaults "task")
  status       ;; string (raw beads status: open/in_progress/blocked/closed/deferred/...)
  priority     ;; integer 0-4, or nil when beads returns no priority
  labels       ;; list of label name strings, as returned by beads-client-list
  parent       ;; string parent issue id, or nil when this bead has no parent
  created-at   ;; string timestamp or nil
  updated-at   ;; string timestamp or nil
  closed-at)   ;; string timestamp or nil
  ;; Deliberately deferred: git worktree/branch/remote-tracking state for
  ;; this bead will be added here once that work starts; not implemented
  ;; yet.

(defun neo-bead-epic-p (bead)
  "Return non-nil when BEAD, a `neo-bead', is an epic."
  (string= (neo-bead-type bead) "epic"))

;; ============================================================
;; Beads alist <-> `neo-bead' mapping
;; ============================================================

(defun neo-workflow-cache--alist-id (alist)
  "Return the issue id string from a beads issue ALIST, or nil."
  (or (alist-get 'id alist) (cdr (assoc "id" alist))))

(defun neo-workflow-cache--alist-number (id)
  "Convert beads issue ID string to an integer sequence number.
Returns 0 if ID cannot be parsed."
  (if (and id (string-match "\\([0-9]+\\)\\'" id))
      (string-to-number (match-string 1 id))
    0))

(defun neo-workflow-cache--alist-short-id (id)
  "Return ID without its workspace prefix, e.g. \"omega-11sv\" -> \"11sv\".
Falls back to ID itself when there is no prefix."
  (if (and id (string-match "-\\([^-]+\\)\\'" id))
      (match-string 1 id)
    (or id "")))

(defun neo-workflow-cache--alist-type (alist)
  "Return the issue type of a beads ALIST as a string (default \"task\")."
  (format "%s" (or (alist-get 'issue_type alist)
                    (alist-get 'type alist)
                    (cdr (assoc "issue_type" alist))
                    (cdr (assoc "type" alist))
                    "task")))

(defun neo-workflow-cache--alist-priority (alist)
  "Return the integer priority (0-4) from a beads ALIST, or nil when absent."
  (let ((p (or (alist-get 'priority alist) (cdr (assoc "priority" alist)))))
    (cond ((integerp p) p)
          ((and (stringp p) (string-match-p "\\`[0-9]+\\'" p)) (string-to-number p))
          (t nil))))

(defun neo-workflow-cache--alist-parent (alist)
  "Return the parent issue id from a beads ALIST, or nil when there is none."
  (let ((parent (or (alist-get 'parent alist) (cdr (assoc "parent" alist)))))
    (and (stringp parent) (not (string-empty-p parent)) parent)))

(defun neo-workflow-cache--alist-labels (alist)
  "Return the list of label name strings from a beads ALIST."
  (let ((labels-raw (or (alist-get 'labels alist) (cdr (assoc "labels" alist)))))
    (mapcar (lambda (label)
              (if (stringp label)
                  label
                (or (alist-get 'name label) (cdr (assoc "name" label))
                    (format "%s" label))))
            (if (arrayp labels-raw) (append labels-raw nil) (or labels-raw nil)))))

(defun neo--beads-alist-to-bead (alist)
  "Convert a beads issue ALIST (as returned by `beads-client-list') to a
`neo-bead' struct."
  (let ((id (neo-workflow-cache--alist-id alist)))
    (make-neo-bead
     :id id
     :number (neo-workflow-cache--alist-number id)
     :short-id (neo-workflow-cache--alist-short-id id)
     :title (or (alist-get 'title alist) (cdr (assoc "title" alist)) "")
     :type (neo-workflow-cache--alist-type alist)
     :status (or (alist-get 'status alist) (cdr (assoc "status" alist)) "open")
     :priority (neo-workflow-cache--alist-priority alist)
     :labels (neo-workflow-cache--alist-labels alist)
     :parent (neo-workflow-cache--alist-parent alist)
     :created-at (or (alist-get 'created_at alist) (cdr (assoc "created_at" alist)))
     :updated-at (or (alist-get 'updated_at alist) (cdr (assoc "updated_at" alist)))
     :closed-at (or (alist-get 'closed_at alist) (cdr (assoc "closed_at" alist))))))

(defun neo--bead-to-alist (bead)
  "Convert a `neo-bead' BEAD back to an alist shaped like a raw beads issue,
so existing alist-consuming helpers (`neo--workflow-stacks-from-issues',
`neo--beads-alist-to-neo-issue', etc.) keep working unchanged."
  `((id . ,(neo-bead-id bead))
    (title . ,(neo-bead-title bead))
    (issue_type . ,(neo-bead-type bead))
    (status . ,(neo-bead-status bead))
    (priority . ,(neo-bead-priority bead))
    (labels . ,(neo-bead-labels bead))
    (parent . ,(neo-bead-parent bead))
    (created_at . ,(neo-bead-created-at bead))
    (updated_at . ,(neo-bead-updated-at bead))
    (closed_at . ,(neo-bead-closed-at bead))))

;; ============================================================
;; Cache storage + public API
;; ============================================================

(defvar neo-workflow-cache--beads nil
  "Cached list of `neo-bead' structs for the last-fetched workspace.")

(defvar neo-workflow-cache--repository-id nil
  "Repository id `neo-workflow-cache--beads' was populated for.")

(defvar neo-workflow-cache--valid nil
  "Non-nil when `neo-workflow-cache--beads' reflects on-disk state.
Kept separate from \"the list is nil\" so an empty workspace (zero issues)
is still a legitimate cache hit.")

(defun neo-workflow-cache--fetch-beads ()
  "Fetch every issue in the current beads workspace as `neo-bead' structs.

Passes `:limit 0' explicitly: `bd list'/`beads-client-list' otherwise
default to capping results at 50 issues, which silently drops issues (in
particular low-priority ones, since the default sort puts them last) from
a board that is supposed to show the whole workspace."
  (mapcar #'neo--beads-alist-to-bead (append (beads-client-list '(:limit 0)) nil)))

(defun neo-workflow-cache-get-beads (repository-id)
  "Return the cached list of `neo-bead' structs for REPOSITORY-ID.
On a cache miss (never populated, explicitly invalidated, or a different
REPOSITORY-ID), fetches via `neo-workflow-cache--fetch-beads', populates
the cache, and starts the file-notify watch.  Returns nil (without
caching) on error, mirroring the degrade-gracefully behavior of the
callers this replaces."
  (if (and neo-workflow-cache--valid
           (equal neo-workflow-cache--repository-id repository-id))
      neo-workflow-cache--beads
    (condition-case err
        (let ((beads (neo-workflow-cache--fetch-beads)))
          (setq neo-workflow-cache--beads beads)
          (setq neo-workflow-cache--repository-id repository-id)
          (setq neo-workflow-cache--valid t)
          (neo-workflow-cache--start-watch)
          beads)
      (error
       (message "neo-workflow-cache: beads fetch failed: %s" (error-message-string err))
       nil))))

(defun neo-workflow-cache--beads-list-equal (a b)
  "Return non-nil when bead lists A and B represent the same workspace
state, ignoring element order (`beads-client-list' does not guarantee a
stable order across calls)."
  (cl-flet ((by-id (beads)
              (sort (copy-sequence beads)
                    (lambda (x y) (string< (neo-bead-id x) (neo-bead-id y))))))
    (equal (by-id a) (by-id b))))

(defun neo-workflow-cache-clear ()
  "Fully reset the cache, discarding the last-known bead list too."
  (setq neo-workflow-cache--beads nil)
  (setq neo-workflow-cache--repository-id nil)
  (setq neo-workflow-cache--valid nil))

;; ============================================================
;; File-notify watch (true recursive: walk once, watch every directory,
;; dynamically add watches for newly-created subdirectories)
;; ============================================================

(defcustom neo-workflow-cache-debounce-delay 0.4
  "Idle seconds after a beads Dolt-store file-notify event before
invalidating the cache and refreshing the board.  Dolt writes are bursty
(a single logical commit touches several files), so this collapses a
burst into a single invalidation/redraw."
  :type 'number
  :group 'neo-workflow)

(defvar neo-workflow-cache--watch-table (make-hash-table :test #'equal)
  "Directory path -> file-notify descriptor, for every directory watched.")

(defvar neo-workflow-cache--watched-root nil
  "The Dolt checkout directory currently under watch, or nil.")

(defvar neo-workflow-cache--debounce-timer nil
  "Pending debounce timer for a batch of file-notify events, or nil.")

(defvar neo-workflow-cache--warned-degraded-watch nil
  "Non-nil once the degraded-mode (no `prefix') watch warning has fired.")

(defun neo-workflow-cache--dolt-dir ()
  "Return the Dolt checkout directory to watch for the current workspace,
or nil when the workspace cannot be resolved.

Prefers `(database_path)/(prefix)', the actual per-workspace Dolt
checkout.  When `prefix' is unavailable (e.g. workspace resolved via the
`beads-dir'/BEADS_DIR filesystem-discovery path, which never learns
`prefix'), falls back to watching `database_path' itself -- broader
scope (could see sibling-workspace writes), but never a correctness bug
given `neo-workflow-cache--debounced-invalidate' is cheap and idempotent."
  (when-let* ((info (beads-client--workspace-info))
              (db-path (alist-get 'database_path info)))
    (let ((prefix (alist-get 'prefix info)))
      (if prefix
          (expand-file-name prefix db-path)
        (unless neo-workflow-cache--warned-degraded-watch
          (setq neo-workflow-cache--warned-degraded-watch t)
          (message "neo-workflow-cache: workspace `prefix' unavailable, watching %s instead of a single-workspace subdirectory"
                   db-path))
        db-path))))

(defun neo-workflow-cache--watch-directory-recursively (dir)
  "Watch DIR and every existing subdirectory of DIR for changes."
  (when (file-directory-p dir)
    (dolist (d (cons dir (seq-filter #'file-directory-p
                                      (directory-files-recursively dir "" t))))
      (unless (gethash d neo-workflow-cache--watch-table)
        (puthash d
                 (file-notify-add-watch d '(change) #'neo-workflow-cache--file-notify-callback)
                 neo-workflow-cache--watch-table)))))

(defun neo-workflow-cache--handle-event (_event)
  "Debounce a file-notify event; schedule a cache invalidation + redraw.
Deliberately takes only an ignored event argument, decoupled from
`file-notify-add-watch' registration, so this logic can be exercised
directly from a test with a synthetic event -- no real inotify needed."
  (when neo-workflow-cache--debounce-timer
    (cancel-timer neo-workflow-cache--debounce-timer))
  (setq neo-workflow-cache--debounce-timer
        (run-with-timer neo-workflow-cache-debounce-delay nil
                         #'neo-workflow-cache--debounced-invalidate)))

(defun neo-workflow-cache--debounced-invalidate ()
  "Timer callback: re-fetch beads and redraw only when something changed.

Fetches fresh data itself, rather than merely marking the cache stale and
letting the next reader refetch, so it can compare against the
last-known list before deciding whether to update the cache and run
`neo-workflow-refresh-hook'.  Dolt writes its own bookkeeping files on
every read too, so most file-notify bursts do not correspond to any
user-visible change -- redrawing (and resetting point, previously) on
every one of them made the board unusable while beads was busy nearby.
When the cache isn't valid to begin with (never populated, or cleared by
`neo-workflow-cache-clear'), there is nothing to compare against, so the
hook always runs and the next reader repopulates the cache as usual."
  (setq neo-workflow-cache--debounce-timer nil)
  (if (not neo-workflow-cache--valid)
      (run-hooks 'neo-workflow-refresh-hook)
    (condition-case err
        (let ((fresh (neo-workflow-cache--fetch-beads)))
          (unless (neo-workflow-cache--beads-list-equal fresh neo-workflow-cache--beads)
            (setq neo-workflow-cache--beads fresh)
            (run-hooks 'neo-workflow-refresh-hook)))
      (error
       (message "neo-workflow-cache: beads fetch failed: %s" (error-message-string err))))))

(defun neo-workflow-cache--file-notify-callback (event)
  "Handle a raw `file-notify' EVENT on the watched Dolt tree.
When EVENT reports a newly created directory, watches it too (catches
subdirectories created after the initial recursive walk, e.g. a fresh
`.dolt/noms/oldgen'-style chunk directory), so writes inside it are
still caught.  Always debounces via `neo-workflow-cache--handle-event'."
  (let ((action (nth 1 event))
        (file (nth 2 event)))
    (when (and (eq action 'created) (file-directory-p file))
      (neo-workflow-cache--watch-directory-recursively file)))
  (neo-workflow-cache--handle-event event))

(defun neo-workflow-cache--start-watch ()
  "Idempotently (re-)establish the file-notify watch for the current
workspace's Dolt directory.  No-ops when the workspace can't be resolved
or the watch is already established for the same root."
  (let ((dir (neo-workflow-cache--dolt-dir)))
    (when (and dir (not (equal dir neo-workflow-cache--watched-root)))
      (neo-workflow-cache-stop-watching)
      (neo-workflow-cache--watch-directory-recursively dir)
      (setq neo-workflow-cache--watched-root dir))))

(defun neo-workflow-cache-stop-watching ()
  "Remove every registered file-notify watch and cancel any pending
debounce timer."
  (interactive)
  (maphash (lambda (_dir descriptor) (file-notify-rm-watch descriptor))
           neo-workflow-cache--watch-table)
  (clrhash neo-workflow-cache--watch-table)
  (setq neo-workflow-cache--watched-root nil)
  (when neo-workflow-cache--debounce-timer
    (cancel-timer neo-workflow-cache--debounce-timer)
    (setq neo-workflow-cache--debounce-timer nil)))

(provide 'neo-workflow-cache)
;;; neo-workflow-cache.el ends here
