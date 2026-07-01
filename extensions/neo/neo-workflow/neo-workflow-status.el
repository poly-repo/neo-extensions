;;; neo-workflow-status.el --- Board UI for Neo Workflow -*- lexical-binding: t; -*-

;; Adapted from workflow/neo-workflow-status.el.
;; Data comes from beads + git; no SQLite, no GitHub.

(require 'cl-lib)
(require 'seq)
(require 'neo-workflow-slug)
(require 'neo-workflow-models)
(require 'neo-workflow-db)
(require 'neo-workflow-git)
(require 'neo-workflow-context)
(require 'vtable)

(defgroup neo-workflow-status nil
  "Neo Workflow board UI."
  :group 'neo-workflow)

(defcustom neo/workflow-worktrees-directory "~/Projects/worktrees"
  "Directory to store git worktrees."
  :type 'directory
  :group 'neo-workflow-status)

(defcustom neo/workflow-sort-by-priority t
  "When non-nil, sort issues by priority."
  :type 'boolean
  :group 'neo-workflow-status)

(defvar neo/current-context nil
  "The current workflow context object (neo-context struct).")

;; ============================================================
;; Mode definitions
;; ============================================================

(defvar neo-workflow-mode-map (make-sparse-keymap)
  "Keymap for neo-workflow-mode.")

;;;###autoload
(define-derived-mode neo-workflow-mode special-mode "NeoWorkflow"
  "A special mode for neo workflow."
  :group 'neo-workflow)

(defvar neo-workflow-status-mode-map (make-sparse-keymap)
  "Keymap for `neo-workflow-status-mode'.")

(set-keymap-parent neo-workflow-status-mode-map neo-workflow-mode-map)
(define-key neo-workflow-status-mode-map (kbd "TAB") #'neo-workflow-next-table)
(define-key neo-workflow-status-mode-map (kbd "<backtab>") #'neo-workflow-prev-table)
(define-key neo-workflow-status-mode-map (kbd "g") #'neo/workflow-sync-and-refresh)
(define-key neo-workflow-status-mode-map (kbd "q") #'quit-window)
(define-key neo-workflow-status-mode-map (kbd "N") #'neo--repo-next)
(define-key neo-workflow-status-mode-map (kbd "P") #'neo--repo-prev)

;;;###autoload
(define-derived-mode neo-workflow-status-mode neo-workflow-mode "NeoWorkflowStatus"
  "A special mode for the neo workflow board."
  :group 'neo-workflow-status
  (hl-line-mode 1)
  (setq-local cursor-type nil
              truncate-lines t
              cursor-in-non-selected-windows nil))

;; ============================================================
;; Faces
;; ============================================================

(defface neo-workflow-repo-face
  '((t :inherit header-line :weight bold :extend t))
  "Face for workflow workspace names."
  :group 'neo-workflow-status)

(defface neo-workflow-priority-base-face
  '((t :family "Space Mono" :slant italic :weight bold))
  "Base face for all workflow issue priorities."
  :group 'neo-workflow-status)

(defface neo-workflow-issue-open-face
  '((t :inherit (neo-workflow-priority-base-face default)))
  "Face for open issues in workflow status."
  :group 'neo-workflow-status)

(defface neo-workflow-issue-active-face
  '((t :inherit (neo-workflow-priority-base-face magit-branch-local)))
  "Face for active issues in workflow status."
  :group 'neo-workflow-status)

(defface neo-workflow-issue-completed-face
  '((t :inherit (neo-workflow-priority-base-face magit-dimmed) :strike-through t))
  "Face for completed issues in workflow status."
  :group 'neo-workflow-status)

(defface neo-workflow-issue-id-face
  '((t :inherit magit-hash))
  "Face for issue IDs in workflow status."
  :group 'neo-workflow-status)

(defface neo-workflow-issue-title-face
  '((t :inherit (neo-workflow-priority-base-face default)))
  "Face for issue titles in workflow status."
  :group 'neo-workflow-status)

(defface neo-workflow-priority-critical-face
  '((t :foreground "white" :background "#d73a49"))
  "Face for critical priority issues."
  :group 'neo-workflow-status)

(defface neo-workflow-priority-high-face
  '((t :inherit neo-workflow-priority-base-face :foreground "white" :background "#f66a0a"))
  "Face for high priority issues."
  :group 'neo-workflow-status)

(defface neo-workflow-priority-medium-face
  '((t :inherit neo-workflow-priority-base-face :foreground "black" :background "#ffd33d"))
  "Face for medium priority issues."
  :group 'neo-workflow-status)

(defface neo-workflow-priority-low-face
  '((t :inherit neo-workflow-priority-base-face :foreground "white" :background "#8b949e"))
  "Face for low priority issues."
  :group 'neo-workflow-status)

(defface neo-workflow-priority-icon-critical-face
  '((t :inherit neo-workflow-priority-base-face :foreground "#d73a49"))
  "Face for critical priority icon."
  :group 'neo-workflow-status)

(defface neo-workflow-priority-icon-high-face
  '((t :inherit neo-workflow-priority-base-face :height 0.8 :foreground "#4338CA"))
  "Face for high priority icon."
  :group 'neo-workflow-status)

(defface neo-workflow-priority-icon-mid-face
  '((t :inherit neo-workflow-priority-base-face :height 0.8 :foreground "#3B82F6"))
  "Face for medium priority icon."
  :group 'neo-workflow-status)

(defface neo-workflow-priority-icon-low-face
  '((t :inherit neo-workflow-priority-base-face :height 0.8 :foreground "#9CA3AF"))
  "Face for low priority icon."
  :group 'neo-workflow-status)

;; ============================================================
;; Priority logic
;; ============================================================

(defconst neo--priority-labels
  '("low" "mid" "high" "critical")
  "Label names mapped to icons in their own column.")

(defvar neo--priority-icons
  '(("critical" . "🔥")
    ("high"     . "P1")
    ("mid"      . "P2")
    ("low"      . "P3"))
  "Alist mapping priority names to display icons.")

(defun neo--get-priority-from-labels (labels)
  "Extract priority from LABELS list of `neo-label' structs.
Returns a symbol ('critical, 'high, 'medium, 'low) or nil."
  (let* ((label-names (mapcar #'neo-label-name labels))
         (found-priority nil))
    (dolist (p neo--priority-labels)
      (when (cl-member p label-names :test #'string-equal)
        (setq found-priority p)))
    (when found-priority (intern found-priority))))

(defun neo--get-priority-face (priority)
  "Return face for a given PRIORITY symbol."
  (pcase priority
    ('critical 'neo-workflow-priority-critical-face)
    ('high 'neo-workflow-priority-high-face)
    ('medium 'neo-workflow-priority-medium-face)
    ('low 'neo-workflow-priority-low-face)
    (_ nil)))

(defun neo--priority (issue)
  "Return the priority label string for ISSUE, or \"\" if none."
  (let* ((labels (mapcar #'neo-label-name (neo-issue-labels issue)))
         (found-priority nil))
    (dolist (p neo--priority-labels)
      (when (cl-member p labels :test #'string-equal)
        (setq found-priority p)))
    (or found-priority "")))

(defun neo--get-issue-priority-score (issue)
  "Return a numeric score for ISSUE priority.  Lower is higher priority."
  (let ((priority-name (neo--priority issue)))
    (or (cl-position priority-name '("critical" "high" "mid" "low") :test #'string-equal)
        99)))

(defun neo--sort-issues (issues)
  "Sort ISSUES by priority if `neo/workflow-sort-by-priority' is non-nil."
  (if neo/workflow-sort-by-priority
      (sort (copy-sequence issues)
            (lambda (a b)
              (< (neo--get-issue-priority-score a)
                 (neo--get-issue-priority-score b))))
    issues))

(defun neo--get-priority-icon-face (priority-name)
  "Return face for a given PRIORITY-NAME string."
  (pcase priority-name
    ("critical" 'neo-workflow-priority-icon-critical-face)
    ("high" 'neo-workflow-priority-icon-high-face)
    ("mid" 'neo-workflow-priority-icon-mid-face)
    ("low" 'neo-workflow-priority-icon-low-face)
    (_ nil)))

(defun neo--priority-icon (issue)
  "Return a propertized priority icon string for ISSUE."
  (let ((priority-name (neo--priority issue)))
    (if (string-empty-p priority-name)
        ""
      (let* ((icon (alist-get priority-name neo--priority-icons "" nil #'string-equal))
             (face (neo--get-priority-icon-face priority-name)))
        (propertize icon 'face face)))))

(defun neo--new-priority (issue direction)
  "Return the new priority for ISSUE given DIRECTION (1 up, -1 down)."
  (let* ((current (neo--priority issue))
         (idx (if (string-empty-p current)
                  -1
                (cl-position current neo--priority-labels :test #'string-equal)))
         (new-idx (max -1 (min (1- (length neo--priority-labels))
                               (+ idx direction)))))
    (if (= new-idx -1)
        ""
      (nth new-idx neo--priority-labels))))

;; ============================================================
;; Label rendering
;; ============================================================

(defun neo--hex-color (c)
  "Return C as a #RRGGBB string, or nil if C is nil."
  (when c
    (if (string-prefix-p "#" c) c (concat "#" c))))

(defun neo--labels (issue)
  "Return a space-separated string of propertized label names for ISSUE,
excluding priority labels."
  (let* ((skip (mapcar #'downcase neo--priority-labels))
         (labels (cl-remove-if (lambda (l) (member (downcase (neo-label-name l)) skip))
                               (neo-issue-labels issue))))
    (mapconcat
     (lambda (label)
       (let* ((name (neo-label-name label))
              (background (neo--hex-color (neo-label-color label))))
         (propertize (concat " " name " ")
                     'help-echo (neo-label-description label)
                     'face (when background
                             `(:background ,background :foreground ,(readable-foreground-color background))))))
     labels
     " ")))

;; ============================================================
;; Issue & stack rendering helpers
;; ============================================================

(defun neo--issue-active-p (issue)
  "Return non-nil if ISSUE is active (has a root stack)."
  (neo-issue-stack issue))

(defun neo--active-issue-p (issue)
  "Return non-nil if ISSUE has an active stack."
  (neo-issue-stack issue))

(defun neo--issue-status (issue)
  "Return \"A\" if ISSUE is active, else \"\"."
  (if (neo-issue-stack issue) "A" ""))

(defun neo--get-issue-status-face (state)
  "Return face for issue STATE."
  (pcase state
    ('open 'neo-workflow-issue-open-face)
    ('active 'neo-workflow-issue-active-face)
    ('closed 'neo-workflow-issue-completed-face)
    (_ nil)))

(defun neo--final-issue-title-face (issue)
  "Return the face list for ISSUE title."
  (let* ((state (neo-issue-state issue))
         (status-face (neo--get-issue-status-face state)))
    (if (neo--active-issue-p issue)
        (list 'magit-head status-face 'neo-workflow-issue-title-face)
      (list status-face 'neo-workflow-issue-title-face))))

(defun neo--final-issue-id-face (issue)
  "Return the face list for ISSUE id."
  (let* ((state (neo-issue-state issue))
         (status-face (neo--get-issue-status-face state)))
    (if (neo--active-issue-p issue)
        (list 'magit-head status-face 'neo-workflow-issue-id-face)
      (list status-face 'neo-workflow-issue-id-face))))

(defun neo--get-branch-name (object)
  "Return the branch name for OBJECT (issue or stack), or nil."
  (if (neo-issue-p object)
      (neo--get-branch-name (neo-issue-stack object))
    (when-let* ((branch (and object (neo-stack-branch object)))
                (branch-name (neo-branch-name branch)))
      branch-name)))

(defun neo--propertize-issue-title (issue)
  "Return a propertized title string for ISSUE."
  (let ((branch-name (neo--get-branch-name issue)))
    (propertize (neo-issue-title issue)
                'face (neo--final-issue-title-face issue)
                'help-echo (when branch-name (format "branch: %s" branch-name)))))

(defun neo--propertize-stack-title (stack)
  "Return a propertized title string for STACK."
  (let ((branch-name (neo--get-branch-name stack)))
    (propertize (concat (neo-stack-prefix stack) (or (neo-stack-title stack) (neo-stack-name stack)))
                'face '(bold)
                'help-echo (when branch-name (format "branch: %s" branch-name)))))

;; ============================================================
;; Issue sorting and partitioning
;; ============================================================

(defun neo--compare-issues (a b)
  "Return non-nil if issue A should come before issue B."
  (let ((pa (neo--get-issue-priority-score a))
        (pb (neo--get-issue-priority-score b))
        (ta (or (neo-issue-created-at a) ""))
        (tb (or (neo-issue-created-at b) "")))
    (cond
     ((and neo/workflow-sort-by-priority (/= pa pb))
      (< pa pb))
     ((not (string= ta tb))
      (string> ta tb))
     (t nil))))

(defun neo--partition-issues (issues)
  "Partition ISSUES into (ACTIVE . INACTIVE)."
  (let (active inactive)
    (dolist (issue issues)
      (if (neo--issue-active-p issue)
          (push issue active)
        (push issue inactive)))
    (cons (nreverse active) (nreverse inactive))))

(defun neo--build-stack-index (issues stacks)
  "Build indices needed for stack expansion.

Returns a plist:
  :stacks-by-name   hash-table stack-name -> stack
  :issues-by-stack  hash-table stack-name -> list of issues
  :parent-issue     hash-table stack-name -> parent issue (or nil)"
  (let ((stacks-by-name (make-hash-table :test #'equal))
        (issues-by-stack (make-hash-table :test #'equal))
        (parent-issue (make-hash-table :test #'equal)))

    (dolist (stack stacks)
      (puthash (neo-stack-name stack) stack stacks-by-name))

    (dolist (issue issues)
      (let ((stack (neo-issue-stack issue)))
        (when stack
          (push issue (gethash (neo-stack-name stack) issues-by-stack)))))

    (maphash
     (lambda (stack-name _)
       (when (string-match "^\\([0-9]+\\)-" stack-name)
         (let* ((num (string-to-number (match-string 1 stack-name)))
                (parent (seq-find
                         (lambda (i) (= (neo-issue-number i) num))
                         issues)))
           (puthash stack-name parent parent-issue))))
     stacks-by-name)

    (list
     :stacks-by-name stacks-by-name
     :issues-by-stack issues-by-stack
     :parent-issue parent-issue)))

(defun neo--expand-stack (stack)
  "Expand a STACK into a flat list, computing tree prefixes."
  (cl-labels ((compute-prefixes (s prefix is-last)
                (let ((connector (if prefix (if is-last "└─ " "├─ ") "")))
                  (setf (neo-stack-prefix s) (concat prefix connector)))
                (let* ((children (neo-stack-children-stacks s))
                       (child-prefix (if prefix (concat prefix (if is-last "   " "│  ")) "")))
                  (dotimes (i (length children))
                    (let ((child (nth i children))
                          (lastp (= i (1- (length children)))))
                      (compute-prefixes child child-prefix lastp)))))
              (flatten (s)
                (cons s (mapcan #'flatten (neo-stack-children-stacks s)))))
    (compute-prefixes stack nil nil)
    (flatten stack)))

(defun neo--expand-issue (issue index processed)
  "Expand ISSUE into a flat list according to stack state.

INDEX is the stack index plist.
PROCESSED is a list of already-expanded parent issues."
  (let* ((stack (neo-issue-stack issue))
         (state (neo-issue-ui-state issue)))
    (cond
     ((null stack)
      (list issue))
     ((member issue processed)
      '())
     ((string= state "expanded")
      (append (list issue) (neo--expand-stack (neo-issue-stack issue))))
     (t
      (list issue)))))

(defun neo--get-sorted-issues-for-repo (repo)
  "Return issues and stacks for REPO in UI display order."
  (let* ((repo-id (neo-repository-id repo))
         (repo-name (neo-repository-full-name repo))
         (all-issues (neo-db-get-issues-for-repo repo-id))
         (issues (neo--issue-filter all-issues repo-name))
         (stacks (neo-db-get-stacks-for-repo repo-id))
         (parts (neo--partition-issues issues))
         (active (sort (car parts) #'neo--compare-issues))
         (inactive (sort (cdr parts) #'neo--compare-issues))
         (index (neo--build-stack-index active stacks))
         (expanded-active
          (mapcan (lambda (issue)
                    (neo--expand-issue issue index '()))
                  active)))
    (append expanded-active inactive)))

;; ============================================================
;; Filtering
;; ============================================================

(defun neo-workflow-set-repo-issue-filter (repo-full-name filter-type)
  "Set the issue filter for REPO-FULL-NAME to FILTER-TYPE.
FILTER-TYPE must be one of 'open, 'closed, 'active, or 'all."
  (unless (memq filter-type '(open closed active all))
    (error "Invalid filter type: %s" filter-type))
  (let* ((repo-id (neo--get-repo-id-by-full-name repo-full-name))
         (current-state (neo-db-get-repo-ui-state repo-id))
         (state (or (plist-get current-state :state) "expanded"))
         (order (or (plist-get current-state :order) "priority")))
    (neo-db-set-repo-ui-state repo-id state (symbol-name filter-type) order)))

(defun neo-workflow-set-global-filter (filter-type)
  "Set the global repository filter to FILTER-TYPE."
  (neo-db-set-workflow-filter (if filter-type (symbol-name filter-type) nil)))

(defun neo-workflow-get-global-filter ()
  "Get the global repository filter as a symbol."
  (let ((val (neo-db-get-workflow-filter)))
    (when val (intern val))))

(defun neo-workflow-get-repo-issue-filter (repo-full-name)
  "Get the issue filter for REPO-FULL-NAME."
  (let ((global (neo-workflow-get-global-filter)))
    (if (memq global '(active open))
        global
      (let* ((repo-id (neo--get-repo-id-by-full-name repo-full-name))
             (state (neo-db-get-repo-ui-state repo-id))
             (filter (plist-get state :filter)))
        (if filter (intern filter) 'open)))))

(defun neo--issue-filter (issues repo-name)
  "Filter ISSUES for REPO-NAME based on the current filter setting."
  (let ((filter-type (neo-workflow-get-repo-issue-filter repo-name)))
    (seq-filter (lambda (issue)
                  (pcase filter-type
                    ('all t)
                    ('open (eq (neo-issue-state issue) 'open))
                    ('closed (eq (neo-issue-state issue) 'closed))
                    ('active (neo-issue-stack issue))
                    (_ (eq (neo-issue-state issue) 'open))))
                issues)))

;; ============================================================
;; Navigation
;; ============================================================

(defun neo--repo-next ()
  "Move to the next line with `repo-header-line'."
  (interactive)
  (let ((pos (save-excursion
               (when (get-text-property (line-beginning-position) 'repo-header-line)
                 (forward-line 1))
               (let (p done)
                 (while (not done)
                   (if (get-text-property (line-beginning-position) 'repo-header-line)
                       (setq p (line-beginning-position) done t)
                     (if (>= (point) (point-max))
                         (setq done t)
                       (forward-line 1))))
                 p))))
    (when pos
      (goto-char pos)
      (recenter-top-bottom 0))))

(defun neo--repo-prev ()
  "Move to the previous line with `repo-header-line'."
  (interactive)
  (let ((pos (save-excursion
               (when (get-text-property (line-beginning-position) 'repo-header-line)
                 (forward-line -1))
               (let (p done)
                 (while (not done)
                   (if (get-text-property (line-beginning-position) 'repo-header-line)
                       (setq p (line-beginning-position) done t)
                     (if (bobp)
                         (setq done t)
                       (forward-line -1))))
                 p))))
    (when pos
      (goto-char pos)
      (recenter-top-bottom 0))))

;; ============================================================
;; UI state
;; ============================================================

(defvar neo--repo-info-alist nil
  "Alist mapping repo-name to a list (TABLE START-POS END-POS).")

(defun neo-workflow-get-table-for-repo (repo-name)
  "Get the vtable for REPO-NAME."
  (interactive "sRepo name: ")
  (when-let* ((info (assoc-string repo-name neo--repo-info-alist)))
    (nth 1 info)))

(defun neo--vtable-update-object (table new old)
  "Update TABLE replacing OLD object with NEW."
  (vtable-update-object table new old))

(defun neo--select-thing (_object)
  "Select OBJECT (interactive stub)."
  (interactive))

(defun neo--toggle-object-visibility (object)
  "Toggle expanded/collapsed state of OBJECT."
  (interactive)
  (when (neo-issue-p object)
    (let* ((current-state (or (neo-issue-ui-state object) "collapsed"))
           (new-state (if (string= current-state "expanded") "collapsed" "expanded"))
           (issue-id (neo-issue-id object))
           (repo-id (neo-issue-repository-id object))
           (repo-name (neo--workflow-get-repo-full-name-by-id repo-id)))
      (neo-db-set-issue-ui-state issue-id new-state)
      (neo/workflow-refresh repo-name issue-id))))

(defun neo--objects-match-p (a b)
  "Return non-nil if A and B represent the same object."
  (cond
   ((and (neo-issue-p a) (neo-issue-p b))
    (equal (neo-issue-id a) (neo-issue-id b)))
   ((and (neo-stack-p a) (neo-stack-p b))
    (or (equal (neo-stack-id a) (neo-stack-id b))
        (string= (neo-stack-name a) (neo-stack-name b))))
   (t (equal a b))))

;; ============================================================
;; Issue editing (write path stubs — Phase 4)
;; ============================================================

(defun neo--edit-issue-at-point ()
  "Edit the issue at point."
  (interactive)
  (if-let* ((table (vtable-current-table))
            (object (vtable-current-object))
            (_ (neo-issue-p object))
            (repo-id (neo-issue-repository-id object))
            (repo-name (neo--workflow-get-repo-full-name-by-id repo-id))
            (issue-id (neo-issue-id object)))
      (neo-workflow-issue-open-template repo-name issue-id)
    (user-error "No issue found at point")))

(defun neo--new-issue-for-repo ()
  "Create a new issue for the workspace at point."
  (interactive)
  (if-let* ((repo-name (get-text-property (point) 'repo-name)))
      (neo-workflow-issue-open-template repo-name)
    (user-error "No workspace found at point")))

(defun neo--close-issue-at-point ()
  "Close the beads issue at point after confirmation."
  (interactive)
  (if-let* ((object (and (vtable-current-table) (vtable-current-object)))
            (_ (neo-issue-p object))
            (issue-id (neo-issue-id object)))
      (when (yes-or-no-p (format "Close issue %s (%s)? "
                                 issue-id (neo-issue-title object)))
        (condition-case err
            (progn
              (beads-client-close issue-id)
              (message "Closed issue %s." issue-id)
              (neo/workflow-refresh
               (neo--workflow-get-repo-full-name-by-id
                (neo-issue-repository-id object))
               issue-id))
          (beads-client-error
           (user-error "Failed to close issue %s: %s" issue-id (cadr err)))))
    (user-error "No issue found at point")))

(defun neo--priority-change (object direction)
  "Change the priority of OBJECT (a neo-issue) by DIRECTION."
  (when (and object (neo-issue-p object))
    (let* ((new-priority (neo--new-priority object direction))
           (current-labels (mapcar #'neo-label-name (neo-issue-labels object)))
           (other-labels (cl-remove-if (lambda (l) (member l neo--priority-labels)) current-labels))
           (new-labels (if (string-empty-p new-priority)
                           other-labels
                         (cons new-priority other-labels)))
           (issue-id (neo-issue-id object)))
      (condition-case err
          (progn
            (apply #'beads-client-update issue-id
                   (list :set-labels (string-join new-labels ",")))
            (neo/workflow-refresh (neo--workflow-get-repo-full-name-by-id
                                   (neo-issue-repository-id object))
                                  issue-id))
        (beads-client-error
         (message "neo-workflow: priority update failed: %s" (cadr err)))))))

(defun neo--priority-up (object)
  "Raise OBJECT priority by one step."
  (interactive)
  (neo--priority-change object 1))

(defun neo--priority-down (object)
  "Lower OBJECT priority by one step."
  (interactive)
  (neo--priority-change object -1))

;; ============================================================
;; Stack/context operations (Phase 4 stubs)
;; ============================================================

(defun neo--ensure-stack-scratch (stack-name)
  "Switch to the standard scratch buffer for STACK-NAME."
  (let ((scratch-name (format "*scratch* (%s)" stack-name)))
    (with-current-buffer (get-buffer-create scratch-name)
      (lisp-interaction-mode)
      (when (= (buffer-size) 0)
        (insert (format ";; Scratch buffer for stack: %s\n\n" stack-name))))
    (switch-to-buffer scratch-name)))

(defun neo--resolve-branch-conflict (repo-path branch-name strategy can-rename &optional _default-branch)
  "Resolve conflict if BRANCH-NAME is checked out in main repo at REPO-PATH.
Returns the resolved branch name."
  (let ((default-directory repo-path))
    (if (and (eq strategy 'worktree)
             (string= (neo/workflow-git-current-branch-uncached) branch-name))
        (if can-rename
            (let ((new-name (format "%s-%04d" branch-name (random 10000))))
              (message "Branch %s checked out in main repo. Renaming to %s." branch-name new-name)
              new-name)
          (let ((status (neo/workflow-git-repo-status)))
            (if (or (neo-workflow-git-repo-status-open-changes status)
                    (neo-workflow-git-repo-status-conflicts status)
                    (neo-workflow-git-repo-status-rebase-in-progress status))
                (user-error "Main repo at %s is dirty or busy and has %s checked out" repo-path branch-name)
              branch-name)))
      branch-name)))

(defun neo--get-current-username ()
  "Return the current user name (from git config)."
  (or (ignore-errors (neo--workflow-git-query "config" "--get" "user.name"))
      "user"))

(defun neo--hack (object)
  "Create and switch to a full development context for OBJECT (issue or stack).
Phase 4 note: write-path (branch creation, beads-client-update) is stubbed."
  (interactive)
  (let* ((issue (when (neo-issue-p object) object))
         (repo-id (when issue (neo-issue-repository-id issue)))
         (project (when repo-id (neo--workflow-beads-workspace-as-project)))
         (repo-path (when project (neo-project-worktree-path project)))
         (strategy (when repo-path
                     (let ((default-directory repo-path))
                       (neo--workflow-choose-workspace-strategy)))))
    (if (and issue repo-path strategy)
        (let* ((base-slug (neo-issue-title-to-slug
                           (neo-issue-number issue) (neo-issue-title issue)))
               (username (neo--get-current-username))
               (slug (format "%s/%s" username base-slug))
               (final-slug (neo--resolve-branch-conflict repo-path slug strategy t))
               (branch-name final-slug))

          ;; Create branch if it doesn't exist
          (let ((default-directory repo-path))
            (unless (neo/workflow-git-branch-exists branch-name)
              (neo/workflow-git-create-branch branch-name "HEAD")))

          ;; Perspective switch
          (when (featurep 'perspective)
            (persp-switch final-slug)
            (neo--ensure-stack-scratch final-slug))

          ;; Create worktree
          (when (eq strategy 'worktree)
            (let ((worktree-path
                   (expand-file-name (string-replace "/" "--" final-slug)
                                     neo/workflow-worktrees-directory)))
              (unless (file-exists-p worktree-path)
                (make-directory (file-name-directory worktree-path) t)
                (let ((default-directory repo-path))
                  (neo--workflow-git-run "worktree" "add" worktree-path branch-name)))
              (message "Switched to worktree: %s" worktree-path)))

          ;; Claim the issue in beads (mark it in progress) now that work started.
          (condition-case err
              (beads-client-update (neo-issue-id issue) :status "in_progress")
            (beads-client-error
             (message "neo-workflow: could not claim issue %s: %s"
                      (neo-issue-id issue) (cadr err))))
          (message "Activated issue %s on branch %s"
                   (neo-issue-id issue) branch-name)
          (neo/workflow-refresh (neo--workflow-get-repo-full-name-by-id repo-id)
                                (neo-issue-id issue)))
      (message "Cannot activate: project root not found"))))

(defun neo--workflow-create-stack-branch (name)
  "Create a git branch NAME off HEAD in the current project, if it doesn't exist.
Creates the branch without checking it out; activation/checkout is `neo--hack'.
Returns NAME."
  (let* ((project (neo--workflow-beads-workspace-as-project))
         (repo-path (and project (neo-project-worktree-path project))))
    (when repo-path
      (let ((default-directory repo-path))
        (unless (neo/workflow-git-branch-exists name)
          (neo/workflow-git-create-branch name "HEAD")))))
  name)

(defun neo--workflow-promote-issue-to-stack (issue)
  "Promote ISSUE to a stack by converting its beads issue to an epic.
Also creates the stack's git branch.  The board re-reads from beads on
refresh, so the promoted issue then renders as a stack."
  (let* ((id (neo-issue-id issue))
         (name (neo--workflow-stack-name id (neo-issue-title issue))))
    (condition-case err
        (progn
          (beads-client-update id :issue-type "epic")
          (neo--workflow-create-stack-branch name)
          (message "Promoted issue %s to stack '%s'." id name))
      (beads-client-error
       (user-error "Failed to promote issue %s to a stack: %s" id (cadr err))))))

(defun neo--workflow-create-child-stack (parent-stack)
  "Create a child stack (beads epic) under PARENT-STACK, with a git branch."
  (let ((title (read-string "New child stack title: ")))
    (unless (string-empty-p title)
      (condition-case err
          (let* ((epic (beads-client-create
                        title
                        :issue-type "epic"
                        :parent (neo-stack-id parent-stack)))
                 (id (or (alist-get 'id epic) (cdr (assoc "id" epic))))
                 (name (neo--workflow-stack-name id title)))
            (neo--workflow-create-stack-branch name)
            (message "Created child stack '%s' under %s."
                     name (neo-stack-name parent-stack)))
        (beads-client-error
         (user-error "Failed to create child stack: %s" (cadr err)))))))

(defun neo--append (object)
  "Create a stack for OBJECT (a beads epic) and its git branch.
An issue without a stack is promoted to a stack; an object that already has
a stack gets a new child stack nested under it."
  (interactive)
  (let ((parent-stack (cond
                       ((neo-stack-p object) object)
                       ((and (neo-issue-p object) (neo-issue-stack object))
                        (neo-issue-stack object)))))
    (cond
     ((and (neo-issue-p object) (not (neo-issue-stack object)))
      (neo--workflow-promote-issue-to-stack object))
     (parent-stack
      (neo--workflow-create-child-stack parent-stack))
     (t
      (user-error "Cannot create a stack for this object"))))
  (neo/workflow-refresh))

(defun neo/workflow-switch-context ()
  "Switch to an existing workflow context (Phase 5 stub)."
  (interactive)
  (message "Context switching (in-memory) is Phase 5."))

;; ============================================================
;; Repo header rendering
;; ============================================================

(defconst neo-icon-private "🔒"
  "Icon for private workspaces.")

(defun neo--insert-repo-header (repo)
  "Insert the repo header line for REPO."
  (let* ((start (point))
         (repo-full-name (neo-repository-full-name repo))
         (project (neo-load-project-by-repo repo-full-name))
         (visibility neo-icon-private))

    (insert
     (propertize (concat repo-full-name " ")
                 'face 'neo-workflow-repo-face
                 'keymap neo/workflow-table-keymap))

    (insert
     (propertize " " 'display `(space :align-to 40)
                 'face 'neo-workflow-repo-face
                 'keymap neo/workflow-table-keymap))

    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map neo/workflow-table-keymap)
      (if project
          (let ((path (neo-project-worktree-path project)))
            (insert (propertize path
                                'face '(shadow neo-workflow-repo-face)
                                'help-echo "Project root"
                                'keymap map)))
        (insert (propertize "📦"
                            'face 'neo-workflow-repo-face
                            'help-echo "No local project root"
                            'keymap map))))

    (insert
     (propertize " " 'display
                 `(space :align-to (- right-margin ,(string-width visibility) 3))
                 'face 'neo-workflow-repo-face
                 'keymap neo/workflow-table-keymap))

    (insert
     (propertize (concat visibility " \n")
                 'face 'neo-workflow-repo-face
                 'keymap neo/workflow-table-keymap))

    (put-text-property start (point) 'repo-header-line t)))

;; ============================================================
;; Narrowing
;; ============================================================

(defvar neo--inhibit-narrowing-restore nil
  "When non-nil, do not restore narrowing after a workflow context restore.")

(defun neo--narrow-to-repo ()
  "Narrow the buffer to the current repository's section."
  (interactive)
  (unless (buffer-narrowed-p)
    (let (start end)
      (save-excursion
        (vtable-beginning-of-table)
        (forward-line -1)
        (setq start (point))
        (forward-line 1)
        (vtable-end-of-table)
        (setq end (point)))
      (when (and start end)
        (narrow-to-region start end)))))

(defun neo--widen ()
  "Widen the buffer."
  (interactive)
  (let ((neo--inhibit-narrowing-restore t))
    (neo--save-workflow-context
      (widen))))

;; ============================================================
;; Filter shortcuts
;; ============================================================

(defun neo--repo-filter-change (repo setting)
  "Set filter for REPO to SETTING and move to table start."
  (vtable-beginning-of-table)
  (neo-workflow-set-repo-issue-filter repo setting))

(defun neo--filter-change (setting global)
  "Apply SETTING filter to current repo, or all repos if GLOBAL is non-nil."
  (if global
      (dolist (repo (neo-load-all-repositories))
        (neo--repo-filter-change (neo-repository-full-name repo) setting))
    (let ((repo (get-text-property (point) 'repo-name)))
      (neo--repo-filter-change repo setting)))
  (neo/workflow-refresh))

(defun neo--filter-global-active () (interactive) (neo-workflow-set-global-filter 'active) (neo/workflow-refresh))
(defun neo--filter-global-open ()   (interactive) (neo-workflow-set-global-filter 'open)   (neo/workflow-refresh))
(defun neo--filter-global-all ()    (interactive) (neo-workflow-set-global-filter nil)      (neo/workflow-refresh))
(defun neo--filter-open   (&optional global) (interactive "P") (neo--filter-change 'open   global))
(defun neo--filter-closed (&optional global) (interactive "P") (neo--filter-change 'closed global))
(defun neo--filter-active (&optional global) (interactive "P") (neo--filter-change 'active global))
(defun neo--filter-all    (&optional global) (interactive "P") (neo--filter-change 'all    global))

;; ============================================================
;; Table keymap and vtable construction
;; ============================================================

(defvar neo/workflow-table-keymap
  (define-keymap
    "n n" #'neo--narrow-to-repo
    "n w" #'neo--widen
    "f A" #'neo--filter-all
    "f a" #'neo--filter-active
    "f o" #'neo--filter-open
    "f c" #'neo--filter-closed
    "F A" #'neo--filter-global-all
    "F a" #'neo--filter-global-active
    "F o" #'neo--filter-global-open
    "+"   #'neo--new-issue-for-repo
    "e"   #'neo--edit-issue-at-point
    "c"   #'neo--close-issue-at-point)
  "Keymap for actions over the entire vtable.")

(defun neo/workflow-make-vtable (objects-fn)
  "Build a vtable whose objects are supplied by OBJECTS-FN."
  (make-vtable
   :use-header-line nil
   :separator-width 1
   :insert t
   :columns `((:name "ID" :width 5 :align 'right
                     :getter ,(lambda (object _)
                                (if (neo-issue-p object)
                                    (propertize (format "%d" (neo-issue-number object))
                                                'face (neo--final-issue-id-face object))
                                  "")))
              (:name "Pri" :width 3 :align 'right
                     :getter ,(lambda (object _)
                                (if (neo-issue-p object) (neo--priority-icon object) "")))
              (:name "Title" :width "80%"
                     :getter ,(lambda (object _)
                                (if (neo-issue-p object)
                                    (neo--propertize-issue-title object)
                                  (neo--propertize-stack-title object))))
              (:name "Labels" :width 20 :align 'right
                     :getter ,(lambda (object _)
                                (if (neo-issue-p object) (neo--labels object) ""))))
   :keymap neo/workflow-table-keymap
   :actions `("h" neo--hack
              "a" neo--append
              "TAB" neo--toggle-object-visibility
              "S-<up>" neo--priority-up
              "S-<down>" neo--priority-down
              "RET" neo--select-thing)
   :objects-function objects-fn))

;; ============================================================
;; Context save/restore macro
;; ============================================================

(defmacro neo--save-workflow-context (&rest body)
  "Save current workflow context, execute BODY, and restore context."
  (declare (indent 0) (debug t))
  `(let ((saved-repo (get-text-property (point) 'repo-name))
         (saved-header-p (get-text-property (point) 'repo-header-line))
         (saved-object (when (vtable-current-table)
                         (vtable-current-object)))
         (was-narrowed (buffer-narrowed-p))
         (narrowed-repo (when (buffer-narrowed-p)
                          (get-text-property (point-min) 'repo-name))))
     (progn ,@body)
     (if (and saved-repo (assoc-string saved-repo neo--repo-info-alist))
         (let* ((info (assoc-string saved-repo neo--repo-info-alist))
                (table (nth 1 info))
                (start-pos (nth 2 info)))
           (cond
            (saved-object
             (let ((found (seq-find (lambda (obj) (neo--objects-match-p obj saved-object))
                                    (vtable-objects table))))
               (if found
                   (progn
                     (vtable-goto-table table)
                     (vtable-goto-object found))
                 (goto-char start-pos))))
            (saved-header-p
             (goto-char start-pos))
            (t
             (goto-char start-pos))))
       (goto-char (point-min)))

     (when (and was-narrowed narrowed-repo (not (bound-and-true-p neo--inhibit-narrowing-restore)))
       (let ((info (assoc-string narrowed-repo neo--repo-info-alist)))
         (when info
           (narrow-to-region (nth 2 info) (nth 3 info)))))))

;; ============================================================
;; Main refresh
;; ============================================================

(defun neo/workflow-refresh (&optional target-repo-name target-issue-id)
  "Refresh the workflow status buffer.
If TARGET-REPO-NAME and TARGET-ISSUE-ID are provided, position point on that issue."
  (interactive)
  (when-let* ((buffer (get-buffer "*NEO Workflow*")))
    (with-current-buffer buffer
      (setq neo--repo-info-alist nil)
      (let ((inhibit-read-only t)
            (global-filter (neo-workflow-get-global-filter)))
        (widen)
        (erase-buffer)
        (dolist (repo (neo-load-all-repositories))
          (let* ((repo-name (neo-repository-full-name repo))
                 (show-repo
                  (cond
                   ((eq global-filter 'active)
                    (let ((issues (neo-db-get-issues-for-repo (neo-repository-id repo))))
                      (seq-some #'neo--issue-active-p issues)))
                   ((eq global-filter 'open)
                    (let ((issues (neo-db-get-issues-for-repo (neo-repository-id repo))))
                      (seq-some (lambda (i) (eq (neo-issue-state i) 'open)) issues)))
                   (t t))))

            (when show-repo
              (let ((start (point)))
                (neo--insert-repo-header repo)

                (cl-letf (((symbol-function #'vtable--insert-header-line)
                           (lambda (_table _width _spacer))))
                  (let* ((table (neo/workflow-make-vtable
                                 (lambda () (neo--get-sorted-issues-for-repo repo)))))
                    (let ((end (point)))
                      (setf (alist-get repo-name neo--repo-info-alist) (list table start end)))
                    (goto-char (point-max))
                    (add-text-properties start (point) `(repo-name ,repo-name))
                    (insert "\n")))))))

      (if (and target-repo-name target-issue-id)
          (let ((info (assoc-string target-repo-name neo--repo-info-alist)))
            (if info
                (let* ((table (nth 1 info))
                       (target-object
                        (seq-find (lambda (obj)
                                    (and (neo-issue-p obj)
                                         (equal (neo-issue-id obj) target-issue-id)))
                                  (vtable-objects table))))
                  (if target-object
                      (progn
                        (vtable-goto-table table)
                        (vtable-goto-object target-object)
                        (redisplay)
                        (hl-line-highlight))
                    (goto-char (point-min))))
              (goto-char (point-min))))
        (goto-char (point-min)))))))

;; ============================================================
;; Sync (beads is authoritative; a refresh re-reads everything)
;; ============================================================

(defun neo/workflow-sync-and-refresh (&optional repo-name issue-id)
  "Re-fetch all data from beads and refresh the board.
REPO-NAME and ISSUE-ID are used to position point after refresh."
  (interactive)
  (message "neo-workflow: refreshing from beads...")
  ;; Invalidate the beads workspace cache so we get fresh data.
  (beads-client-clear-cache)
  (neo/workflow-refresh repo-name issue-id))

;; ============================================================
;; Entry point
;; ============================================================

;;;###autoload
(defun neo/workflow-status ()
  "Show the Neo Workflow board backed by beads + git."
  (interactive)
  (let ((buffer (get-buffer-create "*NEO Workflow*")))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (neo-workflow-status-mode)
      (neo/workflow-refresh))))

(neo/application "Neo Workflow"
  :setup (neo/workflow-status)
  :bind "w")

(provide 'neo-workflow-status)
;;; neo-workflow-status.el ends here
