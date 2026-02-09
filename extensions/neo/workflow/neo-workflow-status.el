;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'json)
(require 'neo-workflow-slug)
(require 'neo-workflow-models)
(require 'neo-workflow-db)
(require 'neo-workflow-git)
(require 'neo-workflow-context)
(require 'github-models)
(require 'auth-source)
(require 'vtable)

(defgroup neo-workflow-status nil
  "Act on issues."
  :group 'neo-workflow)

(defcustom neo/workflow-clone-base-dir "~/Projects"
  "Base directory for cloning repositories."
  :type 'directory
  :group 'neo-workflow-status)

(defcustom neo/workflow-github-organization "poly-repo"
  "GitHub organization to fork repositories into.
If nil, fork into the current user's account."
  :type '(choice (const :tag "User account" nil)
                 (string :tag "Organization"))
  :group 'neo-workflow-status)

(defcustom neo/workflow-fork-visibility "private"
  "Visibility of the forked repository.
Can be \"public\", \"private\", or \"internal\"."
  :type '(choice (const "public")
                 (const "private")
                 (const "internal"))
  :group 'neo-workflow-status)

(defcustom neo/workflow-worktrees-directory "~/Projects/worktrees"
  "Directory to store git worktrees."
  :type 'directory
  :group 'neo-workflow-status)

(defvar neo/current-context nil
  "The current workflow context object (neo-context struct).")

(defun neo/workflow-clone-repo (repo-full-name)
  "Clone REPO-FULL-NAME into `neo/workflow-clone-base-dir` and add to projects DB.
Uses authentication token from `auth-source` if available."
  (interactive "sRepository (user/repo): ")
  (let* ((parts (split-string repo-full-name "/"))
         (owner (car parts))
         (repo-name (cadr parts))
         (target-dir (expand-file-name repo-name neo/workflow-clone-base-dir))
         (token (neo--get-github-token owner))
         (url (if token
                  (format "https://%s@github.com/%s.git" token repo-full-name)
                (format "git@github.com:%s.git" repo-full-name)))
         (buffer-name "*neo-git-clone*"))
    (if (file-exists-p target-dir)
        (message "Directory %s already exists. Skipping clone." target-dir)
      (message "Cloning %s to %s..." (if token (format "https://github.com/%s.git" repo-full-name) url) target-dir)
      (make-directory neo/workflow-clone-base-dir t)
      (let ((proc (make-process :name "neo-clone"
                                :buffer buffer-name
                                :command (list "git" "clone" url target-dir)
                                :sentinel (lambda (p e)
                                            (when (eq (process-status p) 'exit)
                                              (if (zerop (process-exit-status p))
                                                  (progn
                                                    (message "Clone successful: %s" repo-full-name)
                                                    ;; Add to DB
                                                    (neo-db-insert-project repo-name repo-name "git" nil target-dir)
                                                    (neo/workflow-refresh))
                                                (message "Clone failed: %s" (string-trim e))
                                                (pop-to-buffer (process-buffer p))))))))
        (set-process-query-on-exit-flag proc nil)
        proc))))

(defun neo--get-current-username ()
  (string-trim (shell-command-to-string "gh api user -q .login")))

(defun neo--fetch-and-insert-repo (full-name)
  "Fetch repository details from GitHub and insert into DB."
  (let* ((cmd (format "gh api repos/%s" full-name))
         (json-str (string-trim (shell-command-to-string cmd))))
    (condition-case err
        (let* ((data (json-read-from-string json-str))
               (id (alist-get 'id data))
               (full-name (alist-get 'full_name data))
               (fork (if (eq (alist-get 'fork data) t) 1 0))
               (created-at (alist-get 'created_at data))
               (pushed-at (alist-get 'pushed_at data))
               (updated-at (alist-get 'updated_at data))
               (visibility (downcase (alist-get 'visibility data)))
               (forks (alist-get 'forks_count data))
               (default-branch (alist-get 'default_branch data)))
          (neo-db-insert-repository id full-name fork created-at pushed-at updated-at visibility forks default-branch))
      (error
       (message "Failed to fetch repo info for %s. Error: %s" full-name err)
       (message "Command: %s" cmd)
       (message "Output: %S" json-str)))))

(defun neo/workflow-fork-repo (url &optional clone-p)
  "Fork the repository at URL to the configured organization or user account.
If `neo/workflow-github-organization` is set, fork into that organization.
If CLONE-P is non-nil (interactively, with prefix arg), clone the forked repository
into `neo/workflow-clone-base-dir`."
  (interactive (list (read-string "GitHub URL: ") current-prefix-arg))
  (let* ((repo-name (file-name-base (directory-file-name url)))
         (username (or neo/workflow-github-organization (neo--get-current-username)))
         (target-full-name (concat username "/" repo-name)))

    (if (zerop (call-process "gh" nil nil nil "repo" "view" target-full-name))
        (progn
          (message "Fork %s already exists. Ensuring issues are enabled, setting visibility to %s and updating DB..."
                   target-full-name neo/workflow-fork-visibility)
          (call-process "gh" nil nil nil "repo" "edit" target-full-name
                        "--enable-issues")
          (neo--fetch-and-insert-repo target-full-name)
          (when clone-p
            (neo/workflow-clone-repo target-full-name))
          (neo/workflow-refresh))
      
      (message "Forking %s to %s..." url target-full-name)
      (let ((args (append (list "repo" "fork" url "--clone=false")
                          (when neo/workflow-github-organization
                            (list "--org" neo/workflow-github-organization)))))
        (make-process :name "neo-fork"
                      :buffer "*neo-gh-fork*"
                      :command (cons "gh" args)
                      :sentinel (lambda (p e)
                                  (when (eq (process-status p) 'exit)
                                    (if (zerop (process-exit-status p))
                                        (progn
                                          (message "Fork successful: %s. Enabling issues and setting visibility to %s..."
                                                   target-full-name neo/workflow-fork-visibility)
                                          (call-process "gh" nil nil nil "repo" "edit" target-full-name
                                                        "--enable-issues"
                                                        "--visibility" neo/workflow-fork-visibility
                                                        "--accept-visibility-change-consequences")
                                          (neo--fetch-and-insert-repo target-full-name)
                                          (when clone-p
                                            (neo/workflow-clone-repo target-full-name))
                                          (neo/workflow-refresh))
                                      (message "Fork failed: %s" (string-trim e))
                                      (pop-to-buffer (process-buffer p))))))))))

(defface neo-workflow-repo-face
  '((t
     :inherit header-line
     :weight bold
     :extend t))
  "Face for workflow repo names."
  :group 'neo-workflow-status)

(defface neo-workflow-issue-open-face
  '((t :inherit default))
  "Face for open issues in workflow status."
  :group 'neo-workflow-status)

(defface neo-workflow-issue-active-face
  '((t :inherit magit-branch-local))
  "Face for active issues in workflow status."
  :group 'neo-workflow-status)

(defface neo-workflow-issue-completed-face
  '((t :inherit magit-dimmed :strike-through t))
  "Face for completed issues in workflow status."
  :group 'neo-workflow-status)

(defface neo-workflow-issue-id-face
  '((t :inherit magit-hash))
  "Face for issue IDs in workflow status."
  :group 'neo-workflow-status)

(defface neo-workflow-issue-title-face
  '((t :inherit default))
  "Face for issue titles in workflow status."
  :group 'neo-workflow-status)

(defface neo-workflow-priority-critical-face
  '((t :foreground "white" :background "#d73a49"))
  "Face for critical priority issues."
  :group 'neo-workflow-status)

(defface neo-workflow-priority-high-face
  '((t :foreground "white" :background "#f66a0a"))
  "Face for high priority issues."
  :group 'neo-workflow-status)

(defface neo-workflow-priority-medium-face
  '((t :foreground "black" :background "#ffd33d"))
  "Face for medium priority issues."
  :group 'neo-workflow-status)

(defface neo-workflow-priority-low-face
  '((t :foreground "white" :background "#8b949e"))
  "Face for low priority issues."
  :group 'neo-workflow-status)

(defface neo-workflow-priority-icon-critical-face
  '((t :foreground "#d73a49"))
  "Face for critical priority icon."
  :group 'neo-workflow-status)

(defface neo-workflow-priority-icon-high-face
  '((t :foreground "#4338CA"))
  "Face for high priority icon."
  :group 'neo-workflow-status)

(defface neo-workflow-priority-icon-mid-face
  '((t :foreground "#3B82F6"))
  "Face for medium priority icon."
  :group 'neo-workflow-status)

(defface neo-workflow-priority-icon-low-face
  '((t :foreground "#9CA3AF"))
  "Face for low priority icon."
  :group 'neo-workflow-status)

(defvar neo-workflow-mode-map (make-sparse-keymap)
  "Keymap for neo-workflow-mode.")

;;;###autoload
(define-derived-mode neo-workflow-mode special-mode "NeoWorkflow"
  "A special mode for neo workflow."
  :group 'neo-workflow)

(defvar neo-workflow-status-mode-map (make-sparse-keymap)
  "Keymap for `neo-workflow-status-mode'.")

(set-keymap-parent neo-workflow-status-mode-map neo-workflow-mode-map)
;; navigation between tables / sections
(define-key neo-workflow-status-mode-map (kbd "TAB") #'neo-workflow-next-table)
(define-key neo-workflow-status-mode-map (kbd "<backtab>") #'neo-workflow-prev-table)

;; global actions
(define-key neo-workflow-status-mode-map (kbd "g") #'neo/workflow-sync-and-refresh)
(define-key neo-workflow-status-mode-map (kbd "q") #'quit-window)
(define-key neo-workflow-status-mode-map (kbd "N") #'neo--repo-next)
(define-key neo-workflow-status-mode-map (kbd "P") #'neo--repo-prev)
(define-key neo-workflow-status-mode-map (kbd "F") #'neo/workflow-fork-repo)
;;;###autoload
(define-derived-mode neo-workflow-status-mode neo-workflow-mode "NeoWorkflowStatus"
  "A special mode for neo workflow status."
  :group 'neo-workflow-status
  (hl-line-mode 1)
  (setq-local cursor-type nil
	      truncate-lines t
              cursor-in-non-selected-windows nil))

(defun neo/tree-render (tree &optional prefix lastp)
  "Render TREE using Unicode box drawing characters."
  (let* ((label (car tree))
         (children (cdr tree))
         (connector (if prefix
                        (if lastp "└─ " "├─ ")
                      ""))
         (line (concat prefix connector label "\n"))
         (child-prefix (when prefix
                         (concat prefix (if lastp "   " "│  ")))))
    (concat
     line
     (mapconcat
      (lambda (child)
        (neo/tree-render child
                         (or child-prefix "")
                         (eq child (car (last children)))))
      children
      ""))))

(defun neo--select-thing (object)
  (interactive))

(defun neo--vtable-update-object (table new old)
  ;; TODO this should be equivalent to vtable-update-object
  ;; except that it isn't. This has the beneficial property
  ;; of working. Meh, seems working now
  (vtable-update-object table new old)
  )

(defun neo--ensure-stack-scratch (stack-name)
  "Switch to the standard perspective scratch buffer for STACK-NAME, initializing it if empty."
  (let ((scratch-name (format "*scratch* (%s)" stack-name)))
    (with-current-buffer (get-buffer-create scratch-name)
      (lisp-interaction-mode)
      (when (= (buffer-size) 0)
        (insert (format ";; Scratch buffer for stack: %s\n\n" stack-name))))
    (switch-to-buffer scratch-name)))

(defun neo--resolve-branch-conflict (repo-path branch-name strategy can-rename &optional default-branch)
  "Resolve conflict if BRANCH-NAME is checked out in main repo at REPO-PATH.
If STRATEGY is 'worktree and conflict exists:
- If CAN-RENAME is t: Return a new unique branch name (BRANCH-NAME-SUFFIX).
- If CAN-RENAME is nil: Attempt to switch main repo to DEFAULT-BRANCH.
  If main repo is dirty, signal user-error.
Returns the resolved branch name."
  (let ((default-directory repo-path))
    (if (and (eq strategy 'worktree)
             (string= (neo/workflow-git-current-branch-uncached) branch-name))
        (if can-rename
            (let ((new-name (format "%s-%04d" branch-name (random 10000))))
              (message "Branch %s checked out in main repo. Renaming to %s." branch-name new-name)
              new-name)
          ;; Try to free main repo
          (let ((status (neo/workflow-git-repo-status)))
            (if (or (neo-workflow-git-repo-status-open-changes status)
                    (neo-workflow-git-repo-status-conflicts status)
                    (neo-workflow-git-repo-status-rebase-in-progress status))
                (user-error "Main repo at %s is dirty or busy and has %s checked out. Please clean it or switch branch." repo-path branch-name)
              (unless default-branch
                (error "Default branch not provided for repo switch"))
              (message "Switching main repo to %s to free up %s..." default-branch branch-name)
              (neo--workflow-git-run "checkout" default-branch)
              branch-name)))
      branch-name)))

(defun neo--hack-get-details (object)
  "Extract (ISSUE REPO-ID INITIAL-STACK) from OBJECT.
OBJECT can be a neo-issue or a neo-stack."
  (cond
   ((neo-issue-p object)
    (list object
          (neo-issue-repository-id object)
          (neo-issue-stack object)))
   ((neo-stack-p object)
    ;; For stack, issue might be nil.
    ;; We need repository-id which is in stack branch... wait, stack struct has no repository-id directly?
    ;; Check neo-workflow-models.el: (cl-defstruct neo-stack ... issue-id branch ...)
    ;; In DB schema it has repository_id.
    ;; neo-load-stack doesn't populate repository-id slot (it's not in struct definition in models.el?),
    ;; but it loads branch which has repository-id?
    ;; Let's check neo-stack struct in models.el again.
    ;; It seems I cannot easily get repo-id from stack struct if it's not there.
    ;; But neo-load-stack takes repository-id as arg.
    ;; If I have the stack object, maybe I can find the repo ID via its branch?
    ;; (neo-stack-branch object) -> neo-branch.
    ;; neo-branch has no repository-id field in struct?
    ;; Let's assume we need to fix models first if this is missing.
    ;; BUT, neo-db-insert-stack takes repository-id.
    ;; Let's assume for now we can get it or we pass it.
    ;; Actually, `neo-load-stack` returns a stack struct.
    ;; Let's check neo-workflow-models.el again.
    (let ((repo-id (neo--get-repo-id-for-stack object)))
      (list nil repo-id object)))
   (t (list nil nil nil))))

(defun neo--get-repo-id-for-stack (stack)
  "Get repository ID for a stack.
This is a workaround because neo-stack struct doesn't have repository-id field yet.
We try to infer it from the branch or query DB."
  ;; If stack has a branch object, and branch object doesn't have repo-id...
  ;; Query DB using stack ID?
  (if (neo-stack-id stack)
      (caar (sqlite-select (neo-open-db) "SELECT repository_id FROM stacks WHERE id = ?" (list (neo-stack-id stack))))
    nil))

(defun neo--hack (object)
  "Create and switch to a full development context for OBJECT (issue or stack).
This includes:
1. Creating/switching to a Git branch or worktree.
2. Creating/switching to a named perspective.
3. Creating a stack and linking it all in the DB."
  (interactive)
  (pcase-let ((`(,issue ,repo-id ,initial-stack) (neo--hack-get-details object)))
    (when repo-id
      (let* ((repo-name (neo--workflow-get-repo-full-name-by-id repo-id))
             (repo-short-name (cadr (split-string repo-name "/")))
             (repo-path (expand-file-name repo-short-name neo/workflow-clone-base-dir))
             (repo (neo-load-repository repo-id))
             (default-branch (or (and repo (neo-repository-default-branch repo)) "main"))
             (context (when initial-stack
                        (neo/workflow-load-context-for-stack repo-id (neo-stack-id initial-stack))))
             (strategy (let ((default-directory repo-path))
                         (neo--workflow-choose-workspace-strategy))))
        
        (if context
            ;; Context exists
            (let* ((stack (neo-context-stack context))
                   (stack-name (neo-stack-name stack))
                   (branch-name (neo--get-branch-name stack))
                   (perspective (neo-context-perspective context)))

              ;; Resolve potential worktree conflict
              (neo--resolve-branch-conflict repo-path branch-name strategy nil default-branch)
              
              (message "Switching to existing context: %s" perspective)
              
              ;; 1. Perspective Switch (Always)
              (when (featurep 'perspective)
                (persp-switch perspective)
                (neo--ensure-stack-scratch perspective))

              ;; 2. Git Context Switch
              (cond
               ((eq strategy 'worktree)
                (let ((worktree-path (expand-file-name (concat repo-short-name "--" stack-name) neo/workflow-worktrees-directory)))
                  (unless (file-exists-p worktree-path)
                    (make-directory (file-name-directory worktree-path) t)
                    (let ((default-directory repo-path))
                      (neo--workflow-git-run "worktree" "add" worktree-path branch-name)))
                  (message "Switched to worktree: %s" worktree-path)))
               
               (t
                ;; Repo strategy
                ))

              ;; 3. Set Current Context
              (setq neo/current-context context))
          
          ;; No context found (either no stack, or stack exists but no context record)
          (if initial-stack
              ;; Stack exists but no context record. Recover by creating context from stack.
              (let ((stack-name (neo-stack-name initial-stack))
                    (branch-name (neo--get-branch-name initial-stack)))
                
                ;; Resolve potential worktree conflict
                (neo--resolve-branch-conflict repo-path branch-name strategy nil default-branch)

                (message "Recovering context for existing stack: %s" stack-name)
                
                (when (featurep 'perspective)
                  (persp-switch stack-name)
                  (neo--ensure-stack-scratch stack-name))
                
                (cond
                 ((eq strategy 'worktree)
                  (let ((worktree-path (expand-file-name (concat repo-short-name "--" stack-name) neo/workflow-worktrees-directory)))
                    (unless (file-exists-p worktree-path)
                      (make-directory (file-name-directory worktree-path) t)
                      (let ((default-directory repo-path))
                        (neo--workflow-git-run "worktree" "add" worktree-path branch-name)))
                    (message "Switched to worktree: %s" worktree-path)))
                 (t))
                
                (let ((new-context (make-neo-context 
                                    :repository (neo-load-repository repo-id)
                                    :stack initial-stack
                                    :perspective stack-name)))
                  (setq neo/current-context new-context)
                  (neo/workflow-db-upsert-context repo-id (neo-stack-id initial-stack) stack-name)))

            ;; New stack path (Only possible if issue is provided)
            (if issue
                (let* ((issue-to-modify (copy-neo-issue issue))
                       (username (or (neo--get-current-username) "user"))
                       (base-slug (neo-issue-title-to-slug (neo-issue-number issue-to-modify) (neo-issue-title issue-to-modify)))
                       (slug (format "%s/%s" username base-slug))
                       ;; Resolve potential worktree conflict (with rename)
                       (final-slug (neo--resolve-branch-conflict repo-path slug strategy t default-branch))
                       (stack-name final-slug)
                       (branch-name final-slug)
                       (worktree-dirname (string-replace "/" "--" final-slug)))
                  
                  ;; 1. Git Branch Creation
                  (let ((default-directory repo-path))
                    (unless (neo/workflow-git-branch-exists branch-name)
                      (neo/workflow-git-create-branch branch-name default-branch)))
                  
                  ;; 2. Update Issue
                  (let ((new-stack (make-neo-stack :name stack-name :title (neo-issue-title issue-to-modify))))
                    (setf (neo-issue-stack issue-to-modify) new-stack)
                    (neo-db-upsert-issue issue-to-modify)
                    (setq initial-stack new-stack)) ;; Update local var for context creation
                  
                  ;; 3. Refresh
                  (neo/workflow-refresh repo-name (neo-issue-id issue))
                  
                  ;; 4. Perspective Switch
                  (when (featurep 'perspective)
                    (persp-switch stack-name)
                    (neo--ensure-stack-scratch stack-name))

                  ;; 5. Git Context Switch
                  (cond
                   ((eq strategy 'worktree)
                    (let ((worktree-path (expand-file-name (concat repo-short-name "/" worktree-dirname) neo/workflow-worktrees-directory)))
                      (unless (file-exists-p worktree-path)
                        (make-directory (file-name-directory worktree-path) t)
                        (let ((default-directory repo-path))
                          (neo--workflow-git-run "worktree" "add" worktree-path branch-name)))
                      (message "Created and switched to worktree: %s" worktree-path)))
                   (t))
                  
                  ;; 6. Create Context
                  (when-let ((saved-stack (neo-load-stack stack-name repo-id)))
                    (let ((new-context (make-neo-context 
                                        :repository (neo-load-repository repo-id)
                                        :stack saved-stack
                                        :perspective stack-name)))
                      (setq neo/current-context new-context)
                      (neo/workflow-db-upsert-context repo-id (neo-stack-id saved-stack) stack-name)))
                  
                  (message "Created and switched to stack: %s" stack-name))
              (message "Cannot create new stack without an issue object."))))))))
					;  (neo/workflow-refresh))))

(defun neo/workflow-switch-context ()
  "Switch to an existing workflow context (stack)."
  (interactive)
  (let* ((stacks (neo-db-get-all-stacks))
         (candidates (mapcar (lambda (s) 
                               (let ((display-name (format "%s (%s)" 
                                                           (or (plist-get s :title) (plist-get s :name))
                                                           (plist-get s :name))))
                                 (cons display-name s))) 
                             stacks))
         (selection (completing-read "Switch to context: " candidates))
         (stack-info (cdr (assoc selection candidates))))
    (when stack-info
      (let* ((stack-name (plist-get stack-info :name))
             (repo-id (plist-get stack-info :repository-id))
             (stack (neo-load-stack stack-name repo-id)))
        (if stack
            (neo--hack stack)
          (message "Failed to load stack %s" stack-name))))))

(defun neo--append (object)
  (interactive)
  (let* ((table (vtable-current-table))
         (repo-name (get-text-property (point) 'repo-name))
         (repo-id (neo--get-repo-id-by-full-name repo-name)))
    (cond
     ;; Case 1: Issue without a stack. Create a stack for it.
     ((and (neo-issue-p object) (not (neo-issue-stack object)))
      (let* ((issue-to-modify (copy-neo-issue object))
             (stack-name (neo-issue-title-to-slug (neo-issue-number issue-to-modify) (neo-issue-title issue-to-modify)))
             (new-stack (make-neo-stack :name stack-name :title (neo-issue-title issue-to-modify))))
        (setf (neo-issue-stack issue-to-modify) new-stack)
        (neo-db-upsert-issue issue-to-modify)
        (message "Created stack '%s' for issue #%d. Refreshing." stack-name (neo-issue-number issue-to-modify))
        (neo/workflow-refresh)))

     ;; Case 2: Object with a stack (issue or stack itself). Create a child stack.
     (t
      (let ((parent-stack
             (cond
              ((neo-stack-p object) object)
              ((neo-issue-p object) (neo-issue-stack object))
              (t nil))))
        (when parent-stack
          (let ((child-stack-title (read-string "New child stack title: ")))
            (when (not (string-empty-p child-stack-title))
              (let* ((child-slug (neo-issue-title-to-slug nil child-stack-title))
                     (new-stack-name (concat (neo-stack-name parent-stack) "/" child-slug)))
                ;; Pass repo-id to insert-branch
                (neo-db-insert-branch new-stack-name repo-id nil nil nil nil nil nil)
                ;; Pass all required args to insert-stack, including parent-id
                (neo-db-insert-stack new-stack-name new-stack-name nil repo-id child-stack-title (neo-stack-id parent-stack))

                ;; Load the new stack using repo-id and update UI without full refresh
                (if-let ((new-stack (neo-load-stack new-stack-name repo-id)))
                    (progn
                      (setf (neo-stack-children-stacks parent-stack)
                            (append (neo-stack-children-stacks parent-stack) (list new-stack)))
                      (neo--vtable-update-object table parent-stack parent-stack)
                      (message "Created child stack '%s'." new-stack-name))
                  (message "Created stack '%s', but failed to update UI. Refreshing." new-stack-name))))))))))
  (neo/workflow-refresh))

(defun neo--clear-stacks-for-current-table-issues ()
  "For development: remove stack and priority from all issues in the current vtable."
  (interactive)
  (if-let ((table (vtable-current-table)))
      (let* ((count 0)
             (priority-labels-to-remove '("low" "med" "mid" "high" "critical"))
             (priority-regexp (regexp-opt priority-labels-to-remove 'words)))
        (dolist (object (vtable-objects table))
          (when (neo-issue-p object)
            (setf (neo-issue-stack object) nil)
            (setf (neo-issue-labels object)
                  (cl-remove-if (lambda (l)
                                  (string-match-p priority-regexp (neo-label-name l)))
                                (neo-issue-labels object)))
            (neo-db-upsert-issue object)
            (cl-incf count)))
        (message "Cleared stacks and priorities for %d issues. Refreshing..." count)
        (neo/workflow-refresh))
    (message "No vtable at point.")))


(defun neo--repo-next ()
  "Move to the next line with `repo-header-line`."
  (interactive)
  (let ((pos (save-excursion
	       (if (get-text-property (line-beginning-position) 'repo-header-line)
                   (forward-line 1))
	       (let (p done)
                 (while (not done)
                   (if (get-text-property (line-beginning-position) 'repo-header-line)
		       (setq p (line-beginning-position)
                             done t)
                     (if (>= (point) (point-max))
                         (setq done t)
		       (forward-line 1))))
                 p))))
    (when pos
      (goto-char pos)
      (recenter-top-bottom 0))))

(defun neo--repo-prev ()
  "Move to the previous line with `repo-header-line`."
  (interactive)
  (let ((pos (save-excursion
	       (if (get-text-property (line-beginning-position) 'repo-header-line)
                   (forward-line -1))
	       (let (p done)
                 (while (not done)
                   (if (get-text-property (line-beginning-position) 'repo-header-line)
		       (setq p (line-beginning-position)
                             done t)
                     (if (bobp)
                         (setq done t)
		       (forward-line -1))))
                 p))))
    (when pos
      (goto-char pos)
      (recenter-top-bottom 0))))

(defun neo--get-priority-from-labels (labels)
  "Extract priority from a list of LABELS.
If multiple priority labels are present, return the most critical one."
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

(defun neo--get-issue-status-face (state)
  "Return face for issue STATE."
  (pcase state
    ('open 'neo-workflow-issue-open-face)
    ('active 'neo-workflow-issue-active-face)
    ('closed 'neo-workflow-issue-completed-face)
    (_ nil)))

(defvar neo--repo-info-alist nil
  "Alist mapping repo-name to a list (TABLE START-POS END-POS).")

(defun neo-workflow-get-table-for-repo (repo-name)
  "Get the vtable for REPO-NAME.
Interactively, prompt for REPO-NAME and display the table info."
  (interactive "sRepo name: ")
  (if-let ((info (assoc-string repo-name neo--repo-info-alist)))
      (let ((table (car (cdr info))))
        table)
    (message "No table found for repo: %s" repo-name)))

(defun neo--goto-table (repo-name)
  (interactive)
  (vtable-goto-table (neo-workflow-get-table-for-repo repo-name)))

(defvar neo-workflow-repo-issue-filter-alist nil
  "Alist mapping repository full names to issue filter types.
Possible filter types are 'open, 'closed, 'active, 'all.
Defaults to 'open if a repo is not in the alist.")

(defcustom neo/workflow-sort-by-priority t
  "When non-nil, sort issues by priority."
  :type 'boolean
  :group 'neo-workflow-status)

(defun neo-workflow-set-repo-issue-filter (repo-full-name filter-type)
  "Set the issue filter for REPO-FULL-NAME to FILTER-TYPE.
FILTER-TYPE must be one of 'open, 'closed, 'active, or 'all."
  (unless (memq filter-type '(open closed active all))
    (error "Invalid filter type: %s" filter-type))
  (let ((entry (assoc repo-full-name neo-workflow-repo-issue-filter-alist)))
    (if entry
        (setcdr entry filter-type)
      (push (cons repo-full-name filter-type) neo-workflow-repo-issue-filter-alist))))

(defun neo-workflow-get-repo-issue-filter (repo-full-name)
  (or (cdr (assoc-string repo-full-name neo-workflow-repo-issue-filter-alist)) 'open))

(defun neo--issue-filter (issues repo-name)
  "Filter ISSUES for REPO-NAME based on `neo-workflow-repo-issue-filter-alist'."
  (let ((filter-type (neo-workflow-get-repo-issue-filter repo-name)))
    (seq-filter (lambda (issue)
                  (and (string= (neo-issue-type issue) "Issue")
		       (pcase filter-type
			 ('all t)
			 ('open (eq (neo-issue-state issue) 'open))
			 ('closed (eq (neo-issue-state issue) 'closed))
			 ('active (neo-issue-stack issue))
			 (_ (eq (neo-issue-state issue) 'open)))))
		issues)))

(defun neo--repo-filter-change (repo setting)
  (vtable-beginning-of-table)
  (let ((start (point)))
    (neo-workflow-set-repo-issue-filter repo setting)))

(defun neo--filter-change (setting global)
  (if global                                  
      (dolist (repo (neo-load-all-repositories))
	(neo--repo-filter-change (neo-repository-full-name repo) setting))
    (let ((repo (get-text-property (point) 'repo-name)))
      (neo--repo-filter-change repo setting)))
  ;; TODO when changing a single repo we could do with less than a full refresh
  (neo/workflow-refresh))

(defun neo--filter-open (&optional global)
  (interactive "P")
  (neo--filter-change 'open global))

(defun neo--filter-closed (&optional global)
  (interactive "P")
  (neo--filter-change 'closed global))

(defun neo--filter-active (&optional global)
  (interactive "P")
  (neo--filter-change 'active global))

(defun neo--filter-all (&optional global)
  (interactive "P")
  (neo--filter-change 'all global))

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
  (interactive)
  ;; Preserve context when widening
  (let ((neo--inhibit-narrowing-restore t))
    (neo--save-workflow-context
      (widen))))

(defun neo--new-issue-for-repo ()
  "Create a new issue for the repository at point."
  (interactive)
  (if-let ((repo-name (get-text-property (point) 'repo-name)))
      (neo-workflow-issue-open-template repo-name)
    (user-error "No repository found at point")))

(defun neo--edit-issue-at-point ()
  "Edit the issue at point."
  (interactive)
  (if-let* ((table (vtable-current-table))
            (object (vtable-current-object))
            (_ (neo-issue-p object))
            (repo-id (neo-issue-repository-id object))
            (repo-name (neo--workflow-get-repo-full-name-by-id repo-id))
            (issue-number (neo-issue-number object)))
      (neo-workflow-issue-open-template repo-name issue-number)
    (user-error "No issue found at point")))

(defvar neo/workflow-table-keymap (define-keymap
				    "n n" #'neo--narrow-to-repo
				    "n w" #'neo--widen
				    "f A" #'neo--filter-all
				    "f a" #'neo--filter-active
				    "f o" #'neo--filter-open
				    "f c" #'neo--filter-closed
                                    "+"   #'neo--new-issue-for-repo
                                    "e"   #'neo--edit-issue-at-point)
  "A keymap for action over the entire vtable, including the title")

(defun neo--labels (issue)
  (mapcar (lambda (label) (neo-label-name label)) (neo-issue-labels issue)))

(defconst neo--priority-labels
  '("low" "mid" "high" "critical")
  "Label names that should be filtered out from labels shown. These are
mapped to icons in their own column and edited separately")

(defun neo--priority (issue)
  "Return the priority label for ISSUE.
If multiple priority labels are present, return the most critical one.
Return nil if no priority label is present."
  (let* ((labels (mapcar #'neo-label-name (neo-issue-labels issue)))
         (found-priority nil))
    (dolist (p neo--priority-labels)
      (when (cl-member p labels :test #'string-equal)
        (setq found-priority p)))
    (or found-priority "")))

(defun neo--get-issue-priority-score (issue)
  "Return a numeric score for ISSUE's priority. Lower is higher priority."
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

(setq neo--priority-icons
      '(("critical" . "🔥")
	("high"     . "P1")
	("mid"      . "P2")
	("low"      . "P3")))

(defun neo--get-priority-icon-face (priority-name)
  "Return face for a given PRIORITY-NAME string."
  (pcase priority-name
    ("critical" 'neo-workflow-priority-icon-critical-face)
    ("high" 'neo-workflow-priority-icon-high-face)
    ("mid" 'neo-workflow-priority-icon-mid-face)
    ("low" 'neo-workflow-priority-icon-low-face)
    (_ nil)))

(defun neo--priority-icon (issue)
  (let ((priority-name (neo--priority issue)))
    (if (string-empty-p priority-name)
        ""
      (let* ((icon (alist-get priority-name neo--priority-icons "" nil #'string-equal))
             (priority-label (seq-find (lambda (l) (string-equal (neo-label-name l) priority-name))
				       (neo-issue-labels issue)))
             (desc (when priority-label (neo-label-description priority-label)))
             (face (neo--get-priority-icon-face priority-name)))
        (propertize icon
                    'face face
                    'help-echo (when (and desc (not (string-empty-p desc))) desc))))))

(defun neo--hex-color (c)
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

(defun neo--get-repo-id-by-full-name (repo-full-name)
  "Get repository ID from its full name."
  (caar (sqlite-select (neo-open-db)
		       "SELECT id FROM repositories WHERE full_name = ?"
		       (list repo-full-name))))

(defun neo--workflow-get-repo-full-name-by-id (repo-id)
  "Get repository full name from its ID."
  (caar (sqlite-select (neo-open-db)
		       "SELECT full_name FROM repositories WHERE id = ?"
		       (list repo-id))))

(defun neo--make-issue-row (issue)
  "Create a vtable row from an ISSUE struct."
  (let* ((state (neo-issue-state issue))
         (status-face (neo--get-issue-status-face state)))
    (append
     (list (propertize (format "%d" (neo-issue-number issue))
		       'face (list status-face 'neo-workflow-issue-id-face))
           (neo--priority-icon issue)
           (propertize (neo-issue-title issue)
		       'face (list status-face 'neo-workflow-issue-title-face)))
     (neo--labels issue))))

(defun neo--get-github-token (owner)
  "Get GitHub token for OWNER from auth-source."
  (let ((result (car (auth-source-search :host "api.github.com"
                                         :user (concat owner "^forge")
                                         :max 1))))
    (when result
      (let ((secret (plist-get result :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))

(defun neo-workflow-update-issue (issue table old-issue &rest props)
  "Update a GitHub issue from a `neo-issue' struct using PROPS.
PROPS is a plist of properties to update, e.g. :title 'new title'."
  (let* ((repo-id (neo-issue-repository-id issue))
         (repo-full-name (neo--workflow-get-repo-full-name-by-id repo-id))
         (owner (car (split-string repo-full-name "/")))
         (token (neo--get-github-token owner))
         (issue-number (neo-issue-number issue))
         (endpoint (format "/repos/%s/issues/%d" repo-full-name issue-number))
         (data (let (alist)
                 (while props
                   (let ((key (pop props))
                         (value (pop props)))
                     (push (cons (intern (substring (symbol-name key) 1))
                                 (if (eq key :labels) (vconcat value) value))
                           alist)))
                 (nreverse alist))))
    (if token
        (progn
          (message "Updating issue #%d..." issue-number)
          (ghub-request "PATCH" endpoint data
            :host "api.github.com"
            :auth token
            :callback (lambda (result &rest _)
                        (if (listp result)
                            (progn
                              (message "Issue #%d updated on GitHub." issue-number)
                              (let ((updated-issue (neo--ghub-result-to-neo-issue result repo-id)))
                                (when updated-issue
                                  ;; Preserve the stack from the original issue object
                                  (setf (neo-issue-stack updated-issue) (neo-issue-stack issue))
                                  (neo-db-upsert-issue updated-issue)
                                  (neo/workflow-refresh repo-full-name (neo-issue-id updated-issue)))))
                          (message "Failed to update issue #%d: %S" issue-number result)))))
      (message "No GitHub token found for %s" owner))))

(defun neo--new-priority (issue direction)
  "Return the new priority for ISSUE given DIRECTION.
DIRECTION should be 1 (up) or -1 (down).  
Saturates at the top and bottom. Returns a string (\"\" if no priority)."
  (let* ((current (neo--priority issue))
         ;; get index in priority list; -1 if current is ""
         (idx (if (string-empty-p current)
                  -1
                (cl-position current neo--priority-labels :test #'string-equal)))
         ;; calculate new index
         (new-idx (max -1 (min (1- (length neo--priority-labels))
			       (+ idx direction)))))
    ;; return new priority string
    (if (= new-idx -1)
        ""
      (nth new-idx neo--priority-labels))))

(defun neo--priority-change (object direction)
  "Change the priority of OBJECT (a neo-issue) by DIRECTION (1 for up, -1 for down)."
  (let* ((table (vtable-current-table)))
    (when (and object (neo-issue-p object))
      (let* ((issue object)
             (new-priority (neo--new-priority issue direction))
             (current-labels (mapcar #'neo-label-name (neo-issue-labels issue)))
             (other-labels (cl-remove-if (lambda (l) (member l neo--priority-labels)) current-labels))
             (new-labels (if (string-empty-p new-priority)
                             other-labels
                           (cons new-priority other-labels))))
        (neo-workflow-update-issue issue table issue :labels new-labels)))))

(defun neo--priority-up (object)
  (interactive)
  (neo--priority-change object 1))

(defun neo--priority-down (object)
  (interactive)
  (neo--priority-change object -1))

(defun neo--issue-status (issue)
  (if (neo-issue-stack issue)
      "A"
    ""))

(defun neo--active-issue-p (issue)
  (neo-issue-stack issue))

(defun neo--final-issue-title-face (issue)
  (let* ((state (neo-issue-state issue))
	 (status-face (neo--get-issue-status-face state)))
    (if (neo--active-issue-p issue)
	(list 'magit-head status-face 'neo-workflow-issue-title-face)
      (list status-face 'neo-workflow-issue-title-face))
    ))

(defun neo--final-issue-id-face (issue)
  (let* ((state (neo-issue-state issue))
         (status-face (neo--get-issue-status-face state)))
    (if (neo--active-issue-p issue)
        (list 'magit-head status-face 'neo-workflow-issue-id-face)
      (list status-face 'neo-workflow-issue-id-face))))

(defun neo--toggle-object-visibility (object)
  (interactive)
  (when (neo-issue-p object)
    (let* ((current-state (or (neo-issue-ui-state object) "collapsed"))
           (new-state (if (string= current-state "expanded") "collapsed" "expanded"))
           (issue-id (neo-issue-id object))
           (repo-id (neo-issue-repository-id object))
           (repo-name (neo--workflow-get-repo-full-name-by-id repo-id)))
      (neo-db-set-issue-ui-state issue-id new-state)
      (neo/workflow-refresh repo-name issue-id))))

(defun neo--vtable-revert (&optional _)
  (interactive)
  (cl-letf (((symbol-function #'vtable--insert-header-line)
	     (lambda (table width spacer))))
    (vtable-revert-command)))

(defun neo--get-branch-name (object)
  (if (neo-issue-p object)
      (neo--get-branch-name (neo-issue-stack object))
    (when-let* ((branch (and object (neo-stack-branch object)))
 		(branch-name (neo-branch-name branch)))
      branch-name)))

(defun neo--propertize-issue-title (issue)
  (let ((branch-name (neo--get-branch-name issue)))
    (propertize (neo-issue-title issue)
		'face (neo--final-issue-title-face issue)
                'help-echo (when branch-name (format "branch: %s" branch-name)))))

(defun neo--propertize-stack-title (stack)
  (let ((branch-name (neo--get-branch-name stack)))
    (propertize (concat (neo-stack-prefix stack) (or (neo-stack-title stack) (neo-stack-name stack)))
                'face '(bold)
                'help-echo (when branch-name (format "branch: %s" branch-name)))))

(defun neo/workflow-make-vtable (objects-fn)
  (make-vtable
   :use-header-line nil
   :separator-width 1
   :insert t ;nil
   :columns `((:name "ID" :width 5 :align 'right
		     :getter ,(lambda (object _)
                                (if (neo-issue-p object)
				    (propertize (format "%d" (neo-issue-number object))
						'face (neo--final-issue-id-face object))
                                  "")))
	      (:name "Pri" :width 3 :align 'right
		     :getter ,(lambda (object _)
                                (if (neo-issue-p object) (neo--priority-icon object) "")))
	      (:name "Title" :width 60
		     :getter ,(lambda (object _)
				(if (neo-issue-p object)
				    (neo--propertize-issue-title object)
				  (neo--propertize-stack-title object))))
	      (:name "Labels" :width 30
		     :getter ,(lambda (object _)
				(if (neo-issue-p object) (neo--labels object) "")))
	      )
   ;; TODO we should have a header keymap with the same bindings
   :keymap neo/workflow-table-keymap
   :actions `("k" kill-buffer
	      "h" neo--hack
	      "a" neo--append
	      "g" neo--vtable-revert
	      "TAB" neo--toggle-object-visibility
	      "S-<up>" neo--priority-up
	      "S-<down>" neo--priority-down
	      "RET" neo--select-thing)
   :objects-function objects-fn))

(defconst neo-icon-public "🌐"
  "Icon for public repositories.")

(defconst neo-icon-private "🔒"
  "Icon for private repositories.")

(defun neo--insert-repo-header (repo)
  (let* ((start (point))
	 (repo-full-name (neo-repository-full-name repo))
         (repo-short-name (cadr (split-string repo-full-name "/")))
         (project (neo-load-project-by-repo repo-short-name))
	 (visibility (if (string= (neo-repository-visibility repo) "private") neo-icon-private neo-icon-public)))
    
    (insert
     (propertize (concat (format "%s" repo-full-name) " ")
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
            (define-key map [mouse-1] (lambda () (interactive) (browse-url (format "https://github.com/%s" repo-full-name))))
            (define-key map [down-mouse-1] #'ignore)
            (insert (propertize path
                                'face '(shadow neo-workflow-repo-face)
                                'help-echo "Click to browse"
                                'mouse-face 'highlight
                                'keymap map)))
        (define-key map [mouse-1] (lambda () (interactive) (neo/workflow-clone-repo repo-full-name)))
        (define-key map [down-mouse-1] #'ignore)
        (insert (propertize "📦"
                            'face 'neo-workflow-repo-face
                            'help-echo (format "Clone %s" repo-full-name)
                            'mouse-face 'highlight
                            'keymap map))))
    
    (insert
     (propertize " " 'display
		 ;; HACK the '3' comes out of thin, flatulent air, maybe the emoji count for more than 1
		 `(space :align-to (- right-margin ,(string-width visibility) 3))
                 'face 'neo-workflow-repo-face
                 'keymap neo/workflow-table-keymap))
    
    (insert
     (propertize (concat visibility " \n")
                 'face 'neo-workflow-repo-face
                 'keymap neo/workflow-table-keymap))

    (put-text-property start (point) 'repo-header-line t)))

(defun neo--issue-active-p (issue)
  "Return non-nil if ISSUE is active (has a root stack)."
  (neo-issue-stack issue))

(defun neo--compare-issues (a b)
  "Return non-nil if issue A should come before issue B."
  (let ((pa (neo--get-issue-priority-score a))
        (pb (neo--get-issue-priority-score b))
        (ta (neo-issue-created-at a))
        (tb (neo-issue-created-at b)))
    (cond
     ;; Priority first, if enabled and different
     ((and neo/workflow-sort-by-priority (/= pa pb))
      (< pa pb))

     ;; Tie-breaker (or primary when priority disabled):
     ;; newest issue first
     ((not (string= ta tb))
      (string> ta tb))

     ;; Fully equal: keep original order (stable sort fallback)
     (t
      nil))))

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

    ;; Index stacks
    (dolist (stack stacks)
      (puthash (neo-stack-name stack) stack stacks-by-name))

    ;; Assign issues to stacks
    (dolist (issue issues)
      (let ((stack (neo-issue-stack issue)))
        (when stack
          (push issue
                (gethash (neo-stack-name stack) issues-by-stack)))))

    ;; Determine parent issue per stack (by numeric prefix convention)
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
  "Expand a stack into a flat list, computing tree prefixes."
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
         (stack-name (and stack (neo-stack-name stack)))
         (issues-by-stack (plist-get index :issues-by-stack))
         (stacks-by-name (plist-get index :stacks-by-name))
         (children (and stack-name
                        (gethash stack-name issues-by-stack)))
         (state (neo-issue-ui-state issue))
         (result '()))

    (cond
     ;; No stack → just the issue. We shouldn't call neo--expand-issue
     ;; on such issues, but it is harmless to do so.
     ((null stack)
      (list issue))

     ;; Already processed parent → avoid duplication
     ((member issue processed)
      '())

     ;; Expanded stack
     ((string= state "expanded")
      (append (list issue) (neo--expand-stack (neo-issue-stack issue))))
     ;; (dolist (child (sort (copy-sequence children) #'neo--compare-issues))
     ;;   (unless (eq child issue)
     ;;     (setq result
     ;;           (append result
     ;;                   (neo--expand-issue child index
     ;;                                      (cons issue processed))))))
     ;; ;; Optional stack object
     ;; (let ((stack-obj (gethash stack-name stacks-by-name)))
     ;;   (when stack-obj
     ;;     (push stack-obj result)))
     ;;      (append (list issue) result)

     ;; Collapsed stack
     (t
      (list issue)))))

(defun neo--get-sorted-issues-for-repo (repo)
  "Return issues and stacks for REPO in UI display order."
  (let* ((repo-id (neo-repository-id repo))
         (repo-name (neo-repository-full-name repo))
         (all-issues (neo-db-get-issues-for-repo repo-id))
         (issues (neo--issue-filter all-issues repo-name))
         (stacks (neo-db-get-stacks-for-repo repo-id))

         ;; Partition
         (parts (neo--partition-issues issues))
         (active (car parts))
         (inactive (cdr parts))

         ;; Sort groups
         (_ (setq active   (sort active   #'neo--compare-issues)))
         (_ (setq inactive (sort inactive #'neo--compare-issues)))

         ;; Stack model
         (index (neo--build-stack-index active stacks))

         ;; Expand active issues
         (expanded-active
          (mapcan (lambda (issue)
                    (neo--expand-issue issue index '()))
                  active)))

    ;; Final flat list
    (append expanded-active inactive)))

(defun neo--objects-match-p (a b)
  "Return non-nil if A and B represent the same object (issue or stack)."
  (cond
   ((and (neo-issue-p a) (neo-issue-p b))
    (eq (neo-issue-id a) (neo-issue-id b)))
   ((and (neo-stack-p a) (neo-stack-p b))
    (or (eq (neo-stack-id a) (neo-stack-id b))
        (string= (neo-stack-name a) (neo-stack-name b))))
   (t (equal a b))))

(defmacro neo--save-workflow-context (&rest body)
  "Save the current workflow context, execute BODY, and restore context.
Context includes the current repository and the object at point (issue or stack).
If the object is no longer present after BODY (e.g. due to filtering),
attempts to place point at a sensible nearby location within the same repository.
Also preserves narrowing if active, unless `neo--inhibit-narrowing-restore` is non-nil."
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
                 ;; Object not found, go to table start
                 (goto-char start-pos))))
            (saved-header-p
             (goto-char start-pos))
            (t
             (goto-char start-pos))))
       ;; Repo not found or we weren't in a repo
       (goto-char (point-min)))
     
     ;; Restore narrowing if applicable
     (when (and was-narrowed narrowed-repo (not (bound-and-true-p neo--inhibit-narrowing-restore)))
       (let ((info (assoc-string narrowed-repo neo--repo-info-alist)))
         (when info
           (narrow-to-region (nth 2 info) (nth 3 info)))))))

(defun neo/workflow-refresh (&optional target-repo-name target-issue-id)
  "Refresh the workflow status buffer.
If TARGET-REPO-NAME and TARGET-ISSUE-ID are provided, position point on that issue."
  (interactive)
  (setq neo--repo-info-alist nil)
  (let ((inhibit-read-only t))
    (widen)
    (erase-buffer)
    (dolist (repo (neo-load-all-repositories))
      (let* ((repo-name (neo-repository-full-name repo))
	     (start (point)))


	(neo--insert-repo-header repo)

	;; HACK there's no way to be able to define column
	;; properties and at the same time don't show the table
	;; header, so we cheat [this works only because we don't use
	;; vtable functions for sorting and re-arranging the table,
	;; otherwise the header would pop up again]
	;; TODO: maybe we could make this buffer local
	(cl-letf (((symbol-function #'vtable--insert-header-line)
		   (lambda (table width spacer))))
	  (let* ((table (neo/workflow-make-vtable (lambda () (neo--get-sorted-issues-for-repo repo)))))
            (let ((end (point)))
              (setf (alist-get repo-name neo--repo-info-alist) (list table start end)))
	    (goto-char (point-max))
	    (add-text-properties start (point) `(repo-name ,repo-name))
	    (insert "\n"))))))
  (if (and target-repo-name target-issue-id)
      (let ((info (assoc-string target-repo-name neo--repo-info-alist)))
        (if info
            (let* ((table (car (cdr info)))
                   (target-object (seq-find (lambda (obj)
                                              (and (neo-issue-p obj)
                                                   (= (neo-issue-id obj) target-issue-id)))
                                            (vtable-objects table))))
              (if target-object
                  (progn
                    (vtable-goto-table table)
                    (vtable-goto-object target-object)
		    (redisplay)
		    (hl-line-highlight))
		(goto-char (point-min))))
          (goto-char (point-min))))
    (goto-char (point-min))))


(defvar neo--github-sync-in-progress nil
  "Lock to prevent concurrent GitHub syncs.")

(defun neo-workflow-github-sync ()
  "Refresh all repositories from GitHub asynchronously.
If a sync is already in progress, does nothing.
Updates the DB and then schedules a UI refresh."
  (interactive)
  (if neo--github-sync-in-progress
      (message "GitHub sync already in progress.")
    (setq neo--github-sync-in-progress t)
    (message "Starting background GitHub sync...")
    (make-thread
     (lambda ()
       (condition-case err
           (progn
             (dolist (repo (neo-load-all-repositories))
               (neo--fetch-and-insert-repo (neo-repository-full-name repo)))
             (message "Background GitHub sync completed.")
             ;; Schedule UI refresh on main thread
             (run-at-time 0 nil 
                          (lambda ()
                            (setq neo--github-sync-in-progress nil)
                            (neo/workflow-refresh))))
         (error
          (setq neo--github-sync-in-progress nil)
          (message "Error during GitHub sync: %s" err)))))))

(defun neo/workflow-sync-and-refresh ()
  "Sync with GitHub (async) and refresh the workflow status buffer (immediate)."
  (interactive)
  (neo-workflow-github-sync)
  (neo/workflow-refresh))

;;;###autoload
(defun neo/workflow-status ()
  "Show the Neo workflow status buffer.
Loads cached data immediately and triggers a background GitHub sync."
  (interactive)
  (let ((buffer (get-buffer-create "*NEO Workflow*")))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (neo-workflow-status-mode)
      (neo/workflow-refresh)
      (neo-workflow-github-sync))))


(neo/application "Workflow"
  :setup (neo/workflow-status)
  :bind "w")

(provide 'neo-workflow-status)
