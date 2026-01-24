;;; neo-workflow-ui.el --- Neo Workflow UI -*- lexical-binding: t; -*-

;;; neo-workflow-ui.el --- UI for Neo Workflow Mode -*- lexical-binding: t; -*-

(require 'tabulated-list)
(require 'neo-workflow-db) ;; DB layer for caching & async

;;;###autoload
(define-derived-mode neo-workflow-summary-mode tabulated-list-mode "Neo Workflow"
  "Major mode to view stack-level summaries of projects."
  (setq tabulated-list-format [("Stack" 30 t)
                               ("#Branches" 10 nil)
                               ("Open PRs" 10 nil)
                               ("Merged" 10 nil)
                               ("CI ✅" 8 nil)
                               ("CI ❌" 8 nil)
                               ("CI ⚠" 8 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Stack" nil))
  (tabulated-list-init-header)
  (neo-workflow-refresh))

(defun neo-workflow-refresh ()
  "Refresh the tabulated-list entries for stack-level summary."
  (let ((entries (neo-workflow-make-stack-entries)))
    (setq tabulated-list-entries entries))
  (tabulated-list-print t))

(defun neo-workflow-make-stack-entries ()
  "Return tabulated-list entries, one per stack.
Data aggregated from all branches in the stack."
  (mapcar
   (lambda (stack)
     (let* ((summary (neo-stack-summary (plist-get stack :id)))
            (stack-name (plist-get stack :name))
            ;; summary is a plist: (:branches n :open-prs n :merged n :ci-passed n :ci-failed n :ci-skipped n)
            (row (vector stack-name
                         (number-to-string (plist-get summary :branches))
                         (number-to-string (plist-get summary :open-prs))
                         (number-to-string (plist-get summary :merged))
                         (number-to-string (plist-get summary :ci-passed))
                         (number-to-string (plist-get summary :ci-failed))
                         (number-to-string (plist-get summary :ci-skipped)))))
       (list stack-name row)))
   (neo-db-get-all-stacks))) ;; <-- fetch from DB layer

;; Keymap
(define-key neo-workflow-summary-mode-map (kbd "RET") 'neo-workflow-open-stack-details)

(defun neo-workflow-open-stack-details ()
  "Open a buffer showing all branches in the stack under point."
  (interactive)
  (let* ((stack (neo-get-stack-at-point))
         (buffer-name (format "*Stack Details: %s*" (neo-stack-name stack))))
    (with-current-buffer (get-buffer-create buffer-name)
      (neo-stack-detail-mode)
      (neo-stack-detail-refresh stack))
    (switch-to-buffer buffer-name)))

;;; Stack Detail Mode
(define-derived-mode neo-stack-detail-mode tabulated-list-mode "Stack Details"
  "Major mode for showing branch-level details for a stack."
  (setq tabulated-list-format [("Branch" 30 t)
                               ("PR" 6 nil)
                               ("Issue" 10 nil)
                               ("Status" 10 nil)
                               ("CI" 10 nil)
                               ("Worktree" 30 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Branch" nil))
  (tabulated-list-init-header))

(defun neo-stack-detail-refresh (stack)
  "Populate the detail buffer for STACK with branch-level info."
  (let ((entries
         (if-let ((branch-name (neo-db-get-branch-for-stack (neo-stack-id stack))))
           (let* ((branch (neo-load-branch branch-name))
                  (pr (neo-db-get-pr branch-name))
                  (issue (neo-db-get-issue branch-name))
                  ;; (ci (neo-db-get-ci branch)) ;; TODO
                  ;; (worktree (neo-db-get-worktree branch)) ;; TODO
                  (row (vector (neo-branch-name branch)
                               (if pr (format "#%d" (plist-get pr :number)) "-")
                               (or issue "-")
                               (neo-branch-status branch)
                               "" ;; (or ci "-")
                               (or (neo-branch-worktree-path branch) "-"))))
             (list (list (neo-branch-name branch) row))))))
    (setq tabulated-list-entries entries)
    (tabulated-list-print t)))

(provide 'neo-workflow-ui)

;; (require 'tabulated-list)
;; (require 'neo-workflow-models)

(defvar neo-workflow-buffer-name "*Neo Projects*"
  "Name of the Neo workflow buffer.")

;; ============================================================
;; Entry point: open Neo Workflow window
;; ============================================================

(defun neo-workflow-open ()
  "Open the Neo Workflow buffer showing all projects."
  (interactive)
  (let ((buf (get-buffer-create neo-workflow-buffer-name)))
    (with-current-buffer buf
      (neo-workflow-summary-mode)
      (neo-workflow-refresh))
    (pop-to-buffer buf)))

;; ;; ============================================================
;; ;; Major mode definition
;; ;; ============================================================

;; (define-derived-mode neo-workflow-mode tabulated-list-mode "Neo-Workflow"
;;   "Major mode for browsing Neo projects, stacks, and branches."
;;   (setq tabulated-list-format [("Project/Stack/Branch" 40 t)
;;                                ("PR" 6 t)
;;                                ("Issue" 10 t)
;;                                ("Status" 10 t)
;;                                ("CI" 6 t)
;;                                ("Worktree" 30 t)])
;;   (setq tabulated-list-padding 2)
;;   (setq tabulated-list-sort-key (cons "Project/Stack/Branch" nil))
;;   (tabulated-list-init-header))

;; ;; ============================================================
;; ;; Data conversion: load structs into tabulated-list entries
;; ;; ============================================================

;; (defun neo-workflow-make-entries ()
;;   "Generate `tabulated-list-entries' for all projects."
;;   (let ((entries '()))
;;     (dolist (project (mapcar #'neo-load-project
;;                              (mapcar #'car
;;                                      (emacsql (neo-open-db)
;;                                               [:select id :from projects]))))
;;       (let ((proj-name (format "[Repo] %s" (neo-project-repo project))))
;;         ;; Show stacks
;;         (dolist (stack (neo-project-stacks project))
;;           (let ((stack-name (format "└─ [Stack] %s" (neo-stack-name stack))))
;;             ;; Each branch
;;             (dolist (branch (neo-stack-branches stack))
;;               (let ((branch-name (format "    └─ %s" (neo-branch-name branch)))
;;                     (pr-number (or (neo-branch-pr-number branch) ""))
;;                     (issue-id (or (neo-branch-issue-id branch) ""))
;;                     (status (or (neo-branch-status branch) ""))
;;                     (ci (or (neo-branch-ci-status branch) ""))
;;                     (wt (or (neo-branch-worktree-path branch) "")))
;;                 (push (list (format "%s/%s/%s" (neo-project-id project)
;;                                     (neo-stack-name stack)
;;                                     (neo-branch-name branch))
;;                             (vector (concat (neo-project-id project)
;;                                             "/" (neo-stack-name stack)
;;                                             "/" (neo-branch-name branch))
;;                                     (format "%s" pr-number)
;;                                     issue-id
;;                                     status
;;                                     ci
;;                                     wt))
;;                       entries))))))
;;       ;; Optionally add project-level entry
;;       (push (list (neo-project-id project)
;;                   (vector (neo-project-id project)
;;                           "" "" "" "" "")) entries))
;;     (nreverse entries)))

;; ;; ============================================================
;; ;; Refresh the buffer
;; ;; ============================================================

;; (defun neo-workflow-refresh ()
;;   "Refresh the Neo Workflow buffer."
;;   (interactive)
;;   (setq tabulated-list-entries (neo-workflow-make-entries))
;;   (tabulated-list-print t))

;; ;; ============================================================
;; ;; Keybindings
;; ;; ============================================================

;; (define-key neo-workflow-mode-map (kbd "g") 'neo-workflow-refresh)
;; (define-key neo-workflow-mode-map (kbd "RET") 'neo-workflow-open-details)

;; ;; ============================================================
;; ;; Open detail buffer for a branch/PR
;; ;; ============================================================

;; (defun neo-workflow-open-details ()
;;   "Open a detailed view for the item at point."
;;   (interactive)
;;   (let* ((id (tabulated-list-get-id))
;;          (components (split-string id "/"))
;;          (proj-id (nth 0 components))
;;          (stack-name (nth 1 components))
;;          (branch-name (nth 2 components))
;;          (branch (neo-load-branch branch-name))
;;          (pr (when branch (neo-load-pr (neo-branch-pr-number branch)))))
;;     (when branch
;;       (let ((buf (get-buffer-create (format "*Neo Details: %s*" branch-name))))
;;         (with-current-buffer buf
;;           (erase-buffer)
;;           (insert (format "Project: %s\n" proj-id))
;;           (insert (format "Stack: %s\n" stack-name))
;;           (insert (format "Branch: %s\n" (neo-branch-name branch)))
;;           (insert (format "PR Number: %s\n" (neo-branch-pr-number branch)))
;;           (insert (format "PR Title: %s\n" (neo-pr-title pr)))
;;           (insert (format "Issue: %s\n" (neo-branch-issue-id branch)))
;;           (insert (format "CI Status: %s\n" (neo-branch-ci-status branch)))
;;           (insert (format "Worktree: %s\n" (neo-branch-worktree-path branch)))
;;           (goto-char (point-min)))
;;         (pop-to-buffer buf)))))

;; ;; ============================================================
;; ;; Stack-level summary support
;; ;; ============================================================

;; (defun neo-workflow-stack-summary (stack)
;;   "Return a vector summarizing STACK for tabulated-list.
;; Counts branches, PRs, CI, merged status."
;;   (let* ((branches (neo-stack-branches stack))
;;          (total (length branches))
;;          (merged 0)
;;          (open 0)
;;          (ci-passing 0)
;;          (ci-failing 0)
;;          (ci-skipped 0))
;;     (dolist (b branches)
;;       (let* ((pr (neo-branch-pr-number b))
;;              (status (neo-branch-ci-status b)))
;;         (if (and pr (string= (neo-branch-status b) "merged"))
;;             (cl-incf merged)
;;           (cl-incf open))
;;         (cond ((string= status "success") (cl-incf ci-passing))
;;               ((string= status "failure") (cl-incf ci-failing))
;;               ((string= status "skipped") (cl-incf ci-skipped)))))
;;     (vector (format "[Stack] %s (Branches:%d, OpenPRs:%d, Merged:%d, CI✅:%d CI❌:%d CI⚠:%d)"
;;                     (neo-stack-name stack) total open merged ci-passing ci-failing ci-skipped)
;;             "" "" "" "" "")))

;; (provide 'neo-workflow-ui)
;;; neo-workflow-ui.el ends here
