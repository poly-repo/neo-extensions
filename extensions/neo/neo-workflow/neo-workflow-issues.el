;;; neo-workflow-issues.el --- Issue management via beads for Neo Workflow -*- lexical-binding: t; -*-

;; Replaces the GitHub/ghub issue backend with beads-client.
;; The org-mode editing buffer and template are kept from the original;
;; only the create/update/close backend calls are swapped.
;; GitHub-specific operations (clone, fork, sync, labels via ghub) are dropped.

(require 'cl-lib)
(require 'seq)
(require 'beads-client)
(require 'neo-workflow-models)
(require 'neo-workflow-db)
(require 'org)
(require 'ox-md)

(neo/use-package tempel)

;; ============================================================
;; Org template for issues
;; ============================================================

(defun neo-workflow--issue-template-source ()
  "Return the Tempel template for a beads issue."
  (if neo-issue-number
      `((neo-issue
         "* TODO " ,(or neo-issue-original-title "") n
         ":PROPERTIES:" n
         ":Workspace: " ,(or neo-issue-repo-full-name "") n
         ":ID: " ,neo-issue-number n
         ":State: " ,(or neo-issue-state "open") n
         ":Labels: " ,(or neo-issue-labels "") n
         ":END:" n n
         "** Description" n
         ,(or neo-issue-original-body "") n n
         "** Comments" n
         ,(or neo-issue-comments-formatted "") n
         "** New Comment" n
         p n
         q))
    `((neo-issue
       "* TODO " p n
       ":PROPERTIES:" n
       ":Workspace: " ,(or neo-issue-repo-full-name "") n
       ":State: " ,(or neo-issue-state "open") n
       ":Labels: " ,(or neo-issue-labels "") n
       ":END:" n n
       "** Description" n
       ,(or neo-issue-original-body "") n
       q))))

;; ============================================================
;; Markdown <-> Org conversion (kept from the original)
;; ============================================================

(defun neo--org-to-markdown (content)
  "Convert Org-mode CONTENT string to Markdown."
  (if (or (not content) (string-empty-p content))
      ""
    (with-temp-buffer
      (insert content)
      (org-mode)
      (goto-char (point-min))
      (insert "#+OPTIONS: toc:nil\n")
      (org-export-as 'md nil nil t nil))))

(defun neo--markdown-to-org (content)
  "Convert Markdown CONTENT string to Org-mode."
  (if (or (not content) (string-empty-p content))
      ""
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward "^# " nil t) (replace-match "* "))
      (goto-char (point-min))
      (while (re-search-forward "^## " nil t) (replace-match "** "))
      (goto-char (point-min))
      (while (re-search-forward "^### " nil t) (replace-match "*** "))
      (goto-char (point-min))
      (while (re-search-forward "\\*\\*\\(.*?\\)\\*\\*" nil t) (replace-match "*\\1*"))
      (goto-char (point-min))
      (while (re-search-forward "`\\(.*?\\)`" nil t) (replace-match "~\\1~"))
      (goto-char (point-min))
      (while (re-search-forward "\\[\\(.*?\\)\\](\\(.*?\\))" nil t)
        (replace-match "[[\\2][\\1]]"))
      (buffer-string))))

;; ============================================================
;; Beads-backed create / update / close
;; ============================================================

(defun neo--workflow-issue-create-backend (_repo-full-name title body labels)
  "Create a beads issue with TITLE, BODY, and LABELS list.
REPO-FULL-NAME is accepted for signature compat but ignored (workspace is auto-detected)."
  (condition-case err
      (let ((issue (apply #'beads-client-create title
                          (append
                           (when (and body (not (string-empty-p body)))
                             (list :description body))
                           (when labels
                             (list :labels (string-join labels ",")))))))
        (message "Created beads issue: %s" (or (alist-get 'id issue) "?"))
        issue)
    (beads-client-error
     (user-error "Failed to create beads issue: %s" (cadr err)))))

(defun neo--workflow-issue-update-backend (_repo-full-name id title body state labels _milestone)
  "Update beads issue ID with TITLE, BODY, STATE, and LABELS.
REPO-FULL-NAME and MILESTONE are accepted for compat but ignored."
  (condition-case err
      (apply #'beads-client-update id
             (append
              (list :title title)
              (when (and body (not (string-empty-p body)))
                (list :description body))
              (when state
                (list :status state))
              (when labels
                (list :set-labels (string-join labels ",")))))
    (beads-client-error
     (user-error "Failed to update beads issue %s: %s" id (cadr err)))))

(defun neo--workflow-issue-add-comment-backend (_repo-full-name id body)
  "Add a note to beads issue ID with BODY.
REPO-FULL-NAME is accepted for compat but ignored."
  (condition-case err
      (beads-client-update id :notes body)
    (beads-client-error
     (user-error "Failed to add note to beads issue %s: %s" id (cadr err)))))

;; ============================================================
;; Issue editing buffer
;; ============================================================

(defvar neo-workflow-issue-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "C-c C-c") #'neo-workflow-issue-submit)
    map)
  "Keymap for `neo-workflow-issue-mode'.")

(define-derived-mode neo-workflow-issue-mode org-mode "Neo-Issue"
  "Major mode for editing Neo Workflow issues via beads.
\\{neo-workflow-issue-mode-map}"
  (setq-local neo-issue-repo-full-name nil)
  (setq-local neo-issue-number nil)
  (setq-local neo-issue-original-body nil)
  (setq-local neo-issue-original-title nil)
  (setq-local neo-issue-state nil)
  (setq-local neo-issue-milestone nil)
  (setq-local neo-issue-labels nil)
  (setq-local neo-issue-comments-formatted nil))

(defun neo-workflow-issue-open-template (&optional repo-full-name issue-id)
  "Open an Org buffer to create or edit a beads issue.
REPO-FULL-NAME identifies the workspace; ISSUE-ID is the beads issue id string."
  (interactive)
  (unless repo-full-name
    (let* ((repos (neo-load-all-repositories))
           (repo-names (mapcar #'neo-repository-full-name repos)))
      (setq repo-full-name (completing-read "Workspace: " repo-names))))

  (let* ((buffer-name (if issue-id
                          (format "*Neo Issue: %s#%s*" repo-full-name issue-id)
                        (format "*Neo New Issue: %s*" repo-full-name)))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (neo-workflow-issue-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq neo-issue-repo-full-name repo-full-name)
        (setq neo-issue-number issue-id)

        (if issue-id
            (condition-case err
                (let* ((alist (beads-client-show issue-id))
                       (repo-id (or (neo--get-repo-id-by-full-name repo-full-name) ""))
                       (issue (neo--beads-alist-to-neo-issue alist repo-id))
                       (labels-str (mapconcat #'neo-label-name (neo-issue-labels issue) ",")))
                  (setq neo-issue-original-title (neo-issue-title issue))
                  (setq neo-issue-original-body
                        (neo--markdown-to-org
                         (or (alist-get 'description alist)
                             (cdr (assoc "description" alist))
                             "")))
                  (setq neo-issue-state (symbol-name (neo-issue-state issue)))
                  (setq neo-issue-milestone "")
                  (setq neo-issue-labels labels-str)
                  (setq neo-issue-comments-formatted ""))
              (error
               (message "Failed to fetch issue %s: %s" issue-id (error-message-string err))
               (setq neo-issue-original-title "")
               (setq neo-issue-original-body "")
               (setq neo-issue-state "open")
               (setq neo-issue-milestone "")
               (setq neo-issue-labels "")
               (setq neo-issue-comments-formatted "")))
          (setq neo-issue-original-title "")
          (setq neo-issue-original-body "")
          (setq neo-issue-state "open")
          (setq neo-issue-milestone "")
          (setq neo-issue-labels "")
          (setq neo-issue-comments-formatted "")))

      (let ((tempel-template-sources (list #'neo-workflow--issue-template-source)))
        (tempel-insert 'neo-issue))

      (message "Press C-c C-c to submit issue."))
    (switch-to-buffer buffer)))

(defun neo-workflow-issue-submit ()
  "Submit the beads issue in the current Org buffer."
  (interactive)
  (unless (derived-mode-p 'neo-workflow-issue-mode)
    (error "Not in a Neo Issue buffer"))

  (save-excursion
    (goto-char (point-min))
    (unless (looking-at "\\* \\(TODO \\|DONE \\)?\\(.*\\)")
      (error "Cannot find issue title"))
    (let* ((title (string-trim (match-string 2)))
           (id (org-entry-get (point) "ID"))
           (state (org-entry-get (point) "State"))
           (labels-str (org-entry-get (point) "Labels"))
           (labels (when (and labels-str (not (string-empty-p labels-str)))
                     (split-string labels-str "," t " ")))
           (description (progn
                          (goto-char (point-min))
                          (if (re-search-forward "^\\*\\* Description$" nil t)
                              (let ((start (progn (forward-line 1) (point)))
                                    (end (if (re-search-forward "^\\*\\* " nil t)
                                             (match-beginning 0)
                                           (point-max))))
                                (neo--org-to-markdown (string-trim (buffer-substring-no-properties start end))))
                            ""))))
      (when (string-empty-p title)
        (user-error "Issue title cannot be empty"))
      (if id
          (progn
            (neo--workflow-issue-update-backend neo-issue-repo-full-name id title description state labels nil)
            (message "Beads issue %s updated." id)
            (kill-buffer (current-buffer)))
        (neo--workflow-issue-create-backend neo-issue-repo-full-name title description labels)
        (kill-buffer (current-buffer))))))

(provide 'neo-workflow-issues)
;;; neo-workflow-issues.el ends here
