;;; neo-workflow-pr.el --- Neo Workflow PR Management -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'neo-workflow-db)
(require 'neo-workflow-models)
(require 'ghub)
(require 'neo-workflow-status) ; for neo--get-github-token if available, else local helper

(defun neo-pr--get-github-token (owner)
  "Get GitHub token for OWNER from auth-source."
  (let ((result (car (auth-source-search :host "api.github.com"
                                         :user (concat owner "^forge")
                                         :max 1))))
    (when result
      (let ((secret (plist-get result :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))

(defun neo-workflow-sync-prs (repo-full-name &optional _branch-prefix)
  "Sync PRs for REPO-FULL-NAME using conditional requests (ETag, Last-Modified).
Optional BRANCH-PREFIX is currently ignored for server-side filtering as GitHub API
does not support prefix filtering efficiently."
  (interactive "sRepository (user/repo): ")
  (let* ((parts (split-string repo-full-name "/"))
         (owner (car parts))
         (repo (cadr parts))
         (metadata (neo-db-get-sync-metadata owner repo "prs"))
         (etag (plist-get metadata :etag))
         (since (plist-get metadata :last-sync-timestamp))
         (token (neo-pr--get-github-token owner))
         (url (format "/repos/%s/pulls" repo-full-name))
         (headers (if etag `(("If-None-Match" . ,etag)) nil))
         (query (list (cons 'per_page "100")
                      (cons 'state "all")
                      (cons 'sort "updated")
                      (cons 'direction "desc")
                      (cons 'page "1")))
         (all-prs nil)
         (first-etag nil)
         (page 1)
         (continue t))

    ;; GitHub Pulls API does not support 'since' parameter directly on /pulls endpoint
    ;; but it does support sort='updated' and direction='desc'.
    ;; We can stop fetching when we see a PR updated before 'since'.
    ;; However, for full sync with ETag, we rely on ETag.
    ;; If ETag matches (304), we stop.
    ;; If not 304, we fetch pages.
    
    (unless token
      (error "No GitHub token found for %s" owner))

    (message "Syncing PRs for %s..." repo-full-name)
    
    (let ((repo-id (neo-db-get-repository-id repo-full-name)))
      (unless repo-id
        (error "Repository %s not found in DB. Please sync repositories first." repo-full-name))

      (while continue
        (condition-case err
            (let ((response (let ((url-automatic-caching nil))
                              (ghub-request "GET" url
                                            nil
                                            :query query
                                            :headers headers
                                            :auth token
                                            :host "api.github.com"
                                            :unpaginate nil))))
              
              (when (and (= page 1) (boundp 'ghub-response-headers))
                (setq first-etag (cdr (assoc "ETag" ghub-response-headers))))

              (setq all-prs (append all-prs response))

              ;; Pagination & Since check
              (if (< (length response) 100)
                  (setq continue nil)
                ;; Check if the last item is older than 'since' (if we have since)
                ;; response is ordered by updated desc
                (if since
                    (let* ((last-pr (car (last response)))
                           (updated-at (cdr (assoc 'updated_at last-pr))))
                      (if (and updated-at (string< updated-at since))
                          (setq continue nil)
                        (setq page (1+ page))
                        (setf (alist-get 'page query nil nil #'equal) (number-to-string page))))
                  (setq page (1+ page))
                  (setf (alist-get 'page query nil nil #'equal) (number-to-string page)))))
          
          (ghub-http-error
           (let ((status (cdr (assoc 'http-status-code (cdr err)))))
             (if (eq status 304)
                 (progn
                   (message "PRs for %s are up to date." repo-full-name)
                   (setq continue nil))
               (signal (car err) (cdr err)))))
          (file-missing
           (if (string-match-p "url/cache" (format "%s" err))
               (progn
                 (message "PRs for %s are up to date." repo-full-name)
                 (setq continue nil))
             (signal (car err) (cdr err))))
          (error
           (signal (car err) (cdr err)))))

      (when all-prs
        (let ((count 0))
          (dolist (pr-data all-prs)
            (let* ((number (cdr (assoc 'number pr-data)))
                   (issue (neo-db-get-issue-by-number repo-id number)))
              (if issue
                  (let ((issue-id (neo-issue-id issue)))
                    (neo-db-insert-pr
                     number
                     (cdr (assoc 'title pr-data))
                     (cdr (assoc 'login (cdr (assoc 'user pr-data))))
                     (cdr (assoc 'ref (cdr (assoc 'base pr-data))))
                     (cdr (assoc 'ref (cdr (assoc 'head pr-data))))
                     (cdr (assoc 'mergeable pr-data)) ;; might be nil if not fetched detailed
                     (cdr (assoc 'state pr-data))
                     nil ;; ci status not available in list response
                     (cdr (assoc 'created_at pr-data))
                     (cdr (assoc 'updated_at pr-data))
                     (cdr (assoc 'html_url pr-data))
                     issue-id)
                    (cl-incf count))
                ;; If issue not found, we can't insert PR due to FK. 
                ;; We silently skip or could warn.
                )))
          (message "Synced %d PRs for %s." count repo-full-name)))

      (when first-etag
        (neo-db-update-sync-metadata owner repo "prs" first-etag)))))

(defun neo-workflow-sync-pr-statuses (repo-full-name)
  "Fetch and update CI status for all open PRs in REPO-FULL-NAME."
  (interactive "sRepository (user/repo): ")
  (let* ((parts (split-string repo-full-name "/"))
         (owner (car parts))
         (repo-name (cadr parts))
         (token (neo-pr--get-github-token owner))
         (repo-id (neo-db-get-repository-id repo-full-name)))
    
    (unless token
      (error "No GitHub token found for %s" owner))
    (unless repo-id
      (error "Repository %s not found in DB." repo-full-name))

    (message "Updating PR statuses for %s..." repo-full-name)
    (let ((open-prs (neo-db-get-open-prs-for-repo repo-id))
          (count 0))
      (dolist (pr open-prs)
        (let* ((head-sha (neo-branch-last-commit (neo-pr-head pr))) ;; Wait, neo-pr-head is a ref name (branch), not a struct with SHA? 
               ;; neo-db-insert-pr stores 'ref' in 'head' column.
               ;; We need the SHA. The PR list response usually has 'head' object with 'sha'.
               ;; But we stored 'ref' string in DB.
               ;; If we don't have SHA in DB, we might need to fetch PR details or resolve ref.
               ;; HOWEVER, neo-db-insert-pr saves (cdr (assoc 'ref (cdr (assoc 'head pr-data)))) which is branch name.
               ;; The SHA is available in (cdr (assoc 'sha (cdr (assoc 'head pr-data)))).
               ;; We are not storing SHA in prs table. 'head' column is text.
               ;; We should probably use the branch name to get status? 
               ;; GitHub API /repos/:owner/:repo/commits/:ref/status takes ref (SHA or branch name).
               ;; So using the branch name stored in 'head' column should work.
               (ref (neo-pr-head pr))
               (url (format "/repos/%s/commits/%s/status" repo-full-name ref)))
          
          (condition-case err
              (let* ((response (let ((url-automatic-caching nil))
                                 (ghub-request "GET" url
                                               nil
                                               :auth token
                                               :host "api.github.com")))
                     (state (cdr (assoc 'state response))))
                (when state
                  (neo-db-set-pr-ci-status (neo-pr-issue-id pr) state)
                  (cl-incf count)))
            (ghub-http-error
             (message "Failed to fetch status for PR #%d: %S" (neo-pr-number pr) err))
            (error
             (message "Error updating PR #%d: %S" (neo-pr-number pr) err)))))
      (message "Updated status for %d PRs in %s." count repo-full-name))))

(provide 'neo-workflow-pr)
;;; neo-workflow-pr.el ends here
