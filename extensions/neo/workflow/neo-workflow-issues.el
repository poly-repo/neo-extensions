;;; neo-workflow-issues.el --- Neo Workflow Issues Management -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'neo-workflow-models)
(require 'ghub)
(require 'auth-source)
(require 'json)

(defun neo-issues--get-github-token (owner)
  "Get GitHub token for OWNER from auth-source."
  (let ((result (car (auth-source-search :host "api.github.com"
                                         :user (concat owner "^forge")
                                         :max 1))))
    (when result
      (let ((secret (plist-get result :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))

(defun neo-issues--create-cli (repo-full-name title body labels)
  "Create an issue using the GitHub CLI (gh)."
  (message "Creating issue via gh CLI...")
  (let ((args (append (list "issue" "create"
                            "--repo" repo-full-name
                            "--title" title
                            "--body" body)
                      (mapcan (lambda (l) (list "--label" l)) labels))))
    (if (zerop (apply #'call-process "gh" nil nil nil args))
        (message "Issue created successfully via gh CLI.")
      (error "Failed to create issue via gh CLI"))))

(defun neo-workflow-issue-create ()
  "Create a new GitHub issue.
Prompts for repository, title, body, and labels.
Attempts to use `ghub' library first, falls back to `gh' CLI."
  (interactive)
  (let* ((repos (neo-load-all-repositories))
         (repo-names (mapcar #'neo-repository-full-name repos))
         (repo-full-name (completing-read "Repository: " repo-names))
         (repo-obj (seq-find (lambda (r) (string= (neo-repository-full-name r) repo-full-name)) repos))
         (repo-id (neo-repository-id repo-obj))
         (available-labels (neo-db-get-labels-for-repo repo-id))
         (title (read-string "Title: "))
         (body (read-string "Body: "))
         (selected-labels (completing-read-multiple "Labels (comma separated): " available-labels nil t)))
    (condition-case err
        (let* ((owner (car (split-string repo-full-name "/")))
               (token (neo-issues--get-github-token owner)))
          (unless token
            (error "No GitHub token found for %s" owner))
          
          (message "Creating issue via ghub...")
          (let ((result (ghub-request "POST" 
                                      (format "/repos/%s/issues" repo-full-name)
                                      `((title . ,title)
                                        (body . ,body)
                                        (labels . ,(vconcat selected-labels)))
                                      :auth token
                                      :host "api.github.com")))
            (message "Issue created: %s" (cdr (assoc 'html_url result)))))
      (error
       (message "ghub failed (%s), falling back to CLI..." err)
       (neo-issues--create-cli repo-full-name title body selected-labels)))))

(defun neo-workflow-sync-issues (repo-full-name)
  "Sync issues for REPO-FULL-NAME using conditional requests (ETag, Last-Modified)."
  (interactive "sRepository (user/repo): ")
  (let* ((parts (split-string repo-full-name "/"))
         (owner (car parts))
         (repo (cadr parts))
         (metadata (neo-db-get-sync-metadata owner repo "issues"))
         (etag (plist-get metadata :etag))
         (since (plist-get metadata :last-sync-timestamp))
         (token (neo-issues--get-github-token owner))
         (url (format "/repos/%s/issues" repo-full-name))
         (headers (if etag `(("If-None-Match" . ,etag)) nil))
         (query (list (cons 'per_page "100")
                      (cons 'state "all")
                      (cons 'page "1")))
         (all-issues nil)
         (first-etag nil)
         (page 1)
         (continue t))

    (when since
      (push (cons 'since since) query))

    (unless token
      (error "No GitHub token found for %s" owner))

    (message "Syncing issues for %s..." repo-full-name)
    
    (let ((repo-id (neo-db-get-repository-id repo-full-name)))
      (unless repo-id
        (error "Repository %s not found in DB. Please sync repositories first." repo-full-name))

      (while continue
        (condition-case err
            (let ((response (let ((url-automatic-caching nil))
                              (ghub-request "GET" url
                                            nil ; PARAMS
                                            :query query
                                            :headers headers
                                            :auth token
                                            :host "api.github.com"
                                            :unpaginate nil))))
              
              ;; Try to capture ETag from the first response
              (when (and (= page 1) (boundp 'ghub-response-headers))
                (setq first-etag (cdr (assoc "ETag" ghub-response-headers))))

              (setq all-issues (append all-issues response))

              ;; Pagination logic: check if we got a full page
              (if (< (length response) 100)
                  (setq continue nil)
                (setq page (1+ page))
                (setf (alist-get 'page query nil nil #'equal) (number-to-string page))))
          
          (ghub-http-error
           (let ((status (cdr (assoc 'http-status-code (cdr err)))))
             (if (eq status 304)
                 (progn
                   (message "Issues for %s are up to date." repo-full-name)
                   (setq continue nil))
               (signal (car err) (cdr err)))))
          (file-missing
           (if (string-match-p "url/cache" (format "%s" err))
               (progn
                 (message "Issues for %s are up to date." repo-full-name)
                 (setq continue nil))
             (signal (car err) (cdr err))))
          (error
           (signal (car err) (cdr err)))))

      (when all-issues
        (dolist (issue-data all-issues)
          (let ((issue (neo--ghub-result-to-neo-issue issue-data repo-id)))
            (when issue
              (neo-db-upsert-issue issue))))
        (message "Synced %d issues for %s." (length all-issues) repo-full-name))

      (when first-etag
        (neo-db-update-sync-metadata owner repo "issues" first-etag)))))

(defun neo--ghub-label-to-neo-label (data repository-id)
  "Convert a ghub API result alist for a label to a `neo-label' struct."
  (make-neo-label :id (cdr (assoc 'id data))
                  :name (cdr (assoc 'name data))
                  :color (cdr (assoc 'color data))
                  :description (cdr (assoc 'description data))
                  :repository-id repository-id))

(defun neo-workflow-sync-labels (repo-full-name)
  "Sync labels for REPO-FULL-NAME using conditional requests (ETag)."
  (interactive "sRepository (user/repo): ")
  (let* ((parts (split-string repo-full-name "/"))
         (owner (car parts))
         (repo (cadr parts))
         (metadata (neo-db-get-sync-metadata owner repo "labels"))
         (etag (plist-get metadata :etag))
         (token (neo-issues--get-github-token owner))
         (url (format "/repos/%s/labels" repo-full-name))
         (headers (if etag `(("If-None-Match" . ,etag)) nil))
         (query (list (cons 'per_page "100")
                      (cons 'page "1")))
         (all-labels nil)
         (first-etag nil)
         (page 1)
         (continue t))

    (unless token
      (error "No GitHub token found for %s" owner))

    (message "Syncing labels for %s..." repo-full-name)
    
    (let ((repo-id (neo-db-get-repository-id repo-full-name)))
      (unless repo-id
        (error "Repository %s not found in DB. Please sync repositories first." repo-full-name))

      (while continue
        (condition-case err
            (let ((response (let ((url-automatic-caching nil))
                              (ghub-request "GET" url
                                            nil ; PARAMS
                                            :query query
                                            :headers headers
                                            :auth token
                                            :host "api.github.com"
                                            :unpaginate nil))))
              
              ;; Try to capture ETag from the first response
              (when (and (= page 1) (boundp 'ghub-response-headers))
                (setq first-etag (cdr (assoc "ETag" ghub-response-headers))))

              (setq all-labels (append all-labels response))

              ;; Pagination logic
              (if (< (length response) 100)
                  (setq continue nil)
                (setq page (1+ page))
                (setf (alist-get 'page query nil nil #'equal) (number-to-string page))))
          
          (ghub-http-error
           (let ((status (cdr (assoc 'http-status-code (cdr err)))))
             (if (eq status 304)
                 (progn
                   (message "Labels for %s are up to date." repo-full-name)
                   (setq continue nil))
               (signal (car err) (cdr err)))))
          (file-missing
           (if (string-match-p "url/cache" (format "%s" err))
               (progn
                 (message "Labels for %s are up to date." repo-full-name)
                 (setq continue nil))
             (signal (car err) (cdr err))))
          (error
           (signal (car err) (cdr err)))))

      (when all-labels
        (let ((neo-labels (mapcar (lambda (l) (neo--ghub-label-to-neo-label l repo-id)) all-labels)))
          (neo-db-upsert-labels neo-labels repo-id))
        (message "Synced %d labels for %s." (length all-labels) repo-full-name))

      (when first-etag
        (neo-db-update-sync-metadata owner repo "labels" first-etag)))))

(provide 'neo-workflow-issues)
;;; neo-workflow-issues.el ends here
