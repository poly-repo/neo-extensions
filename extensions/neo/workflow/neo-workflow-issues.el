;;; neo-workflow-issues.el --- Neo Workflow Issues Management -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'neo-workflow-models)
(require 'ghub)
(require 'auth-source)
(require 'json)
(require 'subr-x)
(require 'org)
(require 'ox-md)

(neo/use-package tempel)

(defun neo-workflow--issue-template-source ()
  "Return the Tempel template for an issue."
  (if neo-issue-number
      ;; Template for EXISTING issue: Focus on New Comment
      `((neo-issue
         "* TODO " ,(or neo-issue-original-title "") n
         ":PROPERTIES:" n
         ":Repo: " ,(or neo-issue-repo-full-name "") n
         ":Number: " ,neo-issue-number n
         ":State: " ,(or neo-issue-state "open") n
         ":Milestone: " ,(or neo-issue-milestone "") n
         ":Labels: " ,(or neo-issue-labels "") n
         ":END:" n n
         "** Description" n
         ,(or neo-issue-original-body "") n n
         "** Comments" n
         ,(or neo-issue-comments-formatted "") n
         "** New Comment" n
         p n
         q))
    ;; Template for NEW issue: Focus on Title, No Comments section
    `((neo-issue
       "* TODO " p n
       ":PROPERTIES:" n
       ":Repo: " ,(or neo-issue-repo-full-name "") n
       ":State: " ,(or neo-issue-state "open") n
       ":Milestone: " ,(or neo-issue-milestone "") n
       ":Labels: " ,(or neo-issue-labels "") n
       ":END:" n n
       "** Description" n
       ,(or neo-issue-original-body "") n
       q))))

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

(defun neo--org-table-to-gfm ()
  "Convert table at point to GFM format string."
  (let* ((table (org-table-to-lisp))
         (lines (mapcar (lambda (row)
                          (if (eq row 'hline)
                              'hline
                            (mapcar #'string-trim row)))
                        table))
         (gfm-lines '()))
    (when lines
      (let ((header (car lines))
            (rest (cdr lines)))
        ;; Header row
        (push (format "| %s |" (string-join header " | ")) gfm-lines)
        
        ;; Separator row
        (if (eq (car rest) 'hline)
            (progn
              (push (format "|%s|" (string-join (make-list (length header) "---") "|")) gfm-lines)
              (setq rest (cdr rest)))
          ;; Implicit header separator if missing
          (push (format "|%s|" (string-join (make-list (length header) "---") "|")) gfm-lines))
        
        ;; Data rows
        (dolist (row rest)
          (unless (eq row 'hline)
            (push (format "| %s |" (string-join row " | ")) gfm-lines)))))
    (string-join (nreverse gfm-lines) "\n")))

(defun neo--org-to-markdown (content)
  "Convert Org-mode CONTENT string to Markdown."
  (if (or (not content) (string-empty-p content))
      ""
    (with-temp-buffer
      (insert content)
      (org-mode) ;; Enable org-mode for table parsing
      
      ;; Disable TOC
      (goto-char (point-min))
      (insert "#+OPTIONS: toc:nil\n")
      
      ;; Convert underlines (_text_) to italics (/text/)
      (goto-char (point-min))
      (while (re-search-forward "_\\([^[:space:]].*?[^[:space:]]\\)_" nil t)
        (replace-match "/\\1/"))
      
      ;; Convert Org tables to GFM tables manually
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*|" nil t)
        (when (org-at-table-p)
          (let ((gfm (neo--org-table-to-gfm))
                (beg (org-table-begin))
                (end (org-table-end)))
            (delete-region beg end)
            (insert "#+BEGIN_EXPORT md\n" gfm "\n#+END_EXPORT\n"))))
      
      ;; Use body-only export
      (org-export-as 'md nil nil t nil))))

(defun neo--markdown-to-org (content)
  "Convert Markdown CONTENT string to Org-mode."
  (if (or (not content) (string-empty-p content))
      ""
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      
      ;; Headers
      (while (re-search-forward "^# " nil t) (replace-match "* "))
      (goto-char (point-min))
      (while (re-search-forward "^## " nil t) (replace-match "** "))
      (goto-char (point-min))
      (while (re-search-forward "^### " nil t) (replace-match "*** "))
      
      ;; Bold (**text**) -> *text*
      (goto-char (point-min))
      (while (re-search-forward "\\*\\*\\(.*?\\)\\*\\*" nil t) (replace-match "*\\1*"))
      
      ;; Italic (*text*) -> /text/
      (goto-char (point-min))
      (while (re-search-forward "\\*\\(.*?\\)\\*" nil t)
        (replace-match "/\\1/"))
      
      ;; Convert HTML underline span back to Org underline
      (goto-char (point-min))
      (while (re-search-forward "<span class=\"underline\">\\(.*?\\)</span>" nil t)
        (replace-match "_\\1_"))

      ;; Code blocks
      (goto-char (point-min))
      (while (re-search-forward "^```\\(.*\\)$" nil t)
        (replace-match "#+BEGIN_SRC \\1"))
      (goto-char (point-min))
      (while (re-search-forward "^```$" nil t)
        (replace-match "#+END_SRC"))
      
      ;; Inline code
      (goto-char (point-min))
      (while (re-search-forward "`\\(.*?\\)`" nil t) (replace-match "~\\1~"))
      
      ;; Links
      (goto-char (point-min))
      (while (re-search-forward "\\[\\(.*?\\)\\](\\(.*?\\))" nil t)
        (replace-match "[[\\2][\\1]]"))

      ;; Basic Table Handling (GFM to Org)
      ;; Convert |---| to |---+---| for separation line
      (goto-char (point-min))
      ;; Replace |---| or | :--- | or | ---: | etc.
      (while (re-search-forward "|[-: ]*\\([-:]\\{3,\\}\\)[-: ]*|" nil t)
        (replace-match "|-\\1-|")) 
      (goto-char (point-min))
      (while (re-search-forward "|-\\([-]+\\)-|" nil t)
        (replace-match "|+\\1+|"))
      ;; Fix corners for Org: |+---+| -> |---+---|
      (goto-char (point-min))
      (while (re-search-forward "\\+\\+\\+" nil t)
        (replace-match "-+-"))

      (buffer-string))))

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

(defun neo--workflow-issue-create-backend (repo-full-name title body labels)
  "Create an issue using `ghub' or `gh' CLI and update local DB."
  (condition-case err
      (let* ((owner (car (split-string repo-full-name "/")))
             (token (neo-issues--get-github-token owner))
             (repo-id (neo-db-get-repository-id repo-full-name)))
        (unless token
          (error "No GitHub token found for %s" owner))
        
        (message "Creating issue via ghub...")
        (let ((result (ghub-request "POST" 
                                    (format "/repos/%s/issues" repo-full-name)
                                    `((title . ,title)
                                      (body . ,body)
                                      (labels . ,(vconcat labels)))
                                    :auth token
                                    :host "api.github.com")))
          (message "Issue created: %s" (cdr (assoc 'html_url result)))
          (when (and repo-id (listp result))
            (let ((issue (neo--ghub-result-to-neo-issue result repo-id)))
              (when issue
                (neo-db-upsert-issue issue))))
          result))
    (error
     (message "ghub failed (%s), falling back to CLI..." err)
     (neo-issues--create-cli repo-full-name title body labels))))

(defun neo--workflow-issue-update-backend (repo-full-name number title body state labels milestone)
  "Update an issue using `ghub' and update local DB."
  (let* ((owner (car (split-string repo-full-name "/")))
         (token (neo-issues--get-github-token owner))
         (repo-id (neo-db-get-repository-id repo-full-name))
         (data `((title . ,title)
                 (body . ,body)
                 (state . ,state)
                 (labels . ,(vconcat labels)))))
    (when milestone
      (push `(milestone . ,milestone) data))
    
    (let ((result (ghub-request "PATCH"
                                (format "/repos/%s/issues/%s" repo-full-name number)
                                data
                                :auth token
                                :host "api.github.com")))
      (when (and repo-id (listp result))
        (let ((issue (neo--ghub-result-to-neo-issue result repo-id)))
          (when issue
            (neo-db-upsert-issue issue))))
      result)))

(defun neo--workflow-issue-add-comment-backend (repo-full-name number body)
  "Add a comment to an issue using `ghub'."
  (let* ((owner (car (split-string repo-full-name "/")))
         (token (neo-issues--get-github-token owner)))
    (ghub-request "POST"
                  (format "/repos/%s/issues/%s/comments" repo-full-name number)
                  `((body . ,body))
                  :auth token
                  :host "api.github.com")))

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
    (neo--workflow-issue-create-backend repo-full-name title body selected-labels)))

(defun neo/get-github-states ()
  "Return list of valid GitHub issue states."
  '("open" "closed"))

(defun neo/get-github-milestones (repo-full-name)
  "Return list of milestones for REPO-FULL-NAME."
  (let* ((owner (car (split-string repo-full-name "/")))
         (token (neo-issues--get-github-token owner))
         (milestones (ghub-request "GET"
                                   (format "/repos/%s/milestones" repo-full-name)
                                   nil
                                   :auth token
                                   :host "api.github.com"
                                   :query '((state . "open")))))
    (mapcar (lambda (m) (cons (cdr (assoc 'title m)) (cdr (assoc 'number m)))) milestones)))

(defun neo/get-github-labels (repo-full-name)
  "Return list of labels for REPO-FULL-NAME."
  (let ((repo-id (neo-db-get-repository-id repo-full-name)))
    (if repo-id
        (neo-db-get-labels-for-repo repo-id)
      nil)))

(defun neo--workflow-fetch-issue-details (repo-full-name number)
  "Fetch issue details including body and comments."
  (let* ((owner (car (split-string repo-full-name "/")))
         (token (neo-issues--get-github-token owner))
         (issue (ghub-request "GET"
                              (format "/repos/%s/issues/%s" repo-full-name number)
                              nil
                              :auth token
                              :host "api.github.com"))
         (comments (ghub-request "GET"
                                 (format "/repos/%s/issues/%s/comments" repo-full-name number)
                                 nil
                                 :auth token
                                 :host "api.github.com")))
    (list :issue issue :comments comments)))

(defvar neo-workflow-issue-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "C-c C-c") #'neo-workflow-issue-submit)
    map)
  "Keymap for `neo-workflow-issue-mode'.")

(define-derived-mode neo-workflow-issue-mode org-mode "Neo-Issue"
  "Major mode for editing Neo Workflow issues.
\\{neo-workflow-issue-mode-map}"
  (setq-local neo-issue-repo-full-name nil)
  (setq-local neo-issue-number nil)
  (setq-local neo-issue-original-body nil)
  (setq-local neo-issue-original-title nil)
  (setq-local neo-issue-state nil)
  (setq-local neo-issue-milestone nil)
  (setq-local neo-issue-labels nil)
  (setq-local neo-issue-comments-formatted nil))

(defun neo-workflow-issue-open-template (&optional repo-full-name issue-number)
  "Open an Org buffer to create or edit an issue using Tempel."
  (interactive)
  (unless repo-full-name
    (let* ((repos (neo-load-all-repositories))
           (repo-names (mapcar #'neo-repository-full-name repos)))
      (setq repo-full-name (completing-read "Repository: " repo-names))))
  
  (let* ((buffer-name (if issue-number
                          (format "*Neo Issue: %s#%s*" repo-full-name issue-number)
                        (format "*Neo New Issue: %s*" repo-full-name)))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (neo-workflow-issue-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq neo-issue-repo-full-name repo-full-name)
        (setq neo-issue-number (and issue-number (number-to-string issue-number)))
        
        (if issue-number
            (let* ((data (neo--workflow-fetch-issue-details repo-full-name issue-number))
                   (issue (plist-get data :issue))
                   (comments (plist-get data :comments))
                   (milestone-obj (cdr (assoc 'milestone issue))))
              (setq neo-issue-original-body (neo--markdown-to-org (cdr (assoc 'body issue))))
              (setq neo-issue-original-title (cdr (assoc 'title issue)))
              (setq neo-issue-state (cdr (assoc 'state issue)))
              (setq neo-issue-milestone (if milestone-obj (cdr (assoc 'title milestone-obj)) ""))
              (setq neo-issue-labels (string-join (mapcar (lambda (l) (cdr (assoc 'name l))) (cdr (assoc 'labels issue))) ","))
              
              (setq neo-issue-comments-formatted
                    (with-temp-buffer
                      (dolist (comment comments)
                        (let ((user (cdr (assoc 'login (cdr (assoc 'user comment)))))
                              (created (cdr (assoc 'created_at comment)))
                              (c-body (neo--markdown-to-org (cdr (assoc 'body comment)))))
                          (insert (format "*** %s at %s\n" user created))
                          (let ((start (point)))
                            (insert c-body "\n")
                            (add-text-properties start (point) '(read-only t rear-nonsticky (read-only))))))
                      (buffer-string))))
          ;; New Issue Defaults
          (setq neo-issue-original-body "")
          (setq neo-issue-original-title "")
          (setq neo-issue-state "open")
          (setq neo-issue-milestone "")
          (setq neo-issue-labels "")
          (setq neo-issue-comments-formatted "")))

      ;; Insert template using Tempel
      (let ((tempel-template-sources (list #'neo-workflow--issue-template-source)))
        (tempel-insert 'neo-issue))
      
      (message "Press C-c C-c to submit issue."))
    (switch-to-buffer buffer)))

(defun neo-workflow-issue-submit ()
  "Submit the issue in the current Org buffer."
  (interactive)
  (unless (derived-mode-p 'neo-workflow-issue-mode)
    (error "Not in a Neo Issue buffer"))
  
  (save-excursion
    (goto-char (point-min))
    (unless (looking-at "\\* \\(TODO \\|DONE \\)?\\(.*\\)")
      (error "Cannot find issue title"))
    (let* ((title (string-trim (match-string 2)))
           (repo (org-entry-get (point) "Repo"))
           (number (org-entry-get (point) "Number"))
           (state (org-entry-get (point) "State"))
           (milestone-title (org-entry-get (point) "Milestone"))
           (labels-str (org-entry-get (point) "Labels"))
           (labels (if (and labels-str (not (string-empty-p labels-str)))
                       (split-string labels-str "," t " ")
                     nil))
           (milestones (if repo (neo/get-github-milestones repo) nil))
           (milestone-number (when (and milestone-title (not (string-empty-p milestone-title)))
                               (cdr (assoc milestone-title milestones))))
           (description (progn
                          (goto-char (point-min))
                          (if (re-search-forward "^\\*\\* Description$" nil t)
                              (let ((start (progn (forward-line 1) (point)))
                                    (end (if (re-search-forward "^\\*\\* " nil t)
                                             (match-beginning 0)
                                           (point-max))))
                                (neo--org-to-markdown (string-trim (buffer-substring-no-properties start end))))
                            "")))
           (new-comment (progn
                          (goto-char (point-min))
                          (if (re-search-forward "^\\*\\* New Comment$" nil t)
                              (let ((start (progn (forward-line 1) (point)))
                                    (end (point-max)))
                                (neo--org-to-markdown (string-trim (buffer-substring-no-properties start end))))
                            nil))))
      
      (when (string-empty-p title)
        (error "Issue title cannot be empty"))
      (unless repo
        (error "Repository property (:Repo:) is missing"))

      (if number
          ;; Update existing issue
          (progn
            (neo--workflow-issue-update-backend repo number title description state labels milestone-number)
            (message "Issue updated.")
            
            (when (and new-comment (not (string-empty-p new-comment)))
              (neo--workflow-issue-add-comment-backend repo number new-comment)
              (message "Comment added."))
            (kill-buffer (current-buffer)))
        
        ;; Create new issue
        (neo--workflow-issue-create-backend repo title description labels)
        (kill-buffer (current-buffer))))))

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
