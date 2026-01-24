;;; neo-workflow-project.el --- Neo Workflow Project Management -*- lexical-binding: t; -*--

(require 'cl-lib)
(require 'project)
(require 'neo-workflow-db)

(defgroup neo-workflow nil
  "Neo Workflow extension."
  :group 'neo)

(defcustom neo/workflow-project-search-paths '("~/workspace" "~/projects")
  "List of directories to search for projects."
  :type '(repeat directory)
  :group 'neo-workflow)

(defun neo--workflow-project-get-git-remote-url (dir)
  "Get the remote origin URL for the git repository in DIR."
  (let ((default-directory dir))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil "config" "--get" "remote.origin.url"))
        (string-trim (buffer-string))))))

(defun neo--workflow-project-p (dir)
  "Check if DIR is a project.
Prioritizes `project-current' logic if possible, otherwise checks for .git."
  (let ((default-directory dir))
    (or (file-exists-p (expand-file-name ".git" dir))
        (project-current nil dir))))

(defun neo--workflow-project-discover-in-path (path)
  "Discover projects in PATH and return a list of project objects."
  (let ((projects '()))
    (when (file-directory-p path)
      (dolist (file (directory-files path t "^[^.]"))
        (when (and (file-directory-p file)
                   (neo--workflow-project-p file))
          (push file projects))))
    projects))

;;;###autoload
(defun neo/workflow-project-discover ()
  "Discover projects in `neo/workflow-project-search-paths' and persist them to the DB."
  (interactive)
  (let ((count 0))
    (dolist (path neo/workflow-project-search-paths)
      (let ((projects (neo--workflow-project-discover-in-path path)))
        (dolist (proj-path projects)
          (let* ((id (file-name-nondirectory (directory-file-name proj-path)))
                 (repo-url (neo--workflow-project-get-git-remote-url proj-path))
                 (repo (if repo-url 
                           (replace-regexp-in-string "\.git$" "" (car (last (split-string repo-url "/")))) 
                         id)) ;; Fallback to ID if no remote
                 (type "git") ;; Assume git for now
                 (worktree-path proj-path))
            (neo-db-insert-project id repo type nil worktree-path)
            (cl-incf count)))))
    (message "Discovered and synced %d projects." count)))

(provide 'neo-workflow-project)
;;; neo-workflow-project.el ends here