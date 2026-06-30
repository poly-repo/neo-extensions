;;; neo-workflow-project.el --- Neo Workflow Project Management -*- lexical-binding: t; -*-

;; Adapted from workflow/neo-workflow-project.el.
;; Projects are now discovered from the beads workspace, not SQLite.

(require 'cl-lib)
(require 'project)
(require 'neo-workflow-db)
(require 'neo-workflow-models)

(defgroup neo-workflow nil
  "Neo Workflow extension."
  :group 'neo)

(defcustom neo/workflow-project-search-paths '("~/workspace" "~/projects")
  "List of directories to search for projects (used by discover command)."
  :type '(repeat directory)
  :group 'neo-workflow)

(cl-defmethod project-root ((project (head neo-workflow)))
  "Return the root directory for a neo-workflow PROJECT."
  (cdr project))

;;;###autoload
(defun neo/workflow-project-enable ()
  "Enable the Neo Workflow project backend."
  (interactive)
  (add-hook 'project-find-functions #'neo-workflow-project-backend))

;;;###autoload
(defun neo/workflow-project-disable ()
  "Disable the Neo Workflow project backend."
  (interactive)
  (remove-hook 'project-find-functions #'neo-workflow-project-backend))

;;;###autoload
(defun neo/workflow-project-discover ()
  "Register all directories under `neo/workflow-project-search-paths' as projects.
Unlike the old SQLite version this simply reports what was found; no DB write occurs."
  (interactive)
  (let ((count 0))
    (dolist (path neo/workflow-project-search-paths)
      (when (file-directory-p path)
        (dolist (file (directory-files path t "^[^.]"))
          (when (and (file-directory-p file)
                     (file-exists-p (expand-file-name ".git" file)))
            (cl-incf count)
            (message "neo-workflow: discovered project %s" file)))))
    (message "Discovered %d projects (beads workspace is the primary source)." count)))

(provide 'neo-workflow-project)
;;; neo-workflow-project.el ends here
