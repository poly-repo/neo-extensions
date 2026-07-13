;;; tests/test-neo-projects.el --- Tests for neo-projects -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)

(defmacro neo/use-package (&rest _args)
  "Ignore package declarations while loading extension code in tests."
  nil)

(load-file (expand-file-name "../neo-projects.el"
                             (file-name-directory (or load-file-name buffer-file-name))))

(describe "neo-projects"
  (before-each
    (clrhash neo/project-last-switched-times))

  (it "forces Magit visibility for a new project without notes"
    (let (magit-calls)
      (cl-letf (((symbol-function 'projectile-project-root)
                 (lambda () "/tmp/project"))
                ((symbol-function 'neo/projectile-update-treemacs)
                 (lambda () t))
                ((symbol-function 'neo/bury-other-project-buffers)
                 #'ignore)
                ((symbol-function 'persp-curr)
                 (lambda () 'mock-perspective))
                ((symbol-function 'persp-name)
                 (lambda (_perspective) "mock"))
                ((symbol-function 'neo--should-open-notes-p)
                 (lambda (_project-root) nil))
                ((symbol-function 'neo--projects-ensure-project-magit-status)
                 (lambda (project-root &optional force)
                   (push (list project-root force) magit-calls))))
        (neo/projectile-switch-project-action))
      (expect (nreverse magit-calls)
              :to-equal
              '(("/tmp/project" t)))))

  (it "only requests fallback Magit visibility on revisits"
    (let (magit-calls)
      (cl-letf (((symbol-function 'projectile-project-root)
                 (lambda () "/tmp/project"))
                ((symbol-function 'neo/projectile-update-treemacs)
                 (lambda () nil))
                ((symbol-function 'persp-curr)
                 (lambda () 'mock-perspective))
                ((symbol-function 'persp-name)
                 (lambda (_perspective) "mock"))
                ((symbol-function 'neo--projects-ensure-project-magit-status)
                 (lambda (project-root &optional force)
                   (push (list project-root force) magit-calls))))
        (neo/projectile-switch-project-action))
      (expect (nreverse magit-calls)
              :to-equal
              '(("/tmp/project" nil)))))

  (it "opens notes first and then reuses fallback Magit visibility"
    (let (notes-opened magit-calls)
      (cl-letf (((symbol-function 'projectile-project-root)
                 (lambda () "/tmp/project"))
                ((symbol-function 'neo/projectile-update-treemacs)
                 (lambda () t))
                ((symbol-function 'neo/bury-other-project-buffers)
                 #'ignore)
                ((symbol-function 'persp-curr)
                 (lambda () 'mock-perspective))
                ((symbol-function 'persp-name)
                 (lambda (_perspective) "mock"))
                ((symbol-function 'neo--should-open-notes-p)
                 (lambda (_project-root) t))
                ((symbol-function 'file-exists-p)
                 (lambda (_path) t))
                ((symbol-function 'find-file-other-window)
                 (lambda (path) (setq notes-opened path)))
                ((symbol-function 'neo--projects-ensure-project-magit-status)
                 (lambda (project-root &optional force)
                   (push (list project-root force) magit-calls))))
        (neo/projectile-switch-project-action))
      (expect notes-opened
              :to-equal
              "/tmp/project/.personal-notes.org")
      (expect (nreverse magit-calls)
              :to-equal
              '(("/tmp/project" nil)))))

  (it "resets a reused scratch buffer's stale default-directory to the new root"
    (let ((buffer (get-buffer-create "*scratch* (name)")))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq default-directory "/tmp/stale-old-project/"))
            (neo--projects-reset-scratch-directory "name" "/tmp/new-project")
            (expect (buffer-local-value 'default-directory buffer)
                    :to-equal "/tmp/new-project/"))
        (kill-buffer buffer))))

  (it "does nothing when no scratch buffer exists yet for the perspective"
    (expect (get-buffer "*scratch* (does-not-exist)") :to-be nil)
    (expect (neo--projects-reset-scratch-directory "does-not-exist" "/tmp/new-project")
            :to-be nil)))

(describe "neo-projects: new project worktree"
  (it "derives a repo name from an SSH remote URL"
    (expect (neo--projects-repo-name-from-remote-url
             "git@github.com:owner/omega.git")
            :to-equal "omega"))

  (it "derives a repo name from an HTTPS remote URL without .git suffix"
    (expect (neo--projects-repo-name-from-remote-url
             "https://github.com/owner/omega")
            :to-equal "omega"))

  (it "builds the worktree directory from repo and slug"
    (expect (neo--projects-new-worktree-directory "omega" "add-oauth-login")
            :to-equal (expand-file-name "omega_add-oauth-login"
                                         "~/.local/share/wtrees/")))

  (it "builds the branch name from repo and slug"
    (expect (neo--projects-new-worktree-branch "omega" "add-oauth-login")
            :to-equal "omega/add-oauth-login"))

  (it "resolves the current repo name via magit-get"
    (cl-letf (((symbol-function 'magit-get)
               (lambda (&rest _keys) "git@github.com:owner/omega.git")))
      (expect (neo--projects-current-repo-name) :to-equal "omega")))

  (it "errors when there is no origin remote"
    (cl-letf (((symbol-function 'magit-get)
               (lambda (&rest _keys) nil)))
      (expect (neo--projects-current-repo-name) :to-throw)))

  (it "creates a worktree off origin/main and switches to it, in order"
    (let (calls)
      (cl-letf (((symbol-function 'neo--projects-current-repo-name)
                 (lambda () "omega"))
                ((symbol-function 'neo/git--slugify)
                 (lambda (_title &optional _max-words) "add-oauth-login"))
                ((symbol-function 'neo/magit-worktree-create)
                 (lambda (directory branch &optional start-point)
                   (push (list 'create directory branch start-point) calls)))
                ((symbol-function 'neo/better-git-switch-to-project)
                 (lambda (path) (push (list 'switch path) calls))))
        (neo/projectile-new-project "Add OAuth login"))
      (expect (nreverse calls)
              :to-equal
              (list (list 'create
                          (expand-file-name "omega_add-oauth-login"
                                             "~/.local/share/wtrees/")
                          "omega/add-oauth-login"
                          nil)
                    (list 'switch
                          (expand-file-name "omega_add-oauth-login"
                                             "~/.local/share/wtrees/"))))))

  (it "refuses an empty description"
    (expect (neo/projectile-new-project "") :to-throw))

  (it "refuses a description that slugifies to nothing"
    (cl-letf (((symbol-function 'neo--projects-current-repo-name)
               (lambda () "omega"))
              ((symbol-function 'neo/git--slugify)
               (lambda (_title &optional _max-words) "")))
      (expect (neo/projectile-new-project "the a an") :to-throw))))

(provide 'test-neo-projects)
;;; test-neo-projects.el ends here
