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

(provide 'test-neo-projects)
;;; test-neo-projects.el ends here
