;;; tests/test-neo-better-git.el --- Tests for neo-better-git -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)

(defmacro neo/use-package (&rest _args)
  "Ignore package declarations while loading extension code in tests."
  nil)

(defvar magit-display-buffer-function nil)

(provide 'neo-better-git-brancher)

(load-file (expand-file-name "../neo-better-git.el"
                             (file-name-directory (or load-file-name buffer-file-name))))

(describe "neo-better-git"
  (it "switches to a project and then ensures Magit is visible"
    (let (events)
      (cl-letf (((symbol-function 'neo/switch-to-project)
                 (lambda (path) (push (list 'switch path) events)))
                ((symbol-function 'neo--better-git-ensure-project-magit-status)
                 (lambda (path &optional force)
                   (push (list 'magit path force) events))))
        (neo/better-git-switch-to-project "/tmp/project"))
      (expect (nreverse events)
              :to-equal
              '((switch "/tmp/project")
                (magit "/tmp/project" t)))))

  (it "reuses a hidden Magit buffer and adds it to the current perspective"
    (let* ((buffer (get-buffer-create "*neo-better-git-test*"))
           (added nil)
           (shown nil))
      (unwind-protect
          (cl-letf (((symbol-function 'vc-git-responsible-p)
                     (lambda (_path) t))
                    ((symbol-function 'magit-status-setup-buffer)
                     (lambda (&optional _path) buffer))
                    ((symbol-function 'featurep)
                     (lambda (feature) (eq feature 'perspective)))
                    ((symbol-function 'persp-add-buffer)
                     (lambda (buf) (setq added buf)))
                    ((symbol-function 'neo--better-git-project-magit-buffer)
                     (lambda (_path) buffer))
                    ((symbol-function 'get-buffer-window)
                     (lambda (_buf &optional _all-frames) nil))
                    ((symbol-function 'neo--better-git-show-magit-buffer)
                     (lambda (buf &optional _window) (setq shown buf))))
            (neo--better-git-ensure-project-magit-status "/tmp/project" t))
        (kill-buffer buffer))
      (expect added :to-equal buffer)
      (expect shown :to-equal buffer)))

  (it "uses a Magit-compatible display callback when creating a status buffer"
    (let* ((buffer (get-buffer-create "*neo-better-git-display-test*"))
           (displayed nil)
           (main-window (selected-window)))
      (unwind-protect
          (cl-letf (((symbol-function 'vc-git-responsible-p)
                     (lambda (_path) t))
                    ((symbol-function 'neo--better-git-project-magit-buffer)
                     (lambda (_path) nil))
                    ((symbol-function 'neo--better-git-main-window)
                     (lambda () main-window))
                    ((symbol-function 'display-buffer-same-window)
                     (lambda (buf alist)
                       (setq displayed (list buf alist))
                       main-window))
                    ((symbol-function 'magit-status-setup-buffer)
                     (lambda (&optional _path)
                       (funcall magit-display-buffer-function buffer)
                       buffer))
                    ((symbol-function 'get-buffer-window)
                     (lambda (_buf &optional _all-frames) main-window)))
            (neo--better-git-ensure-project-magit-status "/tmp/project" t))
        (kill-buffer buffer))
      (expect displayed :to-equal (list buffer nil))))

  (it "replaces low-priority buffers without forcing Magit into user buffers"
    (let* ((buffer (get-buffer-create "*neo-better-git-fallback-test*"))
           (shown nil)
           (fallback-window (selected-window)))
      (unwind-protect
          (cl-letf (((symbol-function 'vc-git-responsible-p)
                     (lambda (_path) t))
                    ((symbol-function 'magit-status-setup-buffer)
                     (lambda (&optional _path) buffer))
                    ((symbol-function 'neo--better-git-project-magit-buffer)
                     (lambda (_path) buffer))
                    ((symbol-function 'get-buffer-window)
                     (lambda (_buf &optional _all-frames) nil))
                    ((symbol-function 'neo--better-git-fallback-window)
                     (lambda () fallback-window))
                    ((symbol-function 'neo--better-git-show-magit-buffer)
                     (lambda (buf &optional window)
                       (setq shown (list buf window)))))
            (neo--better-git-ensure-project-magit-status "/tmp/project"))
        (kill-buffer buffer))
      (expect shown :to-equal (list buffer fallback-window))))

  (it "leaves user buffers alone when no fallback window exists"
    (let* ((buffer (get-buffer-create "*neo-better-git-user-window-test*"))
           (shown nil))
      (unwind-protect
          (cl-letf (((symbol-function 'vc-git-responsible-p)
                     (lambda (_path) t))
                    ((symbol-function 'magit-status-setup-buffer)
                     (lambda (&optional _path) buffer))
                    ((symbol-function 'neo--better-git-project-magit-buffer)
                     (lambda (_path) buffer))
                    ((symbol-function 'get-buffer-window)
                     (lambda (_buf &optional _all-frames) nil))
                    ((symbol-function 'neo--better-git-fallback-window)
                     (lambda () nil))
                    ((symbol-function 'neo--better-git-show-magit-buffer)
                     (lambda (&rest args) (setq shown args))))
            (neo--better-git-ensure-project-magit-status "/tmp/project"))
        (kill-buffer buffer))
      (expect shown :to-be nil)))
  )

(provide 'test-neo-better-git)
;;; test-neo-better-git.el ends here
