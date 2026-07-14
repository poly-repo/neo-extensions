;;; tests/test-neo-better-git.el --- Tests for neo-better-git -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)

(defmacro neo/use-package (&rest _args)
  "Ignore package declarations while loading extension code in tests."
  nil)

(defvar magit-display-buffer-function nil)
(defvar magit-status-sections-hook nil)
(defvar persp-initial-frame-name "main")

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

  (it "syncs NEO to a worktree Magit creates when the directory now exists"
    (let (switched)
      (cl-letf (((symbol-function 'magit--expand-worktree)
                 (lambda (dir) (concat "/expanded/" dir)))
                ((symbol-function 'file-directory-p)
                 (lambda (_dir) t))
                ((symbol-function 'neo/better-git-switch-to-project)
                 (lambda (path) (setq switched path))))
        (neo--better-git-worktree-sync "new-wt" "some-branch"))
      (expect switched :to-equal "/expanded/new-wt")))

  (it "does not switch project when Magit's worktree creation failed"
    (let (switched)
      (cl-letf (((symbol-function 'magit--expand-worktree)
                 (lambda (dir) (concat "/expanded/" dir)))
                ((symbol-function 'file-directory-p)
                 (lambda (_dir) nil))
                ((symbol-function 'neo/better-git-switch-to-project)
                 (lambda (path) (setq switched path))))
        (neo--better-git-worktree-sync "new-wt" "some-branch"))
      (expect switched :to-be nil)))

  (it "removes Forge issues and discussions from Magit status sections"
    (let ((magit-status-sections-hook '(forge-insert-status-headers
                                        forge-insert-issues
                                        forge-insert-discussions
                                        magit-insert-staged-changes)))
      (neo/forge-remove-topic-sections-from-status)
      (expect magit-status-sections-hook
              :to-equal
              '(forge-insert-status-headers
                magit-insert-staged-changes))))
  )

(require 'eieio)

(defclass neo-test-forge-repo ()
  ((owner :initarg :owner)
   (name :initarg :name)
   (apihost :initarg :apihost))
  "Minimal stand-in for `forge-repository', used only in tests.")

(describe "neo--project-perspective-p"
  (it "is true in a project perspective with an active project"
    (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'perspective)))
              ((symbol-function 'persp-current-name) (lambda () "my-project"))
              (persp-initial-frame-name "main")
              ((symbol-function 'projectile-project-root) (lambda () "/tmp/my-project/")))
      (expect (neo--project-perspective-p) :to-be-truthy)))

  (it "is false in an \"App:\" perspective such as the Dashboard"
    (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'perspective)))
              ((symbol-function 'persp-current-name) (lambda () "App:Dashboard"))
              (persp-initial-frame-name "main")
              ((symbol-function 'projectile-project-root) (lambda () "/tmp/my-project/")))
      (expect (neo--project-perspective-p) :to-be nil)))

  (it "is false in the initial frame perspective"
    (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'perspective)))
              ((symbol-function 'persp-current-name) (lambda () "main"))
              (persp-initial-frame-name "main")
              ((symbol-function 'projectile-project-root) (lambda () "/tmp/my-project/")))
      (expect (neo--project-perspective-p) :to-be nil)))

  (it "is false when there is no active project"
    (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'perspective)))
              ((symbol-function 'persp-current-name) (lambda () "my-project"))
              (persp-initial-frame-name "main")
              ((symbol-function 'projectile-project-root) (lambda () nil)))
      (expect (neo--project-perspective-p) :to-be nil))))

(describe "neo--push-and-create-pr-existing"
  (it "queries GitHub's live pulls endpoint for the branch against main"
    (let (query-args)
      (cl-letf (((symbol-function 'ghub-request)
                 (lambda (method url _payload &rest plist)
                   (push (list method url plist) query-args)
                   nil)))
        (neo--push-and-create-pr-existing "acme" "widgets" "api.github.com" "my-feature"))
      (let* ((call (car query-args))
             (plist (nth 2 call)))
        (expect (nth 0 call) :to-equal "GET")
        (expect (nth 1 call) :to-equal "/repos/acme/widgets/pulls")
        (expect (plist-get plist :query)
                :to-equal '((head . "acme:my-feature") (base . "main") (state . "open")))
        (expect (plist-get plist :auth) :to-equal 'forge)
        (expect (plist-get plist :host) :to-equal "api.github.com")
        (expect (plist-get plist :forge) :to-equal 'github))))

  (it "returns the PR's html_url and number when GitHub reports an open match"
    (cl-letf (((symbol-function 'ghub-request)
               (lambda (&rest _args)
                 '(((html_url . "https://github.com/acme/widgets/pull/1") (number . 1))))))
      (expect (neo--push-and-create-pr-existing "acme" "widgets" "api.github.com" "my-feature")
              :to-equal (cons "https://github.com/acme/widgets/pull/1" 1))))

  (it "returns nil when GitHub reports no open matches"
    (cl-letf (((symbol-function 'ghub-request) (lambda (&rest _args) nil)))
      (expect (neo--push-and-create-pr-existing "acme" "widgets" "api.github.com" "my-feature")
              :to-be nil))))

(describe "neo--push-and-create-pr-create"
  (it "posts directly to GitHub's pulls endpoint with an auto-generated title"
    (let (post-args)
      (cl-letf (((symbol-function 'ghub-request)
                 (lambda (method url _payload &rest plist)
                   (push (list method url plist) post-args)
                   '((html_url . "https://github.com/acme/widgets/pull/2") (number . 2)))))
        (neo--push-and-create-pr-create "acme" "widgets" "api.github.com" "my-feature"))
      (let* ((call (car post-args))
             (plist (nth 2 call)))
        (expect (nth 0 call) :to-equal "POST")
        (expect (nth 1 call) :to-equal "/repos/acme/widgets/pulls")
        (expect (plist-get plist :payload)
                :to-equal '((title . "Ongoing PR for my-feature") (head . "my-feature") (base . "main")))
        (expect (plist-get plist :auth) :to-equal 'forge)
        (expect (plist-get plist :host) :to-equal "api.github.com")
        (expect (plist-get plist :forge) :to-equal 'github))))

  (it "returns the new PR's html_url and number"
    (cl-letf (((symbol-function 'ghub-request)
               (lambda (&rest _args)
                 '((html_url . "https://github.com/acme/widgets/pull/2") (number . 2)))))
      (expect (neo--push-and-create-pr-create "acme" "widgets" "api.github.com" "my-feature")
              :to-equal (cons "https://github.com/acme/widgets/pull/2" 2)))))

(describe "neo/push-and-create-pr"
  (before-each
    (spy-on 'featurep :and-call-fake (lambda (f) (eq f 'perspective)))
    (spy-on 'persp-current-name :and-call-fake (lambda () "my-project"))
    (spy-on 'projectile-project-root :and-call-fake (lambda () "/tmp/my-project/")))

  (it "refuses to run outside a project perspective"
    (spy-on 'persp-current-name :and-call-fake (lambda () "App:Dashboard"))
    (spy-on 'magit-get-current-branch)
    (expect (neo/push-and-create-pr) :to-throw 'user-error)
    (expect 'magit-get-current-branch :not :to-have-been-called))

  (it "refuses to run on detached HEAD"
    (spy-on 'magit-get-current-branch :and-call-fake (lambda () nil))
    (expect (neo/push-and-create-pr) :to-throw 'user-error))

  (it "refuses to push/PR the main branch against itself"
    (spy-on 'magit-get-current-branch :and-call-fake (lambda () "main"))
    (expect (neo/push-and-create-pr) :to-throw 'user-error))

  (it "pushes before checking for an existing PR, then creates one when none exists"
    (let (events)
      (spy-on 'magit-get-current-branch :and-call-fake (lambda () "my-feature"))
      (spy-on 'magit-get-push-remote :and-call-fake (lambda (_branch) nil))
      (spy-on 'magit-run-git :and-call-fake
              (lambda (&rest args) (push (cons 'push args) events) 0))
      (spy-on 'forge-get-repository :and-call-fake
              (lambda (_spec) (neo-test-forge-repo :owner "acme" :name "widgets" :apihost "api.github.com")))
      (spy-on 'neo--push-and-create-pr-existing :and-call-fake
              (lambda (&rest args) (push (cons 'exists-check args) events) nil))
      (spy-on 'neo--push-and-create-pr-create :and-call-fake
              (lambda (&rest args)
                (push (cons 'create-pr args) events)
                (cons "https://github.com/acme/widgets/pull/2" 2)))
      (spy-on 'forge--pull-topic :and-call-fake
              (lambda (&rest args) (push (cons 'pull-topic args) events)))
      (neo/push-and-create-pr)
      (setq events (nreverse events))
      (expect (car (nth 0 events)) :to-equal 'push)
      (expect (nth 0 events)
              :to-equal
              '(push "push" "-v" "-u" "origin" "refs/heads/my-feature:refs/heads/my-feature"))
      (expect (nth 1 events) :to-equal '(exists-check "acme" "widgets" "api.github.com" "my-feature"))
      (expect (nth 2 events) :to-equal '(create-pr "acme" "widgets" "api.github.com" "my-feature"))
      (expect (car (nth 3 events)) :to-equal 'pull-topic)
      (expect (nth 2 (nth 3 events)) :to-equal 2)))

  (it "signals an error and never checks for a PR when the push fails"
    (spy-on 'magit-get-current-branch :and-call-fake (lambda () "my-feature"))
    (spy-on 'magit-get-push-remote :and-call-fake (lambda (_branch) "origin"))
    (spy-on 'magit-run-git :and-call-fake (lambda (&rest _args) 1))
    (spy-on 'neo--push-and-create-pr-existing)
    (spy-on 'forge-get-repository :and-call-fake
            (lambda (_spec) (neo-test-forge-repo :owner "acme" :name "widgets" :apihost "api.github.com")))
    (expect (neo/push-and-create-pr) :to-throw 'error)
    (expect 'neo--push-and-create-pr-existing :not :to-have-been-called))

  (it "reports an already-existing PR's URL instead of creating a new one"
    (let (messages)
      (spy-on 'magit-get-current-branch :and-call-fake (lambda () "my-feature"))
      (spy-on 'magit-get-push-remote :and-call-fake (lambda (_branch) "origin"))
      (spy-on 'magit-run-git :and-call-fake (lambda (&rest _args) 0))
      (spy-on 'forge-get-repository :and-call-fake
              (lambda (_spec) (neo-test-forge-repo :owner "acme" :name "widgets" :apihost "api.github.com")))
      (spy-on 'neo--push-and-create-pr-existing :and-call-fake
              (lambda (&rest _args) (cons "https://github.com/acme/widgets/pull/1" 1)))
      (spy-on 'neo--push-and-create-pr-create)
      (spy-on 'forge--pull-topic)
      (spy-on 'y-or-n-p :and-call-fake (lambda (&rest _args) nil))
      (spy-on 'message :and-call-fake (lambda (fmt &rest args) (push (apply #'format fmt args) messages)))
      (neo/push-and-create-pr)
      (expect 'neo--push-and-create-pr-create :not :to-have-been-called)
      (expect (car messages) :to-match "https://github.com/acme/widgets/pull/1")))

  (it "pulls the existing PR's topic into Forge before offering to browse it"
    (let ((repo (neo-test-forge-repo :owner "acme" :name "widgets" :apihost "api.github.com")))
      (spy-on 'magit-get-current-branch :and-call-fake (lambda () "my-feature"))
      (spy-on 'magit-get-push-remote :and-call-fake (lambda (_branch) "origin"))
      (spy-on 'magit-run-git :and-call-fake (lambda (&rest _args) 0))
      (spy-on 'forge-get-repository :and-call-fake (lambda (_spec) repo))
      (spy-on 'neo--push-and-create-pr-existing :and-call-fake
              (lambda (&rest _args) (cons "https://github.com/acme/widgets/pull/1" 1)))
      (spy-on 'forge--pull-topic)
      (spy-on 'y-or-n-p :and-call-fake (lambda (&rest _args) nil))
      (neo/push-and-create-pr)
      (expect 'forge--pull-topic :to-have-been-called-with repo 1)))

  (it "offers to open the existing PR's URL in a browser when confirmed"
    (spy-on 'magit-get-current-branch :and-call-fake (lambda () "my-feature"))
    (spy-on 'magit-get-push-remote :and-call-fake (lambda (_branch) "origin"))
    (spy-on 'magit-run-git :and-call-fake (lambda (&rest _args) 0))
    (spy-on 'forge-get-repository :and-call-fake
            (lambda (_spec) (neo-test-forge-repo :owner "acme" :name "widgets" :apihost "api.github.com")))
    (spy-on 'neo--push-and-create-pr-existing :and-call-fake
            (lambda (&rest _args) (cons "https://github.com/acme/widgets/pull/1" 1)))
    (spy-on 'forge--pull-topic)
    (spy-on 'y-or-n-p :and-call-fake (lambda (&rest _args) t))
    (spy-on 'browse-url)
    (neo/push-and-create-pr)
    (expect 'browse-url :to-have-been-called-with "https://github.com/acme/widgets/pull/1")))

(provide 'test-neo-better-git)
;;; test-neo-better-git.el ends here
