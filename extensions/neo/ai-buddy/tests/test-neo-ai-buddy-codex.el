;;; tests/test-neo-ai-buddy-codex.el --- Tests for neo-ai-buddy-codex -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)
(require 'compile)

(defmacro neo/use-package (&rest _args)
  "Ignore package declarations while loading extension code in tests."
  nil)

(defun neo/register-side-action (&rest _args)
  "Stub for the real definition in neo-ui, not loaded by this test file."
  nil)

(defvar neo--ai-buddy-test-seen-executable nil
  "Executable captured by the Codex CLI startup test stub.")

(defun codex-cli-start (&optional _session)
  "Stub Codex start command used to verify first-run advice wiring."
  (setq neo--ai-buddy-test-seen-executable codex-cli-executable))

(defun codex-cli-toggle (&optional _session)
  "Stub Codex toggle command for advice wiring tests."
  nil)

(defun codex-cli-toggle-all (&optional _session)
  "Stub Codex toggle-all command for advice wiring tests."
  nil)

(load-file (expand-file-name "../neo-ai-buddy-codex.el"
                             (file-name-directory (or load-file-name buffer-file-name))))

(describe "neo-ai-buddy-codex"
  (it "installs startup advice when the extension loads"
    (expect (null (advice-member-p #'neo--ai-buddy-codex-run-with-startup-context
                                   'codex-cli-start))
            :to-be nil)
    (expect (null (advice-member-p #'neo--ai-buddy-codex-run-with-startup-context
                                   'codex-cli-toggle))
            :to-be nil)
    (expect (null (advice-member-p #'neo--ai-buddy-codex-run-with-startup-context
                                   'codex-cli-toggle-all))
            :to-be nil))

  (it "finds the nearest direnv authorization file"
    (let ((default-directory "/tmp/project/src/"))
      (cl-letf (((symbol-function 'locate-dominating-file)
                 (lambda (directory filename)
                   (when (and (string= directory "/tmp/project/src/")
                              (string= filename ".envrc"))
                     "/tmp/project/"))))
        (expect (neo--ai-buddy-direnv-rc-path)
                :to-equal "/tmp/project/.envrc"))))

  (it "runs direnv allow in the directory containing the rc file"
    (let (call)
      (cl-letf (((symbol-function 'neo--ai-buddy-direnv-rc-path)
                 (lambda (&optional _directory) "/tmp/project/.envrc"))
                ((symbol-function 'executable-find)
                 (lambda (_binary) "/usr/bin/direnv"))
                ((symbol-function 'process-file)
                 (lambda (program infile buffer display &rest args)
                   (setq call (list program infile buffer display args default-directory))
                   0)))
        (neo--ai-buddy-direnv-allow))
      (expect (nth 0 call) :to-equal "direnv")
      (expect (nth 1 call) :to-equal nil)
      (expect (nth 3 call) :to-equal nil)
      (expect (nth 4 call) :to-equal '("allow" "/tmp/project/.envrc"))
      (expect (nth 5 call) :to-equal "/tmp/project/")))

  (it "uses the repo-local launcher on the first autoloaded start"
    (cl-letf (((symbol-function 'neo--ai-buddy-direnv-allow)
               (lambda (&optional _directory) nil))
              ((symbol-function 'neo--ai-buddy-codex-executable)
               (lambda ()
                 "/tmp/project/repo/o-codex")))
      (setq neo--ai-buddy-test-seen-executable nil
            codex-cli-executable "codex")
      (codex-cli-start))
    (expect neo--ai-buddy-test-seen-executable
            :to-equal "/tmp/project/repo/o-codex")
    (expect codex-cli-executable
            :to-equal "/tmp/project/repo/o-codex"))

  (it "persists the repo-local launcher and allows direnv before starting Codex commands"
    (let (events seen-executable)
      (cl-letf (((symbol-function 'neo--ai-buddy-direnv-allow)
                 (lambda (&optional _directory)
                   (push 'allow events)))
                ((symbol-function 'neo--ai-buddy-codex-executable)
                 (lambda ()
                   "/tmp/project/repo/o-codex")))
        (setq codex-cli-executable "codex")
        (expect (neo--ai-buddy-codex-run-with-startup-context
                 (lambda ()
                   (setq seen-executable codex-cli-executable)
                   (push 'run events)
                   'started))
                :to-equal 'started))
      (expect seen-executable :to-equal "/tmp/project/repo/o-codex")
      (expect codex-cli-executable :to-equal "/tmp/project/repo/o-codex")
      (expect events :to-equal '(run allow))))

  (it "keeps the resolved launcher for deferred startup work"
    (let (deferred-check)
      (cl-letf (((symbol-function 'neo--ai-buddy-direnv-allow)
                 (lambda (&optional _directory) nil))
                ((symbol-function 'neo--ai-buddy-codex-executable)
                 (lambda ()
                   "/tmp/project/repo/o-codex")))
        (setq codex-cli-executable "codex")
        (neo--ai-buddy-codex-run-with-startup-context
         (lambda ()
           (setq deferred-check (lambda ()
                                  codex-cli-executable)))))
      (expect (funcall deferred-check)
              :to-equal "/tmp/project/repo/o-codex")))

  (it "registers direnv advice for Codex startup commands"
    (let (calls)
      (cl-letf (((symbol-function 'advice-member-p)
                 (lambda (_function _command) nil))
                ((symbol-function 'advice-add)
                 (lambda (command where function)
                   (push (list command where function) calls))))
        (neo--ai-buddy-codex-enable-direnv-allow))
      (expect (nreverse calls)
              :to-equal '((codex-cli-start :around neo--ai-buddy-codex-run-with-startup-context)
                          (codex-cli-toggle :around neo--ai-buddy-codex-run-with-startup-context)
                          (codex-cli-toggle-all :around neo--ai-buddy-codex-run-with-startup-context)))))

  (it "prefers the repo-local Codex launcher for active projects"
    (let ((default-directory "/tmp/project/src/"))
      (cl-letf (((symbol-function 'locate-dominating-file)
                 (lambda (directory filename)
                   (when (and (string= directory "/tmp/project/src/")
                              (string= filename "repo/o-codex"))
                     "/tmp/project/")))
                ((symbol-function 'file-executable-p)
                 (lambda (path)
                   (string= path "/tmp/project/repo/o-codex"))))
        (expect (neo--ai-buddy-codex-executable)
                :to-equal "/tmp/project/repo/o-codex"))))

  (it "falls back to the plain Codex binary outside repo worktrees"
    (let ((default-directory "/tmp/project/src/"))
      (cl-letf (((symbol-function 'locate-dominating-file)
                 (lambda (&rest _args) nil)))
        (expect (neo--ai-buddy-codex-executable)
                :to-equal "codex"))))

  (it "registers Codex as the weak right-side fallback action"
    (let (calls)
      (cl-letf (((symbol-function 'neo/register-side-action)
                 (lambda (side function weak)
                   (push (list side function weak) calls))))
        (neo--ai-buddy-codex-register-side-action))
      (expect calls
              :to-equal '((right neo--ai-buddy-codex-side-window-action weak)))))

  (it "does nothing when no project is active"
    (let (called)
      (cl-letf (((symbol-function 'require) (lambda (feature &rest _) feature))
                ((symbol-function 'codex-cli-project-root)
                 (lambda () (error "No project found")))
                ((symbol-function 'codex-cli--project-session-buffers)
                 (lambda () (push 'session-buffers called)))
                ((symbol-function 'codex-cli-toggle)
                 (lambda (&optional _session) (push 'toggle called)))
                ((symbol-function 'codex-cli-start)
                 (lambda (&optional _session) (push 'start called))))
        (neo--ai-buddy-codex-side-window-action))
      (expect called :to-equal nil)))

  (it "toggles an existing session without prompting"
    (let (called)
      (cl-letf (((symbol-function 'require) (lambda (feature &rest _) feature))
                ((symbol-function 'codex-cli-project-root)
                 (lambda () "/tmp/project/"))
                ((symbol-function 'codex-cli--project-session-buffers)
                 (lambda () (list (current-buffer))))
                ((symbol-function 'codex-cli-toggle)
                 (lambda (&optional _session) (push 'toggle called)))
                ((symbol-function 'codex-cli-start)
                 (lambda (&optional _session) (push 'start called)))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _args) (error "must not prompt"))))
        (neo--ai-buddy-codex-side-window-action))
      (expect called :to-equal '(toggle))))

  (it "starts a new session without prompting when none exists"
    (let (called)
      (cl-letf (((symbol-function 'require) (lambda (feature &rest _) feature))
                ((symbol-function 'codex-cli-project-root)
                 (lambda () "/tmp/project/"))
                ((symbol-function 'codex-cli--project-session-buffers)
                 (lambda () nil))
                ((symbol-function 'codex-cli-toggle)
                 (lambda (&optional _session) (push 'toggle called)))
                ((symbol-function 'codex-cli-start)
                 (lambda (&optional _session) (push 'start called)))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _args) (error "must not prompt"))))
        (neo--ai-buddy-codex-side-window-action))
      (expect called :to-equal '(start))))

  (it "builds vterm with cmake in the package build directory"
    (let (continued call)
      (cl-letf (((symbol-function 'elpaca<-build-dir)
                 (lambda (_elpaca) "/tmp/neo-ai-buddy-vterm/"))
                ((symbol-function 'elpaca-continue)
                 (lambda (elpaca) (setq continued elpaca)))
                ((symbol-function 'neo--ai-buddy-vterm-stage-installed-module)
                 (lambda (_directory) nil))
                ((symbol-function 'executable-find)
                 (lambda (_binary) "/usr/bin/cmake"))
                ((symbol-function 'call-process)
                 (lambda (program infile buffer display &rest args)
                   (setq call (list program infile buffer display args default-directory))
                   0)))
        (neo--ai-buddy-vterm-build-module 'fake-elpaca))
      (expect continued :to-equal 'fake-elpaca)
      (expect (nth 0 call) :to-equal "sh")
      (expect (nth 1 call) :to-equal nil)
      (expect (nth 3 call) :to-equal t)
      (expect (car (nth 4 call)) :to-equal "-c")
      (expect (cadr (nth 4 call)) :to-equal (neo--ai-buddy-vterm-build-command))
      (expect (nth 5 call) :to-equal "/tmp/neo-ai-buddy-vterm/")))

  (it "prefers a prebuilt vterm module during Elpaca builds"
    (let (continued staged-directory compiled-directory)
      (cl-letf (((symbol-function 'neo--ai-buddy-vterm-build-directory)
                 (lambda () "/tmp/neo-ai-buddy-vterm/"))
                ((symbol-function 'elpaca<-build-dir)
                 (lambda (_elpaca) "/tmp/neo-ai-buddy-vterm/"))
                ((symbol-function 'elpaca-continue)
                 (lambda (elpaca) (setq continued elpaca)))
                ((symbol-function 'neo--ai-buddy-vterm-module-path-in-directory)
                 (lambda (_directory) "/tmp/neo-ai-buddy-vterm/vterm-module.so"))
                ((symbol-function 'file-exists-p)
                 (lambda (_path) nil))
                ((symbol-function 'neo--ai-buddy-vterm-stage-installed-module)
                 (lambda (path)
                   (setq staged-directory path)
                   t))
                ((symbol-function 'neo--ai-buddy-vterm-build-in-directory)
                 (lambda (path) (setq compiled-directory path))))
        (neo--ai-buddy-vterm-build-module 'fake-elpaca))
      (expect continued :to-equal 'fake-elpaca)
      (expect staged-directory :to-equal "/tmp/neo-ai-buddy-vterm/")
      (expect compiled-directory :to-be nil)))

  (it "uses a prebuilt vterm module before rebuilding in the active elpaca build directory"
    (let (staged-directory compiled-directory)
      (cl-letf (((symbol-function 'neo--ai-buddy-vterm-build-directory)
                 (lambda () "/tmp/neo-ai-buddy-vterm/"))
                ((symbol-function 'neo--ai-buddy-vterm-module-path)
                 (lambda () "/tmp/neo-ai-buddy-vterm/vterm-module.so"))
                ((symbol-function 'file-directory-p)
                 (lambda (_path) t))
                ((symbol-function 'file-exists-p)
                 (lambda (_path) nil))
                ((symbol-function 'neo--ai-buddy-vterm-stage-installed-module)
                 (lambda (path)
                   (setq staged-directory path)
                   t))
                ((symbol-function 'neo--ai-buddy-vterm-build-in-directory)
                 (lambda (path) (setq compiled-directory path))))
        (neo--ai-buddy-vterm-ensure-module))
      (expect staged-directory :to-equal "/tmp/neo-ai-buddy-vterm/")
      (expect compiled-directory :to-be nil)))

  (it "rebuilds vterm in the active elpaca build directory when no prebuilt module exists"
    (let (staged-directory compiled-directory)
      (cl-letf (((symbol-function 'neo--ai-buddy-vterm-build-directory)
                 (lambda () "/tmp/neo-ai-buddy-vterm/"))
                ((symbol-function 'neo--ai-buddy-vterm-module-path)
                 (lambda () "/tmp/neo-ai-buddy-vterm/vterm-module.so"))
                ((symbol-function 'file-directory-p)
                 (lambda (_path) t))
                ((symbol-function 'file-exists-p)
                 (lambda (_path) nil))
                ((symbol-function 'neo--ai-buddy-vterm-stage-installed-module)
                 (lambda (path)
                   (setq staged-directory path)
                   nil))
                ((symbol-function 'neo--ai-buddy-vterm-build-in-directory)
                 (lambda (path) (setq compiled-directory path))))
        (neo--ai-buddy-vterm-ensure-module))
      (expect staged-directory :to-equal "/tmp/neo-ai-buddy-vterm/")
      (expect compiled-directory :to-equal "/tmp/neo-ai-buddy-vterm/"))))

(provide 'test-neo-ai-buddy-codex)
;;; test-neo-ai-buddy-codex.el ends here
