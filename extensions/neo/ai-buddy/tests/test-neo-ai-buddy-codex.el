;;; tests/test-neo-ai-buddy-codex.el --- Tests for neo-ai-buddy-codex -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)
(require 'compile)

(defmacro neo/use-package (&rest _args)
  "Ignore package declarations while loading extension code in tests."
  nil)

(load-file (expand-file-name "../neo-ai-buddy-codex.el"
                             (file-name-directory (or load-file-name buffer-file-name))))

(describe "neo-ai-buddy-codex"
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

  (it "allows direnv before starting Codex commands"
    (let (events)
      (cl-letf (((symbol-function 'neo--ai-buddy-direnv-allow)
                 (lambda (&optional _directory)
                   (push 'allow events))))
        (expect (neo--ai-buddy-codex-run-with-direnv
                 (lambda ()
                   (push 'run events)
                   'started))
                :to-equal 'started))
      (expect events :to-equal '(run allow))))

  (it "registers direnv advice for Codex startup commands"
    (let (calls)
      (cl-letf (((symbol-function 'advice-member-p)
                 (lambda (_function _command) nil))
                ((symbol-function 'advice-add)
                 (lambda (command where function)
                   (push (list command where function) calls))))
        (neo--ai-buddy-codex-enable-direnv-allow))
      (expect (nreverse calls)
              :to-equal '((codex-cli-start :around neo--ai-buddy-codex-run-with-direnv)
                          (codex-cli-toggle :around neo--ai-buddy-codex-run-with-direnv)
                          (codex-cli-toggle-all :around neo--ai-buddy-codex-run-with-direnv)))))

  (it "builds vterm with cmake in the package build directory"
    (let (continued call)
      (cl-letf (((symbol-function 'elpaca<-build-dir)
                 (lambda (_elpaca) "/tmp/neo-ai-buddy-vterm/"))
                ((symbol-function 'elpaca-continue)
                 (lambda (elpaca) (setq continued elpaca)))
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

  (it "rebuilds vterm in the active elpaca build directory when the module is missing"
    (let (directory)
      (cl-letf (((symbol-function 'neo--ai-buddy-vterm-build-directory)
                 (lambda () "/tmp/neo-ai-buddy-vterm/"))
                ((symbol-function 'neo--ai-buddy-vterm-module-path)
                 (lambda () "/tmp/neo-ai-buddy-vterm/vterm-module.so"))
                ((symbol-function 'file-directory-p)
                 (lambda (_path) t))
                ((symbol-function 'file-exists-p)
                 (lambda (_path) nil))
                ((symbol-function 'neo--ai-buddy-vterm-build-in-directory)
                 (lambda (path) (setq directory path))))
        (neo--ai-buddy-vterm-ensure-module))
      (expect directory :to-equal "/tmp/neo-ai-buddy-vterm/"))))

(provide 'test-neo-ai-buddy-codex)
;;; test-neo-ai-buddy-codex.el ends here
