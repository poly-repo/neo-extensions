;;; tests/test-neo-dashboard.el --- Tests for neo-dashboard -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)

(defmacro neo/use-package (&rest _args)
  "Ignore package declarations while loading extension code in tests."
  nil)

(defvar neo/extension-registry-alist nil)
(defvar neo--framework nil)
(defvar neo/cache-directory nil)

(provide 'dashboard)
(provide 'perspective)

(load-file (expand-file-name "../neo-dashboard.el"
                             (file-name-directory (or load-file-name buffer-file-name))))

(describe "neo-dashboard"
  (before-each
    (setq neo/dashboard--origin-persp nil)
    (when-let* ((buffer (get-buffer (neo/dashboard--buffer-name))))
      (kill-buffer buffer)))

  (after-each
    (setq neo/dashboard--origin-persp nil)
    (when-let* ((buffer (get-buffer (neo/dashboard--buffer-name))))
      (kill-buffer buffer)))

  (it "opens the dashboard buffer in the dashboard perspective"
    (let ((switches nil)
          (added nil)
          (buffer nil))
      (cl-letf (((symbol-function 'persp-current-name)
                 (lambda () "work"))
                ((symbol-function 'persp-switch)
                 (lambda (name) (push name switches)))
                ((symbol-function 'persp-add-buffer)
                 (lambda (buf) (setq added buf)))
                ((symbol-function 'dashboard-open)
                 (lambda ()
                   (setq buffer (get-buffer-create (neo/dashboard--buffer-name))))))
        (expect (neo/dashboard-initial-buffer) :to-equal buffer))
      (expect (nreverse switches) :to-equal (list neo/dashboard-persp))
      (expect neo/dashboard--origin-persp :to-equal "work")
      (expect added :to-equal buffer)))

  (it "does not record the dashboard perspective as the origin"
    (let ((switches nil))
      (cl-letf (((symbol-function 'persp-current-name)
                 (lambda () neo/dashboard-persp))
                ((symbol-function 'persp-switch)
                 (lambda (name) (push name switches))))
        (neo/dashboard--enter))
      (expect switches :to-equal nil)
      (expect neo/dashboard--origin-persp :to-equal nil)))

  (it "treats the legacy Dashboard perspective as the dashboard"
    (let ((switches nil))
      (cl-letf (((symbol-function 'persp-current-name)
                 (lambda () "Dashboard"))
                ((symbol-function 'persp-switch)
                 (lambda (name) (push name switches))))
        (neo/dashboard--enter))
      (expect switches :to-equal nil)
      (expect neo/dashboard--origin-persp :to-equal nil)))

  (it "reuses the legacy dashboard perspective without switching away"
    (let ((switches nil)
          (added nil)
          (buffer nil))
      (cl-letf (((symbol-function 'persp-current-name)
                 (lambda () (neo/dashboard--buffer-name)))
                ((symbol-function 'persp-switch)
                 (lambda (name) (push name switches)))
                ((symbol-function 'persp-add-buffer)
                 (lambda (buf) (setq added buf)))
                ((symbol-function 'dashboard-open)
                 (lambda ()
                   (setq buffer (get-buffer-create (neo/dashboard--buffer-name))))))
        (expect (neo/dashboard-initial-buffer) :to-equal buffer))
      (expect switches :to-equal nil)
      (expect neo/dashboard--origin-persp :to-equal nil)
      (expect added :to-equal buffer)))

  (it "adds an existing dashboard buffer to the current perspective"
    (let ((added nil)
          (buffer (get-buffer-create (neo/dashboard--buffer-name))))
      (unwind-protect
          (cl-letf (((symbol-function 'persp-add-buffer)
                     (lambda (buf) (setq added buf))))
            (neo/dashboard--ensure-current-buffer-added)
            (expect added :to-equal buffer))
        (kill-buffer buffer))))

  (it "displays the dashboard buffer for interactive calls"
    (let ((buffer (get-buffer-create "*neo-dashboard-test*"))
          (shown nil)
          (added nil))
      (unwind-protect
          (cl-letf (((symbol-function 'neo/dashboard-initial-buffer)
                     (lambda () buffer))
                    ((symbol-function 'neo/dashboard--add-buffer)
                     (lambda (buf) (setq added buf)))
                    ((symbol-function 'switch-to-buffer)
                     (lambda (buf) (setq shown buf))))
            (neo/dashboard))
        (kill-buffer buffer))
      (expect shown :to-equal buffer)
      (expect added :to-equal buffer)))

  (it "records the displaced perspective when landing after restore"
    (let ((switches nil)
          (current "14421225"))
      (setq neo/dashboard--origin-persp "main") ; stale origin from early call
      (cl-letf (((symbol-function 'persp-current-name)
                 (lambda () current))
                ((symbol-function 'persp-switch)
                 (lambda (name) (push name switches) (setq current name)))
                ((symbol-function 'persp-add-buffer) (lambda (_buf) nil))
                ((symbol-function 'dashboard-open)
                 (lambda ()
                   (get-buffer-create (neo/dashboard--buffer-name))))
                ((symbol-function 'switch-to-buffer) (lambda (_buf) nil)))
        (neo/dashboard-land-after-restore))
      ;; The restored perspective, not the stale "main", is now the origin.
      (expect neo/dashboard--origin-persp :to-equal "14421225")
      (expect (member neo/dashboard-persp switches) :to-be-truthy)))

  (it "returns to the displaced perspective on quit"
    (let ((switches nil))
      (setq neo/dashboard--origin-persp "14421225")
      (cl-letf (((symbol-function 'persp-switch)
                 (lambda (name) (push name switches))))
        (neo/dashboard-quit))
      (expect switches :to-equal '("14421225"))
      (expect neo/dashboard--origin-persp :to-equal nil)))

  (it "builds application entries with labels and commands"
    (cl-letf (((symbol-function 'neo/applications)
               (lambda ()
                 (list (list :name "Calc" :binding "c" :command 'neo/app-calc)
                       (list :name "Dashboard" :binding nil :command 'neo/app-dashboard))))
              ((symbol-function 'neo/application-name)
               (lambda (app) (plist-get app :name)))
              ((symbol-function 'neo/application-binding)
               (lambda (app) (plist-get app :binding)))
              ((symbol-function 'neo/application-command)
               (lambda (app) (plist-get app :command))))
      (let ((entries (neo/dashboard--application-entries)))
        (expect entries :to-equal
                '(("Calc  (M-a c)" . neo/app-calc)
                  ("Dashboard" . neo/app-dashboard))))))

  (it "renders environment debug lines from framework and registry state"
    (let* ((available (make-hash-table :test 'equal))
           (installed (make-hash-table :test 'equal))
           (framework (list :available available :installed installed))
           (neo/extension-registry-alist
            '(("neo" . (:override "/tmp/neo-local"))
              ("mav" . (:override nil))))
           lines)
      (puthash "neo:full-monty" t available)
      (puthash "neo:haskell" t available)
      (puthash "neo:full-monty" t installed)
      (cl-letf (((symbol-function 'neo/get-emacs-instance-name)
                 (lambda () "neo-test"))
                ((symbol-function 'neo/use-local-extension-sources-p)
                 (lambda () t))
                ((symbol-function 'neo/config-db-path)
                 (lambda () "/tmp/neo.sqlite"))
                ((symbol-function 'neo/data-directory)
                 (lambda () "/tmp/neo-data"))
                ((symbol-function 'neo/get-config)
                 (lambda (_key) "(\"neo:full-monty\")"))
                ((symbol-function 'neo-framework-available-extensions)
                 (lambda (_framework) available))
                ((symbol-function 'neo-framework-installed-extensions)
                 (lambda (_framework) installed))
                ((symbol-function 'neo--extension-registry-override)
                 (lambda (registry) (plist-get registry :override)))
                ((symbol-function 'neo/cache-file-path)
                 (lambda (path) (concat "/cache/" path))))
        (let ((neo--framework framework)
              (neo/cache-directory "/tmp/neo-cache"))
          (setq lines (neo/dashboard--debug-lines))))
      (expect lines :to-contain "Instance: neo-test")
      (expect lines :to-contain "Extension source: local checkout")
      (expect lines :to-contain "Cache dir: /tmp/neo-cache")
      (expect lines :to-contain "Data dir: /tmp/neo-data")
      (expect lines :to-contain "Enabled roots: (\"neo:full-monty\")")
      (expect lines :to-contain "neo:full-monty available=yes installed=yes")
      (expect lines :to-contain "neo:haskell available=yes installed=no")
      (expect lines :to-contain "neo: local /tmp/neo-local")
      (expect lines :to-contain "mav: cached /cache/extensions/mav/extensions-current.el [missing]"))))

(provide 'test-neo-dashboard)
;;; test-neo-dashboard.el ends here
