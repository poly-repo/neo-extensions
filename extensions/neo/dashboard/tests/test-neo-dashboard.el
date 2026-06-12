;;; tests/test-neo-dashboard.el --- Tests for neo-dashboard -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)

(defmacro neo/use-package (&rest _args)
  "Ignore package declarations while loading extension code in tests."
  nil)

(provide 'dashboard)
(provide 'perspective)

(load-file (expand-file-name "../neo-dashboard.el"
                             (file-name-directory (or load-file-name buffer-file-name))))

(describe "neo-dashboard"
  (before-each
    (setq neo/dashboard--origin-persp nil)
    (when-let ((buffer (get-buffer (neo/dashboard--buffer-name))))
      (kill-buffer buffer)))

  (after-each
    (setq neo/dashboard--origin-persp nil)
    (when-let ((buffer (get-buffer (neo/dashboard--buffer-name))))
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
      (expect added :to-equal buffer))))

(provide 'test-neo-dashboard)
;;; test-neo-dashboard.el ends here
