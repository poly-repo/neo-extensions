;;; tests/test-neo-ui-side-windows.el --- Tests for neo-ui-side-windows -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)

(load-file (expand-file-name "../neo-ui-side-windows.el"
                             (file-name-directory (or load-file-name buffer-file-name))))

(describe "neo-ui-side-windows"
  (before-each
    (setq neo/side-actions nil))

  (it "runs a strong action registered alone"
    (let (called)
      (neo/register-side-action 'right (lambda () (setq called 'strong)))
      (neo/dispatch-side 'right)
      (expect called :to-equal 'strong)))

  (it "runs a weak action registered alone"
    (let (called)
      (neo/register-side-action 'right (lambda () (setq called 'weak)) 'weak)
      (neo/dispatch-side 'right)
      (expect called :to-equal 'weak)))

  (it "prefers the strong action over the weak one for the same side"
    (let (called)
      (neo/register-side-action 'right (lambda () (push 'weak called)) 'weak)
      (neo/register-side-action 'right (lambda () (push 'strong called)))
      (neo/dispatch-side 'right)
      (expect called :to-equal '(strong))))

  (it "replaces the previous strong action when registering a new one"
    (let (called)
      (neo/register-side-window-default 'right (lambda () (push 'first called)))
      (neo/register-side-window-default 'right (lambda () (push 'second called)))
      (neo/dispatch-side 'right)
      (expect called :to-equal '(second)))))

(describe "neo/toggle-side-window slot release"
  (before-each
    (setq neo/side-actions nil))

  (it "releases the slot for a dismissed transient buffer and dispatches the default action"
    (let ((buf (generate-new-buffer " *test-help*"))
          dispatched)
      (unwind-protect
          (progn
            (with-current-buffer buf (setq major-mode 'help-mode))
            (neo/register-side-action 'right (lambda () (setq dispatched t)) 'weak)
            (spy-on 'display-buffer)
            (neo/toggle-side-window 'right)
            (expect dispatched :to-be-truthy)
            (expect 'display-buffer :not :to-have-been-called))
        (kill-buffer buf))))

  (it "resurrects a dismissed persistent buffer instead of dispatching the default action"
    (let ((buf (generate-new-buffer " *test-treemacs*"))
          dispatched)
      (unwind-protect
          (progn
            (with-current-buffer buf (setq major-mode 'treemacs-mode))
            (neo/register-side-action 'left (lambda () (setq dispatched t)) 'weak)
            (spy-on 'display-buffer)
            (neo/toggle-side-window 'left)
            (expect 'display-buffer :to-have-been-called-with buf)
            (expect dispatched :not :to-be-truthy))
        (kill-buffer buf)))))

(provide 'test-neo-ui-side-windows)
;;; test-neo-ui-side-windows.el ends here
