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

(provide 'test-neo-ui-side-windows)
;;; test-neo-ui-side-windows.el ends here
