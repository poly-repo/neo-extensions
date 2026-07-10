;;; -*- lexical-binding: t -*-

;;; This is neo-manager, a NEO extension
;;;
;;; Extension Manager

(require 'neo-utils)

(defvar-local neo/manager-quit-function nil)

(require 'neo-extension-manager-render)
;(require 'neo-extension-manager-navigation)

(defun neo/manager-mode-setup ()
  (neo/extensions-render)
  (setq-local buffer-read-only t))

(define-derived-mode neo/manager-mode special-mode "NEO Extension Manager"
  "Rich UI listing available NEO extensions."
  (setq-local cursor-type nil)
  (setq-local truncate-lines t)
  (setq-local buffer-read-only t)
  (setq-local highlight-nonselected-windows nil)
  (setq-local transient-mark-mode nil)
  (neo/update-divider-face)
  (neo/extensions-refresh-all)
  ;; maybe we should define keys in a hook, like so:
  ;; (defun neo/manager-mode-setup-keys ()
  ;;   (define-key neo/manager-mode-map (kbd "n") #'neo/manager-next-card)
  ;;   (define-key neo/manager-mode-map (kbd "p") #'neo/manager-prev-card)
  ;;   (define-key neo/manager-mode-map (kbd "RET") #'neo/manager-open-card)
  ;;   (define-key neo/manager-mode-map (kbd "q") #'neo/manager-quit))
  ;; (add-hook 'neo/manager-mode-hook #'neo/manager-mode-setup-keys)
  ;; (define-key neo/manager-mode-map (kbd "n") #'neo/next-card)
  ;; (define-key neo/manager-mode-map (kbd "p") #'neo/previous-card)
  ;; (define-key neo/manager-mode-map (kbd "<down>") #'neo/next-card)
  ;; (define-key neo/manager-mode-map (kbd "<up>") #'neo/previous-card)
  (define-key neo/manager-mode-map (kbd "q")
              (lambda ()
                (interactive)
                (funcall neo/manager-quit-function))))


;; (add-hook 'neo/manager-mode-hook
;;           (lambda ()
;;             (neo/extensions-render))
;;           nil t)

(defun neo/manager-render ()
  "Render the NEO Extension Manager buffer into the selected window.

Bare entry point with no perspective management of its own, so it can
be used directly as a Neo application's `:setup' without nesting a
second perspective inside the one `neo/application' already switched
to. Standalone use goes through `neo/manager-show' instead, which
wraps this in `neo/with-ui-session'."
  (let ((buf (get-buffer-create "*NEO Extension Manager*")))
    (switch-to-buffer buf)
    (unless (derived-mode-p 'neo/manager-mode)
      (neo/manager-mode))
    buf))

(defun neo/manager-show ()
  (interactive)
  (let* ((buf (get-buffer-create "*NEO Extension Manager*"))
         (quit-fn
          (neo/with-ui-session
           "NEO Extensions Manager"
           (lambda () (neo/manager-render))
           (lambda ()
             (kill-buffer buf)))))
    (with-current-buffer buf
      (setq-local neo/manager-quit-function quit-fn))))

(neo/application "Neo Extensions"
  :setup (neo/manager-render)
  :teardown (when-let* ((buf (get-buffer "*NEO Extension Manager*")))
              (kill-buffer buf))
  ;; No :quit-keys override here (unlike the old `neo/manager-show'-based
  ;; setup): `neo/manager-render' does no perspective switch of its own, so
  ;; `neo/application''s single switch is the only one in play, and its
  ;; default "q" -> `neo/leave-current-application' correctly restores it.
  ;; That binding takes precedence over `neo/manager-mode-map's own "q"
  ;; (via `minor-mode-overriding-map-alist'), which is only ever exercised
  ;; by the standalone `neo/manager-show' path.
  :bind "n")

(defun neo/app-neo-extensions--dispatch ()
  "Open the Extension Manager, tolerating a missing `perspective' package.

`neo/app-neo-extensions' (defined above by the `neo/application' macro)
unconditionally `require's `perspective', which is only installed once
`neo:projects' has been enabled at least once. Before that -- e.g. a user
who abandoned the \"Start configuration\" flow after disabling everything
but a couple of minimal extensions -- fall back to the same bare,
perspective-free `neo/manager-render' entry point that
`neo/manager--maybe-launch-on-startup' already uses, so `M-a n' always
works regardless of what else is enabled."
  (interactive)
  (if (require 'perspective nil t)
      (neo/app-neo-extensions)
    (neo/manager-render)))

(define-key neo/applications-map (kbd "n") #'neo/app-neo-extensions--dispatch)
(when-let* ((app (gethash "Neo Extensions" neo--applications)))
  (setf (neo/application-command app) #'neo/app-neo-extensions--dispatch))

(defun neo/manager--maybe-launch-on-startup ()
  "Open the Extension Manager once, right after `neo/start-configuration'.

Consumes the one-shot `\"launch-extensions-manager-on-startup\"' config flag
set by `neo/start-configuration' (core/neo-early-init-utils.el): clears it
immediately so this only fires for the restart that follows clicking `Start
configuration', not every subsequent boot.

Uses `neo/manager-render' directly rather than the `neo/application'-wrapped
`neo/app-neo-extensions': the latter unconditionally `require's `perspective',
which is only installed as part of `neo:projects' -- a package this minimal
boot (nothing but `neo:extension-manager' enabled) never pulls in. The bare
render path needs no perspective, so it works regardless of what else is
enabled.

Also pre-selects `neo:dashboard' via `neo/manager--install-extension', so its
card shows as already installed and an abandoned session (user quits without
touching anything) is still self-recovering on the *next* restart. This runs
on `neo/after-framework-bootstrap-hook', i.e. after `neo/bootstrap' has
already finished for the current boot, so persisting the choice here does not
hot-load `neo:dashboard' (or the `neo:projects'/`perspective' it requires)
into this session -- it takes effect starting the boot after this one, so
there is no race with the manager staying on screen right now."
  (when (equal (neo/get-config "launch-extensions-manager-on-startup") "t")
    (neo/set-config "launch-extensions-manager-on-startup" "nil")
    (neo/manager--install-extension "neo:dashboard")
    (let ((buf (neo/manager-render)))
      (with-current-buffer buf
        (setq-local neo/manager-quit-function (lambda () (interactive) (bury-buffer)))))))

(add-hook 'neo/after-framework-bootstrap-hook #'neo/manager--maybe-launch-on-startup)

;;; Note, no (provide 'neo-neo-manager) here, extensions are loaded not required.
