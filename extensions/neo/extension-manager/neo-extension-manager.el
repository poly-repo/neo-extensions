;;; -*- lexical-binding: t -*-

;;; This is neo-manager, a NEO extension
;;;
;;; Extension Manager

(require 'neo-utils)

(defvar-local neo/manager-quit-function nil)

;; TODO DNL
(add-to-list 'load-path "/home/mav/.local/share/wtrees/mav-83-mvp-extension-management/devex/editors/emacs/extensions/extensions/neo/neo-manager")
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


;;; Note, no (provide 'neo-neo-manager) here, extensions are loaded not required.
