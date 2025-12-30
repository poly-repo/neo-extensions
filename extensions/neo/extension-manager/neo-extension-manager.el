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

(defun neo/manager-show ()
  (interactive)
  (let* ((buf (get-buffer-create "*NEO Extension Manager*"))
         (quit-fn
          (neo/with-ui-session
           "NEO Extensions Manager"
           (lambda ()
             (switch-to-buffer buf)
	     (unless (derived-mode-p 'neo/manager-mode)
               (neo/manager-mode)))
           (lambda ()
             (kill-buffer buf)))))
    (with-current-buffer buf
      (setq-local neo/manager-quit-function quit-fn))))


;;; Note, no (provide 'neo-neo-manager) here, extensions are loaded not required.
