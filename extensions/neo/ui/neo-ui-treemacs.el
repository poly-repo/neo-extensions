(neo/use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (setq treemacs-follow-after-init t
	treemacs-width 30
	treemacs-width-is-initially-locked t ; nil ; we'd like it locked, but treemacs-set-width doesn't seem to have any (sane) effect
	treemacs-lock-width t
	treemacs-project-follow-cleanup t
	treemacs-display-in-side-window t
        treemacs-is-never-other-window t
	treemacs-use-all-the-icons-theme t
        treemacs-sorting 'alphabetic-case-insensitive-asc
        treemacs-persist-file (expand-file-name "treemacs-persist" neo/cache-directory)
        treemacs-last-error-persist-file (expand-file-name "treemacs-last-error-persist" neo/cache-directory))
  (treemacs-git-mode 'deferred)
  ;; at least with my font, I hate it.
  ;; TODO: see if we can make it a finer, continuous and dimmed line
  ;; (treemacs-indent-guide-mode 1)
  (treemacs-git-commit-diff-mode 1)
  (treemacs-follow-mode 1))

(defvar neo/treemacs--resize-timer nil
  "Timer used to debounce Treemacs resize after window/frame changes.")

(defvar neo/treemacs--resize-delay 0.5
  "Idle time in seconds to wait after last frame/window change before resizing Treemacs.")

(defun neo/treemacs--resize-timer-fn (frame)
  "Actual function to resize Treemacs according to `treemacs-width`."
  (setq neo/treemacs--resize-timer nil)
  (message "FN")
  (when (and (featurep 'treemacs)
             (fboundp 'treemacs-current-visibility)
             (eq (treemacs-current-visibility) 'visible)
             (floatp treemacs-width))
    (let* ((fw (frame-width frame))
           (desired (max 20 (floor (* fw treemacs-width))))
           (current-win (treemacs-get-local-window))
           (current-width (when current-win (window-width current-win))))
      (when (and current-win (not (= desired current-width)))
        (if treemacs-display-in-side-windows
            (let ((inhibit-redisplay t))
              (treemacs)
              (setq treemacs-width treemacs-width)
              (treemacs))
          (condition-case _
              (treemacs-set-width desired)
            (error
             (ignore-errors
               (window-resize current-win (- desired current-width) t)))))))))

(defun neo/treemacs-resize-to-fraction (_frame)
  "Debounced wrapper for resizing Treemacs after window/frame size changes."
  (message "HOOK")
  ;; cancel existing timer
  (when (timerp neo/treemacs--resize-timer)
    (cancel-timer neo/treemacs--resize-timer))
  ;; capture _frame in lexical closure
  (let ((f _frame))
    (setq neo/treemacs--resize-timer
          (run-with-idle-timer neo/treemacs--resize-delay nil
                               (lambda ()
                                 (message "FN")
                                 (neo/treemacs--resize-timer-fn f))))))



;; Hook it into frame/window size changes
;(add-hook 'window-size-change-functions #'neo/treemacs-resize-to-fraction)


;; (defun neo/treemacs-resize-to-fraction ()
;;   "Keep Treemacs width equal to `treemacs-width` fraction of the frame."
;;   (message "WINDOWS CHANGE")
;;   (when (and (treemacs-current-visibility)
;;              (floatp treemacs-width))
;;     (let* ((fw (frame-width))
;;            (tw (max 20 (floor (* fw treemacs-width)))))
;;       (treemacs-set-width tw))))

;; (add-hook 'window-size-change-functions
;;           (lambda (&rest _) (neo/treemacs-resize-to-fraction)))

;; (neo/use-package treemacs-nerd-icons
;;   :after treemacs
;;   :config (treemacs-load-theme "nerd-icons"))

(neo/use-package treemacs-all-the-icons)

(neo/use-package treemacs-magit
  :after treemacs magit)

;; (neo/use-package treemacs-persp
;;   :after treemacs
;;   :config (treemacs-set-scope-type 'Perspectives))


;; (neo/use-package project-treemacs
;;   :after treemacs
;;   :config
;;   (project-treemacs-mode))

(provide 'neo-ui-treemacs)
