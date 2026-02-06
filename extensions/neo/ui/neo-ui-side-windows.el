;;; -*- lexical-binding: t -*-

(require 'neo-window)
(require 'cl-lib)

(defvar neo/side-window-stack nil
  "Stack (LIFO) of side windows in order of creation.")

(defun neo/track-side-windows (frame)
  "Track side windows on FRAME after window state changes."
  (dolist (w (window-list frame 'no-mini))
    (when (and (window-live-p w)
               (window-parameter w 'window-side)
               (not (memq w neo/side-window-stack)))
      (push w neo/side-window-stack))))

;(remove-hook 'window-buffer-change-functions #'neo/track-side-window)
(add-hook 'window-state-change-functions #'neo/track-side-windows)

(defun neo/delete-last-side-window ()
  "Delete the most recently opened live side window."
  (interactive)
  (while (and neo/side-window-stack
              (not (window-live-p (car neo/side-window-stack))))
    (pop neo/side-window-stack))
  (if-let ((window (pop neo/side-window-stack)))
      (delete-window window)
    (message "No side windows to close")))

;; temp only, will probavbly be the qq chord
(global-set-key (kbd "C-c w d") #'neo/delete-last-side-window)
(with-eval-after-load 'key-chord
  (key-chord-define-global "qq" 'neo/delete-last-side-window))


(setq display-buffer-alist '())

;; TODO add a keyword :create, a lambda, to be used when there's no
;; buffer available for the side window on the requested side. This
;; can be used for treemacs on the left, a terminal at the bottom,
;; maybe a summary dashboard on the top maybe emacs info on the right
(neo/side-window :regex "^\\*gemini.*\\*$" :side 'right :size 80) ; move to ai-buddy
(neo/side-window :mode 'help-mode :include-derived t :side 'right :size 80)
(neo/side-window :mode 'treemacs-mode :side 'left :size 30) ; move to project

(defvar neo/side-actions nil
  "Alist mapping symbols to functions.")

(defun neo/register-side-window-default (side func)
  "Register FUNC for a specific SIDE ('left, 'right, 'up, or 'down)."
  (setf (alist-get side neo/side-actions) func))

(defun neo/dispatch-side (side)
  "Execute the function associated with SIDE."
  (let ((action (alist-get side neo/side-actions)))
    (if (functionp action)
        (funcall action)
      (message "NEO: No action registered for and no buffers targeting %s side" side))))

(defun neo/toggle-side-window (side)
  "Toggle the visibility of the side window at SIDE."
  (interactive)
  (if-let ((window (neo/get-side-window side)))
      (progn
        (message "Hiding side window: %s" side)
        (delete-window window))
    (progn
      (message "Showing side window: %s" side)
      (if-let ((buffers (neo/get-side-window-buffers side)))
          (display-buffer (car buffers))
	(neo/dispatch-side side)))))
					;  (message "No buffers targeting %s side" side)))))

(neo/register-side-window-default 'left #'treemacs)

(defun neo/get-side-window (side)
  "Return the live window at SIDE in the current frame, or nil."
  (cl-find-if (lambda (w) (eq (window-parameter w 'window-side) side))
              (window-list)))

(defun neo/bury-side-window-buffer (side)
  "Bury the current buffer in the side window at SIDE."
  (interactive)
  (when-let ((window (neo/get-side-window side)))
    (with-selected-window window
      (bury-buffer))))

(defun neo/kill-side-window-buffer (side)
  "Kill the current buffer in the side window at SIDE."
  (interactive)
  (when-let ((window (neo/get-side-window side)))
    (with-selected-window window
      (kill-buffer (current-buffer)))))

(defun neo/bind-key-variants (map key command)
  "Bind KEY and M-KEY to COMMAND in MAP."
  (define-key map (kbd key) command)
  (define-key map (kbd (format "M-%s" key)) command))

(dolist (spec '((left "<left>") (right "<right>") (up "<up>") (down "<down>")))
  (let* ((side (car spec))
         (key (cadr spec))
         (map-sym (intern (format "neo/side-window-%s-map" side)))
         (map (make-sparse-keymap)))
    (fset map-sym map)
    (global-set-key (kbd (format "S-%s" key)) map-sym)
    
    (neo/bind-key-variants map key
                           (lambda () (interactive) (neo/toggle-side-window side)))
    (neo/bind-key-variants map "b"
                           (lambda () (interactive) (neo/bury-side-window-buffer side)))
    (neo/bind-key-variants map "k"
                           (lambda () (interactive) (neo/kill-side-window-buffer side)))))

(provide 'neo-ui-side-windows)
