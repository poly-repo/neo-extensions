;;; Eshell-in-its-own-frame: guaranteed char-size, centered on calling frame,
;;; and reliably delete the frame when Eshell exits.

(defun neo/eshell--bury-and-hide-frame ()
  "Hide the current Eshell buffer and delete its associated frame if present.
Does not prompt in the minibuffer. The buffer remains alive for later use."
  (when (derived-mode-p 'eshell-mode)
    (let ((buf (current-buffer)))
      ;; Delete frame if present
      (when (and (local-variable-p 'my/eshell-created-frame)
                 (frame-live-p my/eshell-created-frame))
        (let ((f my/eshell-created-frame))
          (kill-local-variable 'my/eshell-created-frame)
          (when (> (length (frame-list)) 1)
            (ignore-errors (delete-frame f)))))
      ;; Bury buffer non-interactively
      (when (buffer-live-p buf)
        (let ((next-buf (or (and (buffer-live-p (next-buffer)) (next-buffer))
                            (other-buffer buf t))))
          (when next-buf
            (switch-to-buffer next-buf)))))))

(defun my/eshell--delete-associated-frame-for-buffer (buf)
  "Delete the frame stored in BUF's `my/eshell-created-frame' local var, if live."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and (local-variable-p 'my/eshell-created-frame)
                 (frame-live-p my/eshell-created-frame))
        (let ((f my/eshell-created-frame))
          (kill-local-variable 'my/eshell-created-frame)
          (when (frame-live-p f)
            (delete-frame f)))))))

(defun my/eshell--cleanup-after-exit-or-bury ()
  "Scan buffers for eshell buffers that requested deletion and are no longer displayed.
If an eshell buffer has `my/eshell-exited` set and is not displayed anywhere,
delete its associated frame and kill the buffer."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'eshell-mode)
                 (local-variable-p 'my/eshell-exited)
                 my/eshell-exited
                 ;; not displayed in any frame
                 (not (get-buffer-window buf t)))
        ;; Delete the frame (if any) and kill the buffer to fully clean-up.
        (my/eshell--delete-associated-frame-for-buffer buf)
        (when (buffer-live-p buf)
          (with-current-buffer buf
            ;; remove the exit flag to avoid repeat work
            (kill-local-variable 'my/eshell-exited)))
        ;; If buffer still exists, kill it.
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

;; Ensure the cleanup hook is installed once.
(unless (memq #'my/eshell--cleanup-after-exit-or-bury buffer-list-update-hook)
  (add-hook 'buffer-list-update-hook #'my/eshell--cleanup-after-exit-or-bury))

;;; The display function: create a new frame sized in chars and centered on current frame.
(defun my/display-eshell-in-new-frame (buffer alist)
  "Display BUFFER in a new frame sized in columns/rows and centered on the calling frame.
ALIST is ignored (compatible with `display-buffer` API). Returns the window used."
  (let* ((cols 100) (rows 30)                ;; desired char geometry for eshell frame
         ;; compute desired pixel size for the new frame using character cell size
         (char-w (frame-char-width))        ;; width of a character in pixels (on current frame)
         (char-h (frame-char-height))       ;; height of a character in pixels
         (desired-px-width (* cols char-w))
         (desired-px-height (* rows char-h))
         ;; compute center relative to the frame from which we were called
         (src-frame (selected-frame))
         (src-pos (frame-position src-frame)) ; (left . top) in pixels
         (src-left (car src-pos))
         (src-top (cdr src-pos))
         (src-px-w (frame-pixel-width src-frame))
         (src-px-h (frame-pixel-height src-frame))
         (left (max 0 (round (+ src-left (/ (- src-px-w desired-px-width) 2.0)))))
         (top  (max 0 (round (+ src-top  (/ (- src-px-h desired-px-height) 2.0)))))
         ;; frame params: do not supply width/height here (we'll set via set-frame-size),
         ;; but provide user-position/user-size so WMs are more likely to respect our set-frame-position/size.
         (params `((name . "eshell")
                   (minibuffer . nil)
                   (unsplittable . t)
                   (user-position . t)
                   (user-size . t)))
         (frame (make-frame params))
         (win (frame-selected-window frame)))
    ;; Set size in columns/rows (character units). PIXELWISE = nil -> columns/rows units.
    (set-frame-size frame cols rows nil)
    ;; Set pixel position to computed left/top (centered on src-frame).
    (set-frame-position frame left top)
    ;; Put the buffer into the frame's selected window and focus it.
    (set-window-buffer win buffer)
    (select-frame-set-input-focus frame)
    ;; remember the frame on the buffer (buffer-local) and set up kill-hook for direct kills.
    (with-current-buffer buffer
      (setq-local my/eshell-created-frame frame)
      (add-hook 'kill-buffer-hook
                (lambda ()
                  ;; ensure frame removed if buffer is explicitly killed.
                  (my/eshell--delete-associated-frame-for-buffer (current-buffer)))
                nil t))
    win))

;;; Install into display-buffer-alist for *eshell* (exact match).
(setq display-buffer-alist
      '(("^\\*eshell\\*$"
         (my/display-eshell-in-new-frame)
         ((inhibit-same-window . t)
          (reusable-frames . nil))))) ; we always create a fresh frame

;;; Ensure eshell/exit (the usual "exit" command) marks the buffer as exited.
;;; We set a buffer-local flag `my/eshell-exited` — the global buffer-list-update hook
;;; will delete the frame and kill the buffer when the buffer is no longer displayed.
(when (fboundp 'eshell/exit)
  (advice-add 'eshell/exit :after
              (lambda (&rest _args)
                (when (derived-mode-p 'eshell-mode)
                  ;; prefer to leave original behavior (which may bury); just mark for deletion.
                  (setq-local my/eshell-exited t)))))

;; On some configs the eshell command name may be `eshell:exit`; advise that too if present.
(when (fboundp 'eshell:exit)
  (advice-add 'eshell:exit :after
              (lambda (&rest _args)
                (when (derived-mode-p 'eshell-mode)
                  (setq-local my/eshell-exited t)))))


(advice-add 'bury-buffer :around
            (lambda (orig-fn &rest args)
              "If called in a dedicated Eshell buffer, delete frame instead of normal bury."
              (if (and (derived-mode-p 'eshell-mode)
                       (local-variable-p 'my/eshell-created-frame))
                  (neo/eshell--bury-and-hide-frame)
                (apply orig-fn args))))

(advice-add 'quit-window :around
            (lambda (orig-fn &rest args)
              "If called in a dedicated Eshell buffer, delete frame instead of normal quit."
              (if (and (derived-mode-p 'eshell-mode)
                       (local-variable-p 'my/eshell-created-frame))
                  (neo/eshell--bury-and-hide-frame)
                (apply orig-fn args))))
