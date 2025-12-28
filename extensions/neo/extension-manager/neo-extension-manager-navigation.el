;;; -*- lexical-binding: t -*-

(define-fringe-bitmap 'neo/fringe-bar
  (vector #b11111111)
  1 8 'center)

(defface neo/fringe-card-face
  '((t :foreground "#5fafff"))
  "Fringe face for selected NEO card.")

(defun neo/card-line-count (ov)
  (count-lines (overlay-start ov) (overlay-end ov)))

(defun neo/make-card-fringe (ov)
  (let* ((lines (max 1 (neo/card-line-count ov)))
         (bitmap
          (make-vector lines 'neo/fringe-bar)))
    (propertize
     " "
     'display
     `((left-fringe
        ,bitmap
        neo/fringe-card-face)))))

(defun neo/select-card (ov)
  (when (overlayp neo/current-card)
    ;; Remove old fringe + face
    (overlay-put neo/current-card 'before-string nil)
    (overlay-put neo/current-card 'face nil))

  (setq neo/current-card ov)

  ;; Highlight body
  (overlay-put ov 'face 'neo/card-selected-face)

  ;; Add fringe bar
  (overlay-put ov 'before-string
               (neo/make-card-fringe ov))

  (goto-char (overlay-start ov)))

(defun neo/refresh-current-card-fringe ()
  (when neo/current-card
    (overlay-put neo/current-card 'before-string
                 (neo/make-card-fringe neo/current-card))))

(defvar-local neo/current-card nil)

(defun neo/card-at-point ()
  (cl-find-if (lambda (ov) (overlay-get ov 'neo/card))
              (overlays-at (point))))


(defun neo/next-card ()
  (interactive)
  (let* ((pos (if neo/current-card
                  (overlay-end neo/current-card)
                (point)))
         (next (cl-find-if
                (lambda (ov)
                  (and (overlay-get ov 'neo/card)
                       (> (overlay-start ov) pos)))
                (overlays-in pos (point-max)))))
    (when next
      (neo/select-card next)
      (neo/on-card-changed))))

(defun neo/previous-card ()
  (interactive)
  (let* ((pos (if neo/current-card
                  (overlay-start neo/current-card)
                (point)))
         (prev (car (last
                     (cl-remove-if-not
                      (lambda (ov)
                        (and (overlay-get ov 'neo/card)
                             (< (overlay-end ov) pos)))
                      (overlays-in (point-min) pos))))))
    (when prev
      (neo/select-card prev)
      (neo/on-card-changed))))


(defun neo/open-card ()
  (interactive)
  (when neo/current-card
    (let ((ext (overlay-get neo/current-card 'neo/extension)))
      (neo/show-extension-details ext))))

(defun neo/on-card-changed ()
  "Called whenever the selected card changes."
  ;; For now, do nothing
  ;; Later: update detail window if visible
  ;; (when (neo/detail-window-visible-p)
  ;;   (neo/update-detail-window ext))
  )


(provide 'neo-extension-manager-navigation)
