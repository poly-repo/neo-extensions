;; ;;; build/extensions.el --- Batch build for questionable_defaults

;; (setq input-file "extensions/uno/questionable_defaults/manifest.el")
;; (setq output-file "dist/neo-extensions.el")
;; (setq n 10000)

;; (with-temp-buffer
;;   (dotimes (_ n)
;;     (insert-file-contents input-file)
;;     (goto-char (point-max)))
;;   (write-region (point-min) (point-max) output-file))

;; ;;; extensions.el ends here



;;; build/extensions.el --- Batch build for neo extensions with raw binary emblem

(setq input-file "extensions/uno/questionable_defaults/manifest.el")
(setq emblem-file "extensions/uno/questionable_defaults/emblem64.png")
(setq output-file "dist/neo-extensions.el")
(setq n 10000)

(defun read-binary-file (file)
  "Return the contents of FILE as a raw string (not base64)."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(setq emblem-bytes (read-binary-file emblem-file))

(defun embed-emblem-in-extension (form)
  "Return FORM with :emblem key containing raw PNG bytes."
  (let ((new-form (append form (list :emblem emblem-bytes))))
    new-form))

(with-temp-buffer
  (dotimes (_ n)
    (let ((data (with-temp-buffer
                  (insert-file-contents input-file)
                  (read (current-buffer)))))
      ;; Serialize with raw string printing
      (let ((print-escape-nonascii nil) ; allow raw binary
            (print-length nil)
            (print-level nil))
        (prin1 (embed-emblem-in-extension data) (current-buffer))
        (terpri (current-buffer)))))
  (write-region (point-min) (point-max) output-file))

;;; extensions.el ends here
