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

;; (setq input-file "extensions/uno/questionable_defaults/manifest.el")
;; (setq emblem-file "extensions/uno/questionable_defaults/emblem64.png")
;; (setq output-file "dist/neo-extensions.el")
;; (setq n 1)

;; (defun read-binary-file (file)
;;   "Return the contents of FILE as a raw string."
;;   (with-temp-buffer
;;     (insert-file-contents-literally file)
;;     (buffer-string)))

;; (setq emblem-bytes (read-binary-file emblem-file))

;; (defun embed-emblem-in-extension (form)
;;   "Return FORM with :emblem key containing raw PNG bytes."
;;   (let ((new-form (append form (list :emblem emblem-bytes))))
;;     new-form))

;; (with-temp-buffer
;;   (dotimes (_ n)
;;     (let ((data (with-temp-buffer
;;                   (insert-file-contents input-file)
;;                   (read (current-buffer)))))
;;       ;; Serialize with raw string printing
;;       (let ((print-escape-nonascii nil) ; allow raw binary
;;             (print-length nil)
;;             (print-level nil))
;;         (prin1 (embed-emblem-in-extension data) (current-buffer))
;;         (terpri (current-buffer)))))
;;   (write-region (point-min) (point-max) output-file))

(setq base-dir "extensions/")
(setq output-file "dist/neo-extensions.el")

(defun read-binary-string-safe (file)
  "Return FILE contents as a properly escaped binary string."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (prin1-to-string (buffer-string))))

(defun load-manifest (file)
  "Read and return the top-level form in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(defun collect-extension-forms (base-dir)
  "Find all manifest.el files two levels deep and embed their emblems."
  (let ((results '()))
    (dolist (dir1 (directory-files base-dir t "^[^.]"))
      (when (file-directory-p dir1)
        (dolist (dir2 (directory-files dir1 t "^[^.]"))
          (when (file-directory-p dir2)
            (let* ((manifest-file (expand-file-name "manifest.el" dir2))
                   (emblem-file (expand-file-name "emblem64.png" dir2)))
              (when (file-exists-p manifest-file)
                (let* ((form (load-manifest manifest-file))
                       (form+emblem
                        (if (file-exists-p emblem-file)
                            (append form (list :emblem (read-binary-string-safe emblem-file)))
                          form)))
                  (push form+emblem results))))))))
    (nreverse results)))

(defun write-extension-file (forms file)
  "Write all FORMS to FILE, one per line."
  (with-temp-buffer
    (let ((print-escape-nonascii nil)
          (print-length nil)
          (print-level nil))
      (dolist (form forms)
        (prin1 form (current-buffer))
        (terpri (current-buffer))))
    (write-region (point-min) (point-max) file)))

;; Run the generation
(make-directory (file-name-directory output-file) t)
(write-extension-file (collect-extension-forms base-dir) output-file)


;;; extensions.el ends here
