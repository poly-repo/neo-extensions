;; ;;; build/extensions.el --- Build extension manifest

(setq base-dir "extensions/")

(defvar sha (or (pop command-line-args-left) "unknown"))
(defvar output-file (format "dist/extensions-%s.el" sha))
(defvar digest-file (format "dist/extensions-%s.el.sha256" sha))

(message "Building extensions manifest in %s" output-file)

(defun read-binary-string-safe (file)
  "Return FILE contents as a raw string suitable for embedding in Emacs Lisp."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))  ;; return raw string, no quoting here

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
	    (message "Found dir %s" dir2)
	    (message "Result length: %d" (length results))
            (let* ((manifest-file (expand-file-name "manifest.el" dir2))
                   (emblem-file (expand-file-name "emblem64.png" dir2)))
              (when (file-exists-p manifest-file)
		(message "Manifest found")
                (let* ((form (load-manifest manifest-file))
                       (args (if (and (listp form) (eq (car form) 'neo/extension))
                                 (cdr form)
                               form))
                       (form+emblem
                        (if (file-exists-p emblem-file)
                            (append args (list :emblem (read-binary-string-safe emblem-file)))
                          args)))
                  (push form+emblem results))))))))
    (nreverse results)))

(defun write-extension-file (forms file)
  "Write all FORMS to FILE as a series of `neo/extension` calls."
  (with-temp-buffer
    (let ((print-escape-nonascii nil)
          (print-length nil)
          (print-level nil))
      (dolist (form forms)
        (prin1 `(neo/extension ,@form) (current-buffer))
        (terpri (current-buffer))))
    ;; no encoding munging or newline conversion
    (write-region (point-min) (point-max) file nil 0)))

(defun compute-sha256-of-file (file)
  "Return SHA256 hex digest of FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun write-digest-file (target source)
  "Write DIGEST to TARGET, following the standard format."
  (let ((digest (compute-sha256-of-file source)))
    (with-temp-buffer
      (insert digest)
      (write-region (point-min) (point-max) target nil 0))))

;; Run the generation
(make-directory (file-name-directory output-file) t)
(write-extension-file (collect-extension-forms base-dir) output-file)
(write-digest-file digest-file output-file)

;;; extensions.el ends here
