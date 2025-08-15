;;; build/extensions.el --- Build neo-extensions with subtree hashes  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x) ;; string-trim, when-let*, string-empty-p

(setq base-dir "extensions/")
(setq output-file "dist/neo-extensions.el")
(setq digest-file "dist/neo-extensions.sha256")

;;;; ---------- basic I/O ----------

(defun read-file-bytes (file)
  "Return FILE contents as raw bytes (string)."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(defun write-forms-one-per-line (forms file)
  "Write FORMS to FILE, one per line."
  (make-directory (file-name-directory file) t)
  (with-temp-buffer
    (let ((print-escape-nonascii t)   ;; ensure safe Lisp
          (print-length nil)
          (print-level nil))
      (dolist (form forms)
        (prin1 form (current-buffer))
        (terpri (current-buffer))))
    (write-region (point-min) (point-max) file nil 0)))

(defun compute-sha256-of-file (file)
  "Return lowercase SHA256 hex of FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (downcase (secure-hash 'sha256 (current-buffer)))))

(defun write-digest-file (target source)
  (with-temp-buffer
    (insert (compute-sha256-of-file source))
    (write-region (point-min) (point-max) target nil 0)))

;;;; ---------- collect manifests (embed emblem as base64 for safety) ----------

(defun load-manifest (file)
  "Read one top-level form from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(defun collect-extension-forms (root)
  "Find all manifest.el two levels below ROOT and embed emblem64.png if present.
Embed as :emblem-base64 to keep output parseable in future runs."
  (let (results)
    (dolist (d1 (directory-files root t "^[^.]"))
      (when (file-directory-p d1)
        (dolist (d2 (directory-files d1 t "^[^.]"))
          (when (file-directory-p d2)
            (let* ((manifest (expand-file-name "manifest.el" d2))
                   (emblem   (expand-file-name "emblem64.png" d2)))
              (when (file-exists-p manifest)
                (let* ((form (load-manifest manifest))
                       (form* (if (file-exists-p emblem)
                                  (append form
                                          (list :emblem-base64
                                                (base64-encode-string
                                                 (read-file-bytes emblem) t)))
                                form)))
                  (push form* results))))))))
    (nreverse results)))

;;;; ---------- git helpers ----------

(defun git-root ()
  (with-temp-buffer
    (unless (eq 0 (call-process "git" nil t nil "rev-parse" "--show-toplevel"))
      (error "Not in a git repo: %s" (buffer-string)))
    (string-trim (buffer-string))))

(defun git-head ()
  (with-temp-buffer
    (unless (eq 0 (call-process "git" nil t nil "rev-parse" "HEAD"))
      (error "git rev-parse HEAD failed: %s" (buffer-string)))
    (string-trim (buffer-string))))

(defun git-ls-files (subdir)
  "Return tracked files (relative to repo root) under SUBDIR (or root if empty)."
  (let ((path (or subdir "")))
    (with-temp-buffer
      (let ((rc (if (string-empty-p path)
                    (call-process "git" nil t nil "ls-files" "-z")
                  (call-process "git" nil t nil "ls-files" "-z" "--" path)))))
        (unless (eq 0 rc)
          (error "git ls-files failed for %s: %s" path (buffer-string))))
      (goto-char (point-min))
      (let (files)
        (while (not (eobp))
          (let ((fn (buffer-substring-no-properties
                     (point) (progn (search-forward "\0") (1- (point))))))
            (push fn files)))
        (nreverse files))))

(defun subtree-sha256 (subdir)
  "Stable SHA256 over tracked files under SUBDIR at current checkout."
  (let* ((root (git-root))
         (files (sort (git-ls-files subdir) #'string<)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((coding-system-for-read 'no-conversion))
        (dolist (rel files)
          (insert "F " rel "\n")
          (insert-file-contents-literally (expand-file-name rel root))
          (insert "\n")))
      (downcase (secure-hash 'sha256 (current-buffer))))))

;;;; ---------- previous release parsing (robust) ----------

(defun read-all-forms-or-nil (file)
  "Try to read all top-level forms from FILE. On error, return nil (don’t crash)."
  (when (and file (file-exists-p file))
    (condition-case _
        (with-temp-buffer
          (insert-file-contents-literally file)
          (goto-char (point-min))
          (let (forms form)
            (while (setq form (read (current-buffer)))
              (push form forms))
            (nreverse forms)))
      (error nil))))

(defun old-sha-map (old-file)
  "Return alist (NAME . EXTENSION-SHA256) from previous manifest, or empty."
  (let ((forms (read-all-forms-or-nil old-file))
        out)
    (dolist (f forms)
      (when (and (consp f) (eq (car f) 'neo/extension))
        (let* ((plist (cdr f))
               (name  (plist-get plist :name))
               (repo  (plist-get plist :repository))
               (sha   (when (listp repo) (plist-get repo :extension-sha256))))
          (when (and (stringp name) (stringp sha))
            (push (cons name sha) out)))))
    out))

;;;; ---------- plist helpers ----------

(defun plist-set* (plist key val)
  (let ((copy (copy-sequence plist)))
    (plist-put copy key val)))

(defun plist-set-in (plist key path val)
  "Set KEY to VAL inside nested PLIST at PATH (list of keyword keys)."
  (if (null path)
      (plist-set* plist key val)
    (let* ((k (car path))
           (rest (cdr path))
           (sub (plist-get plist k))
           (sub* (plist-set-in (if (listp sub) sub '()) key rest val)))
      (plist-set* plist k sub*))))

;;;; ---------- augmentation ----------

(defun augment-extension (form old-map)
  "Add :repository :extension-sha256 and maybe :repo-commit to one neo/extension FORM."
  (unless (and (consp form) (eq (car form) 'neo/extension)))
    (error "Not a neo/extension form: %S" form))
  (let* ((plist  (cdr form))
         (name   (plist-get plist :name))
         (repo   (plist-get plist :repository))
         (path   (and (listp repo) (plist-get repo :path))))
    (if (not (and (stringp name) (stringp path)))
        form
      (let* ((sha-sub (subtree-sha256 path))
             (prev    (cdr (assoc name old-map)))
             (changed (not (string= (or prev "") sha-sub)))
             (p1 (plist-set-in plist :extension-sha256 '(:repository) sha-sub))
             (p2 (if changed
                     (plist-set-in p1 :repo-commit '(:repository) (git-head))
                   p1)))
        (cons 'neo/extension p2)))))

(defun augment-all (forms old-file)
  (let ((omap (old-sha-map old-file)))
    (mapcar (lambda (f)
              (if (and (consp f) (eq (car f) 'neo/extension))
                  (augment-extension f omap)
                f))
            forms)))

;;;; ---------- main ----------

(let* ((old (or (getenv "OLD_MANIFEST")
                (when (file-exists-p "old.el") "old.el"))))
  (let* ((forms (collect-extension-forms base-dir))
         (aug   (augment-all forms old)))
    (write-forms-one-per-line aug output-file)
    (write-digest-file digest-file output-file)))

;;; extensions.el ends here
