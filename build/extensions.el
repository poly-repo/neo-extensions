;;; build/extensions.el --- Build neo-extensions with subtree hashes  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x) ;; string-trim, string-empty-p

(setq base-dir "extensions/")
(setq output-file "dist/neo-extensions.el")
(setq digest-file "dist/neo-extensions.sha256")

;; ---------- tiny I/O helpers ----------

(defun neo--read-bytes (file)
  "Return FILE contents as raw bytes (unibyte string)."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun neo--write-forms-one-per-line (forms file)
  "Write FORMS to FILE, one form per line."
  (make-directory (file-name-directory file) t)
  (with-temp-buffer
    (let ((print-escape-nonascii t)  ;; keep output always readable by `read`
          (print-length nil)
          (print-level nil))
      (dolist (form forms)
        (prin1 form (current-buffer))
        (terpri (current-buffer))))
    (write-region (point-min) (point-max) file nil 0)))

(defun neo--file-sha256 (file)
  "Return lowercase hex SHA256 of FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (downcase (secure-hash 'sha256 (current-buffer)))))

(defun neo--write-digest (target source)
  "Write SHA256 of SOURCE into TARGET (single-line hex)."
  (with-temp-buffer
    (insert (neo--file-sha256 source))
    (write-region (point-min) (point-max) target nil 0)))

;; ---------- collect manifests, embed emblem as base64 ----------

(defun neo--load-manifest (file)
  "Read and return one top-level form from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(defun neo--collect-extensions (root)
  "Find manifest.el two levels below ROOT; embed emblem64.png as :emblem-base64."
  (let (acc)
    (dolist (d1 (directory-files root t "^[^.]"))
      (when (file-directory-p d1)
        (dolist (d2 (directory-files d1 t "^[^.]"))
          (when (file-directory-p d2)
            (let* ((mf (expand-file-name "manifest.el" d2))
                   (em (expand-file-name "emblem64.png" d2)))
              (when (file-exists-p mf)
                (let* ((form (neo--load-manifest mf))
                       (form* (if (file-exists-p em)
                                  (append form
                                          (list :emblem-base64
                                                (base64-encode-string (neo--read-bytes em) t)))
                                form)))
                  (push form* acc))))))))
    (nreverse acc)))

;; ---------- git helpers ----------

(defun neo--git-root ()
  "Return repo root via `git rev-parse --show-toplevel`."
  (with-temp-buffer
    (unless (eq 0 (call-process "git" nil t nil "rev-parse" "--show-toplevel"))
      (error "Not in a git repo: %s" (buffer-string)))
    (string-trim (buffer-string))))

(defun neo--git-head ()
  "Return HEAD commit SHA."
  (with-temp-buffer
    (unless (eq 0 (call-process "git" nil t nil "rev-parse" "HEAD"))
      (error "git rev-parse HEAD failed: %s" (buffer-string)))
    (string-trim (buffer-string))))

(defun neo--git-ls-files (subdir)
  "List tracked files (relative to root) under SUBDIR (or repo root if empty)."
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

(defun neo--subtree-sha256 (subdir)
  "Stable SHA256 over tracked files under SUBDIR (sorted; raw bytes + framing)."
  (let* ((root  (neo--git-root))
         (files (sort (neo--git-ls-files subdir) #'string<)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((coding-system-for-read 'no-conversion))
        (dolist (rel files)
          (insert "F " rel "\n")
          (insert-file-contents-literally (expand-file-name rel root))
          (insert "\n")))
      (downcase (secure-hash 'sha256 (current-buffer))))))

;; ---------- previous release (robust parse) ----------

(defun neo--read-all-forms-or-nil (file)
  "Read all top-level forms from FILE. On any error, return nil."
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

(defun neo--old-sha-map (old-file)
  "Return alist (NAME . EXTENSION-SHA256) from previous manifest, or empty."
  (let ((forms (neo--read-all-forms-or-nil old-file))
        acc)
    (dolist (f forms)
      (when (and (consp f) (eq (car f) 'neo/extension))
        (let* ((pl   (cdr f))
               (name (plist-get pl :name))
               (repo (plist-get pl :repository))
               (sha  (when (listp repo) (plist-get repo :extension-sha256))))
          (when (and (stringp name) (stringp sha))
            (push (cons name sha) acc)))))
    acc))

;; ---------- plist helpers ----------

(defun neo--plist-set* (plist key val)
  "Return a fresh PLIST where KEY is set to VAL."
  (let ((copy (copy-sequence plist)))
    (plist-put copy key val)))

(defun neo--plist-set-in (plist key path val)
  "Set KEY to VAL inside nested PLIST at PATH (keyword keys). Fresh structure."
  (if (null path)
      (neo--plist-set* plist key val)
    (let* ((k (car path))
           (rest (cdr path))
           (sub (plist-get plist k))
           (sub* (neo--plist-set-in (if (listp sub) sub '()) key rest val)))
      (neo--plist-set* plist k sub*))))

;; ---------- augmentation ----------

(defun neo--augment-one (form old-map)
  "Add :repository :extension-sha256 and maybe :repo-commit to one neo/extension FORM."
  (unless (and (consp form) (eq (car form) 'neo/extension)))
    (error "Not a neo/extension form: %S" form))
  (let* ((pl    (cdr form))
         (name  (plist-get pl :name))
         (repo  (plist-get pl :repository))
         (path  (and (listp repo) (plist-get repo :path))))
    (if (not (and (stringp name) (stringp path)))
        form
      (let* ((sha-sub (neo--subtree-sha256 path))
             (prev    (cdr (assoc name old-map)))
             (changed (not (string= (or prev "") sha-sub)))
             (pl1 (neo--plist-set-in pl :extension-sha256 '(:repository) sha-sub))
             (pl2 (if changed
                      (neo--plist-set-in pl1 :repo-commit '(:repository) (neo--git-head))
                    pl1)))
        (cons 'neo/extension pl2)))))

(defun neo--augment-all (forms old-file)
  "Augment all neo/extension FORMS using OLD-FILE for comparison."
  (let ((omap (neo--old-sha-map old-file)))
    (mapcar (lambda (f)
              (if (and (consp f) (eq (car f) 'neo/extension))
                  (neo--augment-one f omap)
                f))
            forms)))

;; ---------- main ----------

(let* ((old (or (getenv "OLD_MANIFEST")
                (when (file-exists-p "old.el") "old.el")))
       (forms (neo--collect-extensions base-dir))
       (aug   (neo--augment-all forms old)))
  (neo--write-forms-one-per-line aug output-file)
  (neo--write-digest digest-file output-file))

;;; extensions.el ends here
