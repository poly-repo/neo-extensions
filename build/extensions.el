;;; build/extensions.el --- Build neo-extensions with subtree hashes  -*- lexical-binding: t; -*-

;; Usage in CI:
;;   - (optionally) download previous neo-extensions.el to ./old.el
;;   - export OLD_MANIFEST=old.el (or leave unset)
;;   - emacs --batch -l build/extensions.el

(require 'cl-lib)
(require 'subr-x) ; string-trim, string-empty-p

(setq base-dir "extensions/")
(setq output-file "dist/neo-extensions.el")
(setq digest-file "dist/neo-extensions.sha256")

;;;; ------------------------------
;;;; I/O helpers (unchanged semantics)
;;;; ------------------------------

(defun read-binary-string-raw (file)
  "Return FILE contents as a raw (possibly multibyte) string."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(defun load-manifest (file)
  "Read and return the sole top-level form in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(defun write-forms-one-per-line (forms file)
  "Write FORMS to FILE, one form per line."
  (with-temp-buffer
    (let ((print-escape-nonascii nil)
          (print-length nil)
          (print-level nil))
      (dolist (form forms)
        (prin1 form (current-buffer))
        (terpri (current-buffer))))
    (write-region (point-min) (point-max) file nil 0)))

(defun compute-sha256-of-file (file)
  "Return lowercase SHA256 hex digest of FILE bytes."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (downcase (secure-hash 'sha256 (current-buffer)))))

(defun write-digest-file (target source)
  "Write SHA256 of SOURCE to TARGET (single-line hex)."
  (with-temp-buffer
    (insert (compute-sha256-of-file source))
    (write-region (point-min) (point-max) target nil 0)))

;;;; ------------------------------
;;;; Collect manifests + embed emblem (as you had)
;;;; ------------------------------

(defun collect-extension-forms (root)
  "Find all manifest.el files two levels below ROOT and embed emblem64.png if present."
  (let (results)
    (dolist (d1 (directory-files root t "^[^.]"))
      (when (file-directory-p d1)
        (dolist (d2 (directory-files d1 t "^[^.]"))
          (when (file-directory-p d2)
            (let* ((manifest-file (expand-file-name "manifest.el" d2))
                   (emblem-file   (expand-file-name "emblem64.png" d2)))
              (when (file-exists-p manifest-file)
                (let* ((form (load-manifest manifest-file))
                       (form+emblem
                        (if (file-exists-p emblem-file)
                            (append form (list :emblem (read-binary-string-raw emblem-file)))
                          form)))
                  (push form+emblem results))))))))
    (nreverse results)))

;;;; ------------------------------
;;;; Git helpers
;;;; ------------------------------

(defun neo/build--git-root ()
  "Absolute path to git repo root."
  (with-temp-buffer
    (unless (eq 0 (call-process "git" nil t nil "rev-parse" "--show-toplevel"))
      (error "Not in a git repo: %s" (buffer-string)))
    (string-trim (buffer-string))))

(defun neo/build--git-head ()
  "Return HEAD commit SHA for the repo."
  (with-temp-buffer
    (unless (eq 0 (call-process "git" nil t nil "rev-parse" "HEAD"))
      (error "git rev-parse HEAD failed: %s" (buffer-string)))
    (string-trim (buffer-string))))

(defun neo/build--ls-files (subdir)
  "Return a list of tracked files (relative to repo root) under SUBDIR.
SUBDIR may be \"\" or nil for repo root."
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
        (nreverse files)))))

;;;; ------------------------------
;;;; Stable subtree SHA-256
;;;; ------------------------------

(defun neo/build--subtree-sha256 (subdir)
  "Compute SHA256 over tracked files within SUBDIR at the current checkout.
We hash a deterministic stream per file (sorted lexicographically):
  \"F \" + RELPATH + \"\\n\" + RAW-BYTES + \"\\n\""
  (let* ((root  (neo/build--git-root))
         (files (sort (neo/build--ls-files subdir) #'string<)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((coding-system-for-read 'no-conversion))
        (dolist (rel files)
          (insert "F " rel "\n")
          (insert-file-contents-literally (expand-file-name rel root))
          (insert "\n")))
      (downcase (secure-hash 'sha256 (current-buffer))))))

;;;; ------------------------------
;;;; Previous release parsing
;;;; ------------------------------

(defun neo/build--read-forms (file)
  "Read all top-level forms from FILE; return a list or nil."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (goto-char (point-min))
      (let (forms form)
        (condition-case nil
            (while (setq form (read (current-buffer)))
              (push form forms))
          (end-of-file))
        (nreverse forms)))))

(defun neo/build--old-sha-map (old-file)
  "Return alist (NAME . EXTENSION-SHA256) from OLD-FILE, if available."
  (let ((forms (neo/build--read-forms old-file))
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

;;;; ------------------------------
;;;; plist utilities
;;;; ------------------------------

(defun neo/build--plist-set (plist key val)
  "Return a fresh PLIST where KEY is set to VAL."
  (let ((copy (copy-sequence plist)))
    (plist-put copy key val)))

(defun neo/build--plist-set-in (plist key path val)
  "Set KEY to VAL inside nested PLIST under PATH (list of keyword keys).
Creates missing sub-plists. Returns a fresh top-level plist."
  (if (null path)
      (neo/build--plist-set plist key val)
    (let* ((k (car path))
           (rest (cdr path))
           (sub (plist-get plist k))
           (sub* (neo/build--plist-set-in (if (listp sub) sub '()) key rest val)))
      (neo/build--plist-set plist k sub*))))

;;;; ------------------------------
;;;; Augmentation
;;;; ------------------------------

(defun neo/build-augment-extension (form old-map)
  "Given one (neo/extension ...) FORM, add subtree hash and maybe repo commit."
  (unless (and (consp form) (eq (car form) 'neo/extension)))
    (error "Not a neo/extension form: %S" form))
  (let* ((plist  (cdr form))
         (name   (plist-get plist :name))
         (repo   (plist-get plist :repository))
         (path   (and (listp repo) (plist-get repo :path))))
    (if (not (and (stringp name) (stringp path)))
        form
      (let* ((sha-sub (neo/build--subtree-sha256 path))
             (old-sha (and old-map (cdr (assoc name old-map))))
             (changed (not (string= (or old-sha "") sha-sub)))
             (plist1 (neo/build--plist-set-in plist :extension-sha256
                                              '(:repository) sha-sub))
             (plist2 (if changed
                         (neo/build--plist-set-in plist1 :repo-commit
                                                  '(:repository) (neo/build--git-head))
                       plist1)))
        (cons 'neo/extension plist2)))))

(defun neo/build-augment-all (forms old-file)
  "Augment all (neo/extension ...) FORMS using OLD-FILE (previous release)."
  (let ((old-map (neo/build--old-sha-map old-file)))
    (mapcar (lambda (f)
              (if (and (consp f) (eq (car f) 'neo/extension))
                  (neo/build-augment-extension f old-map)
                f))
            forms)))

;;;; ------------------------------
;;;; Main
;;;; ------------------------------

(let* ((old (or (getenv "OLD_MANIFEST")
                (when (file-exists-p "old.el") "old.el"))))
  (make-directory (file-name-directory output-file) t)
  (let* ((forms (collect-extension-forms base-dir))
         (aug   (neo/build-augment-all forms old)))
    (write-forms-one-per-line aug output-file)
    (write-digest-file digest-file output-file)))
;; done

;;; extensions.el ends here
