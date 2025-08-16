;; ;;; build/extensions.el --- Batch build for questionable_defaults

(require 'cl-lib)
(require 'subr-x)

(setq base-dir "extensions/")
(setq output-file "dist/neo-extensions.el")
(setq digest-file "dist/neo-extensions.sha256")

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
                  (call-process "git" nil t nil "ls-files" "-z" "--" path))))
        (unless (eq 0 rc)
          (error "git ls-files failed for %s: %s" path (buffer-string))))
      (goto-char (point-min))
      (let (files)
        (while (not (eobp))
          (let ((fn (buffer-substring-no-properties
                     (point) (progn (search-forward "\0") (1- (point))))))
            (push fn files)))
        (nreverse files)))))

(defcustom neo/hash-skip-dirs
  '(".git" ".svn" ".hg" "node_modules" "dist" "build" ".direnv" ".venv")
  "Directory names to skip when hashing a subtree.
Names are matched on the last path component."
  :type '(repeat string))

(defun neo--path-absolute (path)
  "Return absolute PATH (relative to `default-directory` if PATH is relative).
If PATH is nil or \"\", return `default-directory`."
  (if (and path (not (string-empty-p path)))
      (expand-file-name path default-directory)
    (expand-file-name default-directory)))

(defun neo--regular-file-p (f)
  "Return non-nil if F is a regular file (follows symlinks to files)."
  (and (file-exists-p f) (not (file-directory-p f))))

(defun neo--skip-dir-p (dir)
  "Return non-nil if DIR (absolute) should be skipped per `neo/hash-skip-dirs`."
  (member (file-name-nondirectory (directory-file-name dir)) neo/hash-skip-dirs))

(defun neo--files-under (root)
  "Return a list of absolute file paths under ROOT (or the file itself).
Does not follow symlinked directories (avoids cycles)."
  (setq root (file-name-as-directory (neo--path-absolute root)))
  (cond
   ((neo--regular-file-p root) (list (directory-file-name root)))
   ((file-directory-p root)
    (let (acc)
      (cl-labels
          ((walk (dir)
             (unless (neo--skip-dir-p dir)
               (dolist (name (directory-files dir t "^[^.].*")) ;; skip . and ..
                 (cond
                  ((file-directory-p name)
                   (unless (file-symlink-p name) (walk (file-name-as-directory name))))
                  ((neo--regular-file-p name) (push name acc)))))))
        (walk root))
      (nreverse acc)))
   (t (error "Path does not exist or is not a file/dir: %s" root))))

(defun neo--subtree-sha256 (subdir)
  "Stable SHA256 over all files under SUBDIR (relative to CWD).
Hash is computed over a deterministic stream:
  for each file (sorted by relative path):
    \"F \" RELPATH \"\\n\" BYTES \"\\n\""
  (let* ((base   (file-name-as-directory (neo--path-absolute subdir)))
         (files  (sort (mapcar (lambda (abs) (file-relative-name abs base))
                               (neo--files-under subdir))
                       #'string<)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((coding-system-for-read 'no-conversion))
        (dolist (rel files)
          (insert "F " rel "\n")
          (insert-file-contents-literally (expand-file-name rel base))
          (insert "\n")))
      (downcase (secure-hash 'sha256 (current-buffer))))))

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

;; (defun read-all-forms (file)
;;   "Read all top-level forms from FILE. On any error, return nil."
;;   (when (and file (file-exists-p file))
;;     (condition-case _
;;         (with-temp-buffer
;;           (insert-file-contents-literally file)
;;           (goto-char (point-min))
;;           (let (forms form)
;;             (while (setq form (read (current-buffer)))
;; 	      (dump form)
;;               (push form forms))
;;             (nreverse forms)))
;;       (error nil))))


(defun read-all-forms (file)
  "Read all top-level forms from FILE.
On EOF, return the collected forms (in order).
On any other read error, return nil.
If FILE is missing or nil, return nil."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (goto-char (point-min))
      (let (forms done)
        (while (not done)
          (condition-case err
              (push (read (current-buffer)) forms)
            (end-of-file
             (setq done t))
            (error
             ;; Any other reader error: bail out and return nil.
             (setq forms nil
                   done  t))))
        (and forms (nreverse forms))))))

(defun ext-key (plist)
  "Return a stable composite key (PUBLISHER . NAME) from PLIST or nil."
  (let ((pub  (plist-get plist :publisher))
        (name (plist-get plist :name)))
    (when (and (stringp pub) (stringp name))
      (cons pub name))))

;; (defun read-all-forms-or-nil (file)
;;   "Read all top-level forms from FILE. On any error, return nil."
;;   (when (and file (file-exists-p file))
;;     (condition-case _
;;         (with-temp-buffer
;;           (insert-file-contents-literally file)
;;           (goto-char (point-min))
;;           (let (forms form)
;;             (while (setq form (read (current-buffer)))
;; ;	      (dump form)
;;               (push form forms))
;; 	    (dump forms)
;;             (nreverse forms)))
;;       (error nil))))


;; (defun old-sha-map (old-file)
;;   "Return alist keyed by (PUBLISHER . NAME) -> EXTENSION-SHA256 from OLD-FILE.
;; If OLD-FILE is missing or unreadable, return an empty alist.

;; Backward compatibility: if a previous manifest didn’t include :publisher,
;; also add a secondary entry keyed only by NAME so lookups can still succeed."
;;   (let ((forms (read-all-forms old-file))
;;         acc)
;;     (message ".. %s" old-file)
;;     (dump forms)
;;     (message "..")
;;     (dolist (f forms)
;;       (when (and (consp f) (eq (car f) 'neo/extension))
;;         (let* ((pl   (cdr f))
;;                (key  (ext-key pl))
;;                (name (plist-get pl :name))
;;                (sha  (plist-get pl :extension-sha256)))
;;           (when (and (stringp sha))
;;             (when key
;;               (push (cons key sha) acc))
;;             ;; fallback for older releases lacking :publisher
;;             (when (stringp name)
;;               (push (cons name sha) acc))))))
;;     acc))

;; Only return composite keys; no name-only fallback.
(defun old-sha-map (old-file)
  "Return alist keyed by (PUBLISHER . NAME) -> EXTENSION-SHA256 from OLD-FILE.
If OLD-FILE is missing or unreadable, return an empty alist."
  (let ((forms (read-all-forms old-file))
        acc)
    (dolist (f forms)
      (when (and (consp f) (eq (car f) 'neo/extension))
        (let* ((pl   (cdr f))
               (key  (ext-key pl))
               (sha  (plist-get pl :extension-sha256)))
	  (message "key: %s sha: %s[%s]" key sha (stringp sha))
          (when (and key (stringp sha))
            (push (cons key sha) acc)))))
    (dump acc)
    (nreverse acc)))


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

(defun augment-extension-forms (forms old)
  (dolist (form forms)
    (let* ((pl (cdr form))
	   (repo (plist-get pl :repository))
	   (path (plist-get repo :path))
	   (old-sha256 (plist-get pl :extension-sha256))
	   (sha256 (neo--subtree-sha256 path)))
      (if (not (string= old-sha256 sha256))
	  (let* ((pl1 (neo--plist-set-in pl :extension-sha256 nil sha256))
		 (commit-sha (neo--git-head))
		 (pl2 (neo--plist-set-in pl1 :commit-sha nil commit-sha)))
	    (setcdr form pl2)))
;      (dump pl1)
      (message ">>>> %s [%s, old %s]" path sha256 old-sha256)))
  forms)


(defun write-extension-file (forms file)
  "Write all FORMS to FILE, one per line as escaped raw strings."
  (with-temp-buffer
    (let ((print-escape-nonascii nil)
          (print-length nil)
          (print-level nil))
      (dolist (form forms)
        (prin1 form (current-buffer))
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

(defun dump (obj)
  (let ((print-length nil) (print-level nil) (print-escape-newlines t))
    (message "%s" (pp-to-string obj))))
  

;; Run the generation
(message "DOING")
(let ((old (getenv "OLD_MANIFEST")))
  (dump (old-sha-map old))
  (message "----")
  (make-directory (file-name-directory output-file) t)
  (write-extension-file (augment-extension-forms (collect-extension-forms base-dir) old) output-file)
  (write-digest-file digest-file output-file))

;;; extensions.el ends here
