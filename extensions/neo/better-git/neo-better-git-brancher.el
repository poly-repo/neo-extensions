(defvar neo/magit-branch-delete-checkout-policy 'dirty
  "Policy for considering a branch checked out in worktrees when deciding delete-safety.
Possible values:
 - 'any    : any checkout (even if clean) makes branch unsafe.
 - 'dirty  : only dirty worktrees make branch unsafe (clean checkouts allowed).
 - 'ignore : ignore worktrees entirely (not recommended).")

;; TODO: should we have a stronger version that checks with upstream without needing to fetch?
(defvar neo/magit-branch-delete-compare 'upstream
  "Which \"main\" to compare branch against when deciding whether its commits are merged.
Possible values:
 - 'local          : compare against local 'main' branch
 - 'upstream       : compare against remote-tracking 'origin/main' (if present)
 - 'fresh-upstream : tries to fetch from upstream first, falls back to 'main'")

(defun neo/magit--parse-worktree-porcelain-ht ()
  "Parse `git worktree list --porcelain' into a hash-table mapping branch -> list of paths.
Branch names are trimmed and paths are expanded absolute paths."
  (let* ((lines (magit-git-lines "worktree" "list" "--porcelain"))
         (ht (make-hash-table :test #'equal))
         (current-path nil))
    (dolist (ln lines)
      (cond
       ((string-prefix-p "worktree " ln)
        (setq current-path (expand-file-name (string-trim (substring ln (length "worktree "))))))
       ((string-prefix-p "branch " ln)
        (when current-path
          (let* ((ref (string-trim (substring ln (length "branch "))))
                 (branch (if (string-prefix-p "refs/heads/" ref)
                             (substring ref (length "refs/heads/"))
                           ref))
                 (branch (string-trim branch))
                 (existing (gethash branch ht)))
            (puthash branch (cons current-path existing) ht))))))
    ;; reverse lists for stable ordering
    (maphash (lambda (k v) (puthash k (nreverse v) ht)) ht)
    ht))

(defun neo/magit--worktree-map->alist (ht)
  "Return an alist copy of hash-table HT (branch . (paths...))."
  (let (alist)
    (maphash (lambda (k v) (push (cons k v) alist)) ht)
    (nreverse alist)))

(defun neo/magit--has-commits-not-in-main-p (branch &optional main)
  "Return t if BRANCH has commits not in MAIN (default \"main\")."
  (let ((main (or main "main")))
    (not (null (magit-git-lines "log" branch "--not" main "--oneline")))))

(defun neo/magit--worktree-clean-p (path)
  "Return t if `git -C PATH status --porcelain` is empty."
  (null (magit-git-lines "-C" path "status" "--porcelain")))

(defun neo/magit-branch-safety-info-ht (branch &optional main)
  "Return a plist with robust safety info for BRANCH using a hash-table for worktrees.
Plist keys: :safe, :reasons, :worktrees-ht, :worktrees-list."
  (let* ((branch (string-trim branch))
         (main (or main "main"))
         (ht (neo/magit--parse-worktree-porcelain-ht))
         (worktrees (gethash branch ht))
         (reasons '()))
    (when (neo/magit--has-commits-not-in-main-p branch main)
      (push (format "Branch has commits not in %s (git log ... returned entries)" main) reasons))
    (when worktrees
      (push (format "Branch is checked out in worktree(s): %s" (string-join worktrees ", ")) reasons)
      (when (seq-some (lambda (p) (not (neo/magit--worktree-clean-p p))) worktrees)
        (push "At least one checking-out worktree is dirty (status --porcelain non-empty)" reasons)))
    (list :safe (null reasons)
          :reasons (nreverse reasons)
          :worktrees-ht ht
          :worktrees-list worktrees)))

(defun neo/magit-print-branch-diagnostics-ht (branch &optional main)
  "Print diagnostics for BRANCH using the hash-table based parser."
  (interactive (list (completing-read "Branch: " (magit-list-local-branch-names))))
  (let* ((branch (string-trim branch))
         (main (or main "main"))
         (info (neo/magit-branch-safety-info-ht branch main))
         (ht (plist-get info :worktrees-ht))
         (worktrees (plist-get info :worktrees-list))
         (buf (get-buffer-create "*magit-branch-diagnostics*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Diagnostic for branch: %s (main: %s)\n\n" branch main))
      (insert "== parsed worktree map (from hash-table) ==\n\n")
      (dolist (pair (neo/magit--worktree-map->alist ht))
        (insert (format "%s -> %s\n" (car pair) (string-join (cdr pair) ", "))))
      (insert "\n== commits not in main (git log ...) ==\n")
      (insert (string-join (or (magit-git-lines "log" branch "--not" main "--oneline") '("<<none>>")) "\n"))
      (insert "\n\n== git diff branch...main ==\n")
      (insert (string-join (or (magit-git-lines "diff" (format "%s...%s" branch main)) '("<<none>>")) "\n"))
      (insert "\n\n== status --porcelain for each worktree that checks out branch ==\n")
      (if (null worktrees)
          (insert "<<not checked out in any worktree>>\n")
        (dolist (p worktrees)
          (insert (format "Path: %s\n" p))
          (insert (string-join (or (magit-git-lines "-C" p "status" "--porcelain") '("<<clean>>")) "\n"))
          (insert "\n")))

      (insert "\n== Summary ==\n")
      (insert (format "Safe: %s\n" (if (plist-get info :safe) "yes" "no")))
      (insert "Reasons:\n")
      (dolist (r (plist-get info :reasons))
        (insert (format " - %s\n" r)))
      (goto-char (point-min)))
    (display-buffer buf)
    info))

(defun neo/magit--choose-main-ref ()
  "Return string name of main ref based on `neo/magit-branch-delete-compare'.
For 'fresh-upstream, performs a quiet fetch of the default remote to update origin/main."
  (pcase neo/magit-branch-delete-compare
    ('local "main")
    ('upstream
     (or (ignore-errors (magit-git-string "rev-parse" "--verify" "origin/main"))
         "main"))
    ('fresh-upstream
     ;; fetch quietly to update origin/main
     (ignore-errors
       (magit-git "fetch" "--quiet"))
     ;; now use the updated origin/main
     (or (ignore-errors (magit-git-string "rev-parse" "--verify" "origin/main"))
         "main"))
    (_ "main")))

(defun neo/magit-branch-safe-to-delete-configurable-p (branch)
  "Return t if BRANCH is safe to delete using configurable policies."
  (let* ((main (neo/magit--choose-main-ref))
         (wt-ht (neo/magit--parse-worktree-porcelain-ht)) ; from previous robust code
         (worktrees (gethash branch wt-ht))
         (reasons '()))

    ;; 1) commit check: are there commits on branch not in chosen MAIN?
    (when (neo/magit--has-commits-not-in-main-p branch main)
      (push (format "Branch has commits not in %s" main) reasons))

    ;; 2) worktree policy
    (when worktrees
      (pcase neo/magit-branch-delete-checkout-policy
        ('any
         (push (format "Branch is checked out in worktree(s): %s" (string-join worktrees ", "))
               reasons))
        ('dirty
         (when (seq-some (lambda (p) (not (neo/magit--worktree-clean-p p))) worktrees)
           (push "At least one checking-out worktree is dirty" reasons)))
        ('ignore
         ;; nothing
         nil
         )
        (_
         ;; unknown policy: conservative
         (push (format "Unknown checkout policy: %s" neo/magit-branch-delete-checkout-policy) reasons))))
    (null reasons)))


(defun neo/magit-branches-safe-to-delete-configurable ()
  "Return list of local branches safe to delete using configurable policy."
  (interactive)
  (let ((branches (magit-list-local-branch-names))
        safe
        (protected '("main" "develop"))) ;; branches never safe to delete
    (dolist (b branches)
      (unless (member b protected)
        (when (neo/magit-branch-safe-to-delete-configurable-p b)
          (push b safe))))
    (nreverse safe)))

;; (neo/magit-branch-safe-to-delete-configurable-p "mav/investigate-magit-issues")
;; (neo/magit-branch-safe-to-delete-configurable-p "foobar")
;; (neo/magit-branches-safe-to-delete-configurable)

;; (defun neo/magit--parse-worktree-porcelain-ht ()
;;   "Parse `git worktree list --porcelain' into a hash-table mapping branch -> list of paths.
;; Branch names are trimmed and paths are expanded absolute paths."
;;   (let* ((lines (magit-git-lines "worktree" "list" "--porcelain"))
;;          (ht (make-hash-table :test #'equal))
;;          (current-path nil))
;;     (dolist (ln lines)
;;       (cond
;;        ((string-prefix-p "worktree " ln)
;;         (setq current-path (expand-file-name (string-trim (substring ln (length "worktree "))))))
;;        ((string-prefix-p "branch " ln)
;;         (when current-path
;;           (let* ((ref (string-trim (substring ln (length "branch "))))
;;                  (branch (if (string-prefix-p "refs/heads/" ref)
;;                              (substring ref (length "refs/heads/"))
;;                            ref))
;;                  (branch (string-trim branch))
;;                  (existing (gethash branch ht)))
;;             (puthash branch (cons current-path existing) ht))))))
;;     ;; reverse lists for stable ordering
;;     (maphash (lambda (k v) (puthash k (nreverse v) ht)) ht)
;;     ht))

;; (defun neo/magit--worktree-map->alist (ht)
;;   "Return an alist copy of hash-table HT (branch . (paths...))."
;;   (let (alist)
;;     (maphash (lambda (k v) (push (cons k v) alist)) ht)
;;     (nreverse alist)))

;; (defun neo/magit--has-commits-not-in-main-p (branch &optional main)
;;   "Return t if BRANCH has commits not in MAIN (default \"main\")."
;;   (let ((main (or main "main")))
;;     (not (null (magit-git-lines "log" branch "--not" main "--oneline")))))

;; (defun neo/magit--worktree-clean-p (path)
;;   "Return t if `git -C PATH status --porcelain` is empty."
;;   (null (magit-git-lines "-C" path "status" "--porcelain")))

;; (defun neo/magit-branch-safety-info-ht (branch &optional main)
;;   "Return a plist with robust safety info for BRANCH using a hash-table for worktrees.
;; Plist keys: :safe, :reasons, :worktrees-ht, :worktrees-list."
;;   (let* ((branch (string-trim branch))
;;          (main (or main "main"))
;;          (ht (neo/magit--parse-worktree-porcelain-ht))
;;          (worktrees (gethash branch ht))
;;          (reasons '()))
;;     (when (neo/magit--has-commits-not-in-main-p branch main)
;;       (push (format "Branch has commits not in %s (git log ... returned entries)" main) reasons))
;;     (when worktrees
;;       (push (format "Branch is checked out in worktree(s): %s" (string-join worktrees ", ")) reasons)
;;       (when (seq-some (lambda (p) (not (neo/magit--worktree-clean-p p))) worktrees)
;;         (push "At least one checking-out worktree is dirty (status --porcelain non-empty)" reasons)))
;;     (list :safe (null reasons)
;;           :reasons (nreverse reasons)
;;           :worktrees-ht ht
;;           :worktrees-list worktrees)))

;; (defun neo/magit-print-branch-diagnostics-ht (branch &optional main)
;;   "Print diagnostics for BRANCH using the hash-table based parser."
;;   (interactive (list (completing-read "Branch: " (magit-list-local-branch-names))))
;;   (let* ((branch (string-trim branch))
;;          (main (or main "main"))
;;          (info (neo/magit-branch-safety-info-ht branch main))
;;          (ht (plist-get info :worktrees-ht))
;;          (worktrees (plist-get info :worktrees-list))
;;          (buf (get-buffer-create "*magit-branch-diagnostics*")))
;;     (with-current-buffer buf
;;       (erase-buffer)
;;       (insert (format "Diagnostic for branch: %s (main: %s)\n\n" branch main))
;;       (insert "== parsed worktree map (from hash-table) ==\n\n")
;;       (dolist (pair (neo/magit--worktree-map->alist ht))
;;         (insert (format "%s -> %s\n" (car pair) (string-join (cdr pair) ", "))))
;;       (insert "\n== commits not in main (git log ...) ==\n")
;;       (insert (string-join (or (magit-git-lines "log" branch "--not" main "--oneline") '("<<none>>")) "\n"))
;;       (insert "\n\n== git diff branch...main ==\n")
;;       (insert (string-join (or (magit-git-lines "diff" (format "%s...%s" branch main)) '("<<none>>")) "\n"))
;;       (insert "\n\n== status --porcelain for each worktree that checks out branch ==\n")
;;       (if (null worktrees)
;;           (insert "<<not checked out in any worktree>>\n")
;;         (dolist (p worktrees)
;;           (insert (format "Path: %s\n" p))
;;           (insert (string-join (or (magit-git-lines "-C" p "status" "--porcelain") '("<<clean>>")) "\n"))
;;           (insert "\n")))

;;       (insert "\n== Summary ==\n")
;;       (insert (format "Safe: %s\n" (if (plist-get info :safe) "yes" "no")))
;;       (insert "Reasons:\n")
;;       (dolist (r (plist-get info :reasons))
;;         (insert (format " - %s\n" r)))
;;       (goto-char (point-min)))
;;     (display-buffer buf)
;;     info))

;; (neo/magit-print-branch-diagnostics-ht "foobar")
;; (:safe nil :reasons ("Branch is checked out in worktree(s): /home/mav/.local/share/wtrees/omega_foobar" "At least one checking-out worktree is dirty (status --porcelain non-empty)") :worktrees-ht #s(hash-table test equal data ("main" ("/home/mav/Projects/omega") "foobar" ("/home/mav/.local/share/wtrees/omega_foobar") "mav-27/python-debug-support" ("/home/mav/.local/share/wtrees/omega_mav-27-python-debug-support") "mav-44/add-python-extension-to-neo" ("/home/mav/.local/share/wtrees/omega_mav-44-add-python-extension-to-neo") "mav/investigate-magit-issues" ("/home/mav/.local/share/wtrees/omega_mav-investigate-magit-issues"))) :worktrees-list ("/home/mav/.local/share/wtrees/omega_foobar"))

;; (:safe nil :reasons ("Branch is checked out in worktree(s): /home/mav/.local/share/wtrees/omega_foobar") :worktrees-ht #s(hash-table test equal data ("main" ("/home/mav/Projects/omega") "foobar" ("/home/mav/.local/share/wtrees/omega_foobar") "mav-27/python-debug-support" ("/home/mav/.local/share/wtrees/omega_mav-27-python-debug-support") "mav-44/add-python-extension-to-neo" ("/home/mav/.local/share/wtrees/omega_mav-44-add-python-extension-to-neo") "mav/investigate-magit-issues" ("/home/mav/.local/share/wtrees/omega_mav-investigate-magit-issues"))) :worktrees-list ("/home/mav/.local/share/wtrees/omega_foobar"))


;; (:safe nil :reasons ("Branch is checked out in worktree(s): /home/mav/.local/share/wtrees/omega_foobar" "At least one checking-out worktree is dirty (status --porcelain non-empty)") :worktrees-ht #s(hash-table test equal data ("main" ("/home/mav/Projects/omega") "foobar" ("/home/mav/.local/share/wtrees/omega_foobar") "mav-27/python-debug-support" ("/home/mav/.local/share/wtrees/omega_mav-27-python-debug-support") "mav-44/add-python-extension-to-neo" ("/home/mav/.local/share/wtrees/omega_mav-44-add-python-extension-to-neo") "mav/investigate-magit-issues" ("/home/mav/.local/share/wtrees/omega_mav-investigate-magit-issues"))) :worktrees-list ("/home/mav/.local/share/wtrees/omega_foobar"))

;; (neo/magit-print-branch-diagnostics-ht "mav/investigate-magit-issues")
;; (:safe nil :reasons ("Branch has commits not in main (git log ... returned entries)" "Branch is checked out in worktree(s): /home/mav/.local/share/wtrees/omega_mav-investigate-magit-issues" "At least one checking-out worktree is dirty (status --porcelain non-empty)") :worktrees-ht #s(hash-table test equal data ("main" ("/home/mav/Projects/omega") "foobar" ("/home/mav/.local/share/wtrees/omega_foobar") "mav-27/python-debug-support" ("/home/mav/.local/share/wtrees/omega_mav-27-python-debug-support") "mav-44/add-python-extension-to-neo" ("/home/mav/.local/share/wtrees/omega_mav-44-add-python-extension-to-neo") "mav/investigate-magit-issues" ("/home/mav/.local/share/wtrees/omega_mav-investigate-magit-issues"))) :worktrees-list ("/home/mav/.local/share/wtrees/omega_mav-investigate-magit-issues"))



(provide 'neo-better-git-brancher)
