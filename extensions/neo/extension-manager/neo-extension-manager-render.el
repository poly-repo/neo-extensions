;;; -*- lexical-binding: t -*-

(require 'neo-framework)

(neo/use-package svg-lib
  :config
  (svg-lib-button-mode 1))

(defface neo/divider-face
  '((t :overline unspecified
       :foreground unspecified
       :background unspecified))
  "Face for UI dividers.")

(defface neo/manager-info-label-face
  '((t :inherit font-lock-keyword-face
       :weight bold
       :height 0.7))
  "Face for info block labels in the NEO manager.")

(defface neo/manager-info-value-face
  '((t :inherit default
       :height 0.7))
  "Face for info block values in the NEO manager.")

(defface neo/manager-header-face
  '((t :inherit mode-line
       :height 0.9))
  "Face for the NEO manager header.")

;; (defface neo/header-button-active
;;   '((t :inherit neo/manager-header-face
;;        :weight bold
;;        :underline t))
;;   "Active header button.")

;; (defface neo/header-button-inactive
;;   '((t :inherit neo/manager-header-face)))

(defface neo/header-button-active
  `((t :weight bold
       :underline t
       :foreground ,(or (face-foreground 'neo/manager-header-face) 'unspecified)
       :background ,(or (face-background 'neo/manager-header-face) 'unspecified)))
  "Face for active header buttons in the NEO extension manager."
  :group 'neo)

(defface neo/header-button-inactive
  '((t :inherit header-line))
  "Face for inactive header buttons in the NEO extension manager."
  :group 'neo)

;; (defun neo/header-button (label action active)
;;   (propertize
;;    label
;;    'face (if active
;;              'neo/header-button-active
;;            'neo/header-button-inactive)
;;    'mouse-face 'highlight
;;    'help-echo label
;;    'keymap (let ((map (make-sparse-keymap)))
;;              (define-key map [header-line mouse-1] action)
;;              map)))

(defun neo/header-button (label action active)
  (propertize
   label
   'face (if active
             'neo/header-button-active
           'neo/header-button-inactive)
   'mouse-face 'highlight
   'help-echo label
   'keymap (let ((map (make-sparse-keymap)))
             (define-key map [header-line mouse-1] action)
             map)))

(defun neo/update-divider-face ()
  "Update `neo/divider-face` colors to match the current theme."
  (let ((fg (face-foreground 'default nil t))
        (bg (face-background 'default nil t)))
    (set-face-attribute 'neo/divider-face nil
                        :overline fg
                        :foreground fg
                        :background bg)))

(add-hook 'neo/after-theme-load-hook #'neo/update-divider-face)

(defun neo/insert-thin-divider (&optional color)
  "Insert a thin horizontal divider using underline, aligned to window width.
If COLOR is nil, use the theme's default foreground color."
  (unless (bolp)
    (insert "\n"))
  (insert "\n")
  (let* ((line-color (or color (face-foreground 'default nil t)))
         (bg-color   (face-background 'default nil t)))
    (insert
     (propertize " "
                 'display '(space :align-to right)
                 'face 'neo/divider-face))
    (insert "\n")))

(cl-defgeneric neo/extension-render-card (object)
  "Render OBJECT at point and return a point marker.")

(defconst neo/manager--spinner-frames '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Animation frames for the install spinner.")

(defvar-local neo/manager--installing-label nil
  "Slug of the extension currently being installed, or nil.

Set by `neo/manager--start-install-spinner'. While non-nil,
`neo/extension-render-card' draws that slug's card with a spinner glyph in
place of its Install button, instead of the button itself.")

(defvar-local neo/manager--spinner-index 0
  "Current animation frame index into `neo/manager--spinner-frames'.")

(defvar-local neo/manager--spinner-timer nil
  "Repeating timer driving the install spinner, or nil.")

(defvar-local neo/manager--spinner-marker nil
  "Marker at the animated glyph inside the installing card's button area.

Set by `neo/extension-render-card' when it draws the card matching
`neo/manager--installing-label'. Consumed by
`neo/manager--update-spinner-glyph' to redraw just that one character on
each timer tick, without re-rendering the whole card.")

(defun neo/manager--spinner-glyph ()
  "Return the current spinner glyph."
  (nth (mod neo/manager--spinner-index (length neo/manager--spinner-frames))
       neo/manager--spinner-frames))

(defun neo/manager--info-label (text)
  (propertize text 'face 'neo/manager-info-label-face))

(defun neo/manager--info-value (text)
  (propertize text 'face 'neo/manager-info-value-face))


(defconst neo--dir
  (file-name-directory
   (or load-file-name buffer-file-name))
  "Directory of the NEO package.")

(defun neo--local-file (file)
  (expand-file-name file neo--dir))

(defun neo/manager--enabled-roots ()
  "Return the persisted `enabled-extensions' root slug strings, or nil."
  (let ((config-val (neo/get-config "enabled-extensions")))
    (if config-val (read config-val) nil)))

(defun neo/manager--installed-slugs (framework)
  "Return the slugs that would load on the next restart, per FRAMEWORK.

Computed from the persisted `enabled-extensions' roots (not the live
`installed-extensions' hash table), so the manager's Install/Disable buttons
always reflect what a restart would actually do."
  (neo/topo-sort-from-roots (neo-framework-available-extensions framework)
                            (neo/manager--enabled-roots)
                            '(:on-missing ignore :on-cycle ignore)))

(defun neo/manager--persist-enabled-extension (slug)
  "Add SLUG to the persisted `enabled-extensions' roots, without loading it.

Takes effect on the next restart only. Used where hot-loading would be
unsafe or unwanted right now -- e.g. `neo/manager--maybe-launch-on-startup'
pre-selects `neo:dashboard' this way specifically so it does NOT load (and
race the one-shot manager launch) during the boot that follows `Start
configuration'."
  (let* ((roots (neo/manager--enabled-roots))
         (updated (if (member slug roots) roots (append roots (list slug)))))
    (neo/set-config "enabled-extensions" (prin1-to-string updated))
    (neo/set-config "pretend-new-user" "nil")))

(defun neo/manager--resolve-extension (framework slug)
  "Install, load, and configure SLUG (and any unmet `:requires') in FRAMEWORK.

Mirrors the install/load/replay-packages steps `neo/bootstrap' performs for
the whole `enabled-extensions' set at startup, scoped here to just the
extensions SLUG transitively needs that are not already in FRAMEWORK's
`installed-extensions' -- an extension already installed this session is
left untouched.

Returns the slug strings that were newly installed, in dependency order
(dependencies before dependents).

Caveat: an extension that hooks itself onto `neo/after-framework-bootstrap-
hook' (e.g. via a `neo/use-package' `:hook', or directly via `(if neo/
framework-bootstrapped-p (do-it-now) (add-hook 'neo/after-framework-
bootstrap-hook #'do-it-later))') relies on that hook firing once at startup;
it has already run for this session by the time anything can be hot-
installed. `neo/framework-bootstrapped-p' is bound to nil for the duration
of the load+replay loop below so such code takes its defer path here too,
and any function that gets newly added to `neo/after-framework-bootstrap-
hook' as a result is fired once the loop finishes (with the flag restored to
its real value), so it activates in this session instead of waiting for the
next restart. A hook function already present before this call -- e.g. one
that already fired during real boot and does not remove itself -- is left
alone; only the delta added during this call is fired."
  (let* ((available (neo-framework-available-extensions framework))
         (installed (neo-framework-installed-extensions framework))
         (wanted (neo/topo-sort-from-roots available (list slug)
                                           '(:on-missing warn :on-cycle warn)))
         (new-slugs (seq-remove (lambda (s) (gethash s installed)) wanted))
         (hook-before (copy-sequence neo/after-framework-bootstrap-hook)))
    ;; Extension files loaded here are not yet fully bootstrapped in the
    ;; `neo/bootstrap' sense: their own transitive packages are installed
    ;; and replayed per-slug, interleaved, in the loop below, not all at
    ;; once before any of them load. Leaving the (stale, already-true)
    ;; global flag in place would make code like
    ;; `neo-programming-foundation.el''s `(if neo/framework-bootstrapped-p
    ;; ...)' run immediately instead of deferring, before its own
    ;; `use-package' dependencies (e.g. `vui') have been replayed.
    (let ((neo/framework-bootstrapped-p nil))
      (dolist (slug-string new-slugs)
        (when-let* ((ext (gethash slug-string available))
                    (ext-slug (neo--extension-slug ext)))
          (neo/install-extension framework
                                 (make-neo/installation
                                  :extension-slug ext-slug
                                  :installed-at (current-time)))
          (if (neo--load-extension ext)
              (provide (neo/extension-feature-symbol ext-slug))
            (neo/log-warn 'core "Extension %s file not found or failed to load" slug-string))
          (neo/replay-extension-packages ext-slug))))
    (dolist (fn (seq-difference neo/after-framework-bootstrap-hook hook-before))
      (condition-case err
          (funcall fn)
        (error
         (neo/log-warn 'core "Deferred bootstrap hook %s failed during hot-install: %s"
                       fn err))))
    new-slugs))

(defun neo/manager--install-extension (slug)
  "Install SLUG into the running session and persist the choice.

Resolves SLUG the way `neo/bootstrap' resolves a deferred `neo/extension' at
startup -- installs, loads, and replays packages for SLUG and any of its
unmet `:requires', via `neo/manager--resolve-extension' -- so it (and
whatever it pulls in) is usable immediately, not just after a restart.

Runs a spinner in place of the card's Install button for the duration:
`neo/manager--resolve-extension' blocks synchronously (elpaca package
installs use `:wait t'), so without this there is no feedback at all between
clicking Install and the buffer refreshing once it's done -- and a header-
line-only indicator is too easy to miss."
  (neo/manager--persist-enabled-extension slug)
  (let* ((buf (current-buffer))
         (newly-loaded
          (unwind-protect
              (progn
                (neo/manager--start-install-spinner buf slug)
                (neo/manager--resolve-extension (neo--framework-instance) slug))
            (neo/manager--stop-install-spinner buf))))
    (message (if newly-loaded
                 (format "Installed %s." (mapconcat #'identity newly-loaded ", "))
               (format "%s is already installed." slug))))
  (neo/extensions-refresh-all))

(defun neo/manager--disable-extension (slug)
  "Placeholder: disabling SLUG is not implemented yet."
  (message "Disable is not implemented yet for %s." slug))

(defun neo/manager--uninstall-extension (slug)
  "Placeholder: uninstalling SLUG is not implemented yet."
  (message "Uninstall is not implemented yet for %s." slug))

(cl-defmethod neo/extension-render-card ((ext neo/extension) framework &optional installed-slugs)
  "Render EXT in the current buffer. Return (start . end) position."
  (let ((start (point))
        (inhibit-message nil)
        (message-log-max t))
    ;; Insert image using overlay
    (let* ((emblem (neo/extension-emblem ext))
	   (img (if (stringp emblem)
		    (create-image emblem 'png t) ; :data-p t)
		  (create-image (neo--local-file "default-emblem64.png") 'png))))
      ;; Insert a space and record its bounds *after* insertion
      (insert " ")
      (let ((ov (make-overlay (1- (point)) (point))))
	(overlay-put ov 'display img)
	(overlay-put ov 'neo-image t)
	;; Store it in the extension struct
	(setf (neo/extension-summary-overlay ext) ov)))
    
    (insert " ")

    (insert (propertize (or (neo/extension-title ext) "Unnamed")
			'face '(:weight bold :height 1.2)))
    (insert "\n\n")

    ;; Description
    (when-let* ((desc (neo/extension-description ext)))
      (insert (propertize desc 'face '(:slant italic :height 0.95)))
      (insert "\n\n"))

    (let* ((slug (neo/extension-slug-to-string (neo--extension-slug ext)))
           (installed-p (member slug installed-slugs))
           (installing-p (equal slug neo/manager--installing-label)))
      (cond
       (installing-p
        ;; Replaces the Install button outright (rather than merely
        ;; disabling it) so a mid-install card can't be mistaken for one
        ;; still waiting to be clicked -- this is what the header-line-only
        ;; spinner missed: it was too easy to not notice at all.
        (let ((glyph-start (point)))
          (insert (propertize (neo/manager--spinner-glyph) 'face '(:weight bold)))
          (setq neo/manager--spinner-marker (copy-marker glyph-start)))
        (insert (format " Installing %s…" slug)))
       (installed-p
        (insert (svg-lib-button "[pause] Disable"
                                (lambda () (interactive) (neo/manager--disable-extension slug))))
        (insert " ")
        (insert (svg-lib-button "[trash-can] Uninstall"
                                (lambda () (interactive) (neo/manager--uninstall-extension slug)))))
       (t
        (insert (svg-lib-button "[download] Install"
                                (lambda () (interactive) (neo/manager--install-extension slug)))))))
    (insert "\n")
    
    (let ((repo (neo/extension-repository ext))
	  (used-packages (neo--get-extension-info ext)))
					;  "Alist mapping keys to list render specs."))
      (dolist (pair `(("Publisher" ,(neo/extension-publisher ext))
                      ("Type"      ,(neo/repository-type repo))
                      ("URL"       ,(neo/repository-url repo))
                      ("Path"      ,(neo/repository-path repo))
		      ("Required"  ,(neo/extension-requires ext))
		      ("Recommended" ,(neo/extension-depends-on ext))
		      ("Packages"  ,used-packages)
		      ))
	(when-let* ((key (car pair))
		   (value (cadr pair)))
	  (insert (neo/manager--info-label (format "%-12s" key)) (neo/manager--info-value ": "))

	  (cond
	   ((listp value)
	    (neo--manager-dispatch-list key value))
	   (t
	    (insert (neo/manager--info-value value) "\n"))))))

    ;; (if (listp value)
    ;;     (if (string= key "Packages")
    ;; 	  (neo--insert-packages-desc value))
    ;;	    (insert (neo/manager--info-value value))))))

    (neo/insert-thin-divider)
    
    ;; Final range
    (let ((end (point)))
      (put-text-property start end 'neo-extension ext)
      (let ((ov (make-overlay start (point))))
	(overlay-put ov 'neo/card t)
	(overlay-put ov 'neo/extension ext)
	(overlay-put ov 'evaporate t)
	ov)
      ;;; we probably don't use the return value or the text-property any more
      (cons start end))))

(defun neo--manager-dispatch-list (key value)
  (let* ((list-handlers
	  '(("Packages"    neo--insert-packages-desc)
	    ("Required"    neo--insert-extensions-desc)
	    ("Recommended" neo--insert-extensions-desc)))
	 (spec (assoc key list-handlers)))
    (if spec
	(funcall (cadr spec) value))))
;      (insert (mapconcat #'identity value ", "))))))

(defun neo--insert-extensions-desc (extensions)
  (dolist (slug (sort extensions))
    (let ((ext (neo/get-extension (neo--framework-instance) slug)))
      (indent-to 14)
      (insert (neo/manager--info-value (format "%s" slug)))
      (indent-to 40)
      (if ext
	  (insert (neo/manager--info-value (format "%s\n" (neo/extension-title ext))))
	(insert (neo/manager--info-value (format "Not available\n" slug))))
      )))

(defun neo--insert-package-desc (package-name package-info)
  (let* ((package (cadr (assoc-string package-name package-info)))
	 (desc (if package
		   (package-desc-summary package)
		 (if (string= package-name "emacs")
		     "Low-level Emacs configuration, not part of any package"
		   (format "No description available for package '%s'" package-name)))))
    (indent-to 14)
    (insert (neo/manager--info-value (format "%s" package-name)))
    (indent-to 40)
    (insert (neo/manager--info-value (format "%s\n" desc)))))

(defun neo--insert-packages-desc (package-info)
  (if package-info
      (sort package-info (lambda (a b) (string< (car a) (car b)))))
  (dolist (p package-info)
    (let* ((package-name (car p)))
      (neo--insert-package-desc package-name package-info))))

(defvar-local neo/current-filter 'all)

;; TODO: move these to neo-extension-manager.el
;(define-key neo/manager-mode-map (kbd "a") #'neo/filter-all)
;(define-key neo/manager-mode-map (kbd "e") #'neo/filter-enabled)
;(define-key neo/manager-mode-map (kbd "d") #'neo/filter-disabled)

(defun neo/filter-all ()
  (interactive)
  (setq neo/current-filter 'all)
  (neo/extensions-refresh-all))

(defun neo/filter-installed ()
  (interactive)
  (setq neo/current-filter 'installed)
  (neo/extensions-refresh-all))

(defun neo/filter-recommended ()
  (interactive)
  (setq neo/current-filter 'recommended)
  (neo/extensions-refresh-all))

(defun neo/filter-disabled ()
  (interactive)
  (setq neo/current-filter 'disabled)
  (neo/extensions-refresh-all))

(defun neo/manager--update-spinner-glyph ()
  "Redraw just the animated glyph at `neo/manager--spinner-marker' in place.

Cheap per-tick update: touches only the single glyph character, not a full
`neo/extensions-render', which would re-run `neo--get-extension-info'
package introspection for every card and would itself be slow enough to
fight the spinner's own purpose."
  (when (and neo/manager--spinner-marker (marker-position neo/manager--spinner-marker))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char neo/manager--spinner-marker)
        (delete-char 1)
        (insert (propertize (neo/manager--spinner-glyph) 'face '(:weight bold)))))))

(defun neo/manager--start-install-spinner (buf label)
  "Animate an install spinner for LABEL's card in BUF.

`neo/replay-extension-packages' (invoked by `neo/manager--resolve-extension')
blocks synchronously on elpaca via `:ensure (:wait t)', but elpaca's own wait
loop polls with `sit-for', which still runs timers and redisplay -- so a
timer nudging the display is enough to animate visibly during that blocking
call.

A one-time full re-render happens here so LABEL's card swaps its Install
button for the spinner placeholder (see `neo/extension-render-card'); every
subsequent tick then only touches that placeholder's single glyph character
via `neo/manager--update-spinner-glyph' -- never a second full re-render,
since that would re-run `neo--get-extension-info' package introspection for
every card and fight the spinner's own purpose. Pair with
`neo/manager--stop-install-spinner'."
  (with-current-buffer buf
    (setq neo/manager--installing-label label)
    (setq neo/manager--spinner-index 0)
    (setq neo/manager--spinner-marker nil)
    (neo/extensions-refresh-all)
    (setq neo/manager--spinner-timer
          (run-with-timer
           0.12 0.12
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq neo/manager--spinner-index (1+ neo/manager--spinner-index))
                 (neo/manager--update-spinner-glyph)
                 (force-mode-line-update))))))))

(defun neo/manager--stop-install-spinner (buf)
  "Cancel the spinner started by `neo/manager--start-install-spinner'.

Leaves the actual card/button swap back to Disable/Uninstall (or Install,
on failure) to the caller's own full refresh -- `neo/manager--install-
extension' always does one right after this returns -- so this only clears
state and cancels the timer."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (timerp neo/manager--spinner-timer)
        (cancel-timer neo/manager--spinner-timer))
      (setq neo/manager--spinner-timer nil)
      (setq neo/manager--installing-label nil)
      (setq neo/manager--spinner-marker nil))))

(defun neo/manager-update-header ()
  (setq-local
   header-line-format
   (append
    (list
     (propertize " " 'face 'neo/manager-header-face)
     (svg-lib-button "[check-bold] All"
                         (lambda () (interactive) (message "All")))
     (propertize " " 'face 'neo/manager-header-face)
     (neo/header-button "[All]" #'neo/filter-all
                        (eq neo/current-filter 'all))
     (propertize "  " 'face 'neo/manager-header-face)
     (neo/header-button "[Installed]" #'neo/filter-installed
                        (eq neo/current-filter 'installed))
     (propertize "  " 'face 'neo/manager-header-face)
     (neo/header-button "[Recommended]" #'neo/filter-recommended
                        (eq neo/current-filter 'recommended))
     (propertize "  " 'face 'neo/manager-header-face)
     (neo/header-button "[Disabled]" #'neo/filter-disabled
                        (eq neo/current-filter 'disabled)))
    (list
     (propertize
      " "
      'display '(space :align-to right)
      'face 'neo/manager-header-face)))))

(defun neo/extensions-render ()
  "Redraw the extension list, preserving the current view.

On the very first render the buffer is empty; land on the top of the freshly
drawn list rather than trusting `window-start', which can still hold a stale
value left over from whatever buffer this window showed before
`switch-to-buffer' -- there has been no redisplay yet to refresh it, and a
stale large value would otherwise get clamped down to the very bottom of
the new content. On a refresh triggered by a button (e.g. Install), the
buffer already had real content and a settled display, so the user's scroll
position and point are saved beforehand and restored afterward (clamped to
the new buffer size), so clicking a button does not jump the display.

Rendering all cards can be slow the first time an extension's packages
(e.g. svg-lib) or its `neo--get-extension-info' introspection need to
install/refresh things via elpaca, which pops up `*elpaca-log*' in whatever
window is current -- including this one, mid-loop. When that happens,
Emacs snapshots this buffer's then-current (mid-render, not-yet-final)
point into that window's `window-prev-buffers', and restoring this buffer
later would land back at that stale mid-render position instead of the
top. Re-assert this buffer and the intended point/start on WIN as the last
step, after all cards are drawn, so any such hijack gets overridden."
  (let* ((inhibit-read-only t)
         (buf (current-buffer))
         (framework (neo--framework-instance))
         (installed-slugs (neo/manager--installed-slugs framework))
         (first-render-p (= (point-min) (point-max)))
         (win (get-buffer-window buf t))
         (saved-point (if first-render-p 1 (point)))
         (saved-window-start (if first-render-p 1 (and win (window-start win)))))
    (erase-buffer)

    (maphash (lambda (_ v)
               (unless (neo/extension-hidden v)
                 (condition-case err
                     (neo/extension-render-card v framework installed-slugs)
                   (error
                    (neo/log-warn 'core "Failed to render card for %s: %s"
                                  (neo/extension-slug-to-string (neo--extension-slug v))
                                  (error-message-string err))))))
             (neo-framework-available-extensions framework))
    (goto-char (min saved-point (point-max)))
    (when (window-live-p win)
      (set-window-buffer win buf)
      (set-window-point win (point))
      (set-window-start win (min saved-window-start (point-max))))))

(defun neo/extensions-refresh-all ()
  (interactive)
  (neo/manager-update-header)
  (neo/extensions-render))

;; (defun neo/extensions-render ()
;;   (setq-local
;;    header-line-format
;;    (list
;;     (propertize " " 'face 'neo/manager-header-face)
;;     (neo/header-button "[All]" #'neo/filter-all (eq neo/current-filter 'all))
;;     (propertize "  " 'face 'neo/manager-header-face)
;;     (neo/header-button "[Installed]" #'neo/filter-installed (eq neo/current-filter 'installed))
;;     (propertize "  " 'face 'neo/manager-header-face )
;;     (neo/header-button "[Recommended]" #'neo/filter-recommended (eq neo/current-filter 'recommended))
;;     (propertize "  " 'face 'neo/manager-header-face)
;;     (neo/header-button "[Disabled]" #'neo/filter-disabled (eq neo/current-filter 'disabled))
;;     (propertize
;;      " "
;;      'display '(space :align-to right)
;;      'face 'neo/manager-header-face)))

;;   (let ((inhibit-read-only t))
;;     (let* ((framework  (neo--framework-instance)))
;;       (message "running rendering")
;;       (erase-buffer)
;;       (maphash (lambda (k v)
;; 		 (neo/extension-render-card v))
;;                (neo-framework-available-extensions framework))
;;       (goto-char (point-min)))))

(provide 'neo-extension-manager-render)
