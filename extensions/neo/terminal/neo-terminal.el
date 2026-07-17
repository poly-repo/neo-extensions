;;; -*- lexical-binding: t -*-

;;; This is terminal, a NEO extension
;;;
;;; Terminal emulation in Emacs

(neo/use-package eat)

;; `key-chord-mode' (neo:questionable-defaults) hijacks `input-method-function'
;; globally with no buffer-local opt-out of its own, so a chord like ".."
;; (comment-or-uncomment-region) can misfire from ordinary fast-typed shell
;; input (e.g. "cd ..") in any terminal-emulator buffer. Clear the variable
;; buffer-locally in those buffers instead -- it also disables real
;; multilingual input methods (`C-\') there, which is an acceptable trade-off
;; since terminal-emulator buffers pass keystrokes straight to the
;; subprocess anyway.
(defun neo/terminal-disable-key-chord ()
  "Disable key-chord processing in the current terminal-emulator buffer."
  (setq-local input-method-function nil))

(dolist (hook '(eshell-mode-hook eat-mode-hook vterm-mode-hook))
  (add-hook hook #'neo/terminal-disable-key-chord))

(defun eshell/q ()
  (bury-buffer))

(defun eshell/d ()
  (dired "."))

(defun eshell/vi (&rest args)
  "Invoke `find-file' on the file.
\"vi +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (find-file file)
          (goto-line line))
      (find-file (pop args)))))

;;; stolen from aweshell
(defun aweshell-cat-with-syntax-highlight (filename)
  "Like cat(1) but with syntax highlighting."
  (let ((existing-buffer (get-file-buffer filename))
        (buffer (find-file-noselect filename)))
    (eshell-print
     (with-current-buffer buffer
       (if (fboundp 'font-lock-ensure)
           (font-lock-ensure)
         (with-no-warnings
           (font-lock-fontify-buffer)))
       (let ((contents (buffer-string)))
         (remove-text-properties 0 (length contents) '(read-only nil) contents)
         contents)))
    (unless existing-buffer
      (kill-buffer buffer))
    nil))

(defun neo/eshell--commands->regexps (commands)
  "Convert a list of shell COMMANDS into a list of regexps.
Each command will be anchored to match at the beginning of a line,
allowing for optional whitespace before it."
  (mapcar (lambda (cmd)
            (concat "\\`\\s-*" (regexp-quote cmd) "\\s-*\\'"))
          commands))

(defun neo/eshell-history-filter (input)
  "Filter for Eshell history.
- Ignores trivial commands.
- Converts relative file paths of existing files to absolute paths.
- Returns `nil` to ignore the command, or the (modified) command string to save it."
  (let* ((trivial-commands '("l" "ll" "ls" "cd" "pwd" "clear" "exit"
                             "bg" "fg" "jobs" "top" "htop" "b" "c" "p"))
         (trivial-regexps (neo/eshell--commands->regexps trivial-commands))
         (command (string-trim input)))
    (if (or (string-empty-p command)
            (cl-some (lambda (rx) (string-match-p rx command)) trivial-regexps))
        nil ; Ignore the command
      ;; Process the command to expand file paths
      (let* ((args (split-string command))
             (expanded-args
              (mapcar (lambda (arg)
                        (if (and (file-exists-p arg) (not (file-name-absolute-p arg)))
                            (expand-file-name arg)
                          arg))
                      args)))
        (mapconcat #'identity expanded-args " ")))))


(defmacro eshell-defcmd (name options &rest mapping)
  "Define an eshell command NAME with OPTIONS and a mapping of symbols to shell commands.

OPTIONS should be a literal list of Eshell option declarations.  
MAPPING should be pairs of the form (SYMBOL . COMMAND-FORM), where SYMBOL
matches the `:symbol` in the OPTIONS list, and COMMAND-FORM is a Lisp
form that returns a string to be passed to `eshell-parse-command`."
  `(defun ,(intern (concat "eshell/" (symbol-name name))) (&rest args)
     (eshell-eval-using-options
      ,(symbol-name name) args
      ,(append options '(:show-usage))
      (let* ((cmd
              (cond
               ,@(mapcar (lambda (pair)
                           `(,(car pair) ,(cdr pair)))
                         mapping)
               (t nil))))
        (if cmd
            (eshell-do-eval (eshell-parse-command cmd) t)
          ;; fallback: show usage
          (eshell-show-usage ,(symbol-name name) ,options))))))

(eshell-defcmd deb
	       '((?f "find" t find "list available packages matching a pattern")
		 (?i "installed" t installed "list installed debs matching a pattern")
		 (?l "list-files" t list-files "list files of a package")
		 (?s "show" t show "show an available package")
		 (?v "version" t version "show the version of an installed package")
		 (?w "where" t where "find the package containing the given file")
		 (nil "help" nil nil "show this usage information"))
	       (find . (format "apt-cache search %s" find))
	       (installed . (format "dlocate -l %s | grep '^.i'" installed))
	       (list-files . (format "dlocate -L %s | sort" list-files))
	       (show . (format "apt-cache show %s" show))
	       (version . (format "dlocate -s %s | egrep '^(Package|Status|Version):'" version))
	       (where . (format "dlocate %s" where)))

					; TODO move to somewhere in core
(defun neo/add-current-file-dir-to-load-path ()
  "Add the directory of the current file to `load-path`."
  (interactive)
  (when load-file-name
    (let ((dir (file-name-directory load-file-name)))
      (add-to-list 'load-path dir)
      (message "Added %s to load-path" dir))))

(neo/use-package dired
  :ensure nil				; builtin
  :bind (:map dired-mode-map ("t" . (lambda () (interactive) (eshell t)))))

(neo/add-current-file-dir-to-load-path)
					;(neo/use-package terminal-history)
					;(require 'terminal-history)
					;(require 'terminal-popup)


(defun neo/eshell-set-random-fortune ()
  "Set a per-buffer fortune banner for Eshell."
  (when (executable-find "fortune")
    (setq eshell-banner-message
          (concat
           (string-join (process-lines "fortune" "-s") "\n")
           "\n\n"))))

;; TODO make sure fortune is available. In the wizard offer installation or opting out
(neo/use-package eshell
  :ensure nil				; builtin
  :after eat
  :custom
  (eshell-history-size 100000)
  (eshell-hist-ignoredups t)
  (eshell-directory-name (no-littering-expand-var-file-name "eshell"))
  (eshell-input-filter #'neo/eshell-history-filter)
  (eshell-ls-use-colors t)
  (eshell-prompt-function
   (lambda ()
     (concat (eshell/pwd) "\n $ ")))
  :config
  (advice-add 'eshell/cat :override #'aweshell-cat-with-syntax-highlight)
  :hook
  (eshell-load . #'eat-eshell-mode))

;;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
;; This part is from a post on reddit, unclear what it does if anything
;; Provide history-based completion (via Vertico) for regexp prompts.
;; keep original available
(defvar neo--orig-read-regexp (symbol-function 'read-regexp))

(defun neo/read-regexp-wrapper (orig-fun prompt &optional default hist &rest rest)
  "Call custom regexp-history UI only in Eshell; otherwise call ORIG-FUN."
  (if (eq major-mode 'eshell-mode)
      (let* ((history-sym (or hist 'regexp-history))
	     (hist-val (condition-case nil (symbol-value history-sym) (error nil)))
	     (history (delete-dups (when (ringp hist-val) (ring-elements hist-val))))
	     (init-input (if (and default (stringp default)) (regexp-quote default) ""))
	     (input (completing-read (concat prompt ": ") history nil nil init-input
				     history-sym (when history (car history)))))
	input)
    (apply orig-fun prompt default hist rest)))

(advice-add 'read-regexp :around #'neo/read-regexp-wrapper)

(defvar eshell-history-ring nil
  "Declared to silence byte-compiler warnings; Eshell command history ring.")

(defvar eshell-hist-ignoredups nil
  "Declared to silence byte-compiler warnings; Eshell history duplicate ignore flag.")

(defun neo/eshell-select-history ()
  "Display Eshell command history with Vertico for selection."
  (interactive)
  (unless (eq major-mode 'eshell-mode)
    (user-error "This command must be run in an Eshell buffer"))
  (unless (and (boundp 'eshell-history-ring) (ringp eshell-history-ring))
    (user-error "No Eshell history ring available"))
  (let* ((history (ring-elements eshell-history-ring))
	 (unique-history (delete-dups history))
	 (selected (completing-read "Select Eshell history entry: " unique-history nil t)))
    (when selected
      (beginning-of-line)
      (delete-region (point) (point-max))
      (insert selected))))

					;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

(neo/use-package eshell-fringe-status
  :after eshell
  :hook (eshell-mode . eshell-fringe-status-mode)
  )

(defun neo/eshell--bury-and-hide-frame ()
  "If current buffer has an associated eshell-created frame, delete it, then hide the buffer.
The buffer itself is not killed, only hidden; this keeps it available to be shown later.
Does not prompt in the minibuffer."
  (interactive)
  (let ((buf (current-buffer)))
    ;; Delete associated frame if present
    (when (buffer-live-p buf)
      (let ((next-buf (or (and (buffer-live-p (next-buffer)) (next-buffer))
                          (other-buffer buf t))))
	(when next-buf
          (switch-to-buffer next-buf))))))

(defun neo/project-root ()
  "Return the current project root, or `default-directory`."
  (or (when (fboundp 'projectile-project-root)
        (ignore-errors (projectile-project-root)))
      default-directory))

(defun neo/eshell-buffer-name (root)
  (format "*neo-eshell:%s*"
          (abbreviate-file-name
           (directory-file-name root))))

(defun neo/find-project-eshell-window (root)
  (cl-find-if
   (lambda (win)
     (with-current-buffer (window-buffer win)
       (and (derived-mode-p 'eshell-mode)
            (string= (buffer-name)
                     (neo/eshell-buffer-name root)))))
   (window-list)))

(defun neo/toggle-eshell ()
  "Toggle a project-aware Eshell at the project root."
  (interactive)
  (let* ((root (file-name-as-directory (neo/project-root)))
         (buf-name (neo/eshell-buffer-name root))
         (win (neo/find-project-eshell-window root)))
    (if win
        ;; Hide
        (quit-window nil win)
      ;; Show
      (neo/eshell-set-random-fortune)
      (let ((default-directory root))
        (pop-to-buffer
         (or (get-buffer buf-name)
             (with-current-buffer (get-buffer-create buf-name)
               (eshell-mode)
               (current-buffer))))))))

(defgroup neo/terminal nil
  "Terminal customizations for NEO."
  :group 'applications)

(defcustom neo/terminal-vterm-enable-codex-links t
  "When non-nil, let vterm open Codex-style file references.

This adds a removable layer on top of vterm:
- `mouse-2' opens a reference under the click
- `C-c C-o' opens a reference at point

Set this to nil to disable the behavior without removing the code."
  :type 'boolean
  :group 'neo/terminal)

(defconst neo--terminal-vterm-markdown-link-regexp
  (concat
   "\\(\\[[^]\n]+\\](\\(?:<\\([^>\n]+\\)>\\|\\([^)\n]+\\)\\))\\)")
  "Regexp matching Markdown links that may contain file targets.")

(defconst neo--terminal-vterm-plain-reference-regexp
  "\\(?:~?/\\|[[:alnum:]_.-]+/\\)[^][(){}<>\"' \t\n]+"
  "Regexp matching plain file references in terminal output.")

(defun neo--terminal-vterm-trim-reference (reference)
  "Trim wrapper punctuation from REFERENCE."
  (string-trim reference "[[(<]+" "[]>),.;]+"))

(defun neo--terminal-vterm-existing-file (path)
  "Return an existing expanded file name for PATH, or nil."
  (when (and path (not (string-empty-p path)))
    (let* ((root (ignore-errors (neo/project-root)))
           (candidates
            (delete-dups
             (delq nil
                   (list
                    (expand-file-name path default-directory)
                    (when root (expand-file-name path root))
                    (and (file-name-absolute-p path)
                         (expand-file-name path)))))))
      (seq-find #'file-exists-p candidates))))

(defun neo--terminal-vterm-resolve-reference (reference)
  "Resolve REFERENCE into a plist describing a file location."
  (let* ((trimmed (neo--terminal-vterm-trim-reference reference))
         (direct (neo--terminal-vterm-existing-file trimmed)))
    (cond
     (direct
      (list :file direct :line nil :column nil))
     ((string-match "\\`\\(.+\\):\\([0-9]+\\):\\([0-9]+\\)\\'" trimmed)
      (when-let ((file (neo--terminal-vterm-existing-file (match-string 1 trimmed))))
        (list :file file
              :line (string-to-number (match-string 2 trimmed))
              :column (string-to-number (match-string 3 trimmed)))))
     ((string-match "\\`\\(.+\\):\\([0-9]+\\)\\'" trimmed)
      (when-let ((file (neo--terminal-vterm-existing-file (match-string 1 trimmed))))
        (list :file file
              :line (string-to-number (match-string 2 trimmed))
              :column nil)))
     ((string-match "\\`\\(.+\\)#L\\([0-9]+\\)C\\([0-9]+\\)\\'" trimmed)
      (when-let ((file (neo--terminal-vterm-existing-file (match-string 1 trimmed))))
        (list :file file
              :line (string-to-number (match-string 2 trimmed))
              :column (string-to-number (match-string 3 trimmed)))))
     ((string-match "\\`\\(.+\\)#L\\([0-9]+\\)\\'" trimmed)
      (when-let ((file (neo--terminal-vterm-existing-file (match-string 1 trimmed))))
        (list :file file
              :line (string-to-number (match-string 2 trimmed))
              :column nil))))))

(defun neo--terminal-vterm-logical-line-beginning (&optional position)
  "Return the start of the logical vterm line at POSITION."
  (save-excursion
    (when position
      (goto-char position))
    (beginning-of-line)
    (while (and (not (bobp))
                (get-text-property (1- (point)) 'vterm-line-wrap))
      (forward-char -1)
      (beginning-of-line))
    (point)))

(defun neo--terminal-vterm-logical-line-end (&optional position)
  "Return the end of the logical vterm line at POSITION."
  (save-excursion
    (when position
      (goto-char position))
    (end-of-line)
    (while (get-text-property (point) 'vterm-line-wrap)
      (forward-char)
      (end-of-line))
    (point)))

(defun neo--terminal-vterm-logical-line-context (position)
  "Return normalized logical-line text and index for POSITION.

The returned plist contains the visible logical line text under `:text'
and the matching index for POSITION under `:position'."
  (let ((line-beg (neo--terminal-vterm-logical-line-beginning position))
        (line-end (neo--terminal-vterm-logical-line-end position))
        (parts nil)
        (text-index 0)
        (position-index nil))
    (save-excursion
      (goto-char line-beg)
      (while (< (point) line-end)
        (when (and (null position-index)
                   (= (point) position))
          (setq position-index text-index))
        (unless (get-text-property (point) 'vterm-line-wrap)
          (push (char-to-string (char-after)) parts)
          (setq text-index (1+ text-index)))
        (forward-char 1)))
    (when (null position-index)
      (setq position-index text-index))
    (list :text (apply #'concat (nreverse parts))
          :position position-index)))

(defun neo--terminal-vterm-reference-at-position (position)
  "Return the Codex file reference at POSITION, or nil."
  (let* ((context (neo--terminal-vterm-logical-line-context position))
         (text (plist-get context :text))
         (text-position (plist-get context :position))
         (match-start 0))
    (or
     (catch 'match
       (while (and (< match-start (length text))
                   (string-match neo--terminal-vterm-markdown-link-regexp
                                 text
                                 match-start))
         (when (and (<= (match-beginning 1) text-position)
                    (< text-position (match-end 1)))
           (let ((target (or (match-string-no-properties 2 text)
                             (match-string-no-properties 3 text))))
             (when-let ((location (neo--terminal-vterm-resolve-reference target)))
               (throw 'match location))))
         (setq match-start (match-end 1))))
     (catch 'match
       (setq match-start 0)
       (while (and (< match-start (length text))
                   (string-match neo--terminal-vterm-plain-reference-regexp
                                 text
                                 match-start))
         (when (and (<= (match-beginning 0) text-position)
                    (< text-position (match-end 0)))
           (when-let ((location
                       (neo--terminal-vterm-resolve-reference
                        (match-string-no-properties 0 text))))
             (throw 'match location)))
         (setq match-start (match-end 0)))))))

(defun neo--terminal-vterm-visit-location (location)
  "Visit LOCATION in the current Emacs session."
  (let ((file (plist-get location :file))
        (line (plist-get location :line))
        (column (plist-get location :column)))
    (find-file file)
    (when line
      (goto-char (point-min))
      (forward-line (1- line))
      (when column
        (move-to-column (1- column))))))

(defun neo/terminal-vterm-visit-codex-reference-at-point (&optional position)
  "Open the Codex file reference at POSITION or point."
  (interactive)
  (unless neo/terminal-vterm-enable-codex-links
    (user-error "Codex vterm link opening is disabled"))
  (let ((location
         (neo--terminal-vterm-reference-at-position (or position (point)))))
    (unless location
      (user-error "No Codex file reference at point"))
    (neo--terminal-vterm-visit-location location)))

(defun neo/terminal-vterm-visit-codex-reference-at-mouse (event)
  "Open the Codex file reference at mouse EVENT, or move point."
  (interactive "e")
  (let ((position (posn-point (event-end event))))
    (if (and neo/terminal-vterm-enable-codex-links
             (integer-or-marker-p position))
        (let ((location (neo--terminal-vterm-reference-at-position position)))
          (if location
              (neo--terminal-vterm-visit-location location)
            (vterm-mouse-set-point event)))
      (vterm-mouse-set-point event))))

;; (defun neo/toggle-eshell ()
;;   "Toggle Eshell.
;; If called from a non-Eshell buffer, open or switch to Eshell.
;; If called from an Eshell buffer, hide it — and if that Eshell buffer was displayed
;; in a dedicated frame created earlier, delete that frame as well (but do not kill the buffer)."
;;   (interactive)
;;   (if (derived-mode-p 'eshell-mode)
;;       (neo/eshell--bury-and-hide-frame)
;;     (eshell)))

;; TODO make this into a toggling thing and deine it inside eshell use-package
					;(key-chord-define-global ",t" #'neo/toggle-eshell)

(neo/use-package eshell-syntax-highlighting
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode t)
  :custom-face
  (eshell-syntax-highlighting-invalid-face ((t :inherit diff-error))))


;; (neo/use-package eshell-toggle
;;   :custom
;;   (eshell-toggle-size-fraction 3)
;;   (eshell-toggle-find-project-root-package 'projectile)
;;   (eshell-toggle-run-command nil)
;;   (eshell-toggle-init-function #'eshell-toggle-init-eshell)
;; 					;  :quelpa
;; 					;  (eshell-toggle :repo "4DA/eshell-toggle" :fetcher github :version original)
;;   :bind
;;   ("s-`" . eshell-toggle))

(neo/use-package vterm
  :custom
  (vterm-max-scrollback 100000)
  :bind
  (:map vterm-mode-map
        ("C-c C-o" . neo/terminal-vterm-visit-codex-reference-at-point)
        ([mouse-2] . neo/terminal-vterm-visit-codex-reference-at-mouse)))

;(neo/use-package vterm-toggle
;  :bind
;  (("C-c t"        . vterm-toggle)
;   :map vterm-mode-map
;   ("<C-return>" . vterm-toggle-insert-cd)
;   ("s-n" . vterm-toggle-forward)
;   ("s-p" . vterm-toggle-backward))
;  :config
;  (add-to-list 'display-buffer-alist
;	       '("\*vterm\*"
;		 (display-buffer-in-side-window)
;		 (window-height . 0.3)
;		 (side . bottom)
;		 (slot . 0)))
;  )

(neo/use-package direnv
  :config
  (direnv-mode))

;;; Note, no (provide 'neo-terminal) here, extensions are loaded not required.
