;;; -*- lexical-binding: t -*-

;;; This is terminal, a NEO extension
;;;
;;; Terminal emulation in Emacs

(neo/use-package eat)

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

(advice-add 'eshell/cat :override #'aweshell-cat-with-syntax-highlight)


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

(setq eshell-prompt-function
      (lambda ()
        (concat (eshell/pwd) "\n $ ")))


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
  )

(neo/use-package vterm-toggle
  :bind
  (("C-c t"        . vterm-toggle)
   :map vterm-mode-map
   ("<C-return>" . vterm-toggle-insert-cd)
   ("s-n" . vterm-toggle-forward)
   ("s-p" . vterm-toggle-backward))
  :config
  (add-to-list 'display-buffer-alist
	       '("\*vterm\*"
		 (display-buffer-in-side-window)
		 (window-height . 0.3)
		 (side . bottom)
		 (slot . 0)))
  )

(use-package direnv
  :config
  (direnv-mode))

;;; Note, no (provide 'neo-terminal) here, extensions are loaded not required.
