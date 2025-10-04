;; (use-package shackle)
;; (setq shackle-rules '(("\\`\\*e?shell" :regexp t :frame t)))
;; (setq shackle-rules '(('eshell-mode :frame t)))

;;; Create an Eshell in a fresh frame with controlled geometry,
;;; and delete that frame when the Eshell buffer is killed or buried.


(use-package eat)

(defun eshell/q ()
  (bury-buffer))

(defun eshell/d ()
  (dired "."))

;(eshell/alias "clear" "clear t")

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

(use-package dired
  :ensure nil				; builtin
  :bind (:map dired-mode-map ("t" . (lambda () (interactive) (eshell t)))))

(neo/add-current-file-dir-to-load-path)
(require 'terminal-history)
(require 'terminal-popup)

;; TODO make sure fortune is available. In the wizard offer installation or opting out
(use-package eshell
  :ensure nil				; builtin
  :after eat
  :config
  (eshell-history-size 100000)
  (eshell-directory-name (no-littering-expand-var-file-name "eshell"))
  (eshell-input-filter #'neo/eshell-history-filter)
  (eshell-ls-use-colors t)
  :hook
  (eshell-load . #'eat-eshell-mode)
  (eshell-banner-load . (lambda () (setq eshell-banner-message (concat (shell-command-to-string "fortune -s") "\n\n")))))

(use-package eshell-fringe-status
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
      (with-current-buffer buf
        (when (and (local-variable-p 'my/eshell-created-frame)
                   (frame-live-p my/eshell-created-frame))
          (let ((f my/eshell-created-frame))
            (kill-local-variable 'my/eshell-created-frame)
            (when (> (length (frame-list)) 1)
              (ignore-errors (delete-frame f)))))))
    ;; Bury without prompting
    (when (buffer-live-p buf)
      (let ((next-buf (or (and (buffer-live-p (next-buffer)) (next-buffer))
                          (other-buffer buf t))))
        (when next-buf
          (switch-to-buffer next-buf))))))


(defun neo/toggle-eshell ()
  "Toggle Eshell.
If called from a non-Eshell buffer, open or switch to Eshell.
If called from an Eshell buffer, hide it — and if that Eshell buffer was displayed
in a dedicated frame created earlier, delete that frame as well (but do not kill the buffer)."
  (interactive)
  (if (derived-mode-p 'eshell-mode)
      (neo/eshell--bury-and-hide-frame)
    (eshell)))

;; TODO make this into a toggling thing and deine it inside eshell use-package
(key-chord-define-global ",t" #'neo/toggle-eshell)

(setq eshell-prompt-function
      (lambda ()
        (concat (eshell/pwd) "\n $ ")))


(use-package eshell-syntax-highlighting
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode t)
  :custom-face
  (eshell-syntax-highlighting-invalid-face ((t :inherit diff-error))))

  
(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root 'project) ;; for in-built project.el
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell)
;  :quelpa
;  (eshell-toggle :repo "4DA/eshell-toggle" :fetcher github :version original)
  :bind
  ("s-`" . eshell-toggle))

