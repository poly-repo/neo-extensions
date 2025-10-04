(require 'eshell)
(require 'iimage)

(defun iimage-mode-refresh (command args)
  (when (string= command "echo")
    (let ((image-path (cons default-directory iimage-mode-image-search-path))
          file)
      (with-silent-modifications
        (save-excursion
          (eshell-previous-prompt 0) ; Start scanning from most recent prompt
                                     ; to avoid having to re-display all images.
          (dolist (pair iimage-mode-image-regex-alist)
            (while (re-search-forward (car pair) nil t)
              (when (and (setq file (match-string (cdr pair)))
                         (setq file (locate-file file image-path)))
                (add-text-properties (match-beginning 0) (match-end 0)
                                     `(display ,(create-image file)
                                               modification-hooks
                                               (iimage-modification-hook)))))))))))

(add-hook 'eshell-named-command-hook 'iimage-mode-refresh)

(defun my/iimage-mode-refresh--eshell/cat (orig-fun &rest args)
  "Display image when using cat on it."
  (let ((image-path (cons default-directory iimage-mode-image-search-path)))
    (dolist (arg args)
      (let ((imagep nil)
            file)
        (with-silent-modifications
          (save-excursion
            (dolist (pair iimage-mode-image-regex-alist)
              (when (and (not imagep)
                         (string-match (car pair) arg)
                         (setq file (match-string (cdr pair) arg))
                         (setq file (locate-file file image-path)))
                (setq imagep t)
                (add-text-properties 0 (length arg)
                                     `(display ,(create-image file)
                                               modification-hooks
                                               (iimage-modification-hook))
                                     arg)
                (eshell-buffered-print arg)
                (eshell-flush)))))
        (when (not imagep)
          (apply orig-fun (list arg)))))
    (eshell-flush)))

(advice-add 'eshell/cat :around #'my/iimage-mode-refresh--eshell/cat)


(require 'cl-lib)

(defun mlody--echo (&rest args)
  "Helper to print to eshell."
  (apply #'eshell/echo args))

(defun mlody--usage ()
  (mlody--echo
   "Usage: mlody [--verbose|-v] [--dry-run|-d] <subcommand> [subcommand-options] [args...]"
   ""
   "Subcommands:"
   "  eval      Evaluate an Emacs Lisp expression. Usage: mlody eval [--print|-p] EXPR..."
   "  describe  Describe a symbol. Usage: mlody describe [--doc|-D] SYMBOL..."
   "  resume    Resume a named task. Usage: mlody resume [--force|-f] TASK-NAME..."
   ""
   "Use mlody <subcommand> --help for subcommand-specific help."))

;; ---------------------------
;; Subcommand handlers
;; ---------------------------

(defun mlody--cmd-eval (global-plist subargs)
  "Handle `mlody eval` subcommand.
GLOBAL-PLIST is a plist returned from global parsing (e.g. :verbose :dry-run).
SUBARGS is the token list after the subcommand."
  (let* ;; NOTE: spec is a literal quoted list here (required because the macro
         ;; `eshell-eval-using-options` needs a literal at macroexpansion time).
        ((opts (eshell-eval-using-options
                "mlody eval" subargs
                '((?p "print" nil print "Print result")
                  (?h "help" nil help "Show help"))
                nil nil))
         (print-res (plist-get opts :print))
         (help (plist-get opts :help))
         (arguments (plist-get opts :arguments))
         (dry-run (plist-get global-plist :dry-run))
         (verbose (plist-get global-plist :verbose)))
    (when help
      (mlody--echo "mlody eval [--print|-p] EXPR...  -- Evaluate EXPR(s) and optionally print result")
      (cl-return-from mlody--cmd-eval nil))
    (if (null arguments)
        (mlody--echo "mlody eval: missing EXPR")
      (when verbose
        (mlody--echo (format "[mlody eval] dry-run=%S, printing=%S, exprs=%S" dry-run print-res arguments)))
      (dolist (expr-tok arguments)
        (let ((expr-str expr-tok))
          (if dry-run
              (mlody--echo (format "[dry-run] would evaluate: %s" expr-str))
            (let ((res (condition-case err
                           (eval (read expr-str))
                         (error (format "Error: %S" err)))))
              (when print-res
                (mlody--echo (format "%s => %S" expr-str res))))))))))

(defun mlody--cmd-describe (global-plist subargs)
  "Handle `mlody describe` subcommand.
GLOBAL-PLIST is a plist returned from global parsing (e.g. :verbose :dry-run).
SUBARGS is the token list after the subcommand."
  (let* ((opts (eshell-eval-using-options
                "mlody describe" subargs
                '((?D "doc" nil doc "Include docstring")
                  (?h "help" nil help "Show help"))
                nil nil))
         (show-doc (plist-get opts :doc))
         (help (plist-get opts :help))
         (arguments (plist-get opts :arguments))
         (verbose (plist-get global-plist :verbose)))
    (when help
      (mlody--echo "mlody describe [--doc|-D] SYMBOL...  -- Describe the given symbol(s)")
      (cl-return-from mlody--cmd-describe nil))
    (if (null arguments)
        (mlody--echo "mlody describe: missing SYMBOL")
      (when verbose
        (mlody--echo (format "[mlody describe] show-doc=%S args=%S" show-doc arguments)))
      (dolist (sym-name arguments)
        (let* ((sym (intern-soft sym-name)))
          (if (null sym)
              (mlody--echo (format "No symbol named %s" sym-name))
            (mlody--echo (format "Symbol: %s" sym))
            (when (fboundp sym)
              (mlody--echo (format "  Function signature/doc (trimmed): %s"
                                   (or (let ((d (documentation sym t)))
                                         (and d (truncate-string-to-width d 200)))
                                       "<no doc>"))))
            (when (boundp sym)
              (mlody--echo (format "  Variable value: %S" (symbol-value sym))))
            (when show-doc
              (let ((doc (or (documentation sym t)
                             (documentation-property sym 'variable-documentation t))))
                (mlody--echo (format "  Doc: %s" (or doc "<none>")))))))))))

(defun mlody--cmd-resume (global-plist subargs)
  "Handle `mlody resume` subcommand.
GLOBAL-PLIST is a plist returned from global parsing (e.g. :verbose :dry-run).
SUBARGS is the token list after the subcommand."
  (let* ((opts (eshell-eval-using-options
                "mlody resume" subargs
                '((?f "force" nil force "Force resume")
                  (?h "help" nil help "Show help"))
                nil nil))
         (force (plist-get opts :force))
         (help (plist-get opts :help))
         (arguments (plist-get opts :arguments))
         (dry-run (plist-get global-plist :dry-run))
         (verbose (plist-get global-plist :verbose)))
    (when help
      (mlody--echo "mlody resume [--force|-f] TASK-NAME...  -- Resume named tasks")
      (cl-return-from mlody--cmd-resume nil))
    (if (null arguments)
        (mlody--echo "mlody resume: missing TASK-NAME")
      (when verbose
        (mlody--echo (format "[mlody resume] force=%S dry-run=%S tasks=%S" force dry-run arguments)))
      (dolist (task arguments)
        (if dry-run
            (mlody--echo (format "[dry-run] would resume task: %s%s" task (if force " (forced)" "")))
          ;; Replace the following with your real resume logic.
          (mlody--echo (format "Resuming task: %s%s" task (if force " (forced)" ""))))))))

;; ---------------------------
;; Dispatcher
;; ---------------------------

;;;###autoload
(defun eshell/mlody (&rest args)
  "Eshell command `mlody' with subcommands.

Global options (before subcommand):
  -v, --verbose    Enable verbose output.
  -d, --dry-run    Dry run (don't perform side effects).

Subcommands: eval, describe, resume.
The remaining commandline (after subcommand parsing) is made available to the handler
via `eshell-eval-using-options' returned plist under :arguments."
  ;; IMPORTANT: global SPEC must be a literal list here as well.
  (let* ((gopts (eshell-eval-using-options
                 "mlody" args
                 '((?v "verbose" nil verbose "Enable verbose output")
                   (?d "dry-run" nil dry-run "Dry run only")
                   (?h "help" nil help "Show help"))
                 nil nil))
         (verbose (plist-get gopts :verbose))
         (dry-run (plist-get gopts :dry-run))
         (help (plist-get gopts :help))
         (rest (plist-get gopts :arguments))) ; leftover args after global parsing
    (when help
      (mlody--usage)
      (cl-return-from eshell/mlody nil))
    (when verbose (mlody--echo "[mlody] verbose mode"))
    (when dry-run (mlody--echo "[mlody] dry-run mode (no side effects)"))

    ;; dispatch
    (let ((sub (car rest)))
      (unless sub
        (mlody--usage)
        (cl-return-from eshell/mlody nil))
      (let* ((subargs (cdr rest))
             (sub-name (downcase (format "%s" sub)))
             (global-plist (list :verbose verbose :dry-run dry-run)))
        (cond
         ((string= sub-name "eval")
          (mlody--cmd-eval global-plist subargs))
         ((string= sub-name "describe")
          (mlody--cmd-describe global-plist subargs))
         ((string= sub-name "resume")
          (mlody--cmd-resume global-plist subargs))
         (t
          (mlody--echo (format "Unknown subcommand: %s" sub-name))
          (mlody--usage)))))))

;; optional: provide completion for subcommands in eshell
(with-eval-after-load 'em-hist
  (defun mlody--eshell-complete (string _predicate _flag)
    "Completion function for `mlody` subcommands."
    (let ((subs '("eval" "describe" "resume")))
      (all-completions string subs)))
  (add-to-list 'eshell-command-completions-alist '("mlody" . mlody--eshell-complete)))
