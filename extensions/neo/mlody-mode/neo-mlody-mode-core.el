;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'rx)
(require 'subr-x)

(defgroup neo/mlody-mode nil
  "Major-mode support for Haskell-style Mlody files."
  :group 'neo)

(defcustom neo/mlody-mode-indent-offset 2
  "Indentation width for `neo-mlody-mode'."
  :type 'integer
  :group 'neo/mlody-mode)

(defconst neo--mlody-mode-top-level-keywords
  '("from" "type" "value" "task" "layout" "location" "config")
  "Keywords that start top-level Mlody declarations.")

(defconst neo--mlody-mode-section-keywords
  '("inputs" "outputs" "config")
  "Keywords that start task body sections.")

(defconst neo--mlody-mode-builtin-constants
  '("true" "false")
  "Builtin constants in Mlody source.")

(defvar neo--mlody-mode-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\" "\"" table)
    table)
  "Syntax table for `neo-mlody-mode'.")

(defconst neo--mlody-mode-font-lock-keywords
  (list
   `(,(rx line-start (* space)
          (group (or "from" "type" "value" "task" "layout" "location" "config"))
          word-end)
     1 font-lock-keyword-face)
   `(,(rx line-start (* space) "from" (+ space)
          (group "//" (+ (not (any space ?\"))))
          (+ space) "import" word-end)
     1 font-lock-constant-face)
   `(,(rx line-start (* space) "from" (+ space)
          "//" (+ (not (any space ?\")))
          (+ space) (group "import") word-end)
     1 font-lock-keyword-face)
   `(,(rx line-start (* space)
          (group (or "inputs" "outputs" "config"))
          word-end)
     1 font-lock-keyword-face)
   `(,(rx line-start (* space)
          (group (or "type" "value" "layout" "location"))
          (+ space)
          (group (+ (or word ?_ ?-))))
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))
   `(,(rx line-start (* space)
          (group "task")
          (+ space)
          (group (+ (or word ?_ ?-))))
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
   `(,(rx line-start (* space)
          (group "config")
          (+ space)
          (group (+ (or word ?_ ?-))))
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))
   `(,(rx line-start (* space)
          (group (+ (or word ?_ ?- ?. ?/)))
          (* space)
          (or "::" ":" "="))
     1 font-lock-variable-name-face)
   `(,(rx symbol-start
          (group (or "where" "abstract" "base" "source" "type" "representation"
                     "layout" "location" "freshness" "choices" "canonical"
                     "validate" "predicate" "min" "max" "default" "description"
                     "attrs" "methods" "injectable" "duration" "schedule"
                     "unit" "group" "constraint"))
          symbol-end)
     1 font-lock-keyword-face)
   `(,(rx (or line-start (any space ?\( ?=))
          (group ":" (+ (or word ?_ ?- ?/))
                 (* "." (+ (or word ?_ ?- ?/)))))
     1 font-lock-constant-face)
   `(,(rx (or line-start (any space ?\( ?=))
          (group "//" (+ (not (any space ?\")))))
     1 font-lock-constant-face)
   `(,(rx symbol-start
          (group (+ (or word ?_ ?-))
                 ":" (+ (or word ?_ ?- ?/)))
          symbol-end)
     1 font-lock-builtin-face)
   `(,(regexp-opt neo--mlody-mode-builtin-constants 'symbols)
     . font-lock-constant-face)
   '("\\_<[0-9]+\\(?:\\.[0-9]+\\)?\\_>" . 'font-lock-number-face)
   '("^\\s-*--|?.*$" 0 font-lock-comment-face t))
  "Font-lock rules for `neo-mlody-mode'.")

(defconst neo--mlody-mode-entity-name-regexp
  (rx (+ (or word ?_ ?-)))
  "Regexp for a simple Mlody entity name.")

(defun neo--mlody-mode-strip-inline-comment (text)
  "Return TEXT without a trailing Mlody line comment."
  (let ((index 0)
        (length (length text))
        (in-string nil)
        (escaped nil)
        done)
    (while (and (< index length) (not done))
      (let ((char (aref text index)))
        (cond
         ((and in-string escaped)
          (setq escaped nil))
         ((and in-string (eq char ?\\))
          (setq escaped t))
         ((eq char ?\")
          (setq in-string (not in-string))
          (setq escaped nil))
         ((and (not in-string)
               (< (1+ index) length)
               (eq char ?-)
               (eq (aref text (1+ index)) ?-))
          (setq text (substring text 0 index))
          (setq done t))))
      (setq index (1+ index)))
    text))

(defun neo--mlody-mode-line-code (&optional position)
  "Return the trimmed code portion of the line at POSITION."
  (save-excursion
    (when position
      (goto-char position))
    (string-trim
     (neo--mlody-mode-strip-inline-comment
      (buffer-substring-no-properties (line-beginning-position)
                                      (line-end-position))))))

(defun neo--mlody-mode-classify-code (code)
  "Classify trimmed line CODE for indentation."
  (cond
   ((string-empty-p code) 'blank)
   ((string-match-p (rx string-start "--") code) 'comment)
   ((string-match-p
     (rx string-start "from" (+ space) (+ nonl) (+ space) "import" (* space) string-end)
     code)
    'top-import-open)
   ((string-match-p (rx string-start "task" (+ space) (+ nonl) (+ space) "where" (* space) string-end)
                    code)
    'top-task-open)
   ((string-match-p
     (rx string-start "type" (+ space)
         (+ (or word ?_ ?-))
         (+ space) ":" (+ nonl) (* space) string-end)
     code)
    'top-where-open)
   ((string-match-p
     (rx string-start (or "type" "value" "layout" "location" "config")
         (+ space) (+ (or word ?_ ?-))
         (* nonl) (+ space) "where" (* space) string-end)
     code)
    'top-where-open)
   ((string-match-p
     (rx string-start (or "type" "value" "task" "layout" "location" "config")
         (+ space) (+ (or word ?_ ?-))
         (* space) (* nonl) string-end)
     code)
    'top-decl)
   ((string-match-p (rx string-start (or "inputs" "outputs" "config") (* space) string-end)
                    code)
    'section-open)
   ((string-match-p
     (rx string-start (+ (or word ?_ ?-)) (* space) ":" (* space) string-end)
     code)
    'port-open)
   ((string-match-p
     (rx string-start (+ (or word ?_ ?-)) (* space) ":" (+ space) (+ nonl) string-end)
     code)
    'inline-port)
   ((string-match-p (rx string-start (+ nonl) (+ space) "where" (* space) string-end)
                    code)
    'nested-where-open)
   ((string-match-p (rx string-start (+ (or word ?_ ?-)) (* space) string-end) code)
    'import-item)
   (t 'body)))

(defun neo--mlody-mode-opening-context (kind)
  "Return the context opened by line KIND."
  (pcase kind
    ('top-import-open 'import)
    ('top-task-open 'task)
    ('top-where-open 'where)
    ('section-open 'section)
    ('port-open 'port)
    ('nested-where-open 'where)
    (_ nil)))

(defun neo--mlody-mode-push-context (stack type base-indent)
  "Push a TYPE context with BASE-INDENT onto STACK."
  (cons (list :type type
              :base-indent base-indent
              :child-indent (+ base-indent neo/mlody-mode-indent-offset))
        stack))

(defun neo--mlody-mode-pop-contexts (stack indent)
  "Pop contexts from STACK that do not contain INDENT."
  (while (and stack
              (> (plist-get (car stack) :child-indent) indent))
    (setq stack (cdr stack)))
  stack)

(defun neo--mlody-mode-context-stack (&optional target-position)
  "Return the indentation context stack before TARGET-POSITION."
  (let ((target-position (or target-position (line-beginning-position)))
        stack)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) target-position)
        (let* ((code (neo--mlody-mode-line-code))
               (kind (neo--mlody-mode-classify-code code)))
          (unless (memq kind '(blank comment))
            (setq stack (neo--mlody-mode-pop-contexts stack (current-indentation)))
            (when-let* ((context (neo--mlody-mode-opening-context kind)))
              (setq stack (neo--mlody-mode-push-context stack context
                                                        (current-indentation))))))
        (forward-line 1)))
    stack))

(defun neo--mlody-mode-nearest-context (stack type)
  "Return the nearest TYPE context in STACK."
  (cl-find-if (lambda (context)
                (eq (plist-get context :type) type))
              stack))

(defun neo--mlody-mode-current-child-indent (stack)
  "Return the child indent for the current STACK."
  (if stack
      (plist-get (car stack) :child-indent)
    0))

(defun neo--mlody-mode-preserved-comment-indent (stack)
  "Return the preferred indentation for a comment line in STACK."
  (let ((current (current-indentation)))
    (if (> current 0)
        current
      (neo--mlody-mode-current-child-indent stack))))

(defun neo--mlody-mode-calculate-indentation ()
  "Return the desired indentation for the current line."
  (let* ((code (neo--mlody-mode-line-code))
         (kind (neo--mlody-mode-classify-code code))
         (stack (neo--mlody-mode-context-stack)))
    (pcase kind
      ('blank
       0)
      ('comment
       (neo--mlody-mode-preserved-comment-indent stack))
      ((or 'top-decl 'top-import-open 'top-task-open 'top-where-open)
       0)
      ('import-item
       (if-let* ((context (neo--mlody-mode-nearest-context stack 'import)))
           (plist-get context :child-indent)
         neo/mlody-mode-indent-offset))
      ('section-open
       (if-let* ((context (neo--mlody-mode-nearest-context stack 'task)))
           (plist-get context :child-indent)
         neo/mlody-mode-indent-offset))
      ((or 'port-open 'inline-port)
       (cond
        ((neo--mlody-mode-nearest-context stack 'section)
         (plist-get (neo--mlody-mode-nearest-context stack 'section) :child-indent))
        (stack
         (neo--mlody-mode-current-child-indent stack))
        (t
         neo/mlody-mode-indent-offset)))
      ((or 'body 'nested-where-open)
       (let ((adjusted-stack
              (neo--mlody-mode-pop-contexts stack (current-indentation))))
         (neo--mlody-mode-current-child-indent
          (or adjusted-stack stack))))
      (_ 0))))

(defun neo/mlody-mode-indent-line ()
  "Indent the current line as Mlody code."
  (interactive)
  (let ((indent (neo--mlody-mode-calculate-indentation))
        (offset (- (point) (line-beginning-position))))
    (indent-line-to indent)
    (when (> offset 0)
      (goto-char (+ (line-beginning-position) offset)))))

(defun neo/mlody-mode-indent-region (start end)
  "Indent Mlody code between START and END."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (let ((end-marker (copy-marker end)))
      (while (< (point) end-marker)
        (neo/mlody-mode-indent-line)
        (forward-line 1)))))

(defun neo/mlody-mode-format-buffer ()
  "Format the current Mlody buffer by reindenting it."
  (interactive)
  (neo/mlody-mode-indent-region (point-min) (point-max)))

(defun neo/mlody-mode-format-region (start end)
  "Format the Mlody region between START and END."
  (interactive "r")
  (neo/mlody-mode-indent-region start end))

;;;###autoload
(define-derived-mode neo-mlody-mode prog-mode "MLody"
  "Major mode for editing Haskell-style Mlody source files."
  :syntax-table neo--mlody-mode-syntax-table
  (setq-local comment-start "-- ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "--+\\s-*")
  (setq-local font-lock-defaults '(neo--mlody-mode-font-lock-keywords))
  (setq-local indent-line-function #'neo/mlody-mode-indent-line)
  (setq-local indent-tabs-mode nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mlody\\'" . neo-mlody-mode))

(provide 'neo-mlody-mode-core)

;;; neo-mlody-mode-core.el ends here
