;;; eshell-history-ignore.el --- Ignore certain commands in eshell history -*- lexical-binding: t; -*-

(require 'cl-lib)

(defgroup neo/eshell nil
  "Eshell history filtering helpers."
  :group 'eshell)

(defcustom neo/eshell-history-ignore-commands
  '("ls" "cd")
  "List of command names (bare words) to ignore in Eshell history.
Each command will match when it appears at the start of the input and is
followed by either whitespace or the end of the line, e.g. \"ls\", \"ls -la\",
\"cd\", \"cd   /tmp\"."
  :type '(repeat string)
  :group 'neo/eshell)

(defun neo/eshell--commands->regexps (commands)
  "Return a list of regexps matching COMMANDS at start of line followed by space or EOL.
Each regexp matches expressions like:
  ^cmd$    or
  ^cmd<whitespace>...
so 'ls', 'ls -la', 'cd', 'cd    /path' will match, but 'lsomething' will not."
  (mapcar (lambda (cmd)
            (let ((q (regexp-quote cmd)))
              (concat "^" q "\\(?:\\s-.*\\|$\\)")))
          commands))

(defun neo/eshell-history-filter (input)
  "Filter INPUT for `eshell-input-filter-functions'.
Return non-nil to keep INPUT in history; nil to drop it.

This uses `neo/eshell-history-ignore-commands` to build regexps of the
form `^cmd(?:\\s-.*|$)` so commands at the start of the line that are
followed by whitespace or the end-of-line are filtered out.

Also respects `eshell-input-filter-default` and `eshell-input-filter-initial-space`."
  (let ((ignore-res (neo/eshell--commands->regexps neo/eshell-history-ignore-commands)))
    (and
     (eshell-input-filter-default input)
     (eshell-input-filter-initial-space input)
     (not (cl-some (lambda (re) (string-match-p re input)) ignore-res)))))

(provide 'neo-eshell-history-ignore)
;;; neo-eshell-history-ignore.el ends here
