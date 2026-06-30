;;; neo-workflow-async.el --- Async refresh helpers for Neo Workflow -*- lexical-binding: t; -*-

;; Minimal async module.  The old workflow/ version used `async' + GitHub CLI;
;; this version's refresh is synchronous beads-client calls.
;; The hook infrastructure is kept so callers that run-hooks still work.

(require 'cl-lib)

(defvar neo-workflow-refresh-hook nil
  "Hook run after a data refresh to update the UI.")

(provide 'neo-workflow-async)
;;; neo-workflow-async.el ends here
