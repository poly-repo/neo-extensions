;;; -*- lexical-binding: t -*-

;;; This is workflow, a NEO extension
;;;
;;; Workflow Manager

;; neo-workflow/
;; ├── neo-workflow-db.el          ;; SQLite layer, schema, basic CRUD
;; ├── neo-workflow-models.el      ;; CL structs and functions manipulating them
;; ├── neo-workflow-git.el         ;; Git interaction, worktrees, branch status
;; ├── neo-workflow-github.el      ;; PRs, issues, CI status, GitHub API integration
;; ├── neo-workflow-ui.el          ;; Tabulated-list-mode views, stacks, projects, branches
;; ├── neo-workflow-auto.el        ;; Auto-merge / CI-gate workflow integration
;; ├── neo-workflow-utils.el       ;; Helpers: date/time formatting, sorting, filtering
;; └── neo-workflow.el             ;; Main entrypoint: load all modules, user-facing commands

(require 'neo-utils)

(require 'sqlite)

;; TODO use neo/use/package once we load this properly
(use-package ghub :ensure t)

(elpaca-wait)

;(neo/add-current-file-dir-to-load-path)	; TODO development only. Once NEO loads this extension, this is not needed
;(add-to-list 'load-path "/home/mav/.local/share/wtrees/mav-209-mvp-workflow-manager/devex/editors/emacs/extensions/extensions/neo/workflow")

(require 'neo-workflow-db)
(require 'neo-workflow-models)
(require 'neo-workflow-status)
(require 'neo-workflow-issues)
(require 'neo-workflow-project)
(require 'neo-workflow-git)
(require 'neo-workflow-context)


					;(require 'neo-workflow-db)
					;(require 'neo-workflow-models)
					;(require 'neo-workflow-ui)

;;; Note, no (provide 'neo-workflow) here, extensions are loaded not required.
