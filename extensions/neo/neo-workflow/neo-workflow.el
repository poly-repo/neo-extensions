;;; -*- lexical-binding: t -*-

;;; This is neo-workflow, a NEO extension.
;;;
;;; A beads + git backed reimplementation of the `workflow' board: the same
;;; projects -> stacks -> branches -> issues board, but issues come from beads
;;; (via `beads-client'), stacks are beads epics, and branches/worktrees are
;;; read live from git. No SQLite, no GitHub.
;;;
;;; Modules:
;;;   neo-workflow-slug.el      ;; branch-slug helpers (copied from workflow/)
;;;   neo-workflow-git.el       ;; git worktrees / branch state
;;;   neo-workflow-models.el    ;; cl-structs + beads/git loaders
;;;   neo-workflow-db.el        ;; public API names, in-memory/beads bodies
;;;   neo-workflow-context.el   ;; workspace strategy via git state (no magit)
;;;   neo-workflow-async.el     ;; refresh hook infrastructure
;;;   neo-workflow-project.el   ;; project discovery from beads workspace
;;;   neo-workflow-issues.el    ;; create/update/close via beads-client
;;;   neo-workflow-ui.el        ;; tabulated-list stack summary view
;;;   neo-workflow-status.el    ;; the vtable board + app registration

(require 'neo-utils)

;; The data backend. `beads-client' is provided by neo:programming-foundation,
;; which this extension `:requires', so it is on the load-path by the time the
;; extension loads inside NEO.
(require 'beads-client)

;; Phase 2: full read path loaded.
;; full-monty is intentionally NOT flipped to this extension until the
;; read path is confirmed working — the existing `workflow' board continues
;; to run in the meantime.

(require 'neo-workflow-slug)
(require 'neo-workflow-git)
(require 'neo-workflow-models)
(require 'neo-workflow-db)
(require 'neo-workflow-context)
(require 'neo-workflow-async)
(require 'neo-workflow-project)
(require 'neo-workflow-issues)
(require 'neo-workflow-ui)
(require 'neo-workflow-status)

;;; Note, no (provide 'neo-workflow) here, extensions are loaded not required.
