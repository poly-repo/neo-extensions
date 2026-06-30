;;; -*- lexical-binding: t -*-

;;; This is neo-workflow, a NEO extension.
;;;
;;; A beads + git backed reimplementation of the `workflow' board: the same
;;; projects -> stacks -> branches -> issues board, but issues come from beads
;;; (via `beads-client'), stacks are beads epics, and branches/worktrees are
;;; read live from git. No SQLite, no GitHub.
;;;
;;; Planned modules (ported/rewritten from neo/workflow as each phase lands):
;;;   neo-workflow-models.el    ;; cl-structs + beads/git loaders (no sqlite)
;;;   neo-workflow-db.el        ;; old API names, beads/git/in-memory bodies
;;;   neo-workflow-git.el       ;; git worktrees/branches (copied)
;;;   neo-workflow-status.el    ;; the board UI (copied, re-sourced)
;;;   neo-workflow-ui.el        ;; views (copied)
;;;   neo-workflow-issues.el    ;; create/update/close via beads-client
;;;   neo-workflow-context.el   ;; contexts via perspective.el (in-memory)
;;;   neo-workflow-project.el   ;; projects from the BEADS_DIR workspace(s)
;;;   neo-workflow-slug.el      ;; helpers (copied)
;;;   neo-workflow-async.el     ;; helpers (copied)

(require 'neo-utils)

;; The data backend. `beads-client' is provided by neo:programming-foundation,
;; which this extension `:requires', so it is on the load-path by the time the
;; extension loads inside NEO.
(require 'beads-client)

;; Phase 1 scaffold only: modules above are added as they are ported (see the
;; neo-workflow epic). full-monty is intentionally NOT flipped to this
;; extension until the read path works, so the existing `workflow' board keeps
;; working in the meantime.

;;; Note, no (provide 'neo-workflow) here, extensions are loaded not required.
