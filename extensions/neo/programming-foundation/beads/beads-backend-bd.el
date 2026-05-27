;;; beads-backend-bd.el --- bd backend for Beads -*- lexical-binding: t -*-

;; Copyright (C) 2025 Christian Tietze

;; Author: Christian Tietze
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Backend implementation for the bd (beads) CLI.
;; Current bd releases are CLI-only from Neo's perspective.

;;; Code:

(require 'beads-backend)

(defun beads-backend-bd--translate-key (key)
  "Translate client KEY to the current bd CLI flag name."
  (pcase (beads-backend--key-name key)
    ("issue_type" "type")
    ("acceptance_criteria" "acceptance")
    ("dependencies" "deps")
    ("add_labels" "add_label")
    ("remove_labels" "remove_label")
    (_ (beads-backend--key-name key))))

(defun beads-backend-bd--translate-args (args)
  "Translate client ARGS to the current bd CLI argument schema."
  (mapcar (lambda (pair)
            (cons (beads-backend-bd--translate-key (car pair))
                  (cdr pair)))
          args))

(defun beads-backend-bd--operation-to-cli-args (operation args)
  "Convert RPC OPERATION and ARGS to CLI arguments for bd."
  (setq args (beads-backend-bd--translate-args args))
  (pcase operation
    ("health"
     '("ping"))
    ("list"
     (beads-backend--build-cli-args "list" args
                                    '(status priority type assignee
                                      labels limit title_contains parent)))
    ("show"
     (let ((id (beads-backend--alist-get 'id args)))
       (list "show" id)))
    ("ready"
     (beads-backend--build-cli-args "ready" args
                                    '(assignee priority limit sort_policy parent type)))
    ("create"
     (let ((title (beads-backend--alist-get 'title args))
           (other-args (beads-backend--alist-delete 'title args)))
       (append (list "create" title)
               (beads-backend--alist-to-cli-flags other-args))))
    ("update"
     (let ((id (beads-backend--alist-get 'id args))
           (other-args (beads-backend--alist-delete 'id args)))
       (append (list "update" id)
               (beads-backend--alist-to-cli-flags other-args))))
    ("close"
     (let ((id (beads-backend--alist-get 'id args))
           (reason (beads-backend--alist-get 'reason args)))
       (if reason
           (list "close" id "--reason" reason)
         (list "close" id))))
    ("delete"
     (let ((ids (beads-backend--alist-get 'ids args))
           (force (beads-backend--alist-get 'force args)))
       (append (list "delete")
               (if (listp ids) ids (list ids))
               (when force '("--force")))))
    ("stats"
     '("stats"))
    ("count"
     (append
      (beads-backend--build-cli-args "count" args '(status priority type assignee labels))
      (pcase (beads-backend--alist-get 'group_by args)
        ("status" '("--by-status"))
        ("priority" '("--by-priority"))
        ("type" '("--by-type"))
        ("label" '("--by-label"))
        (_ nil))))
    ("dep_add"
     (let ((from-id (beads-backend--alist-get 'from_id args))
           (to-id (beads-backend--alist-get 'to_id args))
           (dep-type (beads-backend--alist-get 'dep_type args)))
       (if dep-type
           (list "dep" "add" from-id to-id "--type" dep-type)
         (list "dep" "add" from-id to-id))))
    ("dep_remove"
     (let ((from-id (beads-backend--alist-get 'from_id args))
           (to-id (beads-backend--alist-get 'to_id args)))
       (list "dep" "remove" from-id to-id)))
    ("dep_tree"
     (let ((id (beads-backend--alist-get 'id args))
           (max-depth (beads-backend--alist-get 'max_depth args)))
       (if max-depth
           (list "dep" "tree" id "--max-depth" (number-to-string max-depth))
         (list "dep" "tree" id))))
    ("label_add"
     (let ((id (beads-backend--alist-get 'id args))
           (label (beads-backend--alist-get 'label args)))
       (list "label" "add" id label)))
    ("label_remove"
     (let ((id (beads-backend--alist-get 'id args))
           (label (beads-backend--alist-get 'label args)))
       (list "label" "remove" id label)))
    ("get_mutations"
     (let ((since-id (beads-backend--alist-get 'since_id args)))
       (if since-id
           (list "mutations" "--since" since-id)
         '("mutations"))))
    ("types"
     '("types"))
    ("config_get"
     (let ((key (beads-backend--alist-get 'key args)))
       (list "config" "get" key)))
    ("config_set"
     (let ((key (beads-backend--alist-get 'key args))
           (value (beads-backend--alist-get 'value args)))
       (list "config" "set" key value)))
    ("config_unset"
     (let ((key (beads-backend--alist-get 'key args)))
       (list "config" "unset" key)))
    ("resolve-conflicts"
     (let ((dry-run (beads-backend--alist-get 'dry_run args)))
       (if dry-run
           '("resolve-conflicts" "--dry-run")
         '("resolve-conflicts"))))
    ("duplicates"
     '("duplicates"))
    ("duplicate"
     (let ((source-id (beads-backend--alist-get 'source_id args))
           (target-id (beads-backend--alist-get 'target_id args)))
       (list "duplicate" source-id "--of" target-id)))
    ("comments-add"
     (let ((id (beads-backend--alist-get 'id args))
           (text (beads-backend--alist-get 'text args)))
       (list "comments" "add" id text)))
    (_
     (signal 'beads-backend-error
             (list (format "Unknown operation for bd backend: %s" operation))))))

(defconst beads-backend-bd
  (make-beads-backend
   :name "bd"
   :cli-program "bd"
   :supports-daemon nil
   :socket-name nil
   :daemon-start-args nil
   :supported-ops '("health" "list" "show" "ready" "create" "update" "close"
                     "delete" "stats" "count" "dep_add" "dep_remove" "dep_tree"
                     "label_add" "label_remove" "get_mutations" "types"
                     "config_get" "config_set" "config_unset"
                     "resolve-conflicts" "duplicates" "duplicate"
                     "comments-add")
   :op-to-cli-args #'beads-backend-bd--operation-to-cli-args
   :cli-extra-flags nil))

(beads-backend-register beads-backend-bd)

(provide 'beads-backend-bd)
;;; beads-backend-bd.el ends here
