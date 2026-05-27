;;; beads-backend-test.el --- Tests for beads-backend -*- lexical-binding: t -*-

(require 'ert)
(require 'beads-backend)

(ert-deftest beads-backend/alist-get-matches-string-keys ()
  (should
   (equal
    (beads-backend--alist-get 'issue_type '(("issue_type" . "task")))
    "task"))
  (should
   (equal
    (beads-backend--alist-delete 'title
                                 '(("title" . "probe")
                                   ("priority" . 2)))
    '(("priority" . 2)))))

(ert-deftest beads-backend/alist-to-cli-flags-accepts-string-keys ()
  (should
   (equal
    (beads-backend--alist-to-cli-flags
     '(("issue_type" . "task")
       ("priority" . 2)
       ("labels" . ("bug" "ui"))))
    '("--issue-type" "task"
      "--priority" "2"
      "--labels" "bug"
      "--labels" "ui"))))

(ert-deftest beads-backend/build-cli-args-filters-string-keys ()
  (should
   (equal
    (beads-backend--build-cli-args
     "list"
     '(("status" . "open")
       ("title_contains" . "neo")
       ("ignored" . "value"))
     '(status title_contains))
    '("list"
      "--status" "open"
      "--title-contains" "neo"))))

(ert-deftest beads-backend/bd-create-args-accept-string-keys ()
  (should
   (equal
    (beads-backend-bd--operation-to-cli-args
     "create"
     '(("title" . "Probe")
       ("issue_type" . "task")
       ("priority" . 2)
       ("dry_run" . t)))
    '("create" "Probe"
      "--type" "task"
      "--priority" "2"
      "--dry-run"))))

(ert-deftest beads-backend/bd-count-args-translate-grouping ()
  (should
   (equal
    (beads-backend-bd--operation-to-cli-args
     "count"
     '(("status" . "open")
       ("group_by" . "type")))
    '("count"
      "--status" "open"
      "--by-type"))))

(provide 'beads-backend-test)
;;; beads-backend-test.el ends here
