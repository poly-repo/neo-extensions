;;; tests/test-neo-workflow-status.el --- Tests for neo-workflow-status -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'hl-line)
(require 'neo-workflow-status)
(require 'neo-workflow-models)
(require 'github-models)

(describe "neo-workflow-status"

  ;; Helper to create a mock label struct
  (defun neo--make-mock-label (name)
    (make-neo-label :name name))

  ;; Helper to create a mock issue struct with labels
  (defun neo--make-mock-issue (&rest label-names)
    (make-neo-issue :labels (mapcar #'neo--make-mock-label label-names)))

  (it "gets priority from labels"
    (let* ((labels1 (list (neo--make-mock-label "bug") (neo--make-mock-label "high")))
           (labels2 (list (neo--make-mock-label "feature")))
           (labels3 nil)
           (labels4 (list (neo--make-mock-label "low") (neo--make-mock-label "critical"))))
      (expect (neo--get-priority-from-labels labels1) :to-equal 'high)
      (expect (neo--get-priority-from-labels labels2) :to-equal nil)
      (expect (neo--get-priority-from-labels labels3) :to-equal nil)
      (expect (neo--get-priority-from-labels labels4) :to-equal 'critical)))

  (it "gets priority face"
    (expect (neo--get-priority-face 'critical) :to-equal 'neo-workflow-priority-critical-face)
    (expect (neo--get-priority-face 'high) :to-equal 'neo-workflow-priority-high-face)
    (expect (neo--get-priority-face 'medium) :to-equal 'neo-workflow-priority-medium-face)
    (expect (neo--get-priority-face 'low) :to-equal 'neo-workflow-priority-low-face)
    (expect (neo--get-priority-face 'other) :to-equal nil))

  (it "gets issue status face"
    (expect (neo--get-issue-status-face 'open) :to-equal 'neo-workflow-issue-open-face)
    (expect (neo--get-issue-status-face 'active) :to-equal 'neo-workflow-issue-active-face)
    (expect (neo--get-issue-status-face 'closed) :to-equal 'neo-workflow-issue-completed-face)
    (expect (neo--get-issue-status-face 'other) :to-equal nil))

  (it "gets priority string from issue"
    (expect (neo--priority (neo--make-mock-issue "bug" "high")) :to-equal "high")
    (expect (neo--priority (neo--make-mock-issue "bug" "low" "high")) :to-equal "high") ;; chooses most critical
    (expect (neo--priority (neo--make-mock-issue "bug")) :to-equal ""))

  (it "gets issue priority score"
    (expect (neo--get-issue-priority-score (neo--make-mock-issue "critical")) :to-equal 0)
    (expect (neo--get-issue-priority-score (neo--make-mock-issue "high")) :to-equal 1)
    (expect (neo--get-issue-priority-score (neo--make-mock-issue "mid")) :to-equal 2)
    (expect (neo--get-issue-priority-score (neo--make-mock-issue "low")) :to-equal 3)
    (expect (neo--get-issue-priority-score (neo--make-mock-issue "bug")) :to-equal 99))

  (it "sorts issues by priority"
    (let* ((issue-low (neo--make-mock-issue "low"))
           (issue-crit (neo--make-mock-issue "critical"))
           (issue-high (neo--make-mock-issue "high"))
           (issue-none (neo--make-mock-issue "bug"))
           (unsorted (list issue-low issue-crit issue-none issue-high)))
      (let ((neo/workflow-sort-by-priority t))
        (expect (neo--sort-issues unsorted) :to-equal (list issue-crit issue-high issue-low issue-none)))
      (let ((neo/workflow-sort-by-priority nil))
        (expect (neo--sort-issues unsorted) :to-equal unsorted))))

  (it "normalizes hex color strings"
    (expect (neo--hex-color "#aabbcc") :to-equal "#aabbcc")
    (expect (neo--hex-color "aabbcc") :to-equal "#aabbcc")
    (expect (neo--hex-color nil) :to-equal nil))

  (it "calculates next priority level"
    (let ((issue-low (neo--make-mock-issue "low"))
          (issue-high (neo--make-mock-issue "high"))
          (issue-crit (neo--make-mock-issue "critical"))
          (issue-none (neo--make-mock-issue "bug")))
      ;; Up
      (expect (neo--new-priority issue-low 1) :to-equal "mid")
      (expect (neo--new-priority issue-high 1) :to-equal "critical")
      (expect (neo--new-priority issue-crit 1) :to-equal "critical") ;; saturation
      (expect (neo--new-priority issue-none 1) :to-equal "low")
      ;; Down
      (expect (neo--new-priority issue-high -1) :to-equal "mid")
      (expect (neo--new-priority issue-low -1) :to-equal "") ;; to none
      (expect (neo--new-priority issue-none -1) :to-equal "")))

  (it "determines issue status from stack"
    (let ((issue-with-stack (make-neo-issue :stack (make-neo-stack)))
          (issue-no-stack (make-neo-issue)))
      (expect (neo--issue-status issue-with-stack) :to-equal "A")
      (expect (neo--issue-status issue-no-stack) :to-equal "")
      (expect (neo--active-issue-p issue-with-stack) :to-be-truthy)
      (expect (neo--active-issue-p issue-no-stack) :to-equal nil))))

(require 'neo-workflow-db)

(defun neo--normalize-lines-for-test (str)
  "Normalize whitespace in STR for less brittle testing.
Trims each line and collapses multiple spaces into one."
  (string-join
   (mapcar (lambda (line)
             (replace-regexp-in-string "[ 	]+" " " (string-trim line)))
           (split-string str "\n" t))
   "\n"))

(defun neo--status-test-db-load-schemas-from-file (db file)
  "Load schemas from FILE and initialize them in DB."
  (let ((schemas (with-temp-buffer
                   (insert-file-contents file)
                   (eval (read (current-buffer)) t))))
    (neo/workflow-db-init db schemas)))

(defun neo--status-test-insert-repo (full-name)
  "Insert a repository and return its ID."
  (sqlite-execute neo--db-conn "INSERT INTO repositories (full_name, fork, visibility) VALUES (?, ?, ?)" (list full-name 0 "public"))
  (caar (sqlite-select neo--db-conn "SELECT id FROM repositories WHERE full_name = ?" (list full-name))))

(defun neo--status-test-insert-issue (repo-id number title created-at labels)
  "Insert an issue with labels and return the issue struct."
  (sqlite-execute neo--db-conn "INSERT INTO issues (repository_id, number, title, type, state, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?, ?)"
                  (list repo-id number title "Issue" "open" created-at created-at))
  (let ((issue-id (caar (sqlite-select neo--db-conn "SELECT id FROM issues WHERE repository_id = ? AND number = ?" (list repo-id number)))))
    (dolist (label-name labels)
      (sqlite-execute neo--db-conn "INSERT OR IGNORE INTO labels (repository_id, name) VALUES (?, ?)" (list repo-id label-name))
      (let ((label-id (caar (sqlite-select neo--db-conn "SELECT id FROM labels WHERE repository_id = ? AND name = ?" (list repo-id label-name)))))
        (sqlite-execute neo--db-conn "INSERT INTO issue_labels (issue_id, label_id) VALUES (?, ?)" (list issue-id label-id))))
    (neo-load-issue issue-id)))

(defun neo--status-test-make-issue-active (issue)
  "Create a stack for an issue, making it 'active'."
  (let* ((issue-to-modify (copy-neo-issue issue))
         (stack-name (neo-issue-title-to-slug (neo-issue-number issue-to-modify) (neo-issue-title issue-to-modify))))
    (setf (neo-issue-stack issue-to-modify) (make-neo-stack :name stack-name :title (neo-issue-title issue-to-modify)))
    (neo-db-upsert-issue issue-to-modify)))

(defun neo--assert-buffer-content (&rest expected-lines)
  "Assert that the current buffer content matches EXPECTED-LINES.
This function normalizes whitespace and removes the header line before comparison."
  (let ((expected-content (string-join expected-lines "\n")))
    (let* ((buffer-content (buffer-string))
           (content-without-header (string-join (cdr (split-string buffer-content "\n" t)) "\n"))
           (normalized-actual (neo--normalize-lines-for-test content-without-header))
           (normalized-expected (neo--normalize-lines-for-test expected-content)))
      (unless (string= normalized-actual normalized-expected)
        (buttercup-fail "\nBuffer content mismatch.\nExpected:\n---\n%s\n---\nActual:\n---\n%s\n---"
                        normalized-expected
                        normalized-actual)))))

(describe "neo/workflow-refresh"
  (before-each
    ;; Use the global neo--db-conn which neo-open-db will pick up.
    (setq neo--db-conn (sqlite-open))
    (neo--status-test-db-load-schemas-from-file neo--db-conn (expand-file-name "../github-schemas.el" default-directory))
    (neo--ensure-schema neo--db-conn)
    (setq neo--repo-info-alist nil)
    (set-buffer (get-buffer-create "*test-workflow-status*"))
    (neo-workflow-status-mode))

  (after-each
    (when neo--db-conn (sqlite-close neo--db-conn))
    (setq neo--db-conn nil)
    (kill-buffer "*test-workflow-status*"))

  (it "orders issues correctly with priority sorting enabled"
    (let ((neo/workflow-sort-by-priority t))
      ;; Setup DB with 16 issues
      (let* ((repo-b (neo--status-test-insert-repo "owner/mixed-repo"))
             ;; Active Issues
             (issue-101 (neo--status-test-insert-issue repo-b 101 "Active P1 T0" "2023-01-16T12:00:00Z" '("high")))
             (issue-102 (neo--status-test-insert-issue repo-b 102 "Active P1 T8" "2023-01-08T12:00:00Z" '("high")))
             (issue-103 (neo--status-test-insert-issue repo-b 103 "Active P2 T1" "2023-01-15T12:00:00Z" '("mid")))
             (issue-104 (neo--status-test-insert-issue repo-b 104 "Active P2 T9" "2023-01-07T12:00:00Z" '("mid")))
             (issue-105 (neo--status-test-insert-issue repo-b 105 "Active P3 T2" "2023-01-14T12:00:00Z" '("low")))
             (issue-106 (neo--status-test-insert-issue repo-b 106 "Active P3 T10" "2023-01-06T12:00:00Z" '("low")))
             (issue-107 (neo--status-test-insert-issue repo-b 107 "Active NoP T3" "2023-01-13T12:00:00Z" '()))
             (issue-108 (neo--status-test-insert-issue repo-b 108 "Active NoP T11" "2023-01-05T12:00:00Z" '()))
             ;; Inactive Issues
             (issue-109 (neo--status-test-insert-issue repo-b 109 "Inactive P1 T4" "2023-01-12T12:00:00Z" '("high")))
             (issue-110 (neo--status-test-insert-issue repo-b 110 "Inactive P1 T12" "2023-01-04T12:00:00Z" '("high")))
             (issue-111 (neo--status-test-insert-issue repo-b 111 "Inactive P2 T5" "2023-01-11T12:00:00Z" '("mid")))
             (issue-112 (neo--status-test-insert-issue repo-b 112 "Inactive P2 T13" "2023-01-03T12:00:00Z" '("mid")))
             (issue-113 (neo--status-test-insert-issue repo-b 113 "Inactive P3 T6" "2023-01-10T12:00:00Z" '("low")))
             (issue-114 (neo--status-test-insert-issue repo-b 114 "Inactive P3 T14" "2023-01-02T12:00:00Z" '("low")))
             (issue-115 (neo--status-test-insert-issue repo-b 115 "Inactive NoP T7" "2023-01-09T12:00:00Z" '()))
             (issue-116 (neo--status-test-insert-issue repo-b 116 "Inactive NoP T15" "2023-01-01T12:00:00Z" '())))
        (neo--status-test-make-issue-active issue-101) (neo--status-test-make-issue-active issue-102)
        (neo--status-test-make-issue-active issue-103) (neo--status-test-make-issue-active issue-104)
        (neo--status-test-make-issue-active issue-105) (neo--status-test-make-issue-active issue-106)
        (neo--status-test-make-issue-active issue-107) (neo--status-test-make-issue-active issue-108))

      ;; Action
      (neo/workflow-refresh)

      ;; Assertion
      (neo--assert-buffer-content
       "101 P1 Active P1 T0"
       "102 P1 Active P1 T8"
       "103 P2 Active P2 T1"
       "104 P2 Active P2 T9"
       "105 P3 Active P3 T2"
       "106 P3 Active P3 T10"
       "107 Active NoP T3"
       "108 Active NoP T11"
       "109 P1 Inactive P1 T4"
       "110 P1 Inactive P1 T12"
       "111 P2 Inactive P2 T5"
       "112 P2 Inactive P2 T13"
       "113 P3 Inactive P3 T6"
       "114 P3 Inactive P3 T14"
       "115 Inactive NoP T7"
       "116 Inactive NoP T15")))

  (it "orders issues by date when priority sorting is disabled"
    (let ((neo/workflow-sort-by-priority nil))
      ;; 1. Setup
      (let* ((repo-b (neo--status-test-insert-repo "owner/mixed-repo"))
             (issue-101 (neo--status-test-insert-issue repo-b 101 "Active P1 T0" "2023-01-16T12:00:00Z" '("high")))
             (issue-102 (neo--status-test-insert-issue repo-b 102 "Active P1 T8" "2023-01-08T12:00:00Z" '("high")))
             (issue-103 (neo--status-test-insert-issue repo-b 103 "Active P2 T1" "2023-01-15T12:00:00Z" '("mid")))
             (issue-104 (neo--status-test-insert-issue repo-b 104 "Active P2 T9" "2023-01-07T12:00:00Z" '("mid")))
             (issue-105 (neo--status-test-insert-issue repo-b 105 "Active P3 T2" "2023-01-14T12:00:00Z" '("low")))
             (issue-106 (neo--status-test-insert-issue repo-b 106 "Active P3 T10" "2023-01-06T12:00:00Z" '("low")))
             (issue-107 (neo--status-test-insert-issue repo-b 107 "Active NoP T3" "2023-01-13T12:00:00Z" '()))
             (issue-108 (neo--status-test-insert-issue repo-b 108 "Active NoP T11" "2023-01-05T12:00:00Z" '()))
             (issue-109 (neo--status-test-insert-issue repo-b 109 "Inactive P1 T4" "2023-01-12T12:00:00Z" '("high")))
             (issue-110 (neo--status-test-insert-issue repo-b 110 "Inactive P1 T12" "2023-01-04T12:00:00Z" '("high")))
             (issue-111 (neo--status-test-insert-issue repo-b 111 "Inactive P2 T5" "2023-01-11T12:00:00Z" '("mid")))
             (issue-112 (neo--status-test-insert-issue repo-b 112 "Inactive P2 T13" "2023-01-03T12:00:00Z" '("mid")))
             (issue-113 (neo--status-test-insert-issue repo-b 113 "Inactive P3 T6" "2023-01-10T12:00:00Z" '("low")))
             (issue-114 (neo--status-test-insert-issue repo-b 114 "Inactive P3 T14" "2023-01-02T12:00:00Z" '("low")))
             (issue-115 (neo--status-test-insert-issue repo-b 115 "Inactive NoP T7" "2023-01-09T12:00:00Z" '()))
             (issue-116 (neo--status-test-insert-issue repo-b 116 "Inactive NoP T15" "2023-01-01T12:00:00Z" '())))
        (neo--status-test-make-issue-active issue-101) (neo--status-test-make-issue-active issue-102)
        (neo--status-test-make-issue-active issue-103) (neo--status-test-make-issue-active issue-104)
        (neo--status-test-make-issue-active issue-105) (neo--status-test-make-issue-active issue-106)
        (neo--status-test-make-issue-active issue-107) (neo--status-test-make-issue-active issue-108))

      ;; Action
      (neo/workflow-refresh)

      ;; Assertion
      (neo--assert-buffer-content
       "101 P1 Active P1 T0"
       "103 P2 Active P2 T1"
       "105 P3 Active P3 T2"
       "107 Active NoP T3"
       "102 P1 Active P1 T8"
       "104 P2 Active P2 T9"
       "106 P3 Active P3 T10"
       "108 Active NoP T11"
       "109 P1 Inactive P1 T4"
       "111 P2 Inactive P2 T5"
       "113 P3 Inactive P3 T6"
       "115 Inactive NoP T7"
       "110 P1 Inactive P1 T12"
       "112 P2 Inactive P2 T13"
       "114 P3 Inactive P3 T14"
       "116 Inactive NoP T15"))))

(describe "activating an issue"
  (before-each
    ;; Use the global neo--db-conn which neo-open-db will pick up.


    (setq neo--db-conn (sqlite-open))
    (neo--status-test-db-load-schemas-from-file neo--db-conn (expand-file-name "../github-schemas.el" default-directory))
    (neo--ensure-schema neo--db-conn)
    (setq neo--repo-info-alist nil)
    (set-buffer (get-buffer-create "*test-workflow-status*"))
    (neo-workflow-status-mode))

  (after-each
    (when neo--db-conn (sqlite-close neo--db-conn))
    (setq neo--db-conn nil)
    (kill-buffer "*test-workflow-status*"))

  (it "moves the issue to the active section and sorts by priority"
    (let ((neo/workflow-sort-by-priority t))
      ;; 1. Setup
      (let* ((repo-b (neo--status-test-insert-repo "owner/mixed-repo"))
             (issue-101 (neo--status-test-insert-issue repo-b 101 "Active P1 T0" "2023-01-16T12:00:00Z" '("high")))
             (issue-102 (neo--status-test-insert-issue repo-b 102 "Active P1 T8" "2023-01-08T12:00:00Z" '("high")))
             (issue-103 (neo--status-test-insert-issue repo-b 103 "Active P2 T1" "2023-01-15T12:00:00Z" '("mid")))
             (issue-104 (neo--status-test-insert-issue repo-b 104 "Active P2 T9" "2023-01-07T12:00:00Z" '("mid")))
             (issue-105 (neo--status-test-insert-issue repo-b 105 "Active P3 T2" "2023-01-14T12:00:00Z" '("low")))
             (issue-106 (neo--status-test-insert-issue repo-b 106 "Active P3 T10" "2023-01-06T12:00:00Z" '("low")))
             (issue-107 (neo--status-test-insert-issue repo-b 107 "Active NoP T3" "2023-01-13T12:00:00Z" '()))
             (issue-108 (neo--status-test-insert-issue repo-b 108 "Active NoP T11" "2023-01-05T12:00:00Z" '()))
             (issue-109 (neo--status-test-insert-issue repo-b 109 "Inactive P1 T4" "2023-01-12T12:00:00Z" '("high")))
             (issue-110 (neo--status-test-insert-issue repo-b 110 "Inactive P1 T12" "2023-01-04T12:00:00Z" '("high")))
             (issue-111 (neo--status-test-insert-issue repo-b 111 "Inactive P2 T5" "2023-01-11T12:00:00Z" '("mid")))
             (issue-112 (neo--status-test-insert-issue repo-b 112 "Inactive P2 T13" "2023-01-03T12:00:00Z" '("mid")))
             (issue-113 (neo--status-test-insert-issue repo-b 113 "Inactive P3 T6" "2023-01-10T12:00:00Z" '("low")))
             (issue-114 (neo--status-test-insert-issue repo-b 114 "Inactive P3 T14" "2023-01-02T12:00:00Z" '("low")))
             (issue-115 (neo--status-test-insert-issue repo-b 115 "Inactive NoP T7" "2023-01-09T12:00:00Z" '()))
             (issue-116 (neo--status-test-insert-issue repo-b 116 "Inactive NoP T15" "2023-01-01T12:00:00Z" '())))
        (neo--status-test-make-issue-active issue-101) (neo--status-test-make-issue-active issue-102)
        (neo--status-test-make-issue-active issue-103) (neo--status-test-make-issue-active issue-104)
        (neo--status-test-make-issue-active issue-105) (neo--status-test-make-issue-active issue-106)
        (neo--status-test-make-issue-active issue-107) (neo--status-test-make-issue-active issue-108))

      ;; 2. Initial render
      (neo/workflow-refresh)

      ;; -- VERIFY INITIAL STATE --
      (neo--assert-buffer-content
       "101 P1 Active P1 T0"
       "102 P1 Active P1 T8"
       "103 P2 Active P2 T1"
       "104 P2 Active P2 T9"
       "105 P3 Active P3 T2"
       "106 P3 Active P3 T10"
       "107 Active NoP T3"
       "108 Active NoP T11"
       "109 P1 Inactive P1 T4"
       "110 P1 Inactive P1 T12"
       "111 P2 Inactive P2 T5"
       "112 P2 Inactive P2 T13"
       "113 P3 Inactive P3 T6"
       "114 P3 Inactive P3 T14"
       "115 Inactive NoP T7"
       "116 Inactive NoP T15")

      ;; 3. Action: activate issue #111
      (let* ((table (neo-workflow-get-table-for-repo "owner/mixed-repo"))
	     (target-object (seq-find (lambda (obj) (and (neo-issue-p obj) (= (neo-issue-number obj) 111)))
                                      (vtable-objects table))))
        (vtable-goto-object target-object)
        (neo--hack target-object))

      ;; -- VERIFY POINT AFTER REFRESH --
      (let* ((refreshed-table (neo-workflow-get-table-for-repo "owner/mixed-repo"))
             (activated-issue (seq-find (lambda (obj) (and (neo-issue-p obj) (= (neo-issue-number obj) 111)))
                                        (vtable-objects refreshed-table))))
        (expect activated-issue :to-be-truthy)
        (expect (vtable-current-object) :to-equal activated-issue))

      ;; 4. Assertion
      (neo--assert-buffer-content
       "101 P1 Active P1 T0"
       "102 P1 Active P1 T8"
       "103 P2 Active P2 T1"
       "111 P2 Inactive P2 T5"
       "104 P2 Active P2 T9"
       "105 P3 Active P3 T2"
       "106 P3 Active P3 T10"
       "107 Active NoP T3"
       "108 Active NoP T11"
       "109 P1 Inactive P1 T4"
       "110 P1 Inactive P1 T12"
       "112 P2 Inactive P2 T13"
       "113 P3 Inactive P3 T6"
       "114 P3 Inactive P3 T14"
       "115 Inactive NoP T7"
       "116 Inactive NoP T15")))

  (it "moves the issue to the active section and sorts by date"
    (let ((neo/workflow-sort-by-priority nil))
      ;; 1. Setup
      (let* ((repo-b (neo--status-test-insert-repo "owner/mixed-repo"))
             (issue-101 (neo--status-test-insert-issue repo-b 101 "Active P1 T0" "2023-01-16T12:00:00Z" '("high")))
             (issue-102 (neo--status-test-insert-issue repo-b 102 "Active P1 T8" "2023-01-08T12:00:00Z" '("high")))
             (issue-103 (neo--status-test-insert-issue repo-b 103 "Active P2 T1" "2023-01-15T12:00:00Z" '("mid")))
             (issue-104 (neo--status-test-insert-issue repo-b 104 "Active P2 T9" "2023-01-07T12:00:00Z" '("mid")))
             (issue-105 (neo--status-test-insert-issue repo-b 105 "Active P3 T2" "2023-01-14T12:00:00Z" '("low")))
             (issue-106 (neo--status-test-insert-issue repo-b 106 "Active P3 T10" "2023-01-06T12:00:00Z" '("low")))
             (issue-107 (neo--status-test-insert-issue repo-b 107 "Active NoP T3" "2023-01-13T12:00:00Z" '()))
             (issue-108 (neo--status-test-insert-issue repo-b 108 "Active NoP T11" "2023-01-05T12:00:00Z" '()))
             (issue-109 (neo--status-test-insert-issue repo-b 109 "Inactive P1 T4" "2023-01-12T12:00:00Z" '("high")))
             (issue-110 (neo--status-test-insert-issue repo-b 110 "Inactive P1 T12" "2023-01-04T12:00:00Z" '("high")))
             (issue-111 (neo--status-test-insert-issue repo-b 111 "Inactive P2 T5" "2023-01-11T12:00:00Z" '("mid")))
             (issue-112 (neo--status-test-insert-issue repo-b 112 "Inactive P2 T13" "2023-01-03T12:00:00Z" '("mid")))
             (issue-113 (neo--status-test-insert-issue repo-b 113 "Inactive P3 T6" "2023-01-10T12:00:00Z" '("low")))
             (issue-114 (neo--status-test-insert-issue repo-b 114 "Inactive P3 T14" "2023-01-02T12:00:00Z" '("low")))
             (issue-115 (neo--status-test-insert-issue repo-b 115 "Inactive NoP T7" "2023-01-09T12:00:00Z" '()))
             (issue-116 (neo--status-test-insert-issue repo-b 116 "Inactive NoP T15" "2023-01-01T12:00:00Z" '())))
        (neo--status-test-make-issue-active issue-101) (neo--status-test-make-issue-active issue-102)
        (neo--status-test-make-issue-active issue-103) (neo--status-test-make-issue-active issue-104)
        (neo--status-test-make-issue-active issue-105) (neo--status-test-make-issue-active issue-106)
        (neo--status-test-make-issue-active issue-107) (neo--status-test-make-issue-active issue-108))

      ;; 2. Initial render
      (neo/workflow-refresh)

      ;; -- VERIFY INITIAL STATE --
      (neo--assert-buffer-content
       "101 P1 Active P1 T0"
       "103 P2 Active P2 T1"
       "105 P3 Active P3 T2"
       "107 Active NoP T3"
       "102 P1 Active P1 T8"
       "104 P2 Active P2 T9"
       "106 P3 Active P3 T10"
       "108 Active NoP T11"
       "109 P1 Inactive P1 T4"
       "111 P2 Inactive P2 T5"
       "113 P3 Inactive P3 T6"
       "115 Inactive NoP T7"
       "110 P1 Inactive P1 T12"
       "112 P2 Inactive P2 T13"
       "114 P3 Inactive P3 T14"
       "116 Inactive NoP T15")

      ;; 3. Action: activate issue #111
      (let* ((table (neo-workflow-get-table-for-repo "owner/mixed-repo"))
	     (target-object (seq-find (lambda (obj) (and (neo-issue-p obj) (= (neo-issue-number obj) 111)))
                                      (vtable-objects table))))
        (vtable-goto-object target-object)
        (neo--hack target-object))

      ;; -- VERIFY POINT AFTER REFRESH --
      (let* ((refreshed-table (neo-workflow-get-table-for-repo "owner/mixed-repo"))
             (activated-issue (seq-find (lambda (obj) (and (neo-issue-p obj) (= (neo-issue-number obj) 111)))
                                        (vtable-objects refreshed-table))))
        (expect activated-issue :to-be-truthy)
        (expect (vtable-current-object) :to-equal activated-issue))

      ;; 4. Assertion
      (neo--assert-buffer-content
       "101 P1 Active P1 T0"
       "103 P2 Active P2 T1"
       "105 P3 Active P3 T2"
       "107 Active NoP T3"
       "111 P2 Inactive P2 T5"
       "102 P1 Active P1 T8"
       "104 P2 Active P2 T9"
       "106 P3 Active P3 T10"
       "108 Active NoP T11"
       "109 P1 Inactive P1 T4"
       "113 P3 Inactive P3 T6"
       "115 Inactive NoP T7"
       "110 P1 Inactive P1 T12"
       "112 P2 Inactive P2 T13"
       "114 P3 Inactive P3 T14"
       "116 Inactive NoP T15"))))

(describe "neo--expand-stack"
  (it "should correctly compute prefixes for a nested stack"
    (let* ((gc1 (make-neo-stack :name "gc1"))
           (gc2 (make-neo-stack :name "gc2"))
           (child1 (make-neo-stack :name "child1"))
           (child2 (make-neo-stack :name "child2"))
           (root (make-neo-stack :name "root")))
      (setf (neo-stack-children-stacks child1) (list gc1 gc2))
      (setf (neo-stack-children-stacks root) (list child1 child2))
      (let ((expanded (neo--expand-stack root)))
        (expect (neo-stack-prefix (cl-find-if (lambda (s) (string= (neo-stack-name s) "root")) expanded)) :to-equal "")
        (expect (neo-stack-prefix (cl-find-if (lambda (s) (string= (neo-stack-name s) "child1")) expanded)) :to-equal "├─ ")
        (expect (neo-stack-prefix (cl-find-if (lambda (s) (string= (neo-stack-name s) "gc1")) expanded)) :to-equal "│  ├─ ")
        (expect (neo-stack-prefix (cl-find-if (lambda (s) (string= (neo-stack-name s) "gc2")) expanded)) :to-equal "│  └─ ")
        (expect (neo-stack-prefix (cl-find-if (lambda (s) (string= (neo-stack-name s) "child2")) expanded)) :to-equal "└─ ")))))

;; (describe "priority changes preserve stack"
;;   (before-each
;;     (setq neo--db-conn (sqlite-open))
;;     (neo--status-test-db-load-schemas-from-file neo--db-conn (expand-file-name "../github-schemas.el" default-directory))
;;     (neo--ensure-schema neo--db-conn)
;;     (setq neo--repo-info-alist nil)
;;     (set-buffer (get-buffer-create "*test-workflow-status*"))
;;     (neo-workflow-status-mode))
  
;;   (after-each
;;     (when neo--db-conn (sqlite-close neo--db-conn))
;;     (setq neo--db-conn nil)
;;     (kill-buffer "*test-workflow-status*"))

;;   (it "should not detach stack when priority is changed"
;;     (cl-letf (((symbol-function 'neo-workflow-update-issue)
;;                (lambda (issue table old-issue &rest props)
;;                  (let* ((repo-id (neo-issue-repository-id issue))
;;                         (repo-full-name (neo--workflow-get-repo-full-name-by-id repo-id))
;;                         (updated-issue (copy-neo-issue issue))
;;                         (label-names (plist-get props :labels)))

;;                    (when label-names
;;                      (setf (neo-issue-labels updated-issue)
;;                            (mapcar (lambda (name)
;;                                      (let* ((label-info (car (sqlite-select (neo-open-db)
;;                                                                             "SELECT id, name, color, description, repository_id FROM labels WHERE name = ? AND repository_id = ?"
;;                                                                             (list name repo-id)))))
;;                                        (if label-info
;;                                            (make-neo-label :id (nth 0 label-info)
;;                                                            :name (nth 1 label-info)
;;                                                            :color (nth 2 label-info)
;;                                                            :description (nth 3 label-info)
;;                                                            :repository-id (nth 4 label-info))
;;                                          (progn
;;                                            (sqlite-execute (neo-open-db) "INSERT OR IGNORE INTO labels (repository_id, name) VALUES (?, ?)" (list repo-id name))
;;                                            (let ((new-label-info (car (sqlite-select (neo-open-db) "SELECT id, name, color, description, repository_id FROM labels WHERE name = ? AND repository_id = ?" (list name repo-id)))))
;;                                              (make-neo-label :id (nth 0 new-label-info)
;;                                                              :name (nth 1 new-label-info)
;;                                                              :color (nth 2 new-label-info)
;;                                                              :description (nth 3 new-label-info)
;;                                                              :repository-id (nth 4 new-label-info)))))))
;;                                    label-names)))
;;                    (setf (neo-issue-stack updated-issue) (neo-issue-stack issue))
;;                    (neo-db-upsert-issue updated-issue)
;;                    (neo/workflow-refresh repo-full-name (neo-issue-id updated-issue))))))
;;       (let* ((repo-id (neo--status-test-insert-repo "owner/test-repo"))
;;              (issue (neo--status-test-insert-issue repo-id 1 "Test Issue" "2023-01-01T12:00:00Z" '())))
;;         ;; Activate the issue (give it a stack)
;;         (neo--status-test-make-issue-active issue)

;;         ;; Refresh to ensure the stack is loaded
;;         (neo/workflow-refresh (neo--workflow-get-repo-full-name-by-id repo-id) (neo-issue-id issue))
;;         (let* ((reloaded-issue (neo-db-get-issue-by-number repo-id 1)))
;;           (expect (neo-issue-stack reloaded-issue) :to-be-truthy))

;;         ;; Change its priority
;; ;        (neo--priority-change issue 1)

;;         ;; Refresh and re-check if it's still active (has a stack)
;;         (neo/workflow-refresh (neo--workflow-get-repo-full-name-by-id repo-id) (neo-issue-id issue))
;;         (let* ((reloaded-issue-after-change (neo-db-get-issue-by-number repo-id 1)))
;;           (expect (neo-issue-stack reloaded-issue-after-change) :to-be-truthy)
;;           (expect (neo--priority reloaded-issue-after-change) :to-equal "low"))))))
