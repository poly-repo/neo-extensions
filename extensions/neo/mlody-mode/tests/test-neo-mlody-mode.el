;;; tests/test-neo-mlody-mode.el --- Tests for neo-mlody-mode -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)

(defconst neo--mlody-mode-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing `test-neo-mlody-mode.el'.")

(defun neo--mlody-mode-test-face-has-comment-p (position)
  "Return non-nil when POSITION has `font-lock-comment-face'."
  (let ((face (get-text-property position 'face)))
    (if (listp face)
        (memq 'font-lock-comment-face face)
      (eq face 'font-lock-comment-face))))

(load-file (expand-file-name "../neo-mlody-mode.el"
                             neo--mlody-mode-test-dir))

(describe "neo-mlody-mode"
  (it "registers .mlody files for the new mode"
    (expect (assoc-default "workspace.mlody" auto-mode-alist #'string-match)
            :to-equal 'neo-mlody-mode))

  (it "keeps .mlody ownership after the legacy mode loads"
    (load-file (expand-file-name "../../mlody/neo-mlody-starlark-mode.el"
                                 neo--mlody-mode-test-dir))
    (expect (assoc-default "workspace.mlody" auto-mode-alist #'string-match)
            :to-equal 'neo-mlody-mode))

  (it "highlights declaration keywords and names"
    (with-temp-buffer
      (insert "value bootstrap_snapshot where\n")
      (neo-mlody-mode)
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "value")
      (expect (get-text-property (match-beginning 0) 'face)
              :to-equal 'font-lock-keyword-face)
      (search-forward "bootstrap_snapshot")
      (expect (get-text-property (match-beginning 0) 'face)
              :to-equal 'font-lock-type-face)))

  (it "highlights doc comments and regular comments"
    (with-temp-buffer
      (insert "--| top-level workspace model\n-- regular comment\n")
      (neo-mlody-mode)
      (font-lock-ensure)
      (goto-char (point-min))
      (expect (get-text-property (point) 'face)
              :to-equal 'font-lock-comment-face)
      (forward-line 1)
      (expect (get-text-property (point) 'face)
              :to-equal 'font-lock-comment-face)))

  (it "keeps full comment face on comment lines with strings and keywords"
    (with-temp-buffer
      (insert "--  location = posix where\n--    path = \"~/.cache/mlody/artifacts/emloyees.csv\"\n")
      (neo-mlody-mode)
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "where")
      (expect (neo--mlody-mode-test-face-has-comment-p (match-beginning 0))
              :to-be-truthy)
      (forward-line 1)
      (search-forward "path")
      (expect (neo--mlody-mode-test-face-has-comment-p (match-beginning 0))
              :to-be-truthy)
      (search-forward "~/.cache/mlody/artifacts/emloyees.csv")
      (expect (neo--mlody-mode-test-face-has-comment-p (match-beginning 0))
              :to-be-truthy)))

  (it "formats the prompt example with stable indentation"
    (with-temp-buffer
      (insert "from //common/locations import\n  remote\n  posix\n\nfrom //common/types import\n  vector\n  string\n\nvalue bootstrap_snapshot where\n  type = vector\n  source = remote where\n    uri = \"https://raw.githubusercontent.com/apache/airflow/main/airflow-core/docs/tutorial/pipeline_example.csv\"\n  location = posix\n--  location = posix where\n--    path = \"~/.cache/mlody/artifacts/emloyees.csv\"\n  representation = csv\n  freshness = ttl where\n    duration = \"5m\"\n\ntask summarize_snapshot where\n  inputs\n    source_snapshot:\n      type = vector\n      source = bootstrap_snapshot\n  outputs\n    summary:\n      type = string\n      representation = json\n\nconfig value_defaults where\n  :bootstrap_snapshot.freshness.duration = \"10m\"\n")
      (neo-mlody-mode)
      (neo/mlody-mode-format-buffer)
      (expect (buffer-string)
              :to-equal
              "from //common/locations import\n  remote\n  posix\n\nfrom //common/types import\n  vector\n  string\n\nvalue bootstrap_snapshot where\n  type = vector\n  source = remote where\n    uri = \"https://raw.githubusercontent.com/apache/airflow/main/airflow-core/docs/tutorial/pipeline_example.csv\"\n  location = posix\n--  location = posix where\n--    path = \"~/.cache/mlody/artifacts/emloyees.csv\"\n  representation = csv\n  freshness = ttl where\n    duration = \"5m\"\n\ntask summarize_snapshot where\n  inputs\n    source_snapshot:\n      type = vector\n      source = bootstrap_snapshot\n  outputs\n    summary:\n      type = string\n      representation = json\n\nconfig value_defaults where\n  :bootstrap_snapshot.freshness.duration = \"10m\"\n")))

  (it "indents nested type bodies and task sections"
    (with-temp-buffer
      (insert "type yolo26 : struct\n  config :: huggingface_config\n\ntask process where\n  inputs\n    source_snapshot:\n      type = yolo26\n      source = bootstrap_snapshot\n    thresholds: type = string\n  outputs\n    published_report:\n      type = yolo26\n      representation = csv\n      location = s3 where\n        bucket = \"hooli-sandbox\"\n        prefix = \"metrics\"\n")
      (neo-mlody-mode)
      (neo/mlody-mode-format-buffer)
      (expect (buffer-string)
              :to-equal
              "type yolo26 : struct\n  config :: huggingface_config\n\ntask process where\n  inputs\n    source_snapshot:\n      type = yolo26\n      source = bootstrap_snapshot\n    thresholds: type = string\n  outputs\n    published_report:\n      type = yolo26\n      representation = csv\n      location = s3 where\n        bucket = \"hooli-sandbox\"\n        prefix = \"metrics\"\n")))

  (it "updates full-monty to depend on neo:mlody-mode instead of neo:mlody"
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "../../full-monty/manifest.el"
                         neo--mlody-mode-test-dir))
      (let ((manifest (buffer-string)))
        (expect manifest :to-match "\"neo:mlody-mode\"")
        (expect manifest :not :to-match "\"neo:mlody\"")))))

(provide 'test-neo-mlody-mode)
;;; test-neo-mlody-mode.el ends here
