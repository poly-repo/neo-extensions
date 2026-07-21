;;; tests/test-neo-questionable-defaults.el --- Tests for neo-questionable-defaults -*- lexical-binding: t; -*-

(require 'buttercup)

(defconst neo--questionable-defaults-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing `test-neo-questionable-defaults.el'.")

(describe "neo-questionable-defaults"
  (it "enables global auto-revert instead of a buffer-local mode"
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "../neo-questionable-defaults.el"
                         neo--questionable-defaults-test-dir))
      (goto-char (point-min))
      (expect (re-search-forward
               "^  (global-auto-revert-mode 1)$"
               nil
               t)
              :not :to-be nil)
      (goto-char (point-min))
      (expect (re-search-forward
               "^  (auto-revert-mode 1)$"
               nil
               t)
              :to-be nil))))

(provide 'test-neo-questionable-defaults)
;;; test-neo-questionable-defaults.el ends here
