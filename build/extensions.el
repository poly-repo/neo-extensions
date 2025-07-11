;;; build/extensions.el --- Batch build for questionable_defaults

(setq input-file "extensions/uno/questionable_defaults/manifest.el")
(setq output-file "dist/neo-extensions.el")
(setq n 10000)

(with-temp-buffer
  (dotimes (_ n)
    (insert-file-contents input-file)
    (goto-char (point-max)))
  (write-region (point-min) (point-max) output-file))

;;; extensions.el ends here



