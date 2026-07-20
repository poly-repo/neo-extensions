;;; -*- lexical-binding: t -*-

(defvar neo/org-capture-delete-frame-depth 0
  "Number of Org capture exit events to wait before deleting the frame.")

(defun neo/org-capture-delete-frame-after-exit (&optional count)
  "Delete the current frame after COUNT Org capture exit events.
COUNT defaults to 1."
  (setq neo/org-capture-delete-frame-depth (max 1 (or count 1))))

(defun neo--org-capture-delete-frame-if-needed (&rest _args)
  "Delete the current frame when `neo/org-capture-delete-frame-depth' expires."
  (cond
   ((<= neo/org-capture-delete-frame-depth 0) nil)
   ((> neo/org-capture-delete-frame-depth 1)
    (setq neo/org-capture-delete-frame-depth
          (1- neo/org-capture-delete-frame-depth)))
   (t
    (setq neo/org-capture-delete-frame-depth 0)
    (delete-frame))))

(neo/use-package org-capture
  :builtin t
  :after org
  :config
  (advice-add 'org-capture-finalize :after #'neo--org-capture-delete-frame-if-needed)
  (advice-add 'org-capture-kill :after #'neo--org-capture-delete-frame-if-needed)
  (advice-add 'org-capture-refile :after #'neo--org-capture-delete-frame-if-needed))

(provide 'neo-org-capture)
;;; neo-org-capture.el ends here
