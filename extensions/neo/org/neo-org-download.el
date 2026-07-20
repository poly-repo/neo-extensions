;;; -*- lexical-binding: t -*-

(require 'neo-org-core)

(defcustom neo/org-enable-download nil
  "When non-nil, enable `org-download' integration."
  :type 'boolean
  :group 'neo-org)

(defcustom neo/org-download-image-dir nil
  "Directory where `org-download' should store pasted images.
When nil, use an `images/' subdirectory under `neo/org-directory'."
  :type '(choice (const :tag "Use base-directory images/" nil)
                 string)
  :group 'neo-org)

(defcustom neo/org-download-image-org-width 600
  "Default `#+ATTR_ORG' width for `org-download' image links."
  :type 'integer
  :group 'neo-org)

(defun neo--org-download-image-dir ()
  "Return the absolute directory used by `org-download'."
  (file-name-as-directory
   (neo--org-normalize-path
    (or (and (neo--org-string-present-p neo/org-download-image-dir)
             neo/org-download-image-dir)
        "images/")
    (neo--org-base-directory))))

(neo/use-package org-download
  :after org
  :if neo/org-enable-download
  :custom
  (org-download-method 'directory)
  (org-download-image-dir (neo--org-download-image-dir))
  (org-download-image-org-width neo/org-download-image-org-width)
  (org-download-link-format "[[file:%s]]\n")
  (org-download-abbreviate-filename-function #'file-relative-name)
  (org-download-link-format-function #'org-download-link-format-function-default))

(provide 'neo-org-download)
;;; neo-org-download.el ends here
