;;; -*- lexical-binding: t -*-

(require 'org)
(require 'org-element)
(require 'ob-core)
(require 'subr-x)

(declare-function neo--haskell-ensure-standalone-repl "neo-haskell" ())
(declare-function neo/org-haskell-export-latex "neo-org-haskell-pdf" ())
(declare-function neo/org-haskell-export-pdf "neo-org-haskell-pdf" (&optional preamble))

(defvar neo--haskell-standalone-repl-source-buffer)

(defconst neo--org-haskell-language "haskell"
  "Org source block language managed by the Haskell notebook helpers.")

(defconst neo--org-haskell-cache-directory "neo-org-haskell"
  "Cache subdirectory used for generated notebook source files.")

(defcustom neo/org-haskell-temporary-directory temporary-file-directory
  "Base directory for generated notebook artefacts.
The default is `temporary-file-directory', so tangled Haskell sources and
staged LaTeX/PDF build directories stay out of the workspace."
  :type 'directory
  :group 'neo-org)

(defvar neo/org-haskell-notebook-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") #'neo/org-haskell-switch-to-repl)
    (define-key map (kbd "C-c h z") #'neo/org-haskell-switch-to-repl)
    (define-key map (kbd "C-c h e") #'neo/org-haskell-export-latex)
    (define-key map (kbd "C-c h p") #'neo/org-haskell-export-pdf)
    (define-key map (kbd "C-c h s") #'neo/org-haskell-send-block)
    (define-key map (kbd "C-c h l") #'neo/org-haskell-load-document)
    (define-key map (kbd "C-c h t") #'neo/org-haskell-tangle-document)
    map)
  "Keymap for `neo/org-haskell-notebook-mode'.")

(defun neo--org-haskell-source-name ()
  "Return the current notebook source name for cache metadata."
  (or buffer-file-name
      (buffer-name)))

(defun neo--org-haskell-buffer-stem ()
  "Return a filesystem-safe stem for the current notebook buffer."
  (let* ((name (file-name-base (neo--org-haskell-source-name)))
         (slug (replace-regexp-in-string "[^[:alnum:]]+" "-" name)))
    (if (string-empty-p slug) "notebook" (string-trim slug "-" "-"))))

(defun neo--org-haskell-source-fingerprint ()
  "Return a short stable fingerprint for the current notebook source."
  (substring
   (md5 (expand-file-name (neo--org-haskell-source-name) default-directory))
   0
   12))

(defun neo--org-haskell-notebook-slug ()
  "Return a cache-safe slug for the current notebook buffer."
  (format "%s-%s"
          (neo--org-haskell-buffer-stem)
          (neo--org-haskell-source-fingerprint)))

(defun neo--org-haskell-artifact-root (&optional subdirectory)
  "Return the temp root used for generated notebook artefacts.
When SUBDIRECTORY is non-nil, resolve it under the shared notebook temp root."
  (let ((root
         (expand-file-name
          neo--org-haskell-cache-directory
          (file-name-as-directory
           (expand-file-name neo/org-haskell-temporary-directory)))))
    (if subdirectory
        (expand-file-name subdirectory root)
      root)))

(defun neo--org-haskell-generated-file-path ()
  "Return the generated `.hs' path for the current notebook buffer."
  (let ((filename (format "%s.hs" (neo--org-haskell-notebook-slug))))
    (expand-file-name filename
                      (neo--org-haskell-artifact-root "generated"))))

(defun neo--org-haskell-current-block-info ()
  "Return plist metadata for the Haskell source block at point."
  (let ((info (org-babel-get-src-block-info 'no-eval)))
    (unless info
      (user-error "neo-org: point is not in an Org source block"))
    (unless (string= (car info) neo--org-haskell-language)
      (user-error "neo-org: point is not in a Haskell source block"))
    (list :body (nth 1 info)
          :line (1+ (line-number-at-pos (nth 5 info))))))

(defun neo--org-haskell-collect-document-blocks ()
  "Return the current buffer's Haskell source blocks in document order."
  (let (blocks)
    (org-element-map (org-element-parse-buffer) 'src-block
      (lambda (src-block)
        (when (and (string= (org-element-property :language src-block)
                            neo--org-haskell-language)
                   (not (org-with-point-at (org-element-begin src-block)
                          (or (org-in-commented-heading-p)
                              (org-in-archived-heading-p)))))
          (let ((info (org-babel-get-src-block-info 'no-eval src-block)))
            (push (list :body (nth 1 info)
                        :line (1+ (line-number-at-pos
                                   (org-element-property :begin src-block))))
                  blocks)))))
    (unless blocks
      (user-error "neo-org: no Haskell source blocks found in this buffer"))
    (nreverse blocks)))

(defun neo--org-haskell-render-document (blocks)
  "Render BLOCKS into a generated Haskell source file."
  (let ((source (neo--org-haskell-source-name)))
    (concat
     (format "-- Generated from %s by neo/org-haskell-notebook-mode.\n\n" source)
     (mapconcat
      (lambda (block)
        (format "{-# LINE %d %S #-}\n%s"
                (plist-get block :line)
                source
                (plist-get block :body)))
      blocks
      "\n\n")
     "\n")))

(defun neo--org-haskell-write-document-file ()
  "Write the current notebook's Haskell blocks to a generated `.hs' file."
  (let* ((path (neo--org-haskell-generated-file-path))
         (content
          (neo--org-haskell-render-document
           (neo--org-haskell-collect-document-blocks))))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert content))
    path))

(defun neo--org-haskell-ensure-repl ()
  "Return the notebook REPL buffer, associating it with the current source."
  (unless (fboundp 'neo--haskell-ensure-standalone-repl)
    (user-error "neo-org: enable neo:haskell before using notebook REPL commands"))
  (let ((source-buffer (current-buffer))
        (repl-buffer (neo--haskell-ensure-standalone-repl)))
    (with-current-buffer repl-buffer
      (setq neo--haskell-standalone-repl-source-buffer source-buffer))
    repl-buffer))

(defun neo--org-haskell-send-string (string)
  "Send STRING to the notebook REPL and return its buffer."
  (let* ((repl-buffer (neo--org-haskell-ensure-repl))
         (process (get-buffer-process repl-buffer)))
    (unless process
      (user-error "neo-org: notebook REPL is not running"))
    (comint-send-string process string)
    repl-buffer))

(defun neo--org-haskell-format-interactive-snippet (body)
  "Wrap BODY for safe multi-line evaluation in GHCi."
  (format ":{\n%s\n:}\n" (string-trim-right body)))

;;;###autoload
(define-derived-mode neo/org-haskell-notebook-mode org-mode "Neo Org Haskell"
  "Major mode for Org buffers that act as Haskell notebooks.")

;;;###autoload
(defun neo/org-haskell-send-block ()
  "Send the Haskell source block at point to the inferior notebook REPL."
  (interactive)
  (let* ((block (neo--org-haskell-current-block-info))
         (repl-buffer
          (neo--org-haskell-send-string
           (neo--org-haskell-format-interactive-snippet
            (plist-get block :body)))))
    (message "neo-org: sent Haskell block to %s" (buffer-name repl-buffer))))

;;;###autoload
(defun neo/org-haskell-tangle-document ()
  "Write all Haskell source blocks in the current notebook to a cache file."
  (interactive)
  (let ((path (neo--org-haskell-write-document-file)))
    (when (called-interactively-p 'interactive)
      (message "neo-org: tangled Haskell notebook to %s" path))
    path))

;;;###autoload
(defun neo/org-haskell-load-document ()
  "Tangle the current notebook and load it into the inferior notebook REPL."
  (interactive)
  (let* ((path (neo/org-haskell-tangle-document))
         (repl-buffer
          (neo--org-haskell-send-string
           (format ":load %S\n" (expand-file-name path)))))
    (message "neo-org: loaded %s into %s" path (buffer-name repl-buffer))
    repl-buffer))

;;;###autoload
(defun neo/org-haskell-switch-to-repl (&optional no-load)
  "Switch to the notebook REPL.
With prefix argument NO-LOAD, switch without reloading the notebook."
  (interactive "P")
  (pop-to-buffer
   (if no-load
       (neo--org-haskell-ensure-repl)
     (neo/org-haskell-load-document))))

(provide 'neo-org-haskell)
;;; neo-org-haskell.el ends here
