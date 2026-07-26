;;; -*- lexical-binding: t -*-

;;; This is writer, a NEO extension
;;;
;;; Support for writing prose

(defcustom neo/writer-dictionary-directory
  (expand-file-name "~/.local/dictionaries/")
  "Root directory containing dictionaries indexed by Johnson.
Johnson scans this directory recursively."
  :type 'directory
  :group 'neo)

(defun neo--writer-ensure-dictionary-directory ()
  "Create `neo/writer-dictionary-directory' when it does not exist."
  (make-directory (expand-file-name neo/writer-dictionary-directory) t))

(neo/use-package jinx
  :hook
  ((text-mode prog-mode) . jinx-mode)
  :bind
  ("C-;" . jinx-correct)
  :custom
  (jinx-camel-modes '(prog-mode))
  (jinx-delay 0.01))

(neo/use-package vertico-multiform
  :ensure nil
  :after vertico
  :config
  (add-to-list 'vertico-multiform-categories
               '(jinx grid
                      (vertico-grid-annotate . 20)
                      (vertico-count . 4)))
  (vertico-multiform-mode 1))

(neo/use-package dictionary
  :commands dictionary-search
  :custom
  (dictionary-use-single-buffer t)
  (dictionary-server "dict.org"))

(neo/use-package johnson
  :ensure (:host github
           :repo "benthamite/johnson")
  :commands (johnson-index johnson-lookup johnson-menu)
  :bind
  (("C-c d" . johnson-lookup)
   ("C-c j" . johnson-menu))
  :init
  (neo--writer-ensure-dictionary-directory)
  :custom
  (johnson-cache-directory (neo/cache-file-path "johnson/"))
  (johnson-dictionary-directories
   (list (expand-file-name neo/writer-dictionary-directory))))

(neo/use-package olivetti
  :commands olivetti-mode)

(neo/use-package selectric-mode
  :commands selectric-mode)

;;; Note, no (provide 'neo-writer) here, extensions are loaded not required.
