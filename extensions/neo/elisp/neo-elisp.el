;;; -*- lexical-binding: t -*-

;;; This is elisp, a NEO extension
;;;
;;; The Elisp Survival Kit

(require 'button)

(defconst neo--elisp-use-package-button-properties
  '(action button button-data category follow-link help-echo)
  "Properties added to `neo/use-package' package-name buttons.")

(defvar-local neo--elisp-use-package-buttons-enabled-p nil
  "Whether `neo/use-package' package-name buttons are enabled.")

(defun neo--elisp-browse-package (package)
  "Browse PACKAGE using its Elpaca menu URL when available."
  (let* ((item (elpaca-menu-item package))
         (url (plist-get item :url)))
    (if url
        (browse-url url)
      (elpaca-browse package))))

(defun neo--elisp-use-package-button-matcher (limit)
  "Make the next `neo/use-package' package name before LIMIT clickable."
  (catch 'matched
    (while (re-search-forward "(\\s-*neo/use-package\\_>" limit t)
      (let ((form-start (match-beginning 0))
            (name-end (match-end 0)))
        (let ((in-comment-or-string (nth 8 (syntax-ppss form-start))))
          (goto-char name-end)
          (unless in-comment-or-string
            (let ((advanced t))
              (while (and advanced (< (point) limit))
                (let ((start (point)))
                  (forward-comment 1)
                  (when (> (point) limit)
                    (goto-char limit))
                  (setq advanced (> (point) start)))))
            (when (and (< (point) limit)
                       (looking-at "\\(?:\\sw\\|\\s_\\)+"))
              (let* ((start (match-beginning 0))
                     (end (match-end 0))
                     (package (intern (match-string-no-properties 0))))
                (with-silent-modifications
                  (make-text-button
                   start end
                   'button-data package
                   'action #'neo--elisp-browse-package
                   'follow-link t
                   'help-echo (format "Browse %s with Elpaca" package)))
                (set-match-data (list start end))
                (goto-char end)
                (throw 'matched t)))))))))

(defun neo--elisp-enable-use-package-buttons ()
  "Make package names in `neo/use-package' forms clickable."
  (unless neo--elisp-use-package-buttons-enabled-p
    (setq-local font-lock-extra-managed-props
                (append neo--elisp-use-package-button-properties
                        font-lock-extra-managed-props))
    (font-lock-add-keywords
     nil
     '(neo--elisp-use-package-button-matcher)
     'append)
    (setq neo--elisp-use-package-buttons-enabled-p t)))

(neo/use-package elisp-mode
  :ensure nil
  :hook
  (emacs-lisp-mode . neo--elisp-enable-use-package-buttons))

(neo/use-package macrostep)

(neo/use-package ppp)

(neo/use-package eval-expr
  :config
  (eval-expr-install))

;; Declared *before* `helpful': helpful's own Package-Requires lists
;; elisp-refs as a dependency, so if helpful were declared first, Elpaca
;; would auto-resolve+activate elisp-refs as part of helpful's dependency
;; graph, and this explicit declaration would then create a second,
;; independent queue entry for the same package -- racing the first and
;; tripping Elpaca's "loaded before Elpaca activation" warning (the same
;; duplicate-declaration race documented in neo-better-git.el's NOTE about
;; `transient').
;; TODO: not sure this is actually useful to me. el-search is probably more general, only one is likely to stay.
(neo/use-package elisp-refs
  :commands (elisp-refs-function
	     elisp-refs-macro
	     elisp-refs-variable
	     elisp-refs-special
	     elisp-refs-symbol))

(neo/use-package helpful)

;; (neo/use-package elisp-def
;;   :hook
;;   (emacs-lisp-mode . #'elisp-def-mode))

(neo/use-package elisp-depmap
  :bind (("C-c M-d" . elisp-depmap-graphviz-digraph)
         ("C-c M-g" . elisp-depmap-graphviz)
         ("C-c M-s" . elisp-depmap-makesummarytable))
  :config ((elisp-depmap-exec-file "~/graphviz2.dot")))

(neo/use-package aggressive-indent
  :hook
  ('emacs-lisp-mode . #'aggressive-indent-mode))

(neo/use-package buttercup)

(neo/use-package paredit
  :commands paredit-mode
  :hook
  (lisp-data-mode . paredit-mode)
  :config
  (define-key paredit-mode-map (kbd "RET") nil)
  )

(neo/use-package el-search)

;;; Note, no (provide 'neo-elisp) here, extensions are loaded not required.
