;;; -*- lexical-binding: t -*-

;;; This is elisp, a NEO extension
;;;
;;; The Elisp Survival Kit

(neo/use-package macrostep)

(neo/use-package ppp)

(neo/use-package eval-expr
  :config
  (eval-expr-install))

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

;; TODO: not sure this is actually useful to me. el-search is probably more general, only one is likely to stay.
(neo/use-package elisp-refs
  :commands (elisp-refs-function
	     elisp-refs-macro
	     elisp-refs-variable
	     elisp-refs-special
	     elisp-refs-symbol))

(neo/use-package el-search)

;;; Note, no (provide 'neo-elisp) here, extensions are loaded not required.
