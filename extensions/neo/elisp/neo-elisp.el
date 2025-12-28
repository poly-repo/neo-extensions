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

;;; Note, no (provide 'neo-elisp) here, extensions are loaded not required.
