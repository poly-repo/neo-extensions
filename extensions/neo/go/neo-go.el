;;; -*- lexical-binding: t -*-

;;; This is go, a NEO extension
;;;
;;; Go language support

(neo/use-package eglot
  :builtin
  :ensure-system-package
  ((go . golang-go)
   (gopls . gopls))
  :hook
  ((go-mode go-ts-mode) . eglot-ensure)
  :config
  (neo/eglot-set-server '(go-mode go-ts-mode) '("gopls")))

;;; Note, no (provide 'neo-go) here, extensions are loaded not required.
