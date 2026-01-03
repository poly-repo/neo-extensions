;;; -*- lexical-binding: t -*-

;;; This is build, a NEO extension
;;;
;;; Build tools support

(neo/use-package bazel
  :config
  (setq bazel-fix-visibility 'ask)
  (setq bazel-buildifier-before-save t))

(add-to-list
 'auto-mode-alist
 (cons (rx ?/ (+ nonl) ".mlody" eos) #'bazel-starlark-mode))

(use-package build
  :ensure (build :host github :repo "27justin/build.el") ; optional :ref
  :config
  ;; Optional: configuration for build.el goes here
  )

(neo/eglot-set-server '(bazel-build-mode bazel-starlark-mode) '("starpls"))

;;; Note, no (provide 'neo-build) here, extensions are loaded not required.
