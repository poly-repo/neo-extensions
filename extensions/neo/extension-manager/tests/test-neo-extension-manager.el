;;; tests/test-neo-extension-manager.el --- Tests for neo-extension-manager -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)
(require 'neo-early-init-utils)

;; `neo-config' (pulled in transitively via `neo-framework') opens a real
;; sqlite handle at load time; point it at a scratch dir instead of a real
;; profile, same as `core/neo-framework-test.el' does.
(defvar neo/cache-directory temporary-file-directory)
(defvar neo/config-directory temporary-file-directory)

(defmacro neo/use-package (&rest _args)
  "Ignore package declarations while loading extension code in tests."
  nil)

(load-file (expand-file-name "../neo-extension-manager-render.el"
                             (file-name-directory (or load-file-name buffer-file-name))))

(describe "neo/manager--install-extension"
  (it "appends a new slug to the persisted enabled-extensions roots"
    (let (stored)
      (spy-on 'neo/get-config :and-return-value "(\"neo:extension-manager\")")
      (spy-on 'neo/set-config :and-call-fake
              (lambda (key value)
                (when (string= key "enabled-extensions") (setq stored value))))
      (spy-on 'neo/extensions-refresh-all)
      (neo/manager--install-extension "neo:haskell")
      (expect (read stored) :to-equal '("neo:extension-manager" "neo:haskell"))))

  (it "does not duplicate a slug that is already installed"
    (let (stored)
      (spy-on 'neo/get-config
              :and-return-value "(\"neo:extension-manager\" \"neo:haskell\")")
      (spy-on 'neo/set-config :and-call-fake
              (lambda (key value)
                (when (string= key "enabled-extensions") (setq stored value))))
      (spy-on 'neo/extensions-refresh-all)
      (neo/manager--install-extension "neo:haskell")
      (expect (read stored) :to-equal '("neo:extension-manager" "neo:haskell"))))

  (it "refreshes the UI after persisting the choice"
    (spy-on 'neo/get-config :and-return-value nil)
    (spy-on 'neo/set-config)
    (spy-on 'neo/extensions-refresh-all)
    (neo/manager--install-extension "neo:haskell")
    (expect 'neo/extensions-refresh-all :to-have-been-called)))

(describe "neo/manager--installed-slugs"
  (it "reflects the persisted enabled-extensions roots"
    (let* ((available (make-hash-table :test 'equal))
           (ext-a (make-neo/extension :name "a" :publisher "pub" :requires nil))
           (framework (make-neo-framework
                       :available-extensions available
                       :installed-extensions (make-hash-table :test 'equal))))
      (puthash "pub:a" ext-a available)
      (spy-on 'neo/get-config :and-return-value "(\"pub:a\")")
      (expect (neo/manager--installed-slugs framework) :to-equal '("pub:a"))))

  (it "expands roots through :requires, not just the roots themselves"
    (let* ((available (make-hash-table :test 'equal))
           (ext-a (make-neo/extension :name "a" :publisher "pub" :requires nil))
           (ext-b (make-neo/extension :name "b" :publisher "pub" :requires '("pub:a")))
           (framework (make-neo-framework
                       :available-extensions available
                       :installed-extensions (make-hash-table :test 'equal))))
      (puthash "pub:a" ext-a available)
      (puthash "pub:b" ext-b available)
      (spy-on 'neo/get-config :and-return-value "(\"pub:b\")")
      (expect (sort (neo/manager--installed-slugs framework) #'string<)
              :to-equal '("pub:a" "pub:b"))))

  (it "is empty when nothing is persisted yet"
    (let* ((framework (make-neo-framework
                       :available-extensions (make-hash-table :test 'equal)
                       :installed-extensions (make-hash-table :test 'equal))))
      (spy-on 'neo/get-config :and-return-value nil)
      (expect (neo/manager--installed-slugs framework) :to-equal nil))))

(provide 'test-neo-extension-manager)
;;; test-neo-extension-manager.el ends here
