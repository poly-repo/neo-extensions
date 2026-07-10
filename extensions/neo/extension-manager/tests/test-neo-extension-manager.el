;;; tests/test-neo-extension-manager.el --- Tests for neo-extension-manager -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)
(require 'neo-early-init-utils)
(require 'neo-logging)

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

(require 'neo-application)
(load-file (expand-file-name "../neo-extension-manager.el"
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

(describe "neo/extensions-render"
  (it "skips hidden extensions when drawing cards"
    (let* ((available (make-hash-table :test 'equal))
           (visible (make-neo/extension :name "a" :publisher "pub"))
           (hidden (make-neo/extension :name "extension-manager" :publisher "neo" :hidden t))
           (framework (make-neo-framework
                       :available-extensions available
                       :installed-extensions (make-hash-table :test 'equal)))
           (rendered nil))
      (puthash "pub:a" visible available)
      (puthash "neo:extension-manager" hidden available)
      (spy-on 'neo--framework-instance :and-return-value framework)
      (spy-on 'neo/manager--installed-slugs :and-return-value nil)
      (spy-on 'neo/extension-render-card :and-call-fake
              (lambda (ext &rest _) (push ext rendered)))
      (with-temp-buffer
        (neo/extensions-render))
      (expect rendered :to-equal (list visible))))

  (it "keeps rendering remaining cards when one card's render signals an error"
    (let* ((available (make-hash-table :test 'equal))
           (bad (make-neo/extension :name "bad" :publisher "pub"))
           (good (make-neo/extension :name "good" :publisher "pub"))
           (framework (make-neo-framework
                       :available-extensions available
                       :installed-extensions (make-hash-table :test 'equal)))
           (rendered nil))
      (puthash "pub:bad" bad available)
      (puthash "pub:good" good available)
      (spy-on 'neo--framework-instance :and-return-value framework)
      (spy-on 'neo/manager--installed-slugs :and-return-value nil)
      (spy-on 'neo/log-warn)
      (spy-on 'neo/extension-render-card :and-call-fake
              (lambda (ext &rest _)
                (if (eq ext bad)
                    (error "boom")
                  (push ext rendered))))
      (with-temp-buffer
        (neo/extensions-render))
      (expect rendered :to-equal (list good))
      (expect 'neo/log-warn :to-have-been-called))))

(describe "neo/app-neo-extensions--dispatch"
  (it "opens the perspective-backed application when perspective is available"
    (spy-on 'require :and-return-value t)
    (spy-on 'neo/app-neo-extensions)
    (spy-on 'neo/manager-render)
    (neo/app-neo-extensions--dispatch)
    (expect 'neo/app-neo-extensions :to-have-been-called)
    (expect 'neo/manager-render :not :to-have-been-called))

  (it "falls back to the bare manager render when perspective is unavailable"
    (spy-on 'require :and-return-value nil)
    (spy-on 'neo/app-neo-extensions)
    (spy-on 'neo/manager-render)
    (neo/app-neo-extensions--dispatch)
    (expect 'neo/manager-render :to-have-been-called)
    (expect 'neo/app-neo-extensions :not :to-have-been-called)))

(describe "neo/manager--maybe-launch-on-startup"
  (it "does nothing when the one-shot flag is not set"
    (spy-on 'neo/get-config :and-return-value nil)
    (spy-on 'neo/set-config)
    (spy-on 'neo/manager--install-extension)
    (spy-on 'neo/manager-render)
    (neo/manager--maybe-launch-on-startup)
    (expect 'neo/manager-render :not :to-have-been-called)
    (expect 'neo/manager--install-extension :not :to-have-been-called))

  (it "consumes the flag, pre-selects the dashboard, and renders the manager"
    (spy-on 'neo/get-config :and-return-value "t")
    (spy-on 'neo/set-config)
    (spy-on 'neo/manager--install-extension)
    (let ((test-buffer (generate-new-buffer " *test-manager*")))
      (unwind-protect
          (progn
            (spy-on 'neo/manager-render :and-return-value test-buffer)
            (neo/manager--maybe-launch-on-startup)
            (expect 'neo/set-config :to-have-been-called-with
                    "launch-extensions-manager-on-startup" "nil")
            (expect 'neo/manager--install-extension :to-have-been-called-with
                    "neo:dashboard")
            (expect 'neo/manager-render :to-have-been-called))
        (kill-buffer test-buffer)))))

(provide 'test-neo-extension-manager)
;;; test-neo-extension-manager.el ends here
