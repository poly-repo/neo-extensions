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

(describe "neo/manager--persist-enabled-extension"
  (it "appends a new slug to the persisted enabled-extensions roots"
    (let (stored)
      (spy-on 'neo/get-config :and-return-value "(\"neo:extension-manager\")")
      (spy-on 'neo/set-config :and-call-fake
              (lambda (key value)
                (when (string= key "enabled-extensions") (setq stored value))))
      (neo/manager--persist-enabled-extension "neo:haskell")
      (expect (read stored) :to-equal '("neo:extension-manager" "neo:haskell"))))

  (it "does not duplicate a slug that is already installed"
    (let (stored)
      (spy-on 'neo/get-config
              :and-return-value "(\"neo:extension-manager\" \"neo:haskell\")")
      (spy-on 'neo/set-config :and-call-fake
              (lambda (key value)
                (when (string= key "enabled-extensions") (setq stored value))))
      (neo/manager--persist-enabled-extension "neo:haskell")
      (expect (read stored) :to-equal '("neo:extension-manager" "neo:haskell")))))

(describe "neo/manager--resolve-extension"
  (it "installs, loads, and replays packages for a slug with no unmet requires"
    (let* ((available (make-hash-table :test 'equal))
           (ext-a (make-neo/extension :name "a" :publisher "pub" :requires nil))
           (framework (make-neo-framework
                       :available-extensions available
                       :installed-extensions (make-hash-table :test 'equal)))
           (loaded nil)
           (replayed nil))
      (puthash "pub:a" ext-a available)
      (spy-on 'neo--load-extension :and-call-fake
              (lambda (ext) (push (neo/extension-name ext) loaded) t))
      (spy-on 'neo/replay-extension-packages :and-call-fake
              (lambda (slug) (push (neo/extension-slug-to-string slug) replayed)))
      (let ((result (neo/manager--resolve-extension framework "pub:a")))
        (expect result :to-equal '("pub:a"))
        (expect (gethash "pub:a" (neo-framework-installed-extensions framework)) :to-be-truthy)
        (expect loaded :to-equal '("a"))
        (expect replayed :to-equal '("pub:a")))))

  (it "installs unmet :requires before the requested slug, in dependency order"
    (let* ((available (make-hash-table :test 'equal))
           (ext-a (make-neo/extension :name "a" :publisher "pub" :requires nil))
           (ext-b (make-neo/extension :name "b" :publisher "pub" :requires '("pub:a")))
           (framework (make-neo-framework
                       :available-extensions available
                       :installed-extensions (make-hash-table :test 'equal)))
           (loaded nil))
      (puthash "pub:a" ext-a available)
      (puthash "pub:b" ext-b available)
      (spy-on 'neo--load-extension :and-call-fake
              (lambda (ext) (push (neo/extension-name ext) loaded) t))
      (spy-on 'neo/replay-extension-packages)
      (let ((result (neo/manager--resolve-extension framework "pub:b")))
        (expect result :to-equal '("pub:a" "pub:b"))
        (expect (nreverse loaded) :to-equal '("a" "b")))))

  (it "does not re-install or re-load an extension already installed this session"
    (let* ((available (make-hash-table :test 'equal))
           (ext-a (make-neo/extension :name "a" :publisher "pub" :requires nil))
           (installed (make-hash-table :test 'equal))
           (framework (make-neo-framework
                       :available-extensions available
                       :installed-extensions installed)))
      (puthash "pub:a" ext-a available)
      (puthash "pub:a"
               (make-neo/installation
                :extension-slug (make-neo/extension-slug :publisher "pub" :name "a"))
               installed)
      (spy-on 'neo--load-extension)
      (spy-on 'neo/replay-extension-packages)
      (let ((result (neo/manager--resolve-extension framework "pub:a")))
        (expect result :to-equal nil)
        (expect 'neo--load-extension :not :to-have-been-called)
        (expect 'neo/replay-extension-packages :not :to-have-been-called))))

  (it "binds neo/framework-bootstrapped-p to nil while loading and replaying, then restores it"
    (let* ((available (make-hash-table :test 'equal))
           (ext-a (make-neo/extension :name "a" :publisher "pub" :requires nil))
           (framework (make-neo-framework
                       :available-extensions available
                       :installed-extensions (make-hash-table :test 'equal)))
           (seen-during-load 'unset)
           (seen-during-replay 'unset)
           (neo/framework-bootstrapped-p t))
      (puthash "pub:a" ext-a available)
      (spy-on 'neo--load-extension :and-call-fake
              (lambda (_ext) (setq seen-during-load neo/framework-bootstrapped-p) t))
      (spy-on 'neo/replay-extension-packages :and-call-fake
              (lambda (_slug) (setq seen-during-replay neo/framework-bootstrapped-p)))
      (neo/manager--resolve-extension framework "pub:a")
      (expect seen-during-load :to-equal nil)
      (expect seen-during-replay :to-equal nil)
      (expect neo/framework-bootstrapped-p :to-equal t)))

  (it "fires a hook function newly deferred onto neo/after-framework-bootstrap-hook during the call"
    (let* ((available (make-hash-table :test 'equal))
           (ext-a (make-neo/extension :name "a" :publisher "pub" :requires nil))
           (framework (make-neo-framework
                       :available-extensions available
                       :installed-extensions (make-hash-table :test 'equal)))
           (neo/after-framework-bootstrap-hook nil)
           (fired nil))
      (puthash "pub:a" ext-a available)
      (spy-on 'neo--load-extension :and-call-fake
              (lambda (_ext)
                (add-hook 'neo/after-framework-bootstrap-hook
                          (lambda () (setq fired t)))
                t))
      (spy-on 'neo/replay-extension-packages)
      (neo/manager--resolve-extension framework "pub:a")
      (expect fired :to-be-truthy)))

  (it "does not re-fire a hook function already present before the call"
    (let* ((available (make-hash-table :test 'equal))
           (ext-a (make-neo/extension :name "a" :publisher "pub" :requires nil))
           (framework (make-neo-framework
                       :available-extensions available
                       :installed-extensions (make-hash-table :test 'equal)))
           (pre-existing-calls 0)
           (pre-existing-fn (lambda () (setq pre-existing-calls (1+ pre-existing-calls))))
           (neo/after-framework-bootstrap-hook (list pre-existing-fn)))
      (puthash "pub:a" ext-a available)
      (spy-on 'neo--load-extension)
      (spy-on 'neo/replay-extension-packages)
      (neo/manager--resolve-extension framework "pub:a")
      (expect pre-existing-calls :to-equal 0)))

  (it "logs and continues when a newly-deferred hook function signals an error"
    (let* ((available (make-hash-table :test 'equal))
           (ext-a (make-neo/extension :name "a" :publisher "pub" :requires nil))
           (framework (make-neo-framework
                       :available-extensions available
                       :installed-extensions (make-hash-table :test 'equal)))
           (neo/after-framework-bootstrap-hook nil))
      (puthash "pub:a" ext-a available)
      (spy-on 'neo--load-extension :and-call-fake
              (lambda (_ext)
                (add-hook 'neo/after-framework-bootstrap-hook
                          (lambda () (error "boom")))
                t))
      (spy-on 'neo/replay-extension-packages)
      (spy-on 'neo/log-warn)
      (let ((result (neo/manager--resolve-extension framework "pub:a")))
        (expect result :to-equal '("pub:a"))
        (expect 'neo/log-warn :to-have-been-called)))))

(describe "neo/manager--install-extension"
  (it "persists the slug and hot-loads it via neo/manager--resolve-extension"
    (let (stored)
      (spy-on 'neo/get-config :and-return-value "(\"neo:extension-manager\")")
      (spy-on 'neo/set-config :and-call-fake
              (lambda (key value)
                (when (string= key "enabled-extensions") (setq stored value))))
      (spy-on 'neo/extensions-refresh-all)
      (spy-on 'neo/manager-update-header)
      (spy-on 'neo--framework-instance :and-return-value 'fake-framework)
      (spy-on 'neo/manager--resolve-extension :and-return-value '("neo:haskell"))
      (neo/manager--install-extension "neo:haskell")
      (expect (read stored) :to-equal '("neo:extension-manager" "neo:haskell"))
      (expect 'neo/manager--resolve-extension :to-have-been-called-with
              'fake-framework "neo:haskell")
      (expect 'neo/extensions-refresh-all :to-have-been-called)))

  (it "runs the install spinner around the (blocking) resolve call"
    (let ((spinner-active-during-resolve 'unknown))
      (spy-on 'neo/get-config :and-return-value "(\"neo:extension-manager\")")
      (spy-on 'neo/set-config)
      (spy-on 'neo/extensions-refresh-all)
      (spy-on 'neo/manager-update-header)
      (spy-on 'neo--framework-instance :and-return-value 'fake-framework)
      (spy-on 'neo/manager--resolve-extension :and-call-fake
              (lambda (&rest _)
                (setq spinner-active-during-resolve
                      (equal neo/manager--installing-label "neo:haskell"))
                nil))
      (neo/manager--install-extension "neo:haskell")
      (expect spinner-active-during-resolve :to-be-truthy)
      (expect neo/manager--installing-label :to-equal nil)
      (expect neo/manager--spinner-timer :to-equal nil)))

  (it "does not duplicate a slug that is already persisted"
    (let (stored)
      (spy-on 'neo/get-config
              :and-return-value "(\"neo:extension-manager\" \"neo:haskell\")")
      (spy-on 'neo/set-config :and-call-fake
              (lambda (key value)
                (when (string= key "enabled-extensions") (setq stored value))))
      (spy-on 'neo/extensions-refresh-all)
      (spy-on 'neo/manager-update-header)
      (spy-on 'neo--framework-instance :and-return-value 'fake-framework)
      (spy-on 'neo/manager--resolve-extension :and-return-value nil)
      (neo/manager--install-extension "neo:haskell")
      (expect (read stored) :to-equal '("neo:extension-manager" "neo:haskell")))))

(describe "neo/manager--start-install-spinner and neo/manager--stop-install-spinner"
  (it "sets the installing label, starts a repeating timer, and re-renders once"
    (with-temp-buffer
      (spy-on 'neo/extensions-refresh-all)
      (neo/manager--start-install-spinner (current-buffer) "pub:a")
      (expect neo/manager--installing-label :to-equal "pub:a")
      (expect (timerp neo/manager--spinner-timer) :to-be-truthy)
      (expect 'neo/extensions-refresh-all :to-have-been-called)
      (neo/manager--stop-install-spinner (current-buffer))))

  (it "clears the label, marker, and cancels the timer on stop"
    (with-temp-buffer
      (spy-on 'neo/extensions-refresh-all)
      (neo/manager--start-install-spinner (current-buffer) "pub:a")
      (setq neo/manager--spinner-marker (copy-marker (point-min)))
      (let ((timer neo/manager--spinner-timer))
        (neo/manager--stop-install-spinner (current-buffer))
        (expect neo/manager--installing-label :to-equal nil)
        (expect neo/manager--spinner-marker :to-equal nil)
        (expect neo/manager--spinner-timer :to-equal nil)
        (expect (memq timer timer-list) :to-equal nil)))))

(describe "neo/manager--update-spinner-glyph"
  (it "replaces just the glyph character at the marker, leaving the rest untouched"
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (insert "x⠋ Installing pub:a…"))
      (setq neo/manager--spinner-marker (copy-marker 2))
      (setq neo/manager--spinner-index 1)
      (neo/manager--update-spinner-glyph)
      (expect (buffer-string) :to-equal "x⠙ Installing pub:a…")))

  (it "does nothing when there is no active marker"
    (with-temp-buffer
      (insert "unrelated content")
      (setq neo/manager--spinner-marker nil)
      (neo/manager--update-spinner-glyph)
      (expect (buffer-string) :to-equal "unrelated content"))))

(describe "neo/extension-render-card installing state"
  (it "shows a spinner glyph in place of the Install button for the installing slug"
    (let* ((ext (make-neo/extension :name "a" :publisher "pub"
                                    :repository (make-neo/repository :type "git")))
           (framework (make-neo-framework
                       :available-extensions (make-hash-table :test 'equal)
                       :installed-extensions (make-hash-table :test 'equal))))
      (spy-on 'create-image :and-return-value 'fake-image)
      (spy-on 'neo--get-extension-info :and-return-value nil)
      (spy-on 'svg-lib-button :and-return-value "BUTTON")
      (with-temp-buffer
        (setq neo/manager--installing-label "pub:a")
        (neo/extension-render-card ext framework nil)
        (expect 'svg-lib-button :not :to-have-been-called)
        (expect (buffer-string) :to-match "Installing pub:a…")
        (expect neo/manager--spinner-marker :to-be-truthy))))

  (it "shows the Install button (not the spinner) when the slug is not installing"
    (let* ((ext (make-neo/extension :name "a" :publisher "pub"
                                    :repository (make-neo/repository :type "git")))
           (framework (make-neo-framework
                       :available-extensions (make-hash-table :test 'equal)
                       :installed-extensions (make-hash-table :test 'equal))))
      (spy-on 'create-image :and-return-value 'fake-image)
      (spy-on 'neo--get-extension-info :and-return-value nil)
      (spy-on 'svg-lib-button :and-return-value "BUTTON")
      (with-temp-buffer
        (setq neo/manager--installing-label "pub:other")
        (neo/extension-render-card ext framework nil)
        (expect (car (spy-calls-args-for 'svg-lib-button 0)) :to-equal "[download] Install")))))

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
    (spy-on 'neo/manager--persist-enabled-extension)
    (spy-on 'neo/manager-render)
    (neo/manager--maybe-launch-on-startup)
    (expect 'neo/manager-render :not :to-have-been-called)
    (expect 'neo/manager--persist-enabled-extension :not :to-have-been-called))

  (it "consumes the flag, pre-selects the dashboard without hot-loading it, and renders the manager"
    (spy-on 'neo/get-config :and-return-value "t")
    (spy-on 'neo/set-config)
    (spy-on 'neo/manager--persist-enabled-extension)
    (let ((test-buffer (generate-new-buffer " *test-manager*")))
      (unwind-protect
          (progn
            (spy-on 'neo/manager-render :and-return-value test-buffer)
            (neo/manager--maybe-launch-on-startup)
            (expect 'neo/set-config :to-have-been-called-with
                    "launch-extensions-manager-on-startup" "nil")
            (expect 'neo/manager--persist-enabled-extension :to-have-been-called-with
                    "neo:dashboard")
            (expect 'neo/manager-render :to-have-been-called))
        (kill-buffer test-buffer)))))

(provide 'test-neo-extension-manager)
;;; test-neo-extension-manager.el ends here
