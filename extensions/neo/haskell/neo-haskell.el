;;; -*- lexical-binding: t -*-

;;; This is haskell, a NEO extension
;;;
;;; Lazy by default, eager about types.

(defun neo--haskell-ts-mode-preferred-p ()
  "Return non-nil when `haskell-ts-mode' should replace `haskell-mode'.

Only prefer the tree-sitter frontend when the package is installed and
the Haskell grammar is ready to use. This keeps `.hs' files opening
cleanly on machines that have not installed the grammar yet."
  (and (locate-library "haskell-ts-mode")
       (fboundp 'treesit-ready-p)
       (treesit-ready-p 'haskell t)))

(defun neo--haskell-prefer-ts-mode ()
  "Prefer `haskell-ts-mode' for Haskell buffers when tree-sitter is ready."
  (if (neo--haskell-ts-mode-preferred-p)
      (setf (alist-get 'haskell-mode major-mode-remap-alist) 'haskell-ts-mode)
    (setq major-mode-remap-alist
          (assq-delete-all 'haskell-mode major-mode-remap-alist))))

;; haskell-mode supplies the major mode, REPL integration, and the
;; `haskell-mode-stylish-buffer' command. We point that command at an
;; absolute stylish-haskell path below because GUI Emacs often misses
;; user-level Cabal tools on PATH.
(neo/use-package haskell-mode
  :custom
  ;; `haskell-mode' ships its own ad-hoc symbol prettifier that we
  ;; replace with `prettify-symbols-mode' below. Leaving both on causes
  ;; double substitutions and fights over font-lock.
  (haskell-font-lock-symbols nil)
  :hook
  (haskell-mode . neo/haskell-mode-setup))

;; Tree-sitter Haskell lives in a separate package from the classic
;; `haskell-mode' frontend, but we want the same editor behavior in
;; both worlds whenever the user opts into `haskell-ts-mode'.
(neo/use-package haskell-ts-mode
  :init
  (neo--haskell-prefer-ts-mode)
  :hook
  (haskell-ts-mode . neo/haskell-mode-setup))

(defconst neo/haskell-tool-search-directories
  (mapcar #'expand-file-name '("~/.ghcup/bin" "~/.cabal/bin"))
  "Directories to search when GUI Emacs misses Haskell tools on PATH.

Shell sessions usually inherit these paths from login startup files,
but GUI launchers often do not.  Resolving HLS explicitly keeps the
Eglot setup simple while avoiding the interactive \"program not found\"
prompt.")

(defvar neo/haskell-prettify-symbols
  '(;; Lambda and arrows
    ("\\"          . ?λ)
    ("->"          . ?→)
    ("<-"          . ?←)
    ("=>"          . ?⇒)
    ("<="          . ?⇐)
    (">->"         . ?↣)
    ("->>"         . ?↠)
    ;; Type signatures and kinds
    ("::"          . ?∷)
    ;; Quantifiers
    ("forall"      . ?∀)
    ("exists"      . ?∃)
    ;; Comparisons / equality
    ("/="          . ?≠)
    ("=="          . ?≡)
    ("<="          . ?≤)
    (">="          . ?≥)
    ;; Boolean / logic
    ("&&"          . ?∧)
    ("||"          . ?∨)
    ("not"         . ?¬)
    ;; Sets / categories
    ("union"        . ?∪)
    ("intersect"    . ?∩)
    ("elem"         . ?∈)
    ("notElem"      . ?∉)
    ("isSubsetOf"   . ?⊆)
    ("empty"        . ?∅)
    ;; Composition and application
    ("."            . ?∘)
    ("<<<"          . ?⋘)
    (">>>"          . ?⋙)
    ;; Monadic
    (">>="          . ?»)
    ("=<<"          . ?«)
    ;; Bottom
    ("undefined"    . ?⊥)
    ;; Sums / products (the Data.* combinators, when imported)
    ("sum"          . ?Σ)
    ("product"      . ?Π))
  "Source token → Unicode glyph mapping used by `prettify-symbols-mode'
in Haskell buffers.

Heavy by design: replaces operators, keywords, and a handful of
commonly imported combinators. Entries that read as plain identifiers
\(e.g. `union', `elem') only match when `prettify-symbols-mode' would
treat them as syntactic words, so module-qualified or in-string
occurrences stay literal.")

(with-eval-after-load 'project
  ;; `project.el' otherwise falls back to the repository's `.git'
  ;; marker, which is too coarse for multi-package Haskell workspaces.
  ;; Teaching it about the common Haskell roots keeps Eglot/HLS pointed
  ;; at the package or cabal project that actually owns the buffer.
  (add-to-list 'project-vc-extra-root-markers "cabal.project")
  (add-to-list 'project-vc-extra-root-markers "stack.yaml")
  (add-to-list 'project-vc-extra-root-markers "hie.yaml"))

(defun neo--haskell-find-executable (program)
  "Return an absolute path to PROGRAM when neo can find it.

Prefer `executable-find' so user overrides inside Emacs still win.  If
GUI Emacs started without the Haskell toolchain directories on PATH,
fall back to the standard ghcup/cabal user-level bin locations."
  (or (executable-find program)
      (cl-loop
       for dir in neo/haskell-tool-search-directories
       for candidate = (expand-file-name program dir)
       when (file-executable-p candidate)
       return candidate)))

(defun neo--haskell-existing-tool-directories ()
  "Return Haskell tool directories that exist on this machine.

The configured search list includes common user-level install
locations, but not every machine will have both directories. Filtering
up front keeps `exec-path' and subprocess PATH entries tidy."
  (cl-remove-if-not #'file-directory-p neo/haskell-tool-search-directories))

(defun neo--haskell-buffer-path ()
  "Return PATH for Haskell buffers with the preferred toolchain first.

Resolving the wrapper itself is not enough when GUI Emacs launches with
the wrong PATH. HLS asks `cabal' and `ghc' which compiler a project
uses, so those tools must come from the same ghcup/cabal installation
as the wrapper."
  (string-join
   (delete-dups
    (append (neo--haskell-existing-tool-directories)
            (split-string (or (getenv "PATH") "") path-separator t)))
   path-separator))

(defun neo--haskell-prepare-buffer-environment ()
  "Prefer the user-level Haskell toolchain for this buffer's processes.

Making the environment buffer-local keeps the fix narrowly scoped to
Haskell buffers while ensuring Eglot, HLS, and formatters all see the
same `cabal', `ghc', and related tools."
  (let ((buffer-path (neo--haskell-buffer-path)))
    (setq-local exec-path
                (delete-dups
                 (append (neo--haskell-existing-tool-directories) exec-path)))
    (setq-local process-environment
                (cons (format "PATH=%s" buffer-path)
                      (cl-remove-if
                       (lambda (entry)
                         (string-prefix-p "PATH=" entry))
                       process-environment)))))

(defun neo--haskell-configure-stylish-haskell ()
  "Configure format-on-save for the current Haskell buffer.

`haskell-mode-stylish-buffer' delegates to
`haskell-mode-stylish-haskell-path'. Setting that variable
buffer-locally avoids a common GUI Emacs failure mode where
`stylish-haskell' exists in `~/.cabal/bin' but `call-process-region'
cannot resolve the bare executable name."
  (if-let ((stylish-haskell (neo--haskell-find-executable "stylish-haskell")))
      (progn
        (setq-local haskell-mode-stylish-haskell-path stylish-haskell)
        (add-hook 'before-save-hook #'haskell-mode-stylish-buffer nil :local))
    (message "neo-haskell: stylish-haskell not found; skipping format-on-save")))

(defun neo--haskell-locate-dominating-directory (start predicate)
  "Walk upward from START until PREDICATE returns non-nil for a directory.

This exists because Haskell project roots are often identified by file
patterns such as `*.cabal', whereas `locate-dominating-file' only
matches a single literal file or directory name."
  (let ((dir (file-name-as-directory (expand-file-name start)))
        parent
        found)
    (while (and dir (not found))
      (when (funcall predicate dir)
        (setq found dir))
      (setq parent (file-name-directory (directory-file-name dir)))
      (setq dir (unless (or (null parent) (equal dir parent)) parent)))
    found))

(defun neo--haskell-project-root ()
  "Return the smallest practical Haskell workspace root for the buffer.

HLS behaves best when launched from the package or cabal project that
owns the file, not from an arbitrarily large VC root. Prefer explicit
Haskell markers first, then fall back to `project.el' / `.git'."
  (or (locate-dominating-file default-directory "hie.yaml")
      (neo--haskell-locate-dominating-directory
       default-directory
       (lambda (dir)
         (directory-files dir nil "\\.cabal\\'" t)))
      (locate-dominating-file default-directory "cabal.project")
      (locate-dominating-file default-directory "stack.yaml")
      (when-let ((project (and (fboundp 'project-current)
                               (project-current nil))))
        (expand-file-name (project-root project)))
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun neo--haskell-standalone-workspace-p ()
  "Return non-nil when the current buffer lives in a direct-file workspace.

These workspaces are anchored by `hie.yaml' but intentionally do not
define a Cabal or Stack project. In that case, `interactive-haskell-mode'
tries to force a package-oriented REPL, which is exactly the wrong fit
for one-off lab files."
  (let* ((root (file-name-as-directory (neo--haskell-project-root)))
         (hie-yaml (expand-file-name "hie.yaml" root)))
    (and (file-exists-p hie-yaml)
         (not (directory-files root nil "\\.cabal\\'" t))
         (not (file-exists-p (expand-file-name "cabal.project" root)))
         (not (file-exists-p (expand-file-name "stack.yaml" root))))))

(defun neo--haskell-standalone-repl-buffer-name ()
  "Return the REPL buffer name for the current standalone workspace."
  (format "*neo-haskell:%s*"
          (directory-file-name
           (file-name-nondirectory
            (directory-file-name (neo--haskell-project-root))))))

(defun neo--haskell-standalone-repl-input-partial ()
  "Return the current standalone REPL input between the prompt and point."
  (when-let ((process (get-buffer-process (current-buffer))))
    (buffer-substring-no-properties (process-mark process) (point))))

(defun neo--haskell-standalone-repl-parse-completions (raw-response)
  "Parse a GHCi `:complete repl' RAW-RESPONSE."
  (when raw-response
    (let* ((lines (split-string raw-response "\r?\n" t))
           (candidates (mapcar #'haskell-string-literal-decode (cdr lines)))
           (header (car lines)))
      (unless (string-match
               "\\`\\([0-9]+\\) \\([0-9]+\\) \\(\".*\"\\)\\'"
               header)
        (error "neo-haskell: invalid `:complete' response"))
      (let ((count (string-to-number (match-string 1 header)))
            (unused (haskell-string-literal-decode (match-string 3 header))))
        (unless (= count (length candidates))
          (error "neo-haskell: inconsistent `:complete' response"))
        (cons unused candidates)))))

(defun neo--haskell-standalone-repl-completions (input)
  "Return GHCi completions for INPUT in the current standalone REPL."
  (neo--haskell-standalone-repl-parse-completions
   (let ((inferior-haskell-buffer (current-buffer)))
     (inferior-haskell-get-result
      (concat ":complete repl "
              (haskell-string-literal-encode input))))))

(defun neo--haskell-standalone-repl-completion-at-point ()
  "Offer GHCi completions in the current standalone REPL buffer."
  (when-let* ((_process (get-buffer-process (current-buffer)))
              ((comint-after-pmark-p))
              (input (neo--haskell-standalone-repl-input-partial))
              ((string-match-p "\\S-" input))
              (response (neo--haskell-standalone-repl-completions input)))
    (let* ((unused (car response))
           (candidates (append (if (string-prefix-p input "import") '("import"))
                               (if (string-prefix-p input "let") '("let"))
                               (cdr response))))
      (list (- (point) (- (length input) (length unused)))
            (point)
            candidates))))

(defun neo--haskell-standalone-repl-tab ()
  "Complete the current standalone GHCi input."
  (interactive)
  (when (comint-after-pmark-p)
    (completion-at-point)))

(defun neo--haskell-configure-standalone-repl (buffer)
  "Enable Haskell REPL integrations for BUFFER when available."
  (when (require 'inf-haskell nil t)
    (with-current-buffer buffer
      (setq inferior-haskell-buffer buffer)
      (unless (derived-mode-p 'inferior-haskell-mode)
        (inferior-haskell-mode)
        (run-hooks 'inferior-haskell-hook))
      (add-hook 'completion-at-point-functions
                #'neo--haskell-standalone-repl-completion-at-point nil t)
      (local-set-key (kbd "TAB") #'neo--haskell-standalone-repl-tab))))

(defun neo--haskell-ensure-standalone-repl ()
  "Start or reuse a plain `ghci' buffer for the current workspace."
  (require 'comint)
  (let* ((default-directory (neo--haskell-project-root))
         (buffer-name (neo--haskell-standalone-repl-buffer-name))
         (buffer (get-buffer-create buffer-name))
         (process-name (concat "neo-haskell-ghci:" buffer-name))
         (ghci (or (neo--haskell-find-executable "ghci") "ghci")))
    (unless (comint-check-proc buffer)
      (apply #'make-comint-in-buffer
             process-name
             buffer
             ghci
             nil
             '("-ignore-dot-ghci" "-i." "-XGHC2024"))
      (neo--haskell-configure-standalone-repl buffer))
    buffer))

(defun neo--haskell-load-buffer-into-standalone-repl ()
  "Save the current buffer and load it into a plain `ghci' REPL."
  (save-buffer)
  (let ((repl-buffer (neo--haskell-ensure-standalone-repl)))
    (comint-send-string
     (get-buffer-process repl-buffer)
     (format ":load %S\n" (expand-file-name buffer-file-name)))
    repl-buffer))

(defun neo--haskell-enable-eglot-ui ()
  "Turn on HLS-specific Eglot features for the current Haskell buffer.

Inlay hints are the missing piece between hover-only support and the
kind of ambient type feedback users expect from HLS."
  (when (and (derived-mode-p 'haskell-mode 'haskell-ts-mode)
             (fboundp 'eglot-inlay-hints-mode))
    (eglot-inlay-hints-mode 1)))

(defun neo/haskell-switch-to-repl ()
  "Switch to the Haskell interactive shell for the current session.

`neo/haskell-mode-setup' prefers HLS via Eglot and therefore does not
eagerly enable `interactive-haskell-mode'. Turn it on lazily here so
users still get the familiar `C-c C-z' REPL jump when they ask for it."
  (interactive)
  (if (neo--haskell-standalone-workspace-p)
      (pop-to-buffer (neo--haskell-load-buffer-into-standalone-repl))
    (unless (fboundp 'interactive-haskell-mode)
      (user-error "neo-haskell: interactive-haskell-mode is unavailable"))
    (unless (bound-and-true-p interactive-haskell-mode)
      (interactive-haskell-mode 1))
    (neo--haskell-load-buffer-into-repl)
    (cond
     ((fboundp 'haskell-interactive-switch)
      (call-interactively #'haskell-interactive-switch))
     ((fboundp 'haskell-interactive-bring)
      (call-interactively #'haskell-interactive-bring))
     (t
      (user-error "neo-haskell: no haskell-mode REPL switch command is available")))))

(defun neo--haskell-load-buffer-into-repl ()
  "Save the current buffer and load it into the active Haskell REPL."
  (unless buffer-file-name
    (user-error "neo-haskell: current buffer is not visiting a file"))
  (save-buffer)
  (cond
   ((fboundp 'haskell-process-load-file)
    (haskell-process-load-file))
   ((fboundp 'haskell-process-load-or-reload)
    (haskell-process-load-or-reload))
   (t
    (user-error "neo-haskell: no haskell-mode load command is available"))))

(defun neo--haskell-start-language-client ()
  "Start the preferred language client for the current Haskell buffer.

Prefer HLS via Eglot. Fall back to `lsp-mode' if that is the only
client available, and fall back to `interactive-haskell-mode' only
when no LSP client is available. Running both HLS and
`interactive-haskell-mode' together tends to produce the exact
symptom profile we want to avoid: legacy hover/import helpers working
while completion and xref come from the wrong backend or not at all."
  (cond
   ((and (neo/extensionp "neo:programming-foundation")
         (fboundp 'eglot-ensure))
    (let ((default-directory (neo--haskell-project-root)))
      (eglot-ensure))
    'eglot)
   ((fboundp 'lsp-deferred)
    (lsp-deferred)
    'lsp)
   (t
    (interactive-haskell-mode 1)
    'interactive)))

(defun neo/haskell-mode-setup ()
  "Wire up LSP, stylish-haskell autoformat, and Unicode prettification.

Prefer Eglot — built into Emacs 29+ and the project's chosen client
via `neo:programming-foundation' — falling back to `lsp-mode' /
`lsp-haskell' when Eglot is unavailable. stylish-haskell runs as a
buffer-local pre-save hook so neighboring languages keep whatever
format-on-save behavior they had.

`prettify-symbols-mode' is enabled with
`neo/haskell-prettify-symbols'; `unprettify-at-point' is set to
`right-edge' so editing near a glyph reveals the underlying tokens."
  (neo--haskell-prepare-buffer-environment)
  (local-set-key (kbd "C-c C-z") #'neo/haskell-switch-to-repl)
  (add-hook 'eglot-managed-mode-hook #'neo--haskell-enable-eglot-ui nil :local)
  (pcase (neo--haskell-start-language-client)
    ('eglot
     ;; Reused workspaces can flip management on before the local hook
     ;; above gets a chance to fire, so enable the HLS niceties eagerly
     ;; when the buffer is already under Eglot's control.
     (when (and (fboundp 'eglot-managed-p)
                (eglot-managed-p))
       (neo--haskell-enable-eglot-ui))))
  (neo--haskell-configure-stylish-haskell)
  (setq-local prettify-symbols-alist neo/haskell-prettify-symbols)
  (setq-local prettify-symbols-unprettify-at-point 'right-edge)
  (prettify-symbols-mode 1))

;; Register haskell-language-server-wrapper for Eglot. Defining this
;; with `neo/eglot-set-server' (from neo:programming-foundation) means
;; "M-x eglot" works without further configuration as long as HLS is
;; on PATH.
(with-eval-after-load 'eglot
  (when (fboundp 'neo/eglot-set-server)
    (neo/eglot-set-server
     '(haskell-mode haskell-ts-mode)
     (list (or (neo--haskell-find-executable "haskell-language-server-wrapper")
               "haskell-language-server-wrapper")
           "--lsp"))))

;; lsp-haskell is the lsp-mode integration for HLS. It's installed so
;; users who prefer lsp-mode get a working setup, but the
;; `neo/haskell-mode-setup' hook above only activates it when Eglot
;; isn't available.
(neo/use-package lsp-haskell
  :after lsp-mode
  :custom
  (lsp-haskell-server-path
   (or (neo--haskell-find-executable "haskell-language-server-wrapper")
       "haskell-language-server-wrapper"))
  (lsp-haskell-formatting-provider "stylish-haskell"))

;;; Note, no (provide 'neo-haskell) here, extensions are loaded not required.
