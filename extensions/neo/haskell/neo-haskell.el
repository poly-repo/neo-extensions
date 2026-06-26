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
  ;; REPL/process ergonomics, matching what the Spacemacs and Doom
  ;; Haskell layers enable: offer to drop redundant imports, auto-import
  ;; modules already loaded in the session, and skip the GHCi overlay
  ;; since diagnostics come from flymake.
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-show-overlays nil)
  :config
  ;; Interface files are build artifacts, not completion targets.
  (add-to-list 'completion-ignored-extensions ".hi")
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
;; Hoogle commands resolve lazily; declaring them keeps a clean compile.
(declare-function consult-hoogle "consult-hoogle")
(declare-function haskell-hoogle "haskell-mode")
(declare-function haskell-hoogle-lookup-from-website "haskell-mode")
(declare-function haskell-navigate-imports "haskell-mode")
(declare-function haskell-mode-format-imports "haskell-mode")
(declare-function haskell-collapse-mode "haskell-collapse")
(declare-function yas-minor-mode "yasnippet")
(declare-function haskell-auto-insert-module-template "haskell-mode")
(declare-function haskell-interactive-switch "haskell-interactive-mode")
(declare-function haskell-process-load-file "haskell-commands")
(declare-function haskell-process-cabal-build "haskell-commands")
(declare-function haskell-compile "haskell-compile")
(declare-function haskell-cabal-add-dependency "haskell-cabal")
(declare-function haskell-cabal-next-subsection "haskell-cabal")
(declare-function haskell-cabal-previous-subsection "haskell-cabal")
(declare-function electric-indent-local-mode "electric")
(declare-function eglot--TextDocumentIdentifier "eglot")
(declare-function eglot--apply-text-edits "eglot")
(declare-function eglot--current-server-or-lose "eglot")
(declare-function eglot--lsp-position-to-point "eglot")
(declare-function eglot--pos-to-lsp-position "eglot")
(declare-function eglot--update-hints-1 "eglot")
(declare-function eglot-server-capable "eglot")
(declare-function eglot-code-actions "eglot")
(declare-function eglot-execute "eglot")
(declare-function eglot-inlay-hints-mode "eglot")
(declare-function flymake-diagnostic-oneliner "flymake")
(declare-function flymake-diagnostics "flymake")
(declare-function jsonrpc-async-request "jsonrpc")
(declare-function jsonrpc-request "jsonrpc")

(defvar neo/haskell-modes '(haskell-mode haskell-ts-mode haskell-literate-mode)
  "Major modes that count as Haskell source for shared configuration.")

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
  (if-let* ((stylish-haskell (neo--haskell-find-executable "stylish-haskell")))
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
      (when-let* ((project (and (fboundp 'project-current)
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

(defvar-local neo--haskell-standalone-repl-source-buffer nil
  "Most recent source buffer associated with a standalone GHCi REPL.")

(defvar neo--haskell-eglot-quickfix-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] #'ignore)
    (define-key map [mouse-1] #'neo--haskell-apply-eglot-quickfix-at-mouse)
    (define-key map (kbd "RET") #'neo--haskell-apply-eglot-quickfix-at-point)
    map)
  "Keymap used to make HLS quick-fix diagnostics clickable.")

(defconst neo--haskell-eglot-quickfix-overlay-props
  `((mouse-face . highlight)
    (pointer . hand)
    (keymap . ,neo--haskell-eglot-quickfix-map)
    (local-map . ,neo--haskell-eglot-quickfix-map)
    (help-echo . ,#'neo--haskell-eglot-diagnostic-help))
  "Overlay properties used for clickable HLS quick-fix diagnostics.")

(defvar neo--haskell-eglot-inlay-hint-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] #'ignore)
    (define-key map [mouse-1] #'neo--haskell-apply-eglot-inlay-hint-at-mouse)
    (define-key map [mouse-2] #'neo--haskell-apply-eglot-inlay-hint-at-mouse)
    map)
  "Keymap used to activate clickable HLS inlay hints.")

(defun neo--haskell-standalone-repl-switch-back ()
  "Return to the source buffer associated with this standalone REPL."
  (interactive)
  (unless (buffer-live-p neo--haskell-standalone-repl-source-buffer)
    (user-error "neo-haskell: no source buffer is associated with this REPL"))
  (pop-to-buffer neo--haskell-standalone-repl-source-buffer))

(defun neo--haskell-standalone-repl-input-partial ()
  "Return the current standalone REPL input between the prompt and point."
  (when-let* ((process (get-buffer-process (current-buffer))))
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
      (local-set-key (kbd "TAB") #'neo--haskell-standalone-repl-tab)
      (local-set-key (kbd "C-c C-z")
                     #'neo--haskell-standalone-repl-switch-back))))

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
  (unless buffer-file-name
    (user-error "neo-haskell: current buffer is not visiting a file"))
  (save-buffer)
  (let ((source-buffer (current-buffer))
        (repl-buffer (neo--haskell-ensure-standalone-repl)))
    (with-current-buffer repl-buffer
      (setq neo--haskell-standalone-repl-source-buffer source-buffer))
    (comint-send-string
     (get-buffer-process repl-buffer)
     (format ":load %S\n" (expand-file-name buffer-file-name)))
    repl-buffer))

(defun neo--haskell-enable-eglot-ui ()
  "Turn on HLS-specific Eglot features for the current Haskell buffer.

Inlay hints are the missing piece between hover-only support and the
kind of ambient type feedback users expect from HLS."
  (when (and (derived-mode-p 'haskell-mode 'haskell-ts-mode)
             (bound-and-true-p eglot--managed-mode))
    (neo--haskell-enable-eglot-quickfix-overlays)
    (when (fboundp 'eglot-inlay-hints-mode)
      (eglot-inlay-hints-mode 1))))

(defun neo--haskell-eglot-hint-tooltip-string (tooltip)
  "Return TOOLTIP as a plain string, or nil when it is empty."
  (cond
   ((stringp tooltip)
    (unless (string-empty-p tooltip)
      tooltip))
   ((and (listp tooltip)
         (stringp (plist-get tooltip :value)))
    (string-trim (plist-get tooltip :value)))
   (t
    nil)))

(defun neo--haskell-eglot-inlay-hint-action (hint &optional label-part)
  "Return the interactive action carried by HINT or LABEL-PART.

The action is represented as a plist with `:hint' and either
`:text-edits' or `:command'. HLS explicit-import hints can expose
label-part commands, but under Eglot the useful behavior is usually to
accept the whole hint and apply its `:textEdits'."
  (cond
   ((plist-get hint :textEdits)
    (list :hint hint
          :text-edits (plist-get hint :textEdits)))
   ((plist-get label-part :command)
    (list :hint hint
          :command (plist-get label-part :command)))
   ((plist-get hint :command)
    (list :hint hint
          :command (plist-get hint :command)))
   (t
    nil)))

(defun neo--haskell-eglot-resolve-inlay-hint (hint)
  "Resolve HINT when the server can enrich it with actionable edits."
  (if (and hint
           (null (plist-get hint :textEdits))
           (fboundp 'eglot-server-capable)
           (eglot-server-capable :inlayHintProvider :resolveProvider))
      (condition-case nil
          (jsonrpc-request (eglot--current-server-or-lose)
                           :inlayHint/resolve
                           hint)
        (error
         hint))
    hint))

(defun neo--haskell-eglot-inlay-hint-help (hint &optional label-part)
  "Return help text for clickable HLS inlay HINT or LABEL-PART."
  (let* ((tooltip (or (neo--haskell-eglot-hint-tooltip-string
                       (plist-get label-part :tooltip))
                      (neo--haskell-eglot-hint-tooltip-string
                       (plist-get hint :tooltip))))
         (action (neo--haskell-eglot-inlay-hint-action hint label-part)))
    (string-join
     (delq nil
           (list tooltip
                 (when action
                   "mouse-1: apply HLS hint")))
     "\n")))

(defun neo--haskell-eglot-inlay-hint-face (kind)
  "Return the Eglot face appropriate for inlay hint KIND."
  (pcase kind
    (1 'eglot-type-hint-face)
    (2 'eglot-parameter-hint-face)
    (_ 'eglot-inlay-hint-face)))

(defun neo--haskell-propertize-eglot-inlay-hint-text
    (text kind action help tweak-cursor-p)
  "Return TEXT propertized for an Eglot inlay hint.

KIND selects the hint face. ACTION carries a clickable HLS action.
HELP is the hint tooltip string. When TWEAK-CURSOR-P is non-nil, set
the first character's `cursor' property the same way stock Eglot
does for before-string hints."
  (let ((display (propertize text 'face (neo--haskell-eglot-inlay-hint-face kind))))
    (when tweak-cursor-p
      (put-text-property 0 1 'cursor 1 display))
    (when action
      (add-text-properties
       0
       (length display)
       `(follow-link t
                     help-echo ,help
                     keymap ,neo--haskell-eglot-inlay-hint-map
                     local-map ,neo--haskell-eglot-inlay-hint-map
                     mouse-face highlight
                     neo--haskell-eglot-inlay-action ,action
                     pointer hand)
       display))
    display))

(defun neo--haskell-eglot-apply-inlay-hint-action (action)
  "Apply an Eglot inlay hint ACTION for the current Haskell buffer."
  (let* ((hint (plist-get action :hint))
         (resolved (and hint
                        (neo--haskell-eglot-resolve-inlay-hint hint)))
         (text-edits (or (plist-get action :text-edits)
                         (plist-get resolved :textEdits)))
         (command (or (plist-get action :command)
                      (plist-get resolved :command))))
    (cond
     (text-edits
      (eglot--apply-text-edits text-edits nil t)
      (neo--haskell-refresh-eglot-inlay-hints)
      t)
     (command
      (eglot-execute (eglot--current-server-or-lose) command)
      (neo--haskell-refresh-eglot-inlay-hints)
      t)
     (t
      nil))))

(defun neo--haskell-eglot-inlay-hint-action-at-point ()
  "Return the clickable HLS inlay-hint action at point, or nil."
  (cl-loop
   for overlay in (overlays-in (max (point-min) (1- (point)))
                               (min (point-max) (1+ (point))))
   when (overlay-get overlay 'eglot--inlay-hint)
   for action = (or (overlay-get overlay 'neo--haskell-eglot-inlay-action)
                    (let ((display (or (overlay-get overlay 'before-string)
                                       (overlay-get overlay 'after-string))))
                      (and display
                           (get-text-property 0 'neo--haskell-eglot-inlay-action
                                              display))))
   when action
   return action))

(defun neo--haskell-eglot-inlay-hint-action-in-string (string &optional index)
  "Return the clickable HLS inlay-hint action stored in STRING at INDEX."
  (when (and (stringp string)
             (> (length string) 0))
    (get-text-property (min (or index 0) (1- (length string)))
                       'neo--haskell-eglot-inlay-action
                       string)))

(defun neo--haskell-refresh-eglot-inlay-hints ()
  "Refresh visible Eglot inlay hints for the current Haskell buffer."
  (when (and (bound-and-true-p eglot--managed-mode)
             (bound-and-true-p eglot-inlay-hints-mode))
    (remove-overlays nil nil 'eglot--inlay-hint t)
    (save-excursion
      (save-restriction
        (widen)
        (eglot--update-hints-1 (window-start) (window-end nil t))))))

(defun neo--haskell-apply-eglot-inlay-hint-at-point ()
  "Apply the clickable HLS inlay hint at point."
  (interactive)
  (unless (and (bound-and-true-p eglot--managed-mode)
               (when-let* ((action (neo--haskell-eglot-inlay-hint-action-at-point)))
                 (neo--haskell-eglot-apply-inlay-hint-action action)))
    (user-error "neo-haskell: no clickable HLS inlay hint here")))

(defun neo--haskell-apply-eglot-inlay-hint-at-mouse (event)
  "Apply the clickable HLS inlay hint clicked in EVENT."
  (interactive "e")
  (if-let* ((pos-data (posn-string (event-start event)))
            (action (neo--haskell-eglot-inlay-hint-action-in-string
                     (car pos-data)
                     (cdr pos-data))))
      (neo--haskell-eglot-apply-inlay-hint-action action)
    (mouse-set-point event)
    (neo--haskell-apply-eglot-inlay-hint-at-point)))

(defun neo--haskell-paint-eglot-inlay-hint (hint from to)
  "Render HLS inlay HINT between FROM and TO.

This is a compatibility copy of Eglot's inlay-hint painter that keeps
clickable metadata such as HLS import-hint `textEdits'."
  (goto-char (eglot--lsp-position-to-point (plist-get hint :position)))
  (when (<= from (point) to)
    (let* ((kind (plist-get hint :kind))
           (label (plist-get hint :label))
           (padding-left (plist-get hint :paddingLeft))
           (padding-right (plist-get hint :paddingRight))
           (left-pad (and padding-left
                          (not (eq padding-left :json-false))
                          (not (memq (char-before) '(32 9)))
                          " "))
           (right-pad (and padding-right
                           (not (eq padding-right :json-false))
                           (not (memq (char-after) '(32 9)))
                           " "))
           (peg-after-p (eql kind 1))
           (hint-action (neo--haskell-eglot-inlay-hint-action hint))
           (hint-help (neo--haskell-eglot-inlay-hint-help hint)))
      (cl-labels
          ((make-ov ()
             (if peg-after-p
                 (make-overlay (point) (1+ (point)) nil t)
               (make-overlay (1- (point)) (point) nil nil nil)))
           (do-it (segment lpad rpad index count action help)
             (let* ((firstp (zerop index))
                    (tweak-cursor-p (and firstp peg-after-p))
                    (overlay (make-ov))
                    (text (concat lpad segment rpad))
                    (display
                     (neo--haskell-propertize-eglot-inlay-hint-text
                      text kind action help tweak-cursor-p)))
               (overlay-put overlay (if peg-after-p 'before-string 'after-string)
                            display)
               (when action
                 (overlay-put overlay 'help-echo help)
                 (overlay-put overlay 'keymap neo--haskell-eglot-inlay-hint-map)
                 (overlay-put overlay 'local-map neo--haskell-eglot-inlay-hint-map)
                 (overlay-put overlay 'mouse-face 'highlight)
                 (overlay-put overlay 'pointer 'hand)
                 (overlay-put overlay 'neo--haskell-eglot-inlay-action action))
               (overlay-put overlay 'priority (if peg-after-p index (- count index)))
               (overlay-put overlay 'eglot--inlay-hint t)
               (overlay-put overlay 'evaporate t)
               (overlay-put overlay 'eglot--overlay t))))
        (if (stringp label)
            (do-it label left-pad right-pad 0 1 hint-action hint-help)
          (cl-loop
           for index from 0 below (length label)
           for label-part = (aref label index)
           for part-action = (or (neo--haskell-eglot-inlay-hint-action hint label-part)
                                 hint-action)
           for part-help = (or (neo--haskell-eglot-inlay-hint-help hint label-part)
                               hint-help)
           do
           (do-it (plist-get label-part :value)
                  (and (zerop index) left-pad)
                  (and (= index (1- (length label))) right-pad)
                  index
                  (length label)
                  part-action
                  part-help)))))))

(defun neo--haskell-eglot-update-hints-1 (from to)
  "Render clickable HLS inlay hints between FROM and TO."
  (let ((buffer (current-buffer)))
    (jsonrpc-async-request
     (eglot--current-server-or-lose)
     :textDocument/inlayHint
     (list :textDocument (eglot--TextDocumentIdentifier)
           :range (list :start (eglot--pos-to-lsp-position from)
                        :end (eglot--pos-to-lsp-position to)))
     :success-fn
     (lambda (hints)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (save-excursion
             (save-restriction
               (widen)
               (dolist (overlay (overlays-in (1- from) to))
                 (when (and (overlay-get overlay 'eglot--inlay-hint)
                            (cond ((eq (overlay-end overlay) from)
                                   (overlay-get overlay 'after-string))
                                  ((eq (overlay-end overlay) to)
                                   (overlay-get overlay 'before-string))
                                  (t)))
                   (delete-overlay overlay)))
               (mapc (lambda (hint)
                       (neo--haskell-paint-eglot-inlay-hint hint from to))
                     hints))))))
     :deferred 'eglot--update-hints-1)))

(defun neo--haskell-around-eglot-update-hints-1 (original from to)
  "Use clickable inlay-hint rendering for Haskell buffers.

ORIGINAL is Eglot's stock `eglot--update-hints-1'. FROM and TO bound
the region to refresh."
  (if (derived-mode-p 'haskell-mode 'haskell-ts-mode)
      (neo--haskell-eglot-update-hints-1 from to)
    (funcall original from to)))

(defun neo--haskell-eglot-code-action-bounds ()
  "Return the range that HLS quick fixes should inspect at point."
  (let (diagnostics bounds)
    (cond
     ((use-region-p)
      (list (region-beginning) (region-end)))
     ((setq diagnostics (flymake-diagnostics (point)))
      (cl-loop for diagnostic in diagnostics
               minimizing (flymake-diagnostic-beg diagnostic) into beg
               maximizing (flymake-diagnostic-end diagnostic) into end
               finally return (list beg end)))
     ((setq bounds (bounds-of-thing-at-point 'sexp))
      (list (car bounds) (cdr bounds)))
     (t
      (list (point) (point))))))

(defun neo--haskell-run-eglot-quickfix (&optional event)
  "Offer HLS quick fixes at point, using EVENT for popup placement."
  (when event
    (mouse-set-point event))
  (when (and (bound-and-true-p eglot--managed-mode)
             (fboundp 'eglot-code-actions))
    (pcase-let ((`(,beg ,end) (neo--haskell-eglot-code-action-bounds)))
      (when-let* ((actions (eglot-code-actions beg end "quickfix")))
        (let ((last-nonmenu-event (or event last-nonmenu-event)))
          (eglot-code-actions beg end "quickfix" t))
        actions))))

(defun neo--haskell-apply-eglot-quickfix-at-point ()
  "Apply or offer the HLS quick fix available at point."
  (interactive)
  (unless (neo--haskell-run-eglot-quickfix)
    (user-error "neo-haskell: no Eglot quick fixes available here")))

(defun neo--haskell-apply-eglot-quickfix-at-mouse (event)
  "Offer HLS quick fixes for the diagnostic clicked by EVENT."
  (interactive "e")
  (unless (neo--haskell-run-eglot-quickfix event)
    (mouse-set-point event)))

(defun neo--haskell-eglot-diagnostic-help (window _overlay position)
  "Return help text for the HLS diagnostic at POSITION in WINDOW."
  (with-selected-window window
    (concat
     (mapconcat #'flymake-diagnostic-oneliner
                (flymake-diagnostics position)
                "\n")
     "\nmouse-1: show HLS quick fixes\nRET: apply HLS quick fix at point")))

(defun neo--haskell-enable-eglot-quickfix-overlays ()
  "Make HLS quick-fix diagnostics clickable in the current buffer."
  (setq-local flymake-diagnostic-types-alist
              (copy-tree
               (and (boundp 'flymake-diagnostic-types-alist)
                    flymake-diagnostic-types-alist)))
  (dolist (type '(:error :warning :note))
    (let ((existing (copy-tree
                     (alist-get type flymake-diagnostic-types-alist nil nil #'eq))))
      (setq existing
            (cl-remove-if
             (lambda (property)
               (memq (car-safe property)
                     '(mouse-face pointer keymap local-map help-echo)))
             existing))
      (setf (alist-get type flymake-diagnostic-types-alist nil nil #'eq)
            (append existing
                    neo--haskell-eglot-quickfix-overlay-props)))))

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
  ;; Point haskell-mode's own Hoogle command at the resolved binary so
  ;; its local lookups work even when GUI Emacs misses `hoogle' on PATH.
  (when-let* ((hoogle (neo--haskell-find-executable "hoogle")))
    (setq-local haskell-hoogle-command hoogle))
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
  ;; Code folding for `where'/`do'/`let' blocks, like the reference layers.
  (when (fboundp 'haskell-collapse-mode)
    (haskell-collapse-mode 1))
  (setq-local prettify-symbols-alist neo/haskell-prettify-symbols)
  (setq-local prettify-symbols-unprettify-at-point 'right-edge)
  (prettify-symbols-mode 1))

(defun neo/haskell-format-imports ()
  "Sort and align the import block from anywhere in the source file."
  (interactive)
  (save-excursion
    (haskell-navigate-imports)
    (haskell-mode-format-imports)))

;; Register haskell-language-server-wrapper for Eglot. Defining this
;; with `neo/eglot-set-server' (from neo:programming-foundation) means
;; "M-x eglot" works without further configuration as long as HLS is
;; on PATH.
(with-eval-after-load 'eglot
  (advice-remove 'eglot--update-hints-1 #'neo--haskell-around-eglot-update-hints-1)
  (advice-add 'eglot--update-hints-1 :around #'neo--haskell-around-eglot-update-hints-1)
  (when (fboundp 'neo/eglot-set-server)
    (neo/eglot-set-server
     '(haskell-mode haskell-ts-mode)
     (list (or (neo--haskell-find-executable "haskell-language-server-wrapper")
               "haskell-language-server-wrapper")
           "--lsp"))))

(defun neo/haskell-hoogle ()
  "Search Hoogle for documentation, preferring a local database.

When a local `hoogle' executable resolves, use `consult-hoogle' for a
live, consult-driven search of the local database.  Otherwise fall back
to haskell-mode's web lookup against hoogle.haskell.org."
  (interactive)
  (cond
   ((and (neo--haskell-find-executable "hoogle")
         (fboundp 'consult-hoogle))
    (call-interactively #'consult-hoogle))
   ((fboundp 'haskell-hoogle-lookup-from-website)
    (haskell-hoogle-lookup-from-website))
   ((fboundp 'haskell-hoogle)
    (call-interactively #'haskell-hoogle))
   (t
    (user-error "neo-haskell: no Hoogle backend available"))))

(with-eval-after-load 'haskell-mode
  ;; All commands live under the `C-c h' prefix so they don't collide
  ;; with haskell-mode's own `C-c C-*' REPL bindings.
  (define-key haskell-mode-map (kbd "C-c h h") #'neo/haskell-hoogle)
  (define-key haskell-mode-map (kbd "C-c h i") #'haskell-navigate-imports)
  (define-key haskell-mode-map (kbd "C-c h I") #'neo/haskell-format-imports)
  (define-key haskell-mode-map (kbd "C-c h m") #'haskell-auto-insert-module-template)
  ;; REPL / build.  These start a GHCi session on demand without
  ;; enabling `interactive-haskell-mode', which would otherwise take
  ;; over completion and xref from Eglot.
  (define-key haskell-mode-map (kbd "C-c h z") #'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c h l") #'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-c h b") #'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c h c") #'haskell-compile))

(defun neo--haskell-disable-electric-indent ()
  "Disable `electric-indent-mode' locally; it fights cabal's layout."
  (electric-indent-local-mode -1))

;; Cabal files: turn off electric-indent and expose the most-used
;; section/dependency commands under `C-c h'.
(with-eval-after-load 'haskell-cabal
  (add-hook 'haskell-cabal-mode-hook #'neo--haskell-disable-electric-indent)
  (define-key haskell-cabal-mode-map (kbd "C-c h d") #'haskell-cabal-add-dependency)
  (define-key haskell-cabal-mode-map (kbd "C-c h n") #'haskell-cabal-next-subsection)
  (define-key haskell-cabal-mode-map (kbd "C-c h p") #'haskell-cabal-previous-subsection))

;; Alignment rules for the common Haskell operators. Matching both the
;; ASCII source and the prettified glyphs keeps `C-x a a' working
;; regardless of `prettify-symbols-mode'. The `modes' cell is a symbol
;; that align evaluates to the mode list at run time.
(with-eval-after-load 'align
  (add-to-list 'align-rules-list
               '(neo-haskell-types
                 (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                 (modes . neo/haskell-modes)))
  (add-to-list 'align-rules-list
               '(neo-haskell-assignment
                 (regexp . "\\(\\s-+\\)=\\s-+")
                 (modes . neo/haskell-modes)))
  (add-to-list 'align-rules-list
               '(neo-haskell-arrows
                 (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                 (modes . neo/haskell-modes)))
  (add-to-list 'align-rules-list
               '(neo-haskell-left-arrows
                 (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                 (modes . neo/haskell-modes))))

;; Tree-sitter. Register the grammar source with NEO's tree-sitter
;; machinery (from neo:programming-foundation) so that
;; `M-x neo/treesit-install-grammars' builds it into the
;; version-segregated cache on `treesit-extra-load-path'. Plain
;; `treesit-install-language-grammar' installs to a directory NEO does
;; not search, so haskell-ts-mode would then fail to find the grammar.
;; Pin the grammar to the revision haskell-ts-mode targets (per its
;; README). The grammar's master branch renames nodes that the mode's
;; font-lock queries reference, which otherwise fails with
;; `treesit-query-error' the moment a Haskell buffer is fontified.
(with-eval-after-load 'neo-programming-foundation-treesit
  (add-to-list 'treesit-language-source-alist
               '(haskell "https://github.com/tree-sitter/tree-sitter-haskell"
                         "v0.23.1")))

(neo/use-package haskell-ts-mode
  :defer t)

(add-hook 'haskell-ts-mode-hook #'neo/haskell-mode-setup)

(defcustom neo/haskell-use-tree-sitter nil
  "When non-nil, edit Haskell with `haskell-ts-mode' instead of `haskell-mode'.

Tree-sitter highlighting depends on the installed `tree-sitter-haskell'
grammar matching the node names `haskell-ts-mode' queries.  When they
drift, font-lock raises `treesit-query-error' on every redisplay — and
because Eglot hover docs fontify `haskell' code blocks the same way, a
mismatch also breaks documentation popups.  `haskell-mode' has no such
coupling, so it is the default.

To opt in: install the pinned grammar with
`M-x neo/treesit-install-grammars' (NOT the vanilla
`treesit-install-language-grammar'), set this to t, and restart Emacs.
Confirm first with
\\='(treesit-query-validate \\='haskell \"(comment)\")\\=' returning nil."
  :type 'boolean
  :group 'neo)

;; Remap to haskell-ts-mode only when the user opts in AND the grammar is
;; installed and loadable. Otherwise stay on haskell-mode so highlighting
;; and Eglot hover keep working regardless of grammar/mode drift.
(when (and neo/haskell-use-tree-sitter
           (fboundp 'treesit-ready-p)
           (treesit-ready-p 'haskell t))
  (add-to-list 'major-mode-remap-alist '(haskell-mode . haskell-ts-mode)))

;; Snippets. NEO has no global yasnippet, so enable it buffer-locally in
;; Haskell buffers and load haskell-snippets' templates alongside it.
(neo/use-package yasnippet
  :defer t
  :hook
  ((haskell-mode haskell-ts-mode) . yas-minor-mode))

(neo/use-package haskell-snippets
  :after yasnippet)

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

;; consult-hoogle searches a local Hoogle database through NEO's consult
;; UI. `neo/haskell-hoogle' prefers it and falls back to web lookup.
;; Deferred so this optional convenience never taxes startup.
(neo/use-package consult-hoogle
  :defer t)

;;; Note, no (provide 'neo-haskell) here, extensions are loaded not required.
