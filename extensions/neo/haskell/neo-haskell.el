;;; -*- lexical-binding: t -*-

;;; This is haskell, a NEO extension
;;;
;;; Lazy by default, eager about types.

;; haskell-mode supplies the major mode, REPL integration, and the
;; `haskell-mode-stylish-buffer' command that shells out to
;; stylish-haskell on PATH.
(neo/use-package haskell-mode
  :custom
  ;; `haskell-mode' ships its own ad-hoc symbol prettifier that we
  ;; replace with `prettify-symbols-mode' below. Leaving both on causes
  ;; double substitutions and fights over font-lock.
  (haskell-font-lock-symbols nil)
  :hook
  ((haskell-mode . interactive-haskell-mode)
   (haskell-mode . neo/haskell-mode-setup)))

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
  (when (neo/extensionp "neo:programming-foundation")
    (cond
     ((fboundp 'eglot-ensure)
      (eglot-ensure))
     ((fboundp 'lsp-deferred)
      (lsp-deferred))))
  (add-hook 'before-save-hook #'haskell-mode-stylish-buffer nil :local)
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
     '("haskell-language-server-wrapper" "--lsp"))))

;; lsp-haskell is the lsp-mode integration for HLS. It's installed so
;; users who prefer lsp-mode get a working setup, but the
;; `neo/haskell-mode-setup' hook above only activates it when Eglot
;; isn't available.
(neo/use-package lsp-haskell
  :after lsp-mode
  :custom
  (lsp-haskell-server-path "haskell-language-server-wrapper")
  (lsp-haskell-formatting-provider "stylish-haskell"))

;;; Note, no (provide 'neo-haskell) here, extensions are loaded not required.
