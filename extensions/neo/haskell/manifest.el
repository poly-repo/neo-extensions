(neo/extension
 :name "haskell"
 :title "Lazy by default, eager about types."
 :publisher "neo"
 :description "Haskell support: haskell-mode + HLS via eglot, lsp-haskell fallback, Hoogle lookup, tree-sitter, folding, import/align helpers, REPL+build commands, snippets, and autoformat with stylish-haskell."
 :categories (neo)
 :keywords (programming haskell)
 :requires ("neo:programming-foundation")
 :repository (
	      :type "git"
	      :url "https://github.com/poly-repo/neo-extensions.git"
	      :path "extensions/neo/haskell"))
