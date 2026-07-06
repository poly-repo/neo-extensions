;;; -*- lexical-binding: t -*-
(neo/extension
 :name "programming-foundation"
 :title "Language agnostic software development support"
 :publisher "neo"
 :description "What used to be cool for programming before vibe coding\n\n"
 :categories (neo) ; not sure what the intention for this was
 :keywords (convenience programming)

 :requires ()
 :tree-sitter-grammars
 ((bash "https://github.com/tree-sitter/tree-sitter-bash")
  (c "https://github.com/tree-sitter/tree-sitter-c")
  (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
  (css "https://github.com/tree-sitter/tree-sitter-css")
  (elisp "https://github.com/Wilfred/tree-sitter-elisp")
  (go "https://github.com/tree-sitter/tree-sitter-go")
  (html "https://github.com/tree-sitter/tree-sitter-html")
  (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
  (json "https://github.com/tree-sitter/tree-sitter-json")
  (markdown "https://github.com/ikatyang/tree-sitter-markdown")
  (rust "https://github.com/tree-sitter/tree-sitter-rust")
  (toml "https://github.com/tree-sitter/tree-sitter-toml")
  (yaml "https://github.com/ikatyang/tree-sitter-yaml"))
 :repository (
	      :type "git"
	      :url "https://github.com/poly-repo/neo-extensions.git"
	      :path "extensions/neo/programming-foundation"))
