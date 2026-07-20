;;; -*- lexical-binding: t -*-
(neo/extension
 :name "org"
 :title "Org Mode"
 :publisher "neo"
 :description "Order, capture, and enough notebook sprawl to stay useful."
 :categories (neo)
 :keywords (org notes capture)
 :requires ("neo:haskell" "neo:mlody-mode")
 :repository (
              :type "git"
              :url "https://github.com/poly-repo/neo-extensions.git"
              :path "extensions/neo/org"))
