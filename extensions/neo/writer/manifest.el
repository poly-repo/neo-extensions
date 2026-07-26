;;; -*- lexical-binding: t -*-

(neo/extension
 :name "writer"
 :title "Support for writing prose"
 :publisher "neo"
 :description "Elisp-powered eloquence."
 :categories (neo)
 :keywords (text writing spellchecking)
 :requires ("neo:compsel")
 :repository (
              :type "git"
              :url "https://github.com/poly-repo/neo-extensions.git"
              :path "extensions/neo/writer"))
