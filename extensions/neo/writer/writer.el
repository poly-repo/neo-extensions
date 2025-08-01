(neo/use-package jinx
  :hook (((text-mode prog-mode) . jinx-mode))
  :bind (("C-;" . jinx-correct))
  :custom
  (jinx-camel-modes '(prog-mode))
  (jinx-delay 0.01))

(setopt dictionary-use-single-buffer t)
(setopt dictionary-server "dict.org")

(use-package olivetti)

