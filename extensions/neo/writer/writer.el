;;; TODO: requires:
;;; sudo apt install libenchant-2-dev

(neo/use-package jinx
  :hook (((text-mode prog-mode) . jinx-mode))
  :bind (("C-;" . jinx-correct))
  :custom
  (jinx-camel-modes '(prog-mode))
  (jinx-delay 0.01))

;; TODO this is a bit messy as vertico is in a different extension
;; alltogether. We need a better dependency management story.
(with-eval-after-load 'vertico-multiform
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20) (vertico-count . 4)))
  (vertico-multiform-mode))


(setopt dictionary-use-single-buffer t)
(setopt dictionary-server "dict.org")

(neo/use-package olivetti)

(neo/use-package selectric-mode)

