;;; -*- lexical-binding: t -*-

;;; This is arcade, a NEO extension
;;;
;;; Insert coin

(neo/use-package tetris
  :ensure nil
  :commands tetris
  :bind
  (:map tetris-mode-map
        ("k" . tetris-rotate-prev)
        ("l" . tetris-move-down)
        ("j" . tetris-move-left)
        (";" . tetris-move-right)))

;;; Note, no (provide 'neo-arcade) here, extensions are loaded not required.
