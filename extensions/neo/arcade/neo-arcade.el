;;; -*- lexical-binding: t -*-

;;; This is arcade, a NEO extension
;;;
;;; Insert coin

(neo/use-package tetris
  :commands tetris)

(neo/use-package autotetris-mode
  :after tetris
  :bind
  (:map tetris-mode-map
        ("RET" . autotetris-mode)))

;;; Note, no (provide 'neo-arcade) here, extensions are loaded not required.
