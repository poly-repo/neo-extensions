;;; -*- lexical-binding: t -*-

;;; This is leetcode, a NEO extension
;;;
;;; Leetcode client and problem manager

(neo/use-package leetcode
  :custom
  (leetcode-directory "~/Projects/uno_mav-leetcode/interview/leetcode")
  (leetcode-save-solutions t)
  ;; TODO create a leetcode environment when this extension is selected
  (leetcode-python-environment (file-name-concat user-emacs-directory ".python")))


;;; Note, no (provide 'neo-leetcode) here, extensions are loaded not required.
