;;; -*- lexical-binding: t -*-

(require 'neo-ui-frame)

(defun neo/config--apply-font-size (symbol value)
  "Apply the font size specified by SYMBOL to all existing frames.
VALUE is the new value set via customization."
  (set-default symbol value)
  (let* ((display-info (neo/classify-monitor))
	 (size (cond
               ((integerp value) (* 10 value))
               ((eq value 'recommended) (* 10 (floor (plist-get display-info :ui-pt)))) 
               ((eq value 'default) nil)
               (t nil))))
    (when size
      (set-face-attribute 'default nil :height size))))
      ;; ;; Apply to all frames
      ;; (dolist (frame (frame-list))
      ;; 	(set-face-attribute 'default nil :height (* 10 (floor (plist-get display-info :ui-pt))))
      ;;   (set-frame-font (format "Fira Code-%d" size) nil t)))))


(defcustom neo/config/preferred-font-size 'recommended
  "Preferred font size for Neo UI frames.

This can be:

- an integer, specifying the exact font size in points,
- 'recommended, to use the default recommended size for the system,
- 'default, to leave Emacs' default font size unchanged"
  :type '(choice
          (integer :tag "Exact size in points")
          (const :tag "Recommended size" recommended)
          (const :tag "Default (no change)" default))
  :group 'neo-ui
  :set #'neo/config--apply-font-size)

(provide 'neo-ui-fonts)
