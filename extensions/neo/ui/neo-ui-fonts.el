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

(defun neo/font--available-p (family)
  "Return non-nil when FAMILY is an installed font family.
Only meaningful in a graphic environment; on a text terminal there are no
font families so this returns nil."
  (and (stringp family)
       (display-graphic-p)
       (member family (font-family-list))))

(defun neo/config--apply-default-font-family (symbol value)
  "Set SYMBOL to VALUE and apply it as the `default' face family.
The family is only applied when it is actually installed, so an
unavailable font is a silent no-op rather than a broken frame."
  (set-default symbol value)
  (when (neo/font--available-p value)
    (set-face-attribute 'default nil :family value)))

(defun neo/config--apply-comment-font-family (symbol value)
  "Set SYMBOL to VALUE and apply it to the comment faces.
Both `font-lock-comment-face' and `font-lock-comment-delimiter-face' get
the family so comment text and its leading delimiters match.  Applied
only when VALUE names an installed font.

Slant and weight are forced to normal: comment fonts (Patrick Hand, most
handwriting faces) usually ship a single Regular style, while themes
often render comments italic (e.g. via `modus-themes-slant').  Asking for
a missing italic/bold variant makes the font backend fall back to another
family entirely, so the comment would silently render in the default
font instead of the chosen one."
  (set-default symbol value)
  (when (neo/font--available-p value)
    (dolist (face '(font-lock-comment-face font-lock-comment-delimiter-face))
      (set-face-attribute face nil :family value :slant 'normal :weight 'normal))))

(defcustom neo/config/default-font-family "Iosevka Nerd Font"
  "Font family for the `default' face (the main coding font).

The value must match an installed family exactly.  The Nerd Fonts build
of Iosevka registers as \"Iosevka Nerd Font\" (not plain \"Iosevka\"), so
that is the default."
  :type 'string
  :group 'neo-ui
  :set #'neo/config--apply-default-font-family)

(defcustom neo/config/comment-font-family "Patrick Hand"
  "Font family used for comments (text and delimiters).

Applied to `font-lock-comment-face' and
`font-lock-comment-delimiter-face'.  The value must name an installed
family exactly."
  :type 'string
  :group 'neo-ui
  :set #'neo/config--apply-comment-font-family)

(defun neo/fonts--apply-all (&rest _)
  "Apply the configured default and comment font families.
Re-applied on `enable-theme-functions' so the families survive theme
activation: the UI extension loads fonts before `neo-ui-themes', which
then loads a theme that can otherwise re-stamp the comment face."
  (neo/config--apply-default-font-family
   'neo/config/default-font-family neo/config/default-font-family)
  (neo/config--apply-comment-font-family
   'neo/config/comment-font-family neo/config/comment-font-family))

(add-hook 'enable-theme-functions #'neo/fonts--apply-all)
(neo/fonts--apply-all)

(provide 'neo-ui-fonts)
