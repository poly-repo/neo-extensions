;;; -*- lexical-binding: t -*-

(require 'neo-ui-frame)
(require 'seq)

(defun neo--fonts-graphic-frames ()
  "Return all live graphic frames."
  (seq-filter (lambda (frame)
                (and (frame-live-p frame) (display-graphic-p frame)))
              (frame-list)))

(defun neo--fonts-height-for-frame (frame value)
  "Return the face height for FRAME from font-size preference VALUE."
  (cond
   ((integerp value) (* 10 value))
   ((eq value 'recommended)
    (when-let* ((display-info (neo/classify-monitor frame))
                (ui-pt (plist-get display-info :ui-pt)))
      (round (* 10 ui-pt))))
   ((eq value 'default) nil)))

(defun neo--fonts-apply-frame-size (frame)
  "Apply the configured font size to graphic FRAME."
  (when (and (frame-live-p frame) (display-graphic-p frame))
    (when-let* ((height
                 (neo--fonts-height-for-frame
                  frame neo/config/preferred-font-size)))
      (set-face-attribute 'default frame :height height)
      height)))

(defun neo/config--apply-font-size (symbol value)
  "Set SYMBOL to VALUE and apply it to all graphic frames."
  (set-default symbol value)
  (dolist (frame (neo--fonts-graphic-frames))
    (neo--fonts-apply-frame-size frame)))


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

(defun neo/font--available-p (family &optional frame)
  "Return non-nil when FAMILY is an installed font family.
Only meaningful in a graphic environment; on a text terminal there are no
font families so this returns nil."
  (and (stringp family)
       (display-graphic-p frame)
       (member family (font-family-list frame))))

(defun neo--fonts-apply-default-family (frame value)
  "Apply default font family VALUE to graphic FRAME when available."
  (when (neo/font--available-p value frame)
    (set-face-attribute 'default frame :family value)))

(defun neo/config--apply-default-font-family (symbol value)
  "Set SYMBOL to VALUE and apply it as the `default' face family.
The family is only applied when it is actually installed, so an
unavailable font is a silent no-op rather than a broken frame."
  (set-default symbol value)
  (dolist (frame (neo--fonts-graphic-frames))
    (neo--fonts-apply-default-family frame value)))

(defun neo--fonts-apply-comment-family (frame value)
  "Apply comment font family VALUE to graphic FRAME when available."
  (when (neo/font--available-p value frame)
    (dolist (face '(font-lock-comment-face font-lock-comment-delimiter-face))
      (set-face-attribute
       face frame :family value :slant 'normal :weight 'normal))))

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
  (dolist (frame (neo--fonts-graphic-frames))
    (neo--fonts-apply-comment-family frame value)))

(defcustom neo/config/default-font-family "Iosevka Nerd Font"
  "Font family for the `default' face (the main coding font).

The value must match an installed family exactly.  The Nerd Fonts build
of Iosevka registers as \"Iosevka Nerd Font\" (not plain \"Iosevka\"), so
that is the default."
  :type 'string
  :group 'neo-ui
  :set #'neo/config--apply-default-font-family)

(defcustom neo/config/comment-font-family "Recursive Mono Casual"
  "Font family used for comments (text and delimiters).

Applied to `font-lock-comment-face' and
`font-lock-comment-delimiter-face'.  The value must name an installed
family exactly."
  :type 'string
  :group 'neo-ui
  :set #'neo/config--apply-comment-font-family)

(defun neo/fonts-apply (&optional frame)
  "Apply configured fonts for FRAME's current monitor.
FRAME defaults to the selected frame.  Call this interactively after a
display-layout change when the window system has not emitted a frame-move
event."
  (interactive)
  (let ((frame (or frame (selected-frame))))
    (when (and (frame-live-p frame) (display-graphic-p frame))
      (let ((height (neo--fonts-apply-frame-size frame)))
        (neo--fonts-apply-default-family
         frame neo/config/default-font-family)
        (neo--fonts-apply-comment-family
         frame neo/config/comment-font-family)
        (when (called-interactively-p 'interactive)
          (message "Applied Neo fonts%s"
                   (if height (format " at %.1fpt" (/ height 10.0)) "")))))))

(defun neo/fonts--apply-all (&rest _)
  "Apply configured font size and families to all graphic frames.
Re-applied on `enable-theme-functions' so font settings survive theme
activation: the UI extension loads fonts before `neo-ui-themes', which
can otherwise re-stamp the affected faces."
  (dolist (frame (neo--fonts-graphic-frames))
    (neo/fonts-apply frame)))

(add-hook 'enable-theme-functions #'neo/fonts--apply-all)
(add-hook 'after-make-frame-functions #'neo/fonts-apply)
(add-hook 'move-frame-functions #'neo/fonts-apply)
(global-set-key (kbd "s--") #'text-scale-adjust)
(global-set-key (kbd "s-=") #'text-scale-adjust)
(global-set-key (kbd "s-+") #'text-scale-adjust)
(neo/fonts--apply-all)

(provide 'neo-ui-fonts)
