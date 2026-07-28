;;; tests/test-neo-ui-fonts.el --- Tests for neo-ui-fonts -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)

(load-file (expand-file-name "../neo-ui-frame.el"
                             (file-name-directory (or load-file-name buffer-file-name))))
(load-file (expand-file-name "../neo-ui-fonts.el"
                             (file-name-directory (or load-file-name buffer-file-name))))

(describe "neo-ui monitor-aware fonts"
  (it "classifies the monitor containing the frame"
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _frame) t))
              ((symbol-function 'frame-monitor-attributes)
               (lambda (_frame)
                 '((name . "eDP-1")
                   (geometry 0 0 3840 2400)
                   (mm-size 366 229))))
              ((symbol-function 'frame-parameter) (lambda (&rest _) "display"))
              ((symbol-function 'neo/display-dpi) (lambda (&optional _) '(96.0 . 96.0))))
      (let ((classification (neo/classify-monitor 'laptop-frame)))
        (expect (plist-get classification :width) :to-equal 3840)
        (expect (plist-get classification :height) :to-equal 2400)
        (expect (plist-get classification :dpi-class) :to-equal 'ultra-hidpi)
        (expect (plist-get classification :ui-pt) :to-equal 16.0))))

  (it "uses each frame's recommended monitor size"
    (cl-letf (((symbol-function 'neo/classify-monitor)
               (lambda (frame)
                 (list :ui-pt (if (eq frame 'laptop-frame) 16.0 11.0)))))
      (expect (neo--fonts-height-for-frame 'laptop-frame 'recommended)
              :to-equal 160)
      (expect (neo--fonts-height-for-frame 'external-frame 'recommended)
              :to-equal 110)))

  (it "preserves an explicit point size"
    (expect (neo--fonts-height-for-frame 'frame 14) :to-equal 140))

  (it "leaves the font height unchanged for the default preference"
    (expect (neo--fonts-height-for-frame 'frame 'default) :to-be nil))

  (it "applies the size to the specific frame"
    (let (face-call)
      (cl-letf (((symbol-function 'frame-live-p) (lambda (_frame) t))
                ((symbol-function 'display-graphic-p) (lambda (&optional _frame) t))
                ((symbol-function 'neo--fonts-height-for-frame)
                 (lambda (_frame _value) 160))
                ((symbol-function 'set-face-attribute)
                 (lambda (&rest args) (setq face-call args))))
        (expect (neo--fonts-apply-frame-size 'laptop-frame) :to-equal 160)
        (expect face-call
                :to-equal '(default laptop-frame :height 160)))))

  (it "applies size and families interactively to the selected frame"
    (let (calls)
      (cl-letf (((symbol-function 'selected-frame) (lambda () 'laptop-frame))
                ((symbol-function 'frame-live-p) (lambda (_frame) t))
                ((symbol-function 'display-graphic-p) (lambda (&optional _frame) t))
                ((symbol-function 'neo--fonts-apply-frame-size)
                 (lambda (frame) (push (list 'size frame) calls) 160))
                ((symbol-function 'neo--fonts-apply-default-family)
                 (lambda (frame _value) (push (list 'default frame) calls)))
                ((symbol-function 'neo--fonts-apply-comment-family)
                 (lambda (frame _value) (push (list 'comment frame) calls))))
        (neo/fonts-apply)
        (expect (nreverse calls)
                :to-equal '((size laptop-frame)
                            (default laptop-frame)
                            (comment laptop-frame))))))

  (it "reapplies fonts for new and moved frames"
    (expect (memq #'neo/fonts-apply after-make-frame-functions)
            :to-be-truthy)
    (expect (memq #'neo/fonts-apply move-frame-functions)
            :to-be-truthy))

  (it "binds Super-minus to the standard text scaling command"
    (expect (lookup-key global-map (kbd "s--"))
            :to-be #'text-scale-adjust))

  (it "binds Super-equals and Super-plus to the standard text scaling command"
    (expect (lookup-key global-map (kbd "s-="))
            :to-be #'text-scale-adjust)
    (expect (lookup-key global-map (kbd "s-+"))
            :to-be #'text-scale-adjust)))

(provide 'test-neo-ui-fonts)
;;; test-neo-ui-fonts.el ends here
