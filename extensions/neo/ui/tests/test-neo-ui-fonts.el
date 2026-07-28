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
                ((symbol-function 'frame-parameter) (lambda (&rest _) 0))
                ((symbol-function 'neo--fonts-height-for-frame)
                 (lambda (_frame _value) 160))
                ((symbol-function 'set-face-attribute)
                 (lambda (&rest args) (setq face-call args))))
        (expect (neo--fonts-apply-frame-size 'laptop-frame) :to-equal 160)
        (expect face-call
                :to-equal '(default laptop-frame :height 160)))))

  (it "retains a frame-local adjustment when monitor sizing is reapplied"
    (let (face-call)
      (cl-letf (((symbol-function 'frame-live-p) (lambda (_frame) t))
                ((symbol-function 'display-graphic-p) (lambda (&optional _frame) t))
                ((symbol-function 'frame-parameter)
                 (lambda (_frame parameter)
                   (when (eq parameter 'neo--fonts-height-adjustment) 10)))
                ((symbol-function 'neo--fonts-height-for-frame)
                 (lambda (_frame _value) 130))
                ((symbol-function 'set-face-attribute)
                 (lambda (&rest args) (setq face-call args))))
        (expect (neo--fonts-apply-frame-size 'external-frame) :to-equal 140)
        (expect face-call
                :to-equal '(default external-frame :height 140)))))

  (it "increases the selected frame's monitor-aware font size"
    (let ((adjustment 0)
          frame-parameter-call
          face-call)
      (cl-letf (((symbol-function 'selected-frame) (lambda () 'laptop-frame))
                ((symbol-function 'frame-live-p) (lambda (_frame) t))
                ((symbol-function 'display-graphic-p) (lambda (&optional _frame) t))
                ((symbol-function 'frame-parameter)
                 (lambda (_frame parameter)
                   (when (eq parameter 'neo--fonts-height-adjustment)
                     adjustment)))
                ((symbol-function 'set-frame-parameter)
                 (lambda (frame parameter value)
                   (setq frame-parameter-call (list frame parameter value)
                         adjustment value)))
                ((symbol-function 'neo--fonts-height-for-frame)
                 (lambda (_frame _value) 160))
                ((symbol-function 'set-face-attribute)
                 (lambda (&rest args) (setq face-call args))))
        (expect (neo/fonts-increase-size) :to-equal 170)
        (expect frame-parameter-call
                :to-equal
                '(laptop-frame neo--fonts-height-adjustment 10))
        (expect face-call
                :to-equal '(default laptop-frame :height 170)))))

  (it "decreases only the requested frame's monitor-aware font size"
    (let ((adjustment 20)
          frame-parameter-call
          face-call)
      (cl-letf (((symbol-function 'frame-live-p) (lambda (_frame) t))
                ((symbol-function 'display-graphic-p) (lambda (&optional _frame) t))
                ((symbol-function 'frame-parameter)
                 (lambda (_frame parameter)
                   (when (eq parameter 'neo--fonts-height-adjustment)
                     adjustment)))
                ((symbol-function 'set-frame-parameter)
                 (lambda (frame parameter value)
                   (setq frame-parameter-call (list frame parameter value)
                         adjustment value)))
                ((symbol-function 'neo--fonts-height-for-frame)
                 (lambda (_frame _value) 160))
                ((symbol-function 'set-face-attribute)
                 (lambda (&rest args) (setq face-call args))))
        (expect (neo/fonts-decrease-size 'external-frame) :to-equal 170)
        (expect frame-parameter-call
                :to-equal
                '(external-frame neo--fonts-height-adjustment 10))
        (expect face-call
                :to-equal '(default external-frame :height 170)))))

  (it "persists an absolute frame-local override for the default preference"
    (let ((neo/config/preferred-font-size 'default)
          override
          frame-parameter-call
          face-call)
      (cl-letf (((symbol-function 'frame-live-p) (lambda (_frame) t))
                ((symbol-function 'display-graphic-p) (lambda (&optional _frame) t))
                ((symbol-function 'frame-parameter)
                 (lambda (_frame parameter)
                   (when (eq parameter 'neo--fonts-height-override)
                     override)))
                ((symbol-function 'set-frame-parameter)
                 (lambda (frame parameter value)
                   (setq frame-parameter-call (list frame parameter value)
                         override value)))
                ((symbol-function 'face-attribute)
                 (lambda (&rest _) 120))
                ((symbol-function 'set-face-attribute)
                 (lambda (&rest args) (setq face-call args))))
        (expect (neo/fonts-increase-size 'laptop-frame) :to-equal 130)
        (expect frame-parameter-call
                :to-equal
                '(laptop-frame neo--fonts-height-override 130))
        (expect face-call
                :to-equal '(default laptop-frame :height 130)))))

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

  (it "binds Super-minus to decrease the selected frame's font size"
    (expect (lookup-key global-map (kbd "s--"))
            :to-be #'neo/fonts-decrease-size))

  (it "binds Super-equals and Super-plus to increase the selected frame's font size"
    (expect (lookup-key global-map (kbd "s-="))
            :to-be #'neo/fonts-increase-size)
    (expect (lookup-key global-map (kbd "s-+"))
            :to-be #'neo/fonts-increase-size)))

(provide 'test-neo-ui-fonts)
;;; test-neo-ui-fonts.el ends here
