;;; -*- lexical-binding: t -*-

(defun neo/screen-geometry ()
  "Return the geometry of the primary monitor as a plist: (:x X :y Y :width W :height H).
Should work on X11 and Wayland. On text terminal width and height is in
characters and lines raqther than pixels."
  (let* ((monitors (display-monitor-attributes-list))
         (primary (car monitors))  ;; Fallback: first monitor
         (geom (assq 'geometry primary)))
    (when geom
      (let ((g (cdr geom)))
        (list :x (nth 0 g)
              :y (nth 1 g)
              :width (nth 2 g)
              :height (nth 3 g))))))

(defun neo/display-dpi (&optional display)
  "Return the approximate DPI of DISPLAY (default: selected frame's display).
Returns a cons cell (dpi-x . dpi-y), or nil if unavailable (text
terminals for instance)."
  (let* ((display (or display (frame-parameter nil 'display)))
         (mm-width (display-mm-width display))
         (mm-height (display-mm-height display))
         (px-width (display-pixel-width display))
         (px-height (display-pixel-height display)))
    (when (and mm-width mm-height (> mm-width 0) (> mm-height 0))
      (cons
       (/ (* px-width 25.4) mm-width)   ;; dpi-x = pixels / (mm / 25.4)
       (/ (* px-height 25.4) mm-height)))))

(defun neo/font-height-to-fit-lines (lines &optional font-name)
  "Return the font height needed to fit LINES vertically in the current screen.
Optional FONT-NAME can be used to compute based on a specific font family."
  (interactive "nNumber of lines to fit: ")
  (let* ((geom (neo/screen-geometry))
         (screen-height (plist-get geom :height))
         (window (selected-window))
         (line-height
          (condition-case nil
              (let* ((start-pos (posn-at-point (point-min) window))
                     (end-pos (posn-at-point (save-excursion (goto-char (point-min)) (forward-line 1) (point)) window))
                     (start-px (and start-pos (cdr (posn-x-y start-pos))))
                     (end-px   (and end-pos (cdr (posn-x-y end-pos)))))
                (if (and start-px end-px)
                    (- (cdr end-px) (cdr start-px))
                  (frame-char-height)))
            (error (frame-char-height))))
         (current-font-height (face-attribute 'default :height))
         (font-height (floor (* current-font-height (/ (float screen-height) (* lines line-height))))))
    (when (called-interactively-p 'interactive)
      (message "Calculated font height: %d (for %d lines, %d px line height)" font-height lines line-height))
    font-height))

;; TODO maybe we could do something interesting for terminals with window-system-default-frame-alist.
(defun neo--frame-geometry-alist (frame)
  "Return an alist describing FRAME's position and size for restoration.
Size is saved in CHARACTER columns/rows (honored by `default-frame-alist' at
frame creation on this build).  Position is saved for reference only and is
not restored (the window manager places the frame).  When FRAME is maximized
or fullscreen, that state is saved instead of an explicit geometry."
  (let ((fullscreen (frame-parameter frame 'fullscreen)))
    (if fullscreen
        (list (cons 'fullscreen fullscreen))
      ;; Save SIZE ONLY, in CHARACTER columns/rows (honored by
      ;; `default-frame-alist' at frame creation on this build).  Position is
      ;; deliberately NOT persisted or restored: on a reparenting window manager
      ;; the set/report position round-trip drifts by a constant, so restoring
      ;; it makes the frame "walk" across launches.  We let the window manager
      ;; place the frame instead.
      (list (cons 'width (frame-width frame))
            (cons 'height (frame-height frame))))))

(defun neo--frame-alist-string (var alist)
  "Return Elisp setting VAR to the quoted ALIST, one entry per line."
  (concat (format "(setq %s\n      '(" var)
          (mapconcat (lambda (cell) (format "%S" cell)) alist "\n        ")
          "))\n"))

(defun neo--frame-suitable-for-save-p (frame)
  "Return non-nil when FRAME's geometry is worth persisting.
Excludes non-graphic frames, tooltips, child/pop-up frames (posframe,
corfu, …), and frames too small to be usefully split — any of which would
otherwise clobber the saved geometry with an unusable size."
  (and (frame-live-p frame)
       (display-graphic-p frame)
       (frame-visible-p frame)
       (not (frame-parameter frame 'parent-frame))   ; not a child frame
       (not (frame-parameter frame 'tooltip))
       (>= (frame-text-width frame) neo/minimum-frame-pixel-width)
       (>= (frame-text-height frame) neo/minimum-frame-pixel-height)))

(defun neo--frame-to-save ()
  "Return the frame whose geometry should be persisted, or nil if none.
Prefer the selected frame; otherwise fall back to the largest suitable
frame.  Returning nil means \"do not overwrite the saved geometry\", so a
session with only unsuitable frames leaves the last good geometry intact."
  (if (neo--frame-suitable-for-save-p (selected-frame))
      (selected-frame)
    (car (sort (seq-filter #'neo--frame-suitable-for-save-p (frame-list))
               (lambda (a b)
                 (> (* (frame-text-width a) (frame-text-height a))
                    (* (frame-text-width b) (frame-text-height b))))))))

(defun neo/save-initial-frame-properties ()
  "Save a suitable frame's geometry, font, and default face colors.
The result is written to `initial-frame-properties.el' in the current
profile so early-init.el restores the frame on the next launch.  Both
position and size (in pixels) are preserved, along with any maximized or
fullscreen state.  This runs on `kill-emacs-hook'.  Only a real, visible,
top-level graphic frame that is large enough is saved (see
`neo--frame-suitable-for-save-p'); if none exists the previous saved
geometry is left untouched rather than overwritten with a bogus size."
  (interactive)
  (when-let* ((frame (neo--frame-to-save)))
    (let* ((font (frame-parameter frame 'font))
           (look `((font . ,font)
                   (internal-border-width . 0)
                   (undecorated . nil)))
           (bg (face-background 'default nil t))
           (fg (face-foreground 'default nil t))
           (family (face-attribute 'default :family nil 'default))
           (font-height (face-attribute 'default :height nil 'default))
           (file (neo/config-file-path "initial-frame-properties.el")))
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert ";; Auto-generated by neo/save-initial-frame-properties\n")
        (insert ";; Restores frame geometry, font, and default face colors.\n\n")
        ;; Only the initial frame is pinned to the saved geometry; later
        ;; frames inherit just the look so they do not stack at one spot.
        (insert (neo--frame-alist-string "initial-frame-alist"
                                         (append (neo--frame-geometry-alist frame)
                                                 look)))
        (insert "\n")
        (insert (neo--frame-alist-string "default-frame-alist" look))
        (insert "\n")
        (insert (format "(set-face-attribute 'default nil :background %S :foreground %S :family %S :height %d)\n"
                        bg fg family font-height))))))

(add-hook 'kill-emacs-hook #'neo/save-initial-frame-properties)

(defun neo/ensure-frame-onscreen-and-usable (&optional frame)
  "Force FRAME (default selected) to a usable size and on-screen position.
Safety net for a frame that ends up too small to split into windows — e.g.
restored geometry the toolkit did not honor, or a window-manager clamp.
Sizes the frame up to at least the NEO default, never beyond the monitor
work area, and nudges it fully on-screen.  Child/pop-up frames (posframe,
corfu, …) are left untouched.  Idempotent: a frame already large enough and
on-screen is not touched."
  (setq frame (or frame (selected-frame)))
  (when (and (frame-live-p frame)
             (display-graphic-p frame)
             (not (frame-parameter frame 'parent-frame)))
    (let* ((wa (frame-monitor-workarea frame))
           (wx (nth 0 wa)) (wy (nth 1 wa)) (ww (nth 2 wa)) (wh (nth 3 wa))
           (cw (max 1 (frame-char-width frame)))
           (chh (max 1 (frame-char-height frame)))
           (max-cols (max neo/minimum-frame-cols (/ ww cw)))
           (max-rows (max neo/minimum-frame-rows (/ wh chh)))
           (cols (frame-width frame))
           (rows (frame-height frame))
           (want-cols (min max-cols (max cols neo/default-frame-width)))
           (want-rows (min max-rows (max rows neo/default-frame-height))))
      (when (or (/= want-cols cols) (/= want-rows rows))
        (set-frame-size frame want-cols want-rows))
      ;; Reposition ONLY when the frame is off-screen by more than a small
      ;; margin.  Nudging an already-visible frame every launch is what makes
      ;; the position "jump around", so leave a visible frame exactly where the
      ;; window manager put it.
      (let* ((pos (frame-position frame))
             (fx (car pos)) (fy (cdr pos))
             (pw (frame-pixel-width frame))
             (ph (frame-pixel-height frame))
             (margin 16)
             (nx (cond ((< fx (- wx margin)) wx)
                       ((> (+ fx pw) (+ wx ww margin)) (max wx (- (+ wx ww) pw)))
                       (t fx)))
             (ny (cond ((< fy (- wy margin)) wy)
                       ((> (+ fy ph) (+ wy wh margin)) (max wy (- (+ wy wh) ph)))
                       (t fy))))
        (when (or (/= nx fx) (/= ny fy))
          (set-frame-position frame nx ny))))))

(defun neo--repair-collapsed-frame (&optional frame)
  "Resize FRAME back to its intended size if the toolkit created it collapsed.
About 1 launch in 4, this Emacs build creates the initial frame at ~200x200px
(roughly 20x11 chars) instead of the requested size, and it never
self-corrects (see ~/repro for the analysis).  ONLY when the frame is that far
too small do we force the restored size back — the character size, the legacy
`(text-pixels . N)' size, or the NEO default when nothing was saved.  This
NEVER repositions the frame, so running it on retry timers cannot make the
frame walk."
  (setq frame (or frame (selected-frame)))
  (when (and (frame-live-p frame)
             (display-graphic-p frame)
             (not (frame-parameter frame 'parent-frame))
             (or (< (frame-width frame) neo/minimum-frame-cols)
                 (< (frame-height frame) neo/minimum-frame-rows)))
    (let* ((geom (bound-and-true-p neo--restored-frame-geometry))
           (w (cdr (assq 'width geom)))
           (h (cdr (assq 'height geom))))
      (cond
       ((and (consp w) (eq (car w) 'text-pixels)
             (consp h) (eq (car h) 'text-pixels))
        (set-frame-size frame (cdr w) (cdr h) t))
       ((and (integerp w) (integerp h))
        (set-frame-size frame w h))
       (t (set-frame-size frame neo/default-frame-width neo/default-frame-height))))))

(defun neo/apply-restored-frame-geometry (&optional frame)
  "Repair a collapsed FRAME then apply the on-screen clamp and usable floor.
Repositioning happens only here (once at startup / per new frame), never on
the retry timers."
  (neo--repair-collapsed-frame frame)
  (neo/ensure-frame-onscreen-and-usable frame))

;; Repair a collapsed frame at startup AND on a few short retry timers (the
;; collapse can appear late and never self-corrects).  The retries call the
;; resize-ONLY repair — they never reposition — so they cannot make the frame
;; walk.  The on-screen clamp/floor runs once at startup and for each new frame.
(add-hook 'emacs-startup-hook #'neo/apply-restored-frame-geometry)
(dolist (neo--frame-fix-delay '(0.2 0.6 1.2 2.5))
  (run-with-timer neo--frame-fix-delay nil #'neo--repair-collapsed-frame))
(add-hook 'after-make-frame-functions #'neo/ensure-frame-onscreen-and-usable)

(defun neo--nearest-ratio (ratio)
  "Return the nearest canonical ratio described as (W H NAME)."
  (let* ((candidates
          ;; list of (W H NAME)
          '((1 1 "1:1")
            (4 3 "4:3")
            (5 4 "5:4")
            (16 10 "16:10")
            (3 2 "3:2")
            (16 9 "16:9")
            (21 9 "21:9")
            (24 10 "24:10")
            (32 9 "32:9")))
         (best nil)
         (best-diff 1e9))
    (dolist (c candidates)
      (let* ((w (nth 0 c))
             (h (nth 1 c))
             (r (/ (float w) (float h)))
             (diff (abs (- r ratio))))
        (when (< diff best-diff)
          (setq best-diff diff)
          (setq best c))))
    ;; return list (W H NAME DIFF RATIO)
    (let* ((w (nth 0 best))
           (h (nth 1 best))
           (name (nth 2 best))
           (r (/ (float w) (float h)))
           (diff best-diff))
      (list w h name diff r))))

(defun neo--aspect-family-from-ratio (ratio)
  "Return an aspect-family keyword for numeric RATIO (width/height)."
  (cond
   ((= (round (* ratio 100)) 100) 'square) ;; approx 1:1
   ((> ratio 3.0) 'super-ultrawide)
   ((> ratio 2.0) 'ultrawide)
   ((> ratio 1.8) 'wide)
   ((> ratio 1.4) 'regular)
   (t 'tall)))

(defun neo--resolution-class-from-pixels (total-px width height)
  "Classify by total pixel count TOTAL-PX (and optionally width/height)."
  (cond
   ((>= total-px 33000000) '8k)
   ((>= total-px 20000000) '6k)
   ((>= total-px 14000000) '5k)
   ((>= total-px 8000000)  '4k)   ;; ~8.3MP
   ((>= total-px 3500000)  'qhd)  ;; ~3.5MP
   ((>= total-px 2000000)  'fhd)  ;; ~2MP
   ((>= total-px 800000)   'hd)
   (t 'vga)))

(defun neo--dpi-class (dpi)
  "Return dpi class keyword from DPI average."
  (cond
   ((not dpi) 'terminal)
   ((< dpi 100) 'low-dpi)
   ((< dpi 150) 'standard-dpi)
   ((< dpi 250) 'hidpi)
   (t 'ultra-hidpi)))

(defun neo--ui-pt-for-dpi-class (class)
  (pcase class
    ('terminal     0.0)
    ('low-dpi      11.0)
    ('standard-dpi 12.5)
    ('hidpi        14.0)
    ('ultra-hidpi  16.0)
    (_             12.5)))

(defun neo--code-pt-from-ui-pt (ui-pt)
  (* ui-pt 0.92))

(defun neo--pt->px (pt dpi)
  (* pt (/ dpi 72.0)))

(defun neo--recommended-scale (dpi-class dpi)
  "Return a suggested UI scale factor (float) based on DPI-CLASS and DPI."
  (pcase dpi-class
    ('terminal 0.0)
    ('low-dpi 1.25)
    ('standard-dpi 1.0)
    ('hidpi 1.5)
    ('ultra-hidpi (if (> dpi 300) 2.0 1.75))
    (_ 1.0)))

;;;###autoload
;; (defun neo/classify-monitor-aux (width height dpi-h dpi-v)
;;   "Classify a monitor given pixel size WIDTH x HEIGHT and DPI_H / DPI_V.
;; WIDTH and HEIGHT are pixels (integers). DPI_H and DPI_V are pixels-per-inch
;; (horizontal and vertical). Returns a plist with classification fields."
;;   (unless (and (numberp width) (numberp height) (> width 0) (> height 0))
;;     (error "Width and height must be positive numbers"))
;;   (let* ((dpi-h (or dpi-h 1))
;; 	 (dpi-v (or dpi-v 1))
;; 	 (w (float width))
;;          (h (float height))
;;          (orientation (if (>= w h) 'landscape 'portrait))
;;          (ratio (/ w h))
;;          (ratio-inv (/ h w))
;;          (ratio-nearest (neo--nearest-ratio ratio))
;;          (ratio-name (nth 2 ratio-nearest))
;;          (ratio-diff (nth 3 ratio-nearest))
;;          (ratio-canon (format "%d:%d" (nth 0 ratio-nearest) (nth 1 ratio-nearest)))
;;          (aspect-family (neo--aspect-family-from-ratio ratio))
;;          (total-px (round (* w h)))
;;          (resolution-class (neo--resolution-class-from-pixels total-px width height))
;;          (dpi-h (when dpi-h (float dpi-h)))
;;          (dpi-v (when dpi-v (float dpi-v)))
;;          (dpi-avg (when (and dpi-h dpi-v) (/ (+ dpi-h dpi-v) 2.0)))
;;          (dpi-class (neo--dpi-class dpi-avg))
;;          (recommended-scale (neo--recommended-scale dpi-class dpi-avg))
;;          (aspect-string (if (<= ratio-diff 0.03) ; within ~3% -> canonical name
;;                             ratio-canon
;;                           (format "%.2f:1" (round (* ratio 100.0))))) ;; fallback
;;          ;; collect some helpful human strings
;;          (human (list
;;                  (cons 'resolution (format "%dx%d" width height))
;;                  (cons 'total_pixels total-px)
;;                  (cons 'aspect_ratio (format "%.4f" ratio))
;;                  (cons 'aspect_nearest ratio-name)
;;                  (cons 'orientation (symbol-name orientation))
;;                  (cons 'dpi_avg (if dpi-avg (format "%.1f" dpi-avg) "n.a.")))))
;;     ;; Return plist
;;     (list
;;      :width width
;;      :height height
;;      :total-pixels total-px
;;      :resolution-class resolution-class
;;      :orientation orientation
;;      :aspect-ratio aspect-string
;;      :aspect-family aspect-family
;;      :canonical-aspect ratio-canon
;;      :aspect-nearest-name (nth 2 ratio-nearest)
;;      :aspect-nearest-diff (round (* 1000 (nth 3 ratio-nearest))) ; thousandth of difference
;;      :dpi-h dpi-h
;;      :dpi-v dpi-v
;;      :dpi-avg dpi-avg
;;      :dpi-class dpi-class
;;      :recommended-scale recommended-scale
;;      :human human)))

(defun neo/classify-monitor-aux (width height dpi-h dpi-v)
  "Return a plist describing the monitor:
 - orientation (portrait/landscape)
 - aspect-ratio category
 - resolution class (HD/WQHD/4K/etc)
 - DPI class
 - plus recommended UI & code font sizes (pt + px)."
  (let* ((w (float width))
         (h (float height))
         (aspect (/ w h))
         ;; orientation
         (orientation (if (> h w) 'portrait 'landscape))

         ;; aspect ratio class
         (aspect-class
          (cond
           ((< aspect 1.4) 'regular)      ;; 4:3, 5:4
           ((< aspect 2.1) 'widescreen)   ;; 16:10, 16:9
           ((< aspect 3.1) 'ultrawide)    ;; 21:9
           (t               'superultrawide))) ;; 32:9, 48:9

         ;; resolution class
         (res-class
          (cond
           ((<= h 900) 'hd)
           ((<= h 1440) 'wqhd)
           ((<= h 2160) '4k)
           ((<= h 2880) '5k)
           ((<= h 4320) '8k)
           (t 'insane)))
	 (ratio (/ w h))
         (ratio-inv (/ h w))
         (ratio-nearest (neo--nearest-ratio ratio))
         (ratio-name (nth 2 ratio-nearest))
         (ratio-diff (nth 3 ratio-nearest))
         (ratio-canon (format "%d:%d" (nth 0 ratio-nearest) (nth 1 ratio-nearest)))
         (aspect-family (neo--aspect-family-from-ratio ratio))
	 (aspect-string (if (<= ratio-diff 0.03) ; within ~3% -> canonical name
                            ratio-canon
                          (format "%.2f:1" (round (* ratio 100.0))))) ;; fallback
         ;; DPI handling
         (dpi-h (if dpi-h (float dpi-h) 96.0)) ; 96.0 is a default for text terminals
         (dpi-v (if dpi-v (float dpi-v) 96.0)) ; 96.0 is a default for text terminals
         (dpi-avg (/ (+ dpi-h dpi-v) 2.0))
         (dpi-class (neo--dpi-class dpi-avg))

         ;; font recommendations
	 (dpi-avg (/ (+ dpi-h dpi-v) 2.0))
	 (dpi-class (neo--dpi-class dpi-avg))
         (recommended-scale (neo--recommended-scale dpi-class dpi-avg))
         (ui-pt (neo--ui-pt-for-dpi-class dpi-class))
         (code-pt (neo--code-pt-from-ui-pt ui-pt))
         (ui-px (round (neo--pt->px ui-pt dpi-avg)))
         (code-px (round (neo--pt->px code-pt dpi-avg))))

    (list
     ;; basic geometry
     :width width
     :height height
     :orientation orientation
     :aspect aspect
     :aspect-class aspect-class
     :resolution-class res-class
     :aspect-ratio aspect-string
     :aspect-family aspect-family
     :canonical-aspect ratio-canon
     :aspect-nearest-name (nth 2 ratio-nearest)
     :aspect-nearest-diff (round (* 1000 (nth 3 ratio-nearest))) ; thousandth of difference
     :recommended-fullscreen (neo/frame-fullscreen-or-window width height)
     ;; DPI
     :dpi-h dpi-h
     :dpi-v dpi-v
     :dpi dpi-avg
     :dpi-class dpi-class

     ;; recommended fonts
     :recommended-scale recommended-scale
     :ui-pt ui-pt
     :ui-px ui-px
     :code-pt code-pt
     :code-px code-px)))

(defun neo/classify-monitor ()
  (let* ((screen-geometry (neo/screen-geometry))
	 (width (plist-get screen-geometry :width))
	 (height (plist-get screen-geometry :height))
	 (screen-resolution (neo/display-dpi))
	 (x-dpi (car screen-resolution))
	 (y-dpi (cdr screen-resolution)))
    (neo/classify-monitor-aux width height x-dpi y-dpi)))


(defun neo/frame-fullscreen-or-window (width height)
  "Decide whether to make the frame full-screen or windowed.
Use full-screen on large monitors, normal window on smaller ones."
  (let* ((large-width 1920)
         (large-height 1080))
    (if (and width height
             (>= width large-width)
             (>= height large-height))
        'fullboth			; TODO make it customizable there's maximized and fullscreen as well
      nil)))

(defun neo/get-current-monitor-classification ()
  (if (display-graphic-p)
      (neo/classify-monitor-aux 
       (display-pixel-width) (display-pixel-height)
       (frame-parameter nil 'res-x) (frame-parameter nil 'res-y))
    ;; Terminal defaults
    (neo/classify-monitor-aux 
     (frame-width) (frame-height) 96.0 96.0)))

;; ;; Apply to initial frame
;; (add-to-list 'initial-frame-alist
;;              `(fullscreen . ,(neo/frame-fullscreen-or-window)))

(provide 'neo-ui-frame)
