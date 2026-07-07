;;; -*- lexical-binding: t -*-
;; Grammar sources are declared in this extension's manifest.el as
;; `:tree-sitter-grammars'; collection across all installed extensions
;; and the actual grammar builds are handled centrally by
;; core/neo-treesit.el. This file only keeps the UI-level treesit
;; configuration (font-lock level, folding).

(neo/use-package treesit
  :ensure nil				; built-in
  :custom
  (treesit-font-lock-level 3)		; 4 should be more, but as of 30.2 it is less (for python)
  )

(neo/use-package treesit-fold
  :custom
  (treesit-fold-line-count-format " <%d lines> ")
  ;; `dape-breakpoint-global-mode' (neo:python) binds `<left-fringe>
  ;; mouse-1' to breakpoint-toggle in every `prog-mode' buffer,
  ;; Haskell included, and wins the minor-mode-map-alist race over
  ;; `treesit-fold-indicators-mode-map' there. Put the fold control on
  ;; the right fringe instead so both coexist rather than one
  ;; silently shadowing the other.
  (treesit-fold-indicators-fringe 'right-fringe)
  :config
  (global-treesit-fold-indicators-mode 1))

(with-eval-after-load 'treesit-fold-indicators
  ;; Belt-and-suspenders for the fringe-side split above: drop the
  ;; left-fringe binding entirely so a future minor-mode-map-alist
  ;; reordering can't make this map win the left fringe and swallow
  ;; dape's breakpoint click with a silent no-op (the handler checks
  ;; `treesit-fold-indicators-fringe' and does nothing on a mismatch).
  (define-key treesit-fold-indicators-mode-map [left-fringe mouse-1] nil))

(with-eval-after-load 'treesit-fold
  (defun neo/treesit-fold-disable-indicator-refresh-on-typing ()
    "Do not refresh tree-sitter fold indicators refreshing on every character
typed. This helps improve performance."
    (remove-hook 'post-command-hook #'treesit-fold-indicators--post-command t))

  (advice-add 'treesit-fold-indicators--enable
              :after #'neo/treesit-fold-disable-indicator-refresh-on-typing))

(defun neo/treesit-fold--stretch-bitmap-rows (rows scale)
  "Repeat each row integer in ROWS SCALE times, return as a fringe-bitmap vector."
  (apply #'vector (apply #'append (mapcar (lambda (row) (make-list scale row)) rows))))

(with-eval-after-load 'treesit-fold-indicators
  ;; Upstream's +/- glyph is only 7px tall, so on a normal ~20px line
  ;; it renders as a barely-visible speck centered in a lot of empty
  ;; fringe. Redraw it (and the matching box baked into the head-line
  ;; bitmap) at 2x so the toggle control is actually legible.
  (let ((plus-rows '(#b1111111 #b1000001 #b1001001 #b1011101 #b1001001 #b1000001 #b1111111))
        (scale 2))
    (define-fringe-bitmap 'treesit-fold-indicators-fr-plus
      (neo/treesit-fold--stretch-bitmap-rows plus-rows scale))
    (define-fringe-bitmap 'treesit-fold-indicators-fr-minus-tail
      (vconcat (make-list 10 #b00000000)
               (neo/treesit-fold--stretch-bitmap-rows plus-rows scale)
               (make-list 6 #b00011000))))

  ;; Upstream also only wires up mouse-1 on the *head* line of a fold
  ;; range (the `fr-plus'/`fr-minus-tail' bitmap). For a fold spanning
  ;; many lines -- e.g. a large Haskell import block -- the head line
  ;; is frequently scrolled out of view, so clicking anywhere else on
  ;; the visible fold bar (`fr-center'/`fr-end-*') silently does
  ;; nothing. `treesit-fold-toggle' walks up to the enclosing foldable
  ;; node from point regardless of which line inside it we start from,
  ;; so widen the click target to the whole bar.
  (defun treesit-fold-indicators-click-fringe (event)
    "EVENT click on fringe."
    (interactive "e")
    (let ((current-fringe (nth 1 (car (cdr event)))) ovs ov cur-ln)
      (when (eq current-fringe treesit-fold-indicators-fringe)
        (mouse-set-point event)
        (beginning-of-line)
        (setq cur-ln (line-number-at-pos (point)))
        (setq ovs (seq-mapcat
                   (lambda (type) (treesit-fold--overlays-in 'type type))
                   '(treesit-fold-indicators-fr-plus
                     treesit-fold-indicators-fr-minus-tail
                     treesit-fold-indicators-fr-center
                     treesit-fold-indicators-fr-end-left
                     treesit-fold-indicators-fr-end-right)))
        (when ovs
          (setq ov (cl-some
                    (lambda (ov) (= cur-ln (line-number-at-pos (overlay-start ov))))
                    ovs))
          (when ov
            (or (save-excursion
                  (end-of-line)
                  (when (nth 4 (syntax-ppss)) (back-to-indentation))
                  (treesit-fold-toggle))
                (treesit-fold-toggle))))))))

;; (use-package treesit-auto
;;   :custom
;;   (setq treesit-auto-install nil)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

(provide 'neo-programming-foundation-treesit)
