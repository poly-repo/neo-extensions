;;; -*- lexical-binding: t -*-

;;; This is latex, a NEO extension
;;;
;;; Support for LaTeX

(neo/use-package auctex)

(neo/use-package cdlatex
  :hook
  (LaTeX-mode . turn-on-cdlatex))

;;--------------------------------------------------------------------
;; pdf-tools
(neo/use-package pdf-tools
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-annot-activate-created-annotations t "automatically annotate highlights")
  :config
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  :hook
  (pdf-view-mode . (lambda() (setq display-line-numbers-mode nil))))

;;--------------------------------------------------------------------
(add-hook 'LaTeX-mode-hook (lambda ()
                             (pdf-tools-install)
                             (require 'tex-site)
                             (setq pdf-view-use-scaling t)
                             (TeX-fold-mode 1)
                             (auto-fill-mode 1)

                             (flyspell-mode 1)
                             (setq flyspell-sort-corrections nil)
                             (setq flyspell-doublon-as-error-flag nil)

                             (setq split-width-threshold 80) ;  pdf-tool to open a pdf in the right side
                             (turn-on-auto-fill)             ; LaTeX mode，turn off auto fold
                             (latex-math-mode 1)
                             (outline-minor-mode 1)
                             (imenu-add-menubar-index)

                             (setq TeX-show-compilation nil) ; NOT display compilation windows
                             (setq TeX-global-PDF-mode t)    ; PDF mode enable, not plain
			     (setq TeX-engine 'luatex)      ; use xelatex default
                             (setq TeX-clean-confirm nil)
                             (setq TeX-save-query nil)

                             (setq font-latex-fontify-script t)
                             (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)
                             ;;(setq TeX-electric-escape t)      ; press \ then, jump to mini-buffer to input commands
                             ;;(setq TeX-view-program-list '(("Evince" "evince %o"))) ;;
                             ;;(setq TeX-view-program-selection '((output-pdf "Evince")))
                             (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
                                   TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
                                   TeX-source-correlate-start-server t)
                             ;;(add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
                             ;;(setq TeX-command-default "XeLaTeX")
			     (with-eval-after-load "tex"
			       (add-to-list 'TeX-command-list '("Arara" "arara --verbose %s" TeX-run-TeX nil t :help "Run Arara") t))
			     (with-eval-after-load "latex"
			       (define-key LaTeX-mode-map (kbd "C-c C-a") 
					   (lambda () (interactive) (TeX-command-sequence '("Arara") t))))
                             (add-to-list 'TeX-command-list '("LaTeX" "%`pdflatex -shell-escape --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
                             (setq TeX-command-default "LaTeX")
                             ;;(setq TeX-command-default "pdflatex --synctex=1")

                             (setq TeX-fold-env-spec-list (quote (("[comment]" ("comment")) ("[figure]" ("figure")) ("[table]" ("table"))("[itemize]"("itemize"))("[enumerate]"("enumerate"))("[description]"("description"))("[overpic]"("overpic"))("[tabularx]"("tabularx"))("[code]"("code"))("[shell]"("shell")))))


                             (define-key LaTeX-mode-map (kbd "C-c C-p") 'reftex-parse-all)
                             (define-key LaTeX-mode-map (kbd "C-c C-g") #'pdf-sync-forward-search)
			     (define-key LaTeX-mode-map (kbd "C-c '") #'neo/edit-code-block)

                             (setq LaTeX-section-hook
                                   '(LaTeX-section-heading
                                     LaTeX-section-title
                                     LaTeX-section-toc
                                     LaTeX-section-section
                                     LaTeX-section-label))

                             (setq pdf-sync-backward-display-action t
                                   pdf-sync-forward-display-action t
                                   TeX-source-correlate-mode t
                                   TeX-source-correlate-method '(
                                                                 (dvi . source-specials)
                                                                 (pdf . synctex))
                                   TeX-source-correlate-start-server t  ; [C-c C-g] to switch between source code and PDF
                                   reftex-plug-into-AUCTeX t)
                             (add-hook 'TeX-after-compilation-finished-functions
                                       #'TeX-revert-document-buffer) ;
                             (add-hook 'pdf-view-mode-hook (lambda() (display-line-numbers-mode -1)))
                             ))

(defun neo/starlark--bounds-auctex-precise ()
  "Return the bounds of the current starlark environment body."
  (save-excursion
    (unless (string= (LaTeX-current-environment) "starlark")
      (user-error "Not inside a starlark environment"))

    ;; Start after the newline that ends the \begin line.
    (LaTeX-find-matching-begin)
    (end-of-line)
    (let ((beg (if (eobp) (point) (1+ (point)))))

      ;; Stop at the beginning of the \end line.
      (LaTeX-find-matching-end)
      (beginning-of-line)
      (let ((end (point)))
        (cons beg end)))))

(defvar neo--latex-code-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'neo--latex-code-edit-accept)
    (define-key map (kbd "C-c C-k") #'neo--latex-code-edit-abort)
    map)
  "Transient keymap for temporary LaTeX code edit buffers.")

(defvar-local neo--latex-code-edit-source-buffer nil
  "Source buffer for the current temporary LaTeX code edit buffer.")

(defvar-local neo--latex-code-edit-start nil
  "Start marker for the source range being edited.")

(defvar-local neo--latex-code-edit-end nil
  "End marker for the source range being edited.")

(defvar-local neo--latex-code-edit-indent 0
  "Shared indentation stripped from the current code edit buffer.")

(defun neo--latex-code-edit-common-indent ()
  "Return the least indentation among nonblank lines in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (min-indent)
      (while (< (point) (point-max))
        (unless (looking-at-p "[ \t]*$")
          (setq min-indent
                (if min-indent
                    (min min-indent (current-indentation))
                  (current-indentation))))
        (forward-line 1))
      (or min-indent 0))))

(defun neo--latex-code-edit-shift-indentation (columns)
  "Shift nonblank lines in the current buffer by COLUMNS."
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (unless (looking-at-p "[ \t]*$")
        (let ((target-indent (max 0 (+ (current-indentation) columns))))
          (back-to-indentation)
          (delete-region (line-beginning-position) (point))
          (indent-to target-indent)))
      (forward-line 1))))

(defun neo--latex-close-code-edit-buffer (message-text)
  "Close the current temporary code edit buffer with MESSAGE-TEXT."
  (set-buffer-modified-p nil)
  (quit-window t)
  (message "%s" message-text))

(defun neo--latex-code-edit-abort ()
  "Abort the current temporary code edit."
  (interactive)
  (neo--latex-close-code-edit-buffer "Aborted code block edit"))

(defun neo--latex-code-edit-accept ()
  "Write the current temporary code edit back to the source buffer."
  (interactive)
  (let ((source-buffer neo--latex-code-edit-source-buffer)
        (start (and (markerp neo--latex-code-edit-start)
                    (marker-position neo--latex-code-edit-start)))
        (end (and (markerp neo--latex-code-edit-end)
                  (marker-position neo--latex-code-edit-end)))
        (indent neo--latex-code-edit-indent)
        (edited-text (buffer-substring-no-properties (point-min) (point-max))))
    (unless (buffer-live-p source-buffer)
      (user-error "The original LaTeX buffer is no longer available"))
    (unless (and start end)
      (user-error "The original code range is no longer available"))
    (when (> indent 0)
      (setq edited-text
            (with-temp-buffer
              (insert edited-text)
              (neo--latex-code-edit-shift-indentation indent)
              (buffer-string))))
    (with-current-buffer source-buffer
      (save-excursion
        (atomic-change-group
          (delete-region start end)
          (goto-char start)
          (insert edited-text))))
    (neo--latex-close-code-edit-buffer "Updated code block")))

(defun neo--latex-open-code-edit-buffer (code-range)
  "Open CODE-RANGE in a temporary `python-mode' buffer."
  (let* ((source-buffer (current-buffer))
         (start (car code-range))
         (end (cdr code-range))
         (buffer-name (format "*neo latex code: %s*" (buffer-name source-buffer)))
         (source-text (buffer-substring-no-properties start end))
         (start-marker (with-current-buffer source-buffer
                         (copy-marker start)))
         (end-marker (with-current-buffer source-buffer
                       (copy-marker end)))
         (buffer (generate-new-buffer buffer-name)))
    (with-current-buffer buffer
      (insert source-text)
      (let ((common-indent (neo--latex-code-edit-common-indent)))
        (when (> common-indent 0)
          (neo--latex-code-edit-shift-indentation (- common-indent)))
        (goto-char (point-min))
        (python-mode)
        (use-local-map
         (make-composed-keymap neo--latex-code-edit-map (current-local-map)))
        (setq-local header-line-format
                    "C-c C-c accept edits   C-c C-k abort")
        (setq-local neo--latex-code-edit-indent common-indent)
        (setq-local neo--latex-code-edit-source-buffer source-buffer)
        (setq-local neo--latex-code-edit-start start-marker)
        (setq-local neo--latex-code-edit-end end-marker)))
    (pop-to-buffer buffer)
    (message "Edit code block, then press C-c C-c to accept or C-c C-k to abort.")))

(defun neo/edit-code-block (code-range)
  "Edit CODE-RANGE from the current starlark environment."
  (interactive (list (neo/starlark--bounds-auctex-precise)))
  (neo--latex-open-code-edit-buffer code-range))

;;; Note, no (provide 'neo-latex) here, extensions are loaded not required.
