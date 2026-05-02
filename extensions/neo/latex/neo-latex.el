;;; -*- lexical-binding: t -*-

;;; This is latex, a NEO extension
;;;
;;; Support for LaTeX

;;; Score / build helpers

(defconst neo/latex-score-command "o-score")
(defconst neo/latex-score-pdf "the-score-online.pdf")
(defconst neo/latex-arara-jobname "main")

(defun neo/latex-score-root ()
  "Return the score project root."
  (or (locate-dominating-file default-directory ".rules")
      (locate-dominating-file default-directory "arara.yaml")
      (locate-dominating-file default-directory "main.tex")
      default-directory))

(defun neo/latex-score-preamble ()
  "Return o-score preamble based on prefix arg."
  (if current-prefix-arg
      "online"       ; normal, not fast, not draft
    nil))            ; script default: draft-fast-online

(defun neo/latex-score-build ()
  "Build score. With C-u, use normal online build."
  (interactive)
  (let* ((root (neo/latex-score-root))
         (default-directory root)
         (preamble (neo/latex-score-preamble))
         (cmd (if preamble
                  (format "%s %s" neo/latex-score-command preamble)
                neo/latex-score-command)))
    (compile cmd)))

(defun neo/latex-score-build-current-chapter ()
  "Build current chapter. With C-u, use normal online build."
  (interactive)
  (let* ((target (or (neo/latex-current-chapter-target)
                     (completing-read "Chapter target: " neo/latex-targets nil t)))
         (root (neo/latex-score-root))
         (default-directory root)
         (preamble (neo/latex-score-preamble))
         (cmd (if preamble
                  (format "%s %s --target %s"
                          neo/latex-score-command
                          preamble
                          (shell-quote-argument target))
                (format "%s --target %s"
                        neo/latex-score-command
                        (shell-quote-argument target)))))
    (compile cmd)))

(defun neo/latex-score-pdf-path ()
  "Return the online PDF path."
  (expand-file-name neo/latex-score-pdf (neo/latex-score-root)))

(defun neo/latex-score-view ()
  "Open the online score PDF in Zathura."
  (interactive)
  (let ((pdf (neo/latex-score-pdf-path)))
    (unless (file-exists-p pdf)
      (user-error "PDF does not exist yet: %s" pdf))
    (start-process "zathura" nil "zathura" pdf)))

(defun neo/latex-current-chapter-target ()
  "Infer chap:... target from current buffer filename."
  (when-let* ((file (buffer-file-name))
              (base (file-name-base file)))
    (when (string-match "^[0-9]+-\\(.+\\)$" base)
      (concat "chap:" (match-string 1 base)))))

(defun neo/latex-score-build-current ()
  "Build only the current chapter using o-score."
  (interactive)
  (let* ((target (neo/latex-current-chapter-target))
         (root (neo/latex-score-root))
         (default-directory root)
         (cmd (if target
                  (format "%s --target %s"
                          neo/latex-score-command
                          (shell-quote-argument target))
                neo/latex-score-command)))
    (message "Running: %s" cmd)
    (compile cmd)))

(defun neo/latex-output-pdf ()
  "Return the online PDF produced by arara."
  (let* ((master (TeX-master-file))
         (dir (file-name-directory master)))
    (expand-file-name
     (concat neo/latex-arara-jobname ".pdf")
     dir)))

(defun neo/latex-view-online-pdf ()
  "Open the online arara PDF in Evince."
  (interactive)
  (let ((pdf (neo/latex-output-pdf)))
    (unless (file-exists-p pdf)
      (user-error "PDF does not exist yet: %s" pdf))
    (start-process "evince" nil "evince" pdf)))

;;; Code-block edit infrastructure

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

;;; Mode setup

(defun neo/latex-mode-setup ()
  "Configure LaTeX-mode buffer for NEO workflow."
  (add-to-list
   'TeX-command-list
   '("Arara" "arara --verbose %s"
     TeX-run-TeX nil t
     :help "Run arara on the master file"))
  (TeX-fold-mode 1)
  (LaTeX-math-mode 1)
  (outline-minor-mode 1)
  (flyspell-mode 1)
  (auto-fill-mode 1)
  (setq-local TeX-command-default "Arara")
  (local-set-key (kbd "C-c C-a") #'neo/latex-score-build)
  (local-set-key (kbd "C-c C-c") #'neo/latex-score-build-current)
  (local-set-key (kbd "C-c C-t") #'neo/latex-score-build-current-chapter)
  (local-set-key (kbd "C-c C-v") #'neo/latex-score-view)
  (local-set-key (kbd "C-c C-p") #'reftex-parse-all)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'reftex-reference)
    (define-key map (kbd "c") #'reftex-citation)
    (define-key map (kbd "l") #'reftex-label)
    (define-key map (kbd "t") #'reftex-toc)
    (define-key map (kbd "p") #'reftex-parse-all)
    (local-set-key (kbd "C-c C-r") map))
  (local-set-key (kbd "C-c '") #'neo/edit-code-block))

;;; Packages

(neo/use-package reftex
  :ensure nil
  :after auctex
  :hook (LaTeX-mode . turn-on-reftex)
  :custom
  (reftex-plug-into-AUCTeX t)
  :config
  (setq reftex-label-alist
        '(("section"  ?s "sec:"  "~\\ref{%s}"    nil nil)
          ("figure"   ?f "fig:"  "~\\ref{%s}"    nil nil)
          ("table"    ?t "tab:"  "~\\ref{%s}"    nil nil)
          ("equation" ?e "eq:"   "~\\eqref{%s}"  nil nil)
          ("chapter"  ?c "chap:" "~\\ref{%s}"    nil nil))))

(neo/use-package auctex
  :defer t
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)                 ; ask once, then store as file-local
  (TeX-save-query nil)
  (TeX-clean-confirm nil)
  (TeX-show-compilation t)
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-start-server t)
  (TeX-PDF-mode t)
  :config
  (setq TeX-command-default "Arara")
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (add-hook 'LaTeX-mode-hook #'neo/latex-mode-setup)
  (add-hook 'compilation-finish-functions
            (lambda (_buf _msg)
              (when (derived-mode-p 'latex-mode)
                (ignore-errors (reftex-parse-all))))))

(with-eval-after-load 'tex
  (add-to-list 'TeX-view-program-list
               '("Evince" "evince --page-index=%(outpage) %o"))
  (setq TeX-view-program-selection '((output-pdf "Evince")))
  (setq TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server t))

;;; Note, no (provide 'neo-latex) here, extensions are loaded not required.
