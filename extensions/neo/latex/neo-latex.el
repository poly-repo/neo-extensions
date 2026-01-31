;;; -*- lexical-binding: t -*-

;;; This is latex, a NEO extension
;;;
;;; Support for LaTeX

(use-package auctex)

(use-package cdlatex
  :ensure t
  :hook
  (LaTeX-mode . turn-on-cdlatex))

;;--------------------------------------------------------------------
;; pdf-tools
(use-package pdf-tools
  :ensure t
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
                             ;;(setq TeX-engine 'default)      ; use xelatex default
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
                             (add-to-list 'TeX-command-list '("LaTeX" "%`pdflatex -shell-escape --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
                             (setq TeX-command-default "LaTeX")
                             ;;(setq TeX-command-default "pdflatex --synctex=1")

                             (setq TeX-fold-env-spec-list (quote (("[comment]" ("comment")) ("[figure]" ("figure")) ("[table]" ("table"))("[itemize]"("itemize"))("[enumerate]"("enumerate"))("[description]"("description"))("[overpic]"("overpic"))("[tabularx]"("tabularx"))("[code]"("code"))("[shell]"("shell")))))


                             (define-key LaTeX-mode-map (kbd "C-c C-p") 'reftex-parse-all)
                             (define-key LaTeX-mode-map (kbd "C-c C-g") #'pdf-sync-forward-search)

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

;;; Note, no (provide 'neo-latex) here, extensions are loaded not required.
