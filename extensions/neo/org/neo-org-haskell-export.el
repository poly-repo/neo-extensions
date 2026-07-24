;;; -*- lexical-binding: t -*-

(require 'ox-latex)
(require 'neo-org-report-export)

(defconst neo--org-haskell-latex-class-name "neo-haskell-notebook"
  "Org LaTeX class name used for Haskell notebook exports.")

(defconst neo--org-haskell-mlody-book-relative-path
  "common/latex/mlody-book/mlody-book.cls"
  "Repository-relative path used to locate the shared MLody book class.")

(defconst neo--org-haskell-latex-top-section-command
  "
% Notebook headings use the standard section hierarchy.
% The book class is chapter-oriented, so remove the leading chapter 0 prefix.
\\renewcommand{\\thesection}{\\arabic{section}}
\\renewcommand{\\thesubsection}{\\thesection.\\arabic{subsection}}
\\renewcommand{\\thesubsubsection}{\\thesubsection.\\arabic{subsubsection}}
\\renewcommand{\\theparagraph}{\\thesubsubsection.\\arabic{paragraph}}
\\renewcommand{\\thesubparagraph}{\\theparagraph.\\arabic{subparagraph}}
"
  "LaTeX numbering overrides used for notebook section headings.")

(defconst neo--org-haskell-latex-sidenote-footnote-command
  "\\sidenote{%s%s}"
  "Footnote command used by Haskell notebook LaTeX exports.")

(defconst neo--org-haskell-latex-minted-style-setup
  "
% Notebook-local code block styling aligned with the shared MLody palette.
\\colorlet{neoNotebookCodeBg}{neogrey!6!white}
\\colorlet{neoNotebookHaskellBg}{neoblue!7!white}
\\colorlet{neoNotebookMlodyBg}{neoorange!9!white}

\\NewDocumentEnvironment{neoNotebookCode}{O{} m}
  {\\VerbatimEnvironment\\begin{minted}[bgcolor=neoNotebookCodeBg,#1]{#2}}
  {\\end{minted}}

\\newminted[neoNotebookHaskellCode]{haskell}{
  bgcolor=neoNotebookHaskellBg
}

\\newminted[neoNotebookMlodyCode]{mlody}{
  bgcolor=neoNotebookMlodyBg
}
"
  "Notebook-local minted wrappers layered on top of `mlody-book'.")

(defconst neo--org-haskell-latex-documentclass
  (concat
   "\\documentclass[
    mode=auto,
    chapter-banners=false,
    fontsize=10pt,
    secnumdepth=2,
    bem=section,
    numbers=noenddot,
]{mlody-book}

"
   neo--org-haskell-latex-top-section-command
   "\n"
   neo--org-haskell-latex-minted-style-setup)
  "Document class block used for Haskell notebook LaTeX exports.")

(defconst neo--org-haskell-latex-class-sectioning
  '(("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
  "Heading commands used by the Haskell notebook LaTeX class.")

(defconst neo--org-haskell-minted-language-alist
  '((mlody "mlody"))
  "Extra Org-to-minted language mappings used by notebook exports.")

(defun neo--org-haskell-latex-class-entry ()
  "Return the Org LaTeX class entry for Haskell notebooks."
  (append (list neo--org-haskell-latex-class-name
                neo--org-haskell-latex-documentclass)
          neo--org-haskell-latex-class-sectioning))

(defun neo--org-haskell-register-latex-class ()
  "Register the shared MLody book class for Haskell notebook exports."
  (setq org-latex-classes
        (cons (neo--org-haskell-latex-class-entry)
              (assoc-delete-all neo--org-haskell-latex-class-name
                                org-latex-classes))))

(defun neo--org-haskell-apply-book-structure (output)
  "Insert notebook frontmatter and mainmatter commands into OUTPUT."
  (let ((result output))
    (unless (string-match-p "\\\\frontmatter\\b" result)
      (setq result
            (replace-regexp-in-string
             "\\\\begin{document}[[:space:]\n]*"
             "\\\\begin{document}\n\n\\\\frontmatter\n"
             result nil nil)))
    (unless (string-match-p "\\\\mainmatter\\b" result)
      (setq result
            (cond
             ((string-match-p "\\\\tableofcontents\\b" result)
              (replace-regexp-in-string
               "\\\\tableofcontents\\b"
               "\\\\tableofcontents\n\n\\\\mainmatter"
               result nil nil))
             ((string-match-p "\\\\maketitle\\b" result)
              (replace-regexp-in-string
               "\\\\maketitle\\b"
               "\\\\maketitle\n\n\\\\mainmatter"
               result nil nil))
             (t
              (replace-regexp-in-string
               "\\\\frontmatter\\b"
               "\\\\frontmatter\n\\\\mainmatter"
               result nil nil)))))
    result))

(defun neo--org-haskell-structure-final-output (output backend _info)
  "Apply notebook book structure to LaTeX OUTPUT for BACKEND."
  (if (and (org-export-derived-backend-p backend 'latex)
           (not (org-export-derived-backend-p backend 'neo-mlody-report)))
      (neo--org-haskell-apply-book-structure output)
    output))

(defun neo--org-haskell-explicit-latex-class ()
  "Return the notebook's explicit `LATEX_CLASS' keyword, if any."
  (cadr (assoc "LATEX_CLASS" (org-collect-keywords '("LATEX_CLASS")))))

(defun neo--org-haskell-report-export-p ()
  "Return non-nil when the notebook explicitly selects `mlody-report'."
  (equal (neo--org-haskell-explicit-latex-class)
         neo--org-report-latex-class-name))

(defun neo--org-haskell-export-backend ()
  "Return the Org export backend selected by the current notebook."
  (if (neo--org-haskell-report-export-p)
      'neo-mlody-report
    'latex))

(defun neo--org-haskell-add-report-minted-preamble (info backend)
  "Add notebook minted definitions to report export INFO for BACKEND."
  (when (org-export-derived-backend-p backend 'neo-mlody-report)
    (let ((extra-header (plist-get info :latex-header-extra)))
      (plist-put
       info
       :latex-header-extra
       (string-join
        (delq nil
              (list (org-string-nw-p extra-header)
                    neo--org-haskell-latex-minted-style-setup))
        "\n"))))
  info)

(defun neo--org-haskell-configure-minted-languages ()
  "Extend `org-latex-minted-langs' for notebook-local source languages."
  (setq-local org-latex-minted-langs
              (copy-tree org-latex-minted-langs))
  (dolist (entry neo--org-haskell-minted-language-alist)
    (setq org-latex-minted-langs
          (cons entry
                (assoc-delete-all (car entry) org-latex-minted-langs)))))

(defun neo--org-haskell-notebook-minted-environment (language)
  "Return the notebook minted wrapper environment for LANGUAGE."
  (pcase (downcase language)
    ("haskell" "neoNotebookHaskellCode")
    ("mlody" "neoNotebookMlodyCode")
    (_ "neoNotebookCode")))

(defun neo--org-haskell-style-src-block (output backend _info)
  "Rewrite LaTeX minted OUTPUT into notebook-specific wrapper environments."
  (if (and (org-export-derived-backend-p backend 'latex)
           (string-match "\\\\begin{minted}\\(\\[[^]]*\\]\\)?{\\([^}]+\\)}" output))
      (let* ((options (or (match-string 1 output) ""))
             (language (match-string 2 output))
             (environment
              (neo--org-haskell-notebook-minted-environment language))
             (begin-replacement
              (if (string= environment "neoNotebookCode")
                  (format "\\begin{%s}%s{%s}" environment options language)
                (format "\\begin{%s}%s" environment options))))
        (setq output (replace-match begin-replacement t t output))
        (replace-regexp-in-string
         "\\\\end{minted}"
         (format "\\end{%s}" environment)
         output
         t
         t))
    output))

(defun neo--org-haskell-configure-export ()
  "Configure LaTeX export defaults for the current Haskell notebook buffer."
  (setq-local org-latex-default-class neo--org-haskell-latex-class-name)
  (setq-local org-latex-src-block-backend 'minted)
  ;; Org turns headings deeper than `org-export-headline-levels' into
  ;; enumerate/itemize entries.  Keep every level supported by our LaTeX
  ;; class as a real heading so its KOMA fonts and secnumdepth apply.
  (setq-local org-export-headline-levels
              (length neo--org-haskell-latex-class-sectioning))
  (setq-local org-latex-default-footnote-command
              neo--org-haskell-latex-sidenote-footnote-command)
  (neo--org-haskell-configure-minted-languages)
  ;; Export filters are consumed as a plain function list inside Org's
  ;; export plist, not through the normal hook runner.  Using a local
  ;; `add-hook' here can therefore leave a `t' sentinel in the list,
  ;; which later blows up as `Symbol's function definition is void: t'
  ;; when `org-export-filter-apply-functions' `funcall's each entry.
  (setq-local org-export-filter-final-output-functions
              (copy-sequence org-export-filter-final-output-functions))
  (setq-local org-export-filter-options-functions
              (copy-sequence org-export-filter-options-functions))
  (setq-local org-export-filter-src-block-functions
              (copy-sequence org-export-filter-src-block-functions))
  (cl-pushnew #'neo--org-haskell-add-report-minted-preamble
              org-export-filter-options-functions)
  (cl-pushnew #'neo--org-haskell-structure-final-output
              org-export-filter-final-output-functions)
  (cl-pushnew #'neo--org-haskell-style-src-block
              org-export-filter-src-block-functions))

(neo/use-package ox-latex
  :builtin t
  :after org
  :config
  (neo--org-haskell-register-latex-class)
  (add-hook 'neo/org-haskell-notebook-mode-hook
            #'neo--org-haskell-configure-export))

(provide 'neo-org-haskell-export)
;;; neo-org-haskell-export.el ends here
