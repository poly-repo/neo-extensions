;;; -*- lexical-binding: t -*-

(require 'ox-latex)

(defconst neo--org-haskell-latex-class-name "neo-haskell-notebook"
  "Org LaTeX class name used for Haskell notebook exports.")

(defconst neo--org-haskell-score-relative-directory "mlody/docs/the-score"
  "Repository-relative score directory reused by notebook exports.")

(defconst neo--org-haskell-score-preamble-relative-path
  (concat neo--org-haskell-score-relative-directory "/preamble.tex")
  "Repository-relative preamble path reused by notebook exports.")

(defconst neo--org-haskell-latex-build-mode-prefix
  "% --- Build-mode detection (print vs online) ---
% jobname ending in \"-print\"  → twoside layout with blank recto/verso pages
% jobname ending in \"-online\" → oneside layout, no blank pages
% arara draft profiles set MLODY_DRAFT=1 → chapter banner images are hidden, spacing kept
\\directlua{
  if string.find(tex.jobname, \"online\") then
    tex.sprint(\"\\\\PassOptionsToClass{oneside}{kaobook}\")
  else
    tex.sprint(\"\\\\PassOptionsToClass{twoside,open=right}{kaobook}\")
  end
}

"
  "Comment and Lua preamble inserted at the start of notebook LaTeX exports.")

(defconst neo--org-haskell-latex-preamble-input
  (format "\\input{%s}" neo--org-haskell-score-preamble-relative-path)
  "Preamble input included by Haskell notebook LaTeX exports.")

(defconst neo--org-haskell-latex-documentclass
  (concat
   "\\documentclass[
    fontsize=10pt,        % Standard for this layout
    secnumdepth=2,        % Numbering depth
    bem=section,          % Start the \"Block Enumeration Matrix\" (if needed)
    numbers=noenddot,
    %chapterentrydots=true, % Uncomment to output dots from the chapter name to the page number in the table of contents
% draft mode is a mess
%    draft,                      % ...or not
]{kaobook}

"
   neo--org-haskell-latex-preamble-input)
  "Document class block used for Haskell notebook LaTeX exports.")

(defconst neo--org-haskell-latex-class-sectioning
  '(("\\chapter{%s}" . "\\chapter*{%s}")
    ("\\section{%s}" . "\\section*{%s}")
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
  "Register the Kaobook-based LaTeX class for Haskell notebook exports."
  (setq org-latex-classes
        (cons (neo--org-haskell-latex-class-entry)
              (assoc-delete-all neo--org-haskell-latex-class-name
                                org-latex-classes))))

(defun neo--org-haskell-prefix-final-output (output backend _info)
  "Prefix LaTeX OUTPUT with notebook build-mode boilerplate for BACKEND."
  (if (org-export-derived-backend-p backend 'latex)
      (if (string-prefix-p neo--org-haskell-latex-build-mode-prefix output)
          output
        (concat neo--org-haskell-latex-build-mode-prefix output))
    output))

(defun neo--org-haskell-configure-minted-languages ()
  "Extend `org-latex-minted-langs' for notebook-local source languages."
  (setq-local org-latex-minted-langs
              (copy-tree org-latex-minted-langs))
  (dolist (entry neo--org-haskell-minted-language-alist)
    (setq org-latex-minted-langs
          (cons entry
                (assoc-delete-all (car entry) org-latex-minted-langs)))))

(defun neo--org-haskell-configure-export ()
  "Configure LaTeX export defaults for the current Haskell notebook buffer."
  (setq-local org-latex-default-class neo--org-haskell-latex-class-name)
  (setq-local org-latex-src-block-backend 'minted)
  (neo--org-haskell-configure-minted-languages)
  ;; Export filters are consumed as a plain function list inside Org's
  ;; export plist, not through the normal hook runner.  Using a local
  ;; `add-hook' here can therefore leave a `t' sentinel in the list,
  ;; which later blows up as `Symbol's function definition is void: t'
  ;; when `org-export-filter-apply-functions' `funcall's each entry.
  (setq-local org-export-filter-final-output-functions
              (copy-sequence org-export-filter-final-output-functions))
  (cl-pushnew #'neo--org-haskell-prefix-final-output
              org-export-filter-final-output-functions))

(neo/use-package ox-latex
  :builtin t
  :after org
  :config
  (neo--org-haskell-register-latex-class)
  (add-hook 'neo/org-haskell-notebook-mode-hook
            #'neo--org-haskell-configure-export))

(provide 'neo-org-haskell-export)
;;; neo-org-haskell-export.el ends here
