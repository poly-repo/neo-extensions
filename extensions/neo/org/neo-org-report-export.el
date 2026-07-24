;;; -*- lexical-binding: t -*-

(require 'ox-latex)
(require 'subr-x)

(defconst neo--org-report-latex-class-name "mlody-report"
  "Org LaTeX class name used for MLody report exports.")

(defconst neo--org-report-latex-class-sectioning
  '(("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
  "Heading commands used by the MLody report LaTeX class.")

(defconst neo--org-report-latex-documentclass
  "\\documentclass{mlody-report}"
  "Document class declaration used for MLody report exports.")

(defconst neo--org-report-options-alist
  '((:date "DATE" nil nil parse)
    (:report-id "ID" nil nil t)
    (:report-status "STATUS" nil nil t)
    (:report-requires "REQUIRES" nil nil split)
    (:report-replaces "REPLACES" nil nil t)
    (:report-superseded-by "SUPERSEDED_BY" nil nil t)
    (:report-purpose nil nil nil)
    (:report-purpose-seen nil nil nil))
  "Backend-specific Org export options for MLody reports.")

(defun neo--org-report-latex-class-entry ()
  "Return the Org LaTeX class entry for MLody reports."
  (append (list neo--org-report-latex-class-name
                neo--org-report-latex-documentclass)
          neo--org-report-latex-class-sectioning))

(defun neo--org-report-register-latex-class ()
  "Register the reusable MLody report class with Org."
  (setq org-latex-classes
        (cons (neo--org-report-latex-class-entry)
              (assoc-delete-all neo--org-report-latex-class-name
                                org-latex-classes))))

(defun neo--org-report-purpose-headline-p (headline)
  "Return non-nil when HEADLINE is the top-level report Purpose section."
  (and (= (org-element-property :level headline) 1)
       (string-equal-ignore-case
        (string-trim (org-element-property :raw-value headline))
        "Purpose")))

(defun neo--org-report-headline (headline contents info)
  "Transcode HEADLINE, capturing the top-level Purpose CONTENTS in INFO."
  (if (neo--org-report-purpose-headline-p headline)
      (progn
        (when (plist-get info :report-purpose-seen)
          (user-error
           "neo-org: an MLody report can contain only one top-level Purpose section"))
        (plist-put info :report-purpose-seen t)
        (plist-put info :report-purpose (or contents ""))
        "")
    (org-latex-headline headline contents info)))

(defun neo--org-report-export-data (data info)
  "Transcode optional report metadata DATA using export INFO."
  (when data
    (org-string-nw-p (org-export-data data info))))

(defun neo--org-report-macro (name value)
  "Return a preamble macro named NAME containing VALUE."
  (when (org-string-nw-p value)
    (format "\\%s{%s}" name value)))

(defun neo--org-report-render-main-file (path)
  "Render the absolute report source PATH for LaTeX and Lua."
  (when path
    (let ((absolute-path (expand-file-name path)))
      (when (string-match-p "[{}%\n\r]" absolute-path)
        (user-error
         "neo-org: report source paths cannot contain braces, percent signs, or newlines"))
      (format "\\mainfile{\\detokenize{%s}}" absolute-path))))

(defun neo--org-report-render-requires (requires info)
  "Render report REQUIRES metadata using export INFO."
  (when requires
    (string-join
     (delq nil
           (mapcar
            (lambda (identifier)
              (neo--org-report-export-data identifier info))
            requires))
     ", ")))

(defun neo--org-report-render-metadata (info)
  "Return MLody report preamble metadata rendered from INFO."
  (string-join
   (delq
    nil
    (list
     (neo--org-report-macro
      "id"
      (neo--org-report-export-data (plist-get info :report-id) info))
     (neo--org-report-macro
      "status"
      (neo--org-report-export-data (plist-get info :report-status) info))
     (neo--org-report-macro
      "requires"
      (neo--org-report-render-requires
       (plist-get info :report-requires)
       info))
     (neo--org-report-macro
      "replaces"
      (neo--org-report-export-data (plist-get info :report-replaces) info))
     (neo--org-report-macro
      "supersededby"
      (neo--org-report-export-data
       (plist-get info :report-superseded-by)
       info))
     (neo--org-report-render-main-file (plist-get info :input-file))
     (neo--org-report-macro
      "purpose"
      (plist-get info :report-purpose))))
   "\n"))

(defun neo--org-report-template (contents info)
  "Return a complete MLody report document for CONTENTS and INFO."
  (let* ((report-info (copy-sequence info))
         (metadata (neo--org-report-render-metadata report-info))
         (extra-header (plist-get report-info :latex-header-extra)))
    (plist-put report-info :latex-class neo--org-report-latex-class-name)
    (plist-put report-info :latex-subtitle-format "\\subtitle{%s}")
    (plist-put report-info :latex-subtitle-separate t)
    (when (org-string-nw-p metadata)
      (plist-put report-info
                 :latex-header-extra
                 (string-join (delq nil
                                    (list (org-string-nw-p extra-header)
                                          metadata))
                              "\n")))
    (org-latex-template contents report-info)))

(defun neo--org-report-register-export-backend ()
  "Register the MLody report backend derived from Org's LaTeX backend."
  (org-export-define-derived-backend
   'neo-mlody-report
   'latex
   :menu-entry
   '(?r "Export as MLody report"
        ((?l "As LaTeX file" neo/org-export-mlody-report-latex)))
   :options-alist neo--org-report-options-alist
   :translate-alist
   '((headline . neo--org-report-headline)
     (template . neo--org-report-template))))

;;;###autoload
(defun neo/org-export-mlody-report-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export the current Org buffer as an MLody report LaTeX file.
ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, and EXT-PLIST are passed through
to `org-export-to-file'."
  (interactive)
  (neo--org-report-register-latex-class)
  (neo--org-report-register-export-backend)
  (org-export-to-file
   'neo-mlody-report
   (org-export-output-file-name ".tex" subtreep)
   async
   subtreep
   visible-only
   body-only
   ext-plist))

(neo/use-package ox-latex
  :builtin t
  :after org
  :config
  (neo--org-report-register-latex-class)
  (neo--org-report-register-export-backend))

(provide 'neo-org-report-export)
;;; neo-org-report-export.el ends here
