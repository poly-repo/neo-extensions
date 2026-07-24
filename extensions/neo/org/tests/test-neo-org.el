;;; tests/test-neo-org.el --- Tests for neo-org -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)

(defmacro neo/use-package (&rest _args)
  "Ignore package declarations while loading extension code in tests."
  nil)

(defvar org-directory nil)
(defvar org-structure-template-alist nil)
(defvar prettify-symbols-alist nil)
(defvar prettify-symbols-compose-predicate nil)
(defvar prettify-symbols-unprettify-at-point nil)

(defun prettify-symbols-mode (&optional _arg)
  "Test stub for `prettify-symbols-mode'."
  nil)

(defun neo-mlody-mode ()
  "Test stub for `neo-mlody-mode'."
  (interactive)
  (setq major-mode 'neo-mlody-mode
        mode-name "MLody"))

(defun neo--test-org-write-file (path content)
  "Write CONTENT to PATH, creating parent directories as needed."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert content))
  path)

(defun neo--test-org-prepare-mlody-latex-fixture (root)
  "Create minimal shared MLody LaTeX support under ROOT."
  (let ((style-root
         (expand-file-name neo--org-haskell-mlody-style-relative-directory root))
        (book-root
         (expand-file-name neo--org-haskell-mlody-book-relative-directory root))
        (report-root
         (expand-file-name
          neo--org-haskell-mlody-report-relative-directory
          root)))
    (make-directory style-root t)
    (make-directory (expand-file-name "assets/images" book-root) t)
    (make-directory report-root t)
    (neo--test-org-write-file
     (expand-file-name "mlody-style.sty" style-root)
     "% test shared style\n")
    (neo--test-org-write-file
     (expand-file-name "mlody-code.sty" style-root)
     "% test shared code support\n")
    (neo--test-org-write-file
     (expand-file-name "mlody-book.cls" book-root)
     "% test book class\n")
    (neo--test-org-write-file
     (expand-file-name "mlody-report.cls" report-root)
     "% test report class\n")
    (neo--test-org-write-file
     (expand-file-name neo--org-haskell-arara-rule-relative-path root)
     "!config\n")
    book-root))

(load-file (expand-file-name "../neo-org.el"
                             (file-name-directory (or load-file-name buffer-file-name))))

(describe "neo-org"
  (it "prefers the extension's Org directory override"
    (let ((neo/org-directory "~/notes")
          (org-directory "~/ignored"))
      (expect (neo--org-base-directory)
              :to-equal
              (file-name-as-directory (expand-file-name "~/notes")))))

  (it "falls back to Org's directory when no override is set"
    (let ((neo/org-directory nil)
          (org-directory "~/fallback"))
      (expect (neo--org-base-directory)
              :to-equal
              (file-name-as-directory (expand-file-name "~/fallback")))))

  (it "builds protocol capture templates against the configured notes file"
    (let* ((neo/org-directory "~/notes")
           (neo/org-capture-notes-file-name "capture.org")
           (templates (neo--org-capture-templates))
           (protocol-template (assoc "p" templates))
           (store-link-template (assoc "L" templates)))
      (expect neo/org-protocol-default-template-key :to-equal "p")
      (expect protocol-template :not :to-be nil)
      (expect store-link-template :not :to-be nil)
      (expect (nth 3 protocol-template)
              :to-equal
              `(file+headline ,(expand-file-name "~/notes/capture.org") "Inbox"))))

  (it "enables Haskell source blocks for Org Babel dispatch"
    (let ((inhibit-message t)
          (org-confirm-babel-evaluate nil)
          dispatched-body)
      (spy-on 'org-babel-do-load-languages)
      (cl-letf (((symbol-function 'org-babel-execute:haskell)
                 (lambda (body _params)
                   (setq dispatched-body body)
                   "dispatched")))
        (neo--org-configure-babel)
        (expect 'org-babel-do-load-languages
                :to-have-been-called-with
                'org-babel-load-languages
                neo/org-babel-languages)
        (expect (alist-get 'haskell neo/org-babel-languages)
                :to-be-truthy)
        (with-temp-buffer
          (org-mode)
          (insert "#+begin_src haskell :results silent\n"
                  "main = putStrLn \"ok\"\n"
                  "#+end_src\n")
          (goto-char (point-min))
          (forward-line 1)
          (expect (org-babel-execute-src-block)
                  :to-equal
                  "dispatched")
          (expect dispatched-body
                  :to-equal
                  "main = putStrLn \"ok\"")))))

  (it "overrides Org's default <h shorthand with a Haskell source block"
    (require 'org-tempo)
    (let ((org-structure-template-alist (copy-tree org-structure-template-alist))
          (org-tempo-tags nil))
      (neo--org-configure-structure-templates)
      (with-temp-buffer
        (org-mode)
        (insert "<h")
        (expect (org-tempo-complete-tag) :to-be-truthy)
        (expect (buffer-string)
                :to-equal
                "#+begin_src haskell\n\n#+end_src"))))

  (it "keeps the extended Haskell Org tempo shorthands"
    (let (org-structure-template-alist)
      (neo--org-configure-structure-templates)
      (expect (cdr (assoc "hs" org-structure-template-alist))
              :to-equal
              "src haskell")
      (expect (cdr (assoc "haskell" org-structure-template-alist))
              :to-equal
              "src haskell")))

  (it "registers .orghs files for the Haskell notebook mode"
    (expect (assoc-default "demo.orghs" auto-mode-alist #'string-match)
            :to-equal 'neo/org-haskell-notebook-mode))

  (it "adds the Mlody Org tempo shorthand"
    (let (org-structure-template-alist)
      (neo--org-configure-structure-templates)
      (expect (cdr (assoc "m" org-structure-template-alist))
              :to-equal
              "src mlody")))

  (it "configures Org prose for 100-column auto-fill with proportional text"
    (let ((neo/org-fill-column 100)
          (neo/org-auto-fill t)
          (neo/org-use-variable-pitch t)
          (neo/org-code-block-font-height 0.9))
      (with-temp-buffer
        (org-mode)
        (neo--org-mode-setup)
        (expect fill-column :to-equal 100)
        (expect auto-fill-function :to-equal #'org-auto-fill-function)
        (expect (assq 'default face-remapping-alist) :not :to-be nil)
        (expect (assq 'org-block face-remapping-alist)
                :to-equal
                '(org-block (:inherit fixed-pitch :height 0.9) org-block))
        (expect (assq 'org-table face-remapping-alist)
                :to-equal
                '(org-table (:inherit fixed-pitch) org-table))
        (expect (length neo--org-fixed-pitch-cookies)
                :to-equal
                (length neo--org-fixed-pitch-faces)))))

  (it "can keep Org fully fixed-pitch when proportional prose is disabled"
    (let ((neo/org-use-variable-pitch nil))
      (with-temp-buffer
        (org-mode)
        (neo--org-mode-setup)
        (expect (assq 'default face-remapping-alist) :to-be nil)
        (expect neo--org-fixed-pitch-cookies :to-be nil))))

  (it "registers the shared MLody book class for Haskell notebooks"
    (let ((org-latex-classes '(("article" "\\documentclass{article}"))))
      (neo--org-haskell-register-latex-class)
      (let ((entry (assoc neo--org-haskell-latex-class-name org-latex-classes)))
        (expect entry :not :to-be nil)
        (expect (cadr entry) :to-equal neo--org-haskell-latex-documentclass)
        (expect (string-match-p
                 (regexp-quote "]{mlody-book}")
                 (cadr entry))
                :not :to-be nil)
        (expect (cadr entry)
                :to-match
                (regexp-quote neo--org-haskell-latex-top-section-command))
        (expect (cadr entry) :not :to-match "kaobook")
        (expect (cadr entry) :not :to-match "the-score")
        (expect (cadr entry)
                :to-match
                "\\\\newminted\\[neoNotebookHaskellCode\\]{haskell}")
        (expect (caddr entry) :to-equal '("\\section{%s}" . "\\section*{%s}")))))

  (it "registers the reusable MLody report class and export backend"
    (let ((org-latex-classes '(("article" "\\documentclass{article}"))))
      (neo--org-report-register-latex-class)
      (neo--org-report-register-export-backend)
      (let ((entry (assoc neo--org-report-latex-class-name org-latex-classes))
            (backend (org-export-get-backend 'neo-mlody-report)))
        (expect entry :not :to-be nil)
        (expect (cadr entry) :to-equal "\\documentclass{mlody-report}")
        (expect (caddr entry)
                :to-equal
                '("\\section{%s}" . "\\section*{%s}"))
        (expect backend :not :to-be nil)
        (expect (org-export-derived-backend-p 'neo-mlody-report 'latex)
                :to-be-truthy))))

  (it "exports MLody report LaTeX through the public command"
    (with-temp-buffer
      (setq buffer-file-name "/tmp/reports/command-report.org")
      (org-mode)
      (let (export-call)
        (cl-letf (((symbol-function 'org-export-to-file)
                   (lambda (&rest args)
                     (setq export-call args)
                     (cadr args))))
          (expect (neo/org-export-mlody-report-latex)
                  :to-equal
                  "command-report.tex")
          (expect export-call
                  :to-equal
                  '(neo-mlody-report
                    "command-report.tex"
                    nil nil nil nil nil))))))

  (it "maps Org report metadata to escaped MLody report macros"
    (with-temp-buffer
      (setq buffer-file-name "/tmp/reports/report_source with spaces.org")
      (insert "#+title: Export Report\n"
              "#+subtitle: Metadata & purpose\n"
              "#+author: Ada & Grace\n"
              "#+date: 2026-07-24\n"
              "#+id: DOC_42\n"
              "#+status: Active\n"
              "#+requires: REQ_1 REQ-2\n"
              "#+replaces: OLD_1\n"
              "#+superseded_by: NEW_1\n\n"
              "* Body\nReport body.\n")
      (org-mode)
      (neo--org-report-register-latex-class)
      (neo--org-report-register-export-backend)
      (let ((latex (org-export-as 'neo-mlody-report nil nil nil nil)))
        (expect latex :to-match (regexp-quote "\\documentclass{mlody-report}"))
        (expect latex :to-match (regexp-quote "\\title{Export Report}"))
        (expect latex
                :to-match
                (regexp-quote "\\subtitle{Metadata \\& purpose}"))
        (expect latex :to-match (regexp-quote "\\author{Ada \\& Grace}"))
        (expect latex :to-match (regexp-quote "\\date{2026-07-24}"))
        (expect latex :to-match (regexp-quote "\\id{DOC\\_42}"))
        (expect latex :to-match (regexp-quote "\\status{Active}"))
        (expect latex
                :to-match
                (regexp-quote "\\requires{REQ\\_1, REQ-2}"))
        (expect latex :to-match (regexp-quote "\\replaces{OLD\\_1}"))
        (expect latex :to-match (regexp-quote "\\supersededby{NEW\\_1}"))
        (expect latex
                :to-match
                (regexp-quote
                 (concat
                  "\\mainfile{\\detokenize{"
                  "/tmp/reports/report_source with spaces.org}}"))))))

  (it "rejects report source paths that cannot be passed losslessly to TeX"
    (expect (neo--org-report-render-main-file "/tmp/report{draft}.org")
            :to-throw 'user-error)
    (expect (neo--org-report-render-main-file "/tmp/report%20.org")
            :to-throw 'user-error))

  (it "omits optional MLody report metadata that was not provided"
    (with-temp-buffer
      (insert "#+title: Minimal Report\n\n* Body\nReport body.\n")
      (org-mode)
      (neo--org-report-register-latex-class)
      (neo--org-report-register-export-backend)
      (let ((latex (org-export-as 'neo-mlody-report nil nil nil nil)))
        (expect latex :to-match (regexp-quote "\\date{}"))
        (expect latex :not :to-match (regexp-quote "\\date{\\today}"))
        (expect latex :not :to-match (regexp-quote "\\id{"))
        (expect latex :not :to-match (regexp-quote "\\status{"))
        (expect latex :not :to-match (regexp-quote "\\requires{"))
        (expect latex :not :to-match (regexp-quote "\\replaces{"))
        (expect latex :not :to-match (regexp-quote "\\supersededby{"))
        (expect latex :not :to-match (regexp-quote "\\mainfile{"))
        (expect latex :not :to-match (regexp-quote "\\purpose{"))
        (expect latex :not :to-match (regexp-quote "\\lastedit{"))
        (expect latex :not :to-match (regexp-quote "\\gitcommit{")))))

  (it "moves a top-level Purpose section into the report title metadata"
    (with-temp-buffer
      (let ((source
             (concat "#+title: Purpose Report\n\n"
                     "* Purpose\nThis is the *only* purpose.\n\n"
                     "* Findings\nBody text.\n")))
        (insert source)
        (org-mode)
        (neo--org-report-register-latex-class)
        (neo--org-report-register-export-backend)
        (let ((latex (org-export-as 'neo-mlody-report nil nil nil nil)))
          (expect latex
                  :to-match
                  (regexp-quote
                   "\\purpose{This is the \\textbf{only} purpose."))
          (expect latex :not :to-match (regexp-quote "\\section{Purpose}"))
          (expect (length
                   (split-string latex
                                 (regexp-quote "This is the \\textbf{only} purpose.")
                                 t))
                  :to-equal
                  2)
          (expect latex :to-match (regexp-quote "\\section{Findings}"))
          (expect (buffer-string) :to-equal source)))))

  (it "rejects duplicate top-level Purpose sections"
    (with-temp-buffer
      (insert "#+title: Duplicate Purpose\n\n"
              "* Purpose\nFirst.\n\n"
              "* Purpose\nSecond.\n")
      (org-mode)
      (neo--org-report-register-latex-class)
      (neo--org-report-register-export-backend)
      (expect (org-export-as 'neo-mlody-report nil nil nil nil)
              :to-throw 'user-error)))

  (it "keeps a nested Purpose heading in the report body"
    (with-temp-buffer
      (insert "#+title: Nested Purpose\n\n"
              "* Context\n"
              "** Purpose\nNested purpose remains here.\n")
      (org-mode)
      (neo--org-report-register-latex-class)
      (neo--org-report-register-export-backend)
      (let ((latex (org-export-as 'neo-mlody-report nil nil nil nil)))
        (expect latex :not :to-match (regexp-quote "\\purpose{"))
        (expect latex :to-match (regexp-quote "\\subsection{Purpose}"))
        (expect latex :to-match (regexp-quote "Nested purpose remains here.")))))

  (it "leaves ordinary LaTeX exports unchanged by report translation"
    (with-temp-buffer
      (insert "#+title: Ordinary Document\n\n"
              "* Purpose\nOrdinary body purpose.\n")
      (org-mode)
      (neo--org-report-register-latex-class)
      (neo--org-report-register-export-backend)
      (let ((latex (org-export-as 'latex nil nil nil nil)))
        (expect latex :not :to-match (regexp-quote "\\documentclass{mlody-report}"))
        (expect latex :not :to-match (regexp-quote "\\purpose{"))
        (expect latex :to-match (regexp-quote "\\section{Purpose}"))
        (expect latex :to-match (regexp-quote "Ordinary body purpose.")))))

  (it "delegates print and online mode selection to mlody-book"
    (expect neo--org-haskell-latex-documentclass
            :to-match
            (regexp-quote "mode=auto"))
    (expect neo--org-haskell-latex-documentclass
            :to-match
            (regexp-quote "chapter-banners=false"))
    (expect neo--org-haskell-latex-documentclass
            :not :to-match
            (regexp-quote "\\PassOptionsToClass")))

  (it "uses the normal section hierarchy without a chapter-zero prefix"
    (expect neo--org-haskell-latex-top-section-command
            :to-match
            (regexp-quote "\\renewcommand{\\thesection}{\\arabic{section}}"))
    (expect neo--org-haskell-latex-top-section-command
            :to-match
            (regexp-quote "\\renewcommand{\\thesubsection}{\\thesection.\\arabic{subsection}}"))
    (expect neo--org-haskell-latex-top-section-command
            :not :to-match
            (regexp-quote "\\neohaskelltopsection"))
    (expect neo--org-haskell-latex-top-section-command
            :not :to-match
            (regexp-quote "\\marginpar")))

  (it "adds the optional Org Roam project template only when configured"
    (let ((neo/org-directory "~/notes")
          (neo/org-roam-project-template-file nil))
      (expect (neo--org-roam-project-capture-template) :to-be nil))
    (let ((neo/org-directory "~/notes")
          (neo/org-roam-project-template-file "templates/project.org"))
      (expect (car (neo--org-roam-project-capture-template)) :to-equal "p")))

  (it "resolves the Org Roam database through Neo's data path"
    (cl-letf (((symbol-function 'neo/data-file-path)
               (lambda (filename) (concat "/neo-data/" filename))))
      (expect (neo--org-roam-db-location) :to-equal "/neo-data/org-roam.db")))

  (it "resolves org-download images relative to the Org base directory"
    (let ((neo/org-directory "~/notes")
          (neo/org-download-image-dir "images"))
      (expect (neo--org-download-image-dir)
              :to-equal
              (file-name-as-directory (expand-file-name "~/notes/images")))))

  (it "enables org-crypt only when both the flag and key are present"
    (let ((neo/org-enable-crypt nil)
          (neo/org-crypt-key "ABC123"))
      (expect (neo--org-crypt-enabled-p) :to-be nil))
    (let ((neo/org-enable-crypt t)
          (neo/org-crypt-key nil))
      (expect (neo--org-crypt-enabled-p) :to-be nil))
    (let ((neo/org-enable-crypt t)
          (neo/org-crypt-key "ABC123"))
      (expect (neo--org-crypt-enabled-p) :to-be-truthy)))

  (it "prettifies haskell only in begin_src headers"
    (with-temp-buffer
      (insert "#+begin_src haskell\n")
      (search-backward "haskell")
      (let ((start (point))
            (neo--org-prettify-symbols-parent-predicate nil))
        (expect (neo--org-prettify-symbols-compose-p start (+ start 7) "haskell")
                :to-be-truthy))))

  (it "does not prettify plain haskell text"
    (with-temp-buffer
      (insert "haskell\n")
      (goto-char (point-min))
      (let ((neo--org-prettify-symbols-parent-predicate nil))
        (expect (neo--org-prettify-symbols-compose-p (point) (+ (point) 7) "haskell")
                :to-be nil))))

  (it "prettifies mlody only in begin_src headers"
    (with-temp-buffer
      (insert "#+begin_src mlody\n")
      (search-backward "mlody")
      (let ((start (point))
            (neo--org-prettify-symbols-parent-predicate nil))
        (expect (neo--org-prettify-symbols-compose-p start (+ start 5) "mlody")
                :to-be-truthy))))

  (it "maps mlody Org source editing to neo-mlody-mode"
    (let ((org-src-window-setup 'current-window))
      (neo--org-configure-src-editing)
      (expect (org-src-get-lang-mode "mlody") :to-equal 'neo-mlody-mode)
      (let (edit-buffer)
        (unwind-protect
            (with-temp-buffer
              (org-mode)
              (insert "#+begin_src mlody\npipeline example\n#+end_src\n")
              (goto-char (point-min))
              (search-forward "pipeline")
              (cl-letf (((symbol-function 'pop-to-buffer-same-window)
                         (lambda (buffer &rest _args)
                           (setq edit-buffer buffer)
                           (set-buffer buffer)
                           buffer))
                        ((symbol-function 'pop-to-buffer)
                         (lambda (buffer &rest _args)
                           (setq edit-buffer buffer)
                           (set-buffer buffer)
                           buffer)))
                (expect (org-edit-special) :to-be-truthy)
                (expect edit-buffer :not :to-be nil)
                (with-current-buffer edit-buffer
                  (expect major-mode :to-equal 'neo-mlody-mode))))
          (when (buffer-live-p edit-buffer)
            (kill-buffer edit-buffer))))))

  (it "uses the notebook LaTeX class in Haskell notebook buffers"
    (with-temp-buffer
      (neo/org-haskell-notebook-mode)
      (neo--org-haskell-configure-export)
      (expect org-latex-default-class
              :to-equal
              neo--org-haskell-latex-class-name)
      (expect org-latex-src-block-backend :to-equal 'minted)
      (expect (cadr (assq 'mlody org-latex-minted-langs))
              :to-equal
              "mlody")
      (expect (member #'neo--org-haskell-structure-final-output
                      org-export-filter-final-output-functions)
              :not :to-be nil)
      (expect (member #'neo--org-haskell-add-report-minted-preamble
                      org-export-filter-options-functions)
              :not :to-be nil)
      (expect (member #'neo--org-haskell-style-src-block
                      org-export-filter-src-block-functions)
              :not :to-be nil)
      (expect (local-variable-p 'org-latex-default-class) :to-be-truthy)))

  (it "leaves non-LaTeX exports unchanged"
    (expect (neo--org-haskell-structure-final-output "plain text" 'ascii nil)
            :to-equal
            "plain text"))

  (it "exports notebook LaTeX without a local-hook sentinel in final-output filters"
    (with-temp-buffer
      (insert "#+title: Demo\n\nHello, notebook export.\n")
      (neo/org-haskell-notebook-mode)
      (neo--org-haskell-register-latex-class)
      (neo--org-haskell-configure-export)
      (expect org-export-filter-final-output-functions :not :to-contain t)
      (let ((latex (org-export-as 'latex nil nil nil nil)))
        (expect (string-match-p (regexp-quote "]{mlody-book}") latex)
                :not :to-be nil)
        (expect latex :not :to-match "kaobook")
        (expect latex :not :to-match "mlody/docs/the-score")
        (expect latex :to-match "\\\\begin{document}\n\n\\\\frontmatter\n")
        (expect latex :to-match "\\\\tableofcontents\n\n\\\\mainmatter\n"))))

  (it "exports an explicitly selected MLody report with notebook code styling"
    (with-temp-buffer
      (setq buffer-file-name "/tmp/report-notebook.orghs")
      (insert "#+latex_class: mlody-report\n"
              "#+title: Report Notebook\n"
              "#+date: 2026-07-24\n"
              "#+options: toc:nil\n\n"
              "* Purpose\nExplain the report.\n\n"
              "* Findings\n"
              "#+begin_src haskell\nanswer = 42\n#+end_src\n")
      (neo/org-haskell-notebook-mode)
      (neo--org-haskell-register-latex-class)
      (neo--org-report-register-latex-class)
      (neo--org-report-register-export-backend)
      (neo--org-haskell-configure-export)
      (let* ((backend (neo--org-haskell-export-backend))
             (latex (org-export-as backend nil nil nil nil)))
        (expect backend :to-equal 'neo-mlody-report)
        (expect latex :to-match (regexp-quote "\\documentclass{mlody-report}"))
        (expect latex
                :to-match
                (regexp-quote "\\purpose{Explain the report."))
        (expect latex :not :to-match (regexp-quote "\\section{Purpose}"))
        (expect latex :to-match (regexp-quote "\\section{Findings}"))
        (expect latex
                :to-match
                (regexp-quote "\\begin{neoNotebookHaskellCode}[]"))
        (expect latex :to-match "neoNotebookHaskellBg")
        (expect latex :not :to-match (regexp-quote "\\frontmatter"))
        (expect latex :not :to-match (regexp-quote "\\mainmatter")))))

  (it "keeps other notebooks on the standard LaTeX backend"
    (with-temp-buffer
      (insert "#+title: Book Notebook\n")
      (neo/org-haskell-notebook-mode)
      (expect (neo--org-haskell-export-backend) :to-equal 'latex))
    (with-temp-buffer
      (insert "#+latex_class: article\n#+title: Other Notebook\n")
      (neo/org-haskell-notebook-mode)
      (expect (neo--org-haskell-export-backend) :to-equal 'latex)))

  (it "exports notebook footnotes as MLody-style sidenotes"
    (with-temp-buffer
      (insert "#+title: Demo\n\n* Section\nFootnote[fn:1]\n\n[fn:1] Side note text.\n")
      (neo/org-haskell-notebook-mode)
      (neo--org-haskell-register-latex-class)
      (neo--org-haskell-configure-export)
      (let* ((latex (org-export-as 'latex nil nil nil nil))
             (body (substring latex
                              (string-match
                               (regexp-quote "\\begin{document}")
                               latex))))
        (expect body :to-match (regexp-quote "\\section{Section}"))
        (expect body :to-match
                "\\\\frontmatter\n\\\\maketitle\n\\\\tableofcontents\n\n\\\\mainmatter\n\n\\\\section{Section}")
        (expect body :to-match (regexp-quote "\\sidenote{Side note text.}"))
        (expect body :not :to-match (regexp-quote "\\footnote{")))))

  (it "enters mainmatter before top-level sections even when the global TOC is disabled"
    (with-temp-buffer
      (insert "#+title: Demo\n#+options: toc:nil\n\n* Section\n** Subsection\nBody.\n")
      (neo/org-haskell-notebook-mode)
      (neo--org-haskell-register-latex-class)
      (neo--org-haskell-configure-export)
      (let* ((latex (org-export-as 'latex nil nil nil nil))
             (body (substring latex
                              (string-match
                               (regexp-quote "\\begin{document}")
                               latex))))
        (expect body :not :to-match (regexp-quote "\\tableofcontents"))
        (expect body :to-match
                "\\\\frontmatter\n\\\\maketitle\n\n\\\\mainmatter\n\\\\section{Section}")
        (expect body :to-match (regexp-quote "\\subsection{Subsection}")))))

  (it "keeps deeply nested notebook headings styled and unnumbered as headings"
    (with-temp-buffer
      (insert
       "#+title: Demo\n#+options: toc:nil\n\n* Examples\n** Processing\n*** Basic aggregate processing\n**** Fixture\nBody.\n")
      (neo/org-haskell-notebook-mode)
      (neo--org-haskell-register-latex-class)
      (neo--org-haskell-configure-export)
      (let* ((latex (org-export-as 'latex nil nil nil nil))
             (body (substring latex
                              (string-match
                               (regexp-quote "\\begin{document}")
                               latex))))
        (expect org-export-headline-levels
                :to-equal
                (length neo--org-haskell-latex-class-sectioning))
        (expect body :to-match
                (regexp-quote "\\subsubsection{Basic aggregate processing}"))
        (expect body :to-match (regexp-quote "\\paragraph{Fixture}"))
        (expect body :not :to-match (regexp-quote "\\begin{enumerate}"))
        (expect body :not :to-match (regexp-quote "\\item Fixture")))))

  (it "exports Haskell source blocks with notebook shading"
    (with-temp-buffer
      (insert "#+title: Demo\n\n#+begin_src haskell\nf x = x + 1\n#+end_src\n")
      (neo/org-haskell-notebook-mode)
      (neo--org-haskell-register-latex-class)
      (neo--org-haskell-configure-export)
      (let* ((latex (org-export-as 'latex nil nil nil nil))
             (body (substring latex
                              (string-match
                               (regexp-quote "\\begin{document}")
                               latex))))
        (expect body :to-match (regexp-quote "\\begin{neoNotebookHaskellCode}[]"))
        (expect latex :to-match "neoNotebookHaskellBg")
        (expect body :not :to-match (regexp-quote "\\begin{minted}"))
        (expect body :not :to-match (regexp-quote "\\begin{verbatim}")))))

  (it "exports notebook blocks with language-specific shaded wrappers"
    (with-temp-buffer
      (insert "#+title: Demo\n\n"
              "#+begin_src mlody\nvalue(name=\"greeting\")\n#+end_src\n\n"
              "#+begin_src haskell\nf x = x + 1\n#+end_src\n\n"
              "#+begin_src python\nprint('hi')\n#+end_src\n")
      (neo/org-haskell-notebook-mode)
      (neo--org-haskell-register-latex-class)
      (neo--org-haskell-configure-export)
      (let* ((latex (org-export-as 'latex nil nil nil nil))
             (body (substring latex
                              (string-match
                               (regexp-quote "\\begin{document}")
                               latex))))
        (expect body :to-match (regexp-quote "\\begin{neoNotebookMlodyCode}[]"))
        (expect body :to-match (regexp-quote "\\begin{neoNotebookHaskellCode}[]"))
        (expect body :to-match (regexp-quote "\\begin{neoNotebookCode}[]{python}"))
        (expect latex :to-match "neoNotebookMlodyBg")
        (expect body :not :to-match (regexp-quote "\\begin{minted}")))))

  (it "runs a second LuaLaTeX pass for fast notebook arara profiles"
    (let* ((profile (neo--org-haskell-render-arara-build-profile "online" t t))
           (lines (split-string profile "\n" t))
            (latex-lines
             (cl-remove-if-not
              (lambda (line)
                (string-match-p "mlodylualatex" line))
              lines)))
      (expect (length latex-lines) :to-equal 2)
      (expect (car latex-lines) :to-equal (cadr latex-lines))
      (expect (car latex-lines) :to-match "jobname: .*online")
      (expect (car latex-lines) :to-match "texinputs: \"\\.latex//:\"")
      (expect (car latex-lines) :to-match "pythonpath: \"\\.latexminted\"")
      (expect (car latex-lines) :to-match "texmfcache: \"\\.texmf-cache\"")
      (expect (car latex-lines) :to-match "draft: true")
      (expect profile :not :to-match "biber")
      (expect profile :not :to-match "makeindex")))

  (it "stages shared MLody LaTeX support without Score content"
    (let ((repo-root (make-temp-file "neo-org-haskell-repo" t))
          (temp-dir (make-temp-file "neo-org-haskell-temp" t)))
      (unwind-protect
          (with-temp-buffer
            (let ((notebook-file (expand-file-name "notes/demo.org" repo-root))
                  (neo/org-haskell-temporary-directory temp-dir))
              (neo--test-org-prepare-mlody-latex-fixture repo-root)
              (setq default-directory repo-root
                    buffer-file-name notebook-file)
              (insert "#+title: Demo\n\nHello, notebook export.\n")
              (neo/org-haskell-notebook-mode)
              (cl-letf (((symbol-function 'org-export-to-file)
                         (lambda (_backend file &rest _args)
                           (with-temp-file file
                             (insert "LATEX\n"))
                           file)))
                (let* ((path (neo/org-haskell-export-latex))
                       (build-directory (neo--org-haskell-pdf-build-directory))
                       (config-path
                        (expand-file-name
                         neo--org-haskell-arara-config-file-name
                         build-directory))
                       (config
                        (with-temp-buffer
                          (insert-file-contents config-path)
                          (buffer-string))))
                  (expect path
                          :to-equal
                          (expand-file-name
                           neo--org-haskell-latex-entry-file-name
                           build-directory))
                  (expect path
                          :to-match
                          (concat "\\`"
                                  (regexp-quote
                                   (file-name-as-directory
                                    (expand-file-name temp-dir)))))
                  (expect (file-symlink-p
                           (expand-file-name
                            (file-name-concat
                             neo--org-haskell-latex-support-relative-directory
                             "mlody")
                            build-directory))
                          :not :to-be nil)
                  (expect (file-symlink-p
                           (expand-file-name
                            (file-name-concat
                             neo--org-haskell-latex-support-relative-directory
                             "mlody-book")
                            build-directory))
                          :not :to-be nil)
                  (expect
                   (file-exists-p
                    (expand-file-name
                     (file-name-concat
                      neo--org-haskell-latex-support-relative-directory
                      "mlody-report")
                     build-directory))
                   :to-be nil)
                  (expect (file-symlink-p
                           (expand-file-name ".rules/mlodylualatex.yaml" build-directory))
                          :not :to-be nil)
                  (expect (file-symlink-p
                           (expand-file-name
                            neo--org-haskell-latexminted-support-relative-directory
                            build-directory))
                          :not :to-be nil)
                  (expect config :to-match "demo-print")
                  (expect config :to-match "demo-online")
                  (expect config :to-match "texinputs: \"\\.latex//:\"")
                  (expect config :to-match "pythonpath: \"\\.latexminted\"")
                  (expect config :to-match "texmfcache: \"\\.texmf-cache\"")
                  (expect (file-directory-p
                           (expand-file-name
                            neo--org-haskell-texmf-cache-relative-directory
                            build-directory))
                          :to-be t)
                  (expect (file-exists-p
                           (expand-file-name "mlody/docs/the-score" build-directory))
                          :to-be nil)
                  (expect (file-exists-p (expand-file-name "chapters" build-directory))
                          :to-be nil)
                  (expect (file-exists-p (expand-file-name "main.bib" build-directory))
                          :to-be nil)))))
        (delete-directory repo-root t)
        (delete-directory temp-dir t))))

  (it "uses the report backend and stages its class for an opted-in notebook"
    (let ((repo-root (make-temp-file "neo-org-haskell-repo" t))
          (temp-dir (make-temp-file "neo-org-haskell-temp" t)))
      (unwind-protect
          (with-temp-buffer
            (let ((notebook-file
                   (expand-file-name "notes/report-demo.orghs" repo-root))
                  (neo/org-haskell-temporary-directory temp-dir)
                  export-backend)
              (neo--test-org-prepare-mlody-latex-fixture repo-root)
              (setq default-directory repo-root
                    buffer-file-name notebook-file)
              (insert "#+latex_class: mlody-report\n"
                      "#+title: Report Demo\n\n"
                      "* Purpose\nTest report staging.\n")
              (neo/org-haskell-notebook-mode)
              (cl-letf (((symbol-function 'org-export-to-file)
                         (lambda (backend file &rest _args)
                           (setq export-backend backend)
                           (with-temp-file file
                             (insert "LATEX\n"))
                           file)))
                (let* ((path (neo/org-haskell-export-latex))
                       (build-directory
                        (neo--org-haskell-pdf-build-directory))
                       (report-support
                        (expand-file-name
                         (file-name-concat
                          neo--org-haskell-latex-support-relative-directory
                          "mlody-report")
                         build-directory)))
                  (expect path
                          :to-equal
                          (expand-file-name
                           neo--org-haskell-latex-entry-file-name
                           build-directory))
                  (expect export-backend :to-equal 'neo-mlody-report)
                  (expect (file-symlink-p report-support)
                          :not :to-be nil)))))
        (delete-directory repo-root t)
        (delete-directory temp-dir t))))

  (it "compiles notebook PDFs from the staged LaTeX via arara"
    (let ((repo-root (make-temp-file "neo-org-haskell-repo" t))
          (temp-dir (make-temp-file "neo-org-haskell-temp" t)))
      (unwind-protect
          (with-temp-buffer
            (let ((notebook-file (expand-file-name "notes/demo.org" repo-root))
                  (neo/org-haskell-temporary-directory temp-dir)
                  compile-call)
              (neo--test-org-prepare-mlody-latex-fixture repo-root)
              (setq default-directory repo-root
                    buffer-file-name notebook-file)
              (insert "#+title: Demo\n\nHello, notebook export.\n")
              (neo/org-haskell-notebook-mode)
              (cl-letf (((symbol-function 'org-export-to-file)
                         (lambda (_backend file &rest _args)
                           (with-temp-file file
                             (insert "LATEX\n"))
                           file))
                        ((symbol-function 'compile)
                         (lambda (command)
                           (setq compile-call (list default-directory command))
                           'compilation-buffer))
                        ((symbol-function 'message)
                         (lambda (&rest _args) nil)))
                (let ((build-directory
                       (file-name-as-directory
                        (neo--org-haskell-pdf-build-directory))))
                  (expect (neo/org-haskell-export-pdf)
                          :to-equal
                          'compilation-buffer)
                  (expect compile-call
                          :to-equal
                          (list build-directory
                                "arara --preamble draft-fast-online main.tex"))
                  (expect (file-exists-p
                           (expand-file-name
                            neo--org-haskell-latex-entry-file-name
                            build-directory))
                          :to-be-truthy)))))
        (delete-directory repo-root t)
        (delete-directory temp-dir t))))

  (it "uses the selected arara profile when interactive notebook PDF export gets a prefix arg"
    (let ((repo-root (make-temp-file "neo-org-haskell-repo" t))
          (temp-dir (make-temp-file "neo-org-haskell-temp" t)))
      (unwind-protect
          (with-temp-buffer
            (let ((notebook-file (expand-file-name "notes/demo.org" repo-root))
                  (neo/org-haskell-temporary-directory temp-dir)
                  compile-call
                  selected-profile)
              (neo--test-org-prepare-mlody-latex-fixture repo-root)
              (setq default-directory repo-root
                    buffer-file-name notebook-file)
              (insert "#+title: Demo\n\nHello, notebook export.\n")
              (neo/org-haskell-notebook-mode)
              (cl-letf (((symbol-function 'org-export-to-file)
                         (lambda (_backend file &rest _args)
                           (with-temp-file file
                             (insert "LATEX\n"))
                           file))
                        ((symbol-function 'neo--org-haskell-read-arara-profile)
                         (lambda ()
                           (setq selected-profile "print")
                           selected-profile))
                        ((symbol-function 'compile)
                         (lambda (command)
                           (setq compile-call (list default-directory command))
                           'compilation-buffer))
                        ((symbol-function 'message)
                         (lambda (&rest _args) nil)))
                (let ((build-directory
                       (file-name-as-directory
                        (neo--org-haskell-pdf-build-directory)))
                      (current-prefix-arg '(4)))
                  (expect (call-interactively #'neo/org-haskell-export-pdf)
                          :to-equal
                          'compilation-buffer)
                  (expect selected-profile :to-equal "print")
                  (expect compile-call
                          :to-equal
                          (list build-directory
                                "arara --preamble print main.tex"))))))
        (delete-directory repo-root t)
        (delete-directory temp-dir t))))

  (it "routes online and print convenience commands through the shared notebook PDF export"
    (let (calls)
      (cl-letf (((symbol-function 'neo/org-haskell-export-pdf)
                 (lambda (&optional preamble)
                   (push preamble calls)
                   preamble)))
        (expect (neo/org-haskell-export-online-pdf) :to-equal "online")
        (expect (neo/org-haskell-export-print-pdf) :to-equal "print")
        (expect (nreverse calls) :to-equal '("online" "print")))))

  (it "routes Org's standard PDF export for notebooks through the staged arara build"
    (let ((repo-root (make-temp-file "neo-org-haskell-repo" t))
          (temp-dir (make-temp-file "neo-org-haskell-temp" t)))
      (unwind-protect
          (with-temp-buffer
            (let ((notebook-file (expand-file-name "notes/demo.org" repo-root))
                  (neo/org-haskell-temporary-directory temp-dir)
                  export-call
                  compiled-tex)
              (neo--test-org-prepare-mlody-latex-fixture repo-root)
              (setq default-directory repo-root
                    buffer-file-name notebook-file)
              (insert "#+title: Demo\n\nHello, notebook export.\n")
              (neo/org-haskell-notebook-mode)
              (cl-letf (((symbol-function 'org-export-to-file)
                         (lambda (backend file async subtreep visible-only body-only ext-plist post-process)
                           (setq export-call
                                 (list backend file async subtreep visible-only body-only ext-plist))
                           (with-temp-file file
                             (insert "LATEX\n"))
                           (funcall post-process file)))
                        ((symbol-function 'neo--org-haskell-compile-pdf-file)
                         (lambda (file &optional _preamble)
                           (setq compiled-tex file)
                           "/tmp/demo-online.pdf")))
                (expect (neo--org-haskell-export-to-pdf nil t nil nil '(:foo bar))
                        :to-equal
                        "/tmp/demo-online.pdf")
                (let ((entry-path
                       (expand-file-name
                        neo--org-haskell-latex-entry-file-name
                        (neo--org-haskell-pdf-build-directory))))
                  (expect export-call
                          :to-equal
                          (list 'latex entry-path nil t nil nil '(:foo bar)))
                  (expect compiled-tex :to-equal entry-path)))))
        (delete-directory repo-root t)
        (delete-directory temp-dir t))))

  (it "uses the selected arara profile for notebook Org PDF export when prefix selection is requested"
    (let ((repo-root (make-temp-file "neo-org-haskell-repo" t))
          (temp-dir (make-temp-file "neo-org-haskell-temp" t)))
      (unwind-protect
          (with-temp-buffer
            (let ((notebook-file (expand-file-name "notes/demo.org" repo-root))
                  (neo/org-haskell-temporary-directory temp-dir)
                  selected-profile
                  compiled-call)
              (neo--test-org-prepare-mlody-latex-fixture repo-root)
              (setq default-directory repo-root
                    buffer-file-name notebook-file)
              (insert "#+title: Demo\n\nHello, notebook export.\n")
              (neo/org-haskell-notebook-mode)
              (cl-letf (((symbol-function 'org-export-to-file)
                         (lambda (_backend file _async _subtreep _visible-only _body-only _ext-plist post-process)
                           (with-temp-file file
                             (insert "LATEX\n"))
                           (funcall post-process file)))
                        ((symbol-function 'neo--org-haskell-read-arara-profile)
                         (lambda ()
                           (setq selected-profile "print")
                           selected-profile))
                        ((symbol-function 'neo--org-haskell-compile-pdf-file)
                         (lambda (file &optional preamble)
                           (setq compiled-call (list file preamble))
                           "/tmp/demo-print.pdf")))
                (let ((entry-path
                       (expand-file-name
                        neo--org-haskell-latex-entry-file-name
                        (neo--org-haskell-pdf-build-directory)))
                      (current-prefix-arg '(4)))
                  (expect (neo--org-haskell-export-to-pdf nil t nil nil '(:foo bar))
                          :to-equal
                          "/tmp/demo-print.pdf")
                  (expect selected-profile :to-equal "print")
                  (expect compiled-call :to-equal (list entry-path "print"))))))
        (delete-directory repo-root t)
        (delete-directory temp-dir t))))

  (it "intercepts Org's standard PDF export only for notebook buffers"
    (let (notebook-call fallback-call)
      (with-temp-buffer
        (neo/org-haskell-notebook-mode)
        (cl-letf (((symbol-function 'neo--org-haskell-export-to-pdf)
                   (lambda (&rest args)
                     (setq notebook-call args)
                     "/tmp/notebook.pdf")))
          (expect
           (neo--org-haskell-around-latex-export-to-pdf
            (lambda (&rest args)
              (setq fallback-call args)
              "/tmp/fallback.pdf")
            nil t nil nil '(:demo t))
           :to-equal
           "/tmp/notebook.pdf")))
      (expect notebook-call :to-equal '(nil t nil nil (:demo t)))
      (expect fallback-call :to-be nil)
      (setq notebook-call nil
            fallback-call nil)
      (with-temp-buffer
        (org-mode)
        (expect
         (neo--org-haskell-around-latex-export-to-pdf
          (lambda (&rest args)
            (setq fallback-call args)
            "/tmp/fallback.pdf")
          nil nil t nil '(:demo t))
         :to-equal
         "/tmp/fallback.pdf"))
      (expect notebook-call :to-be nil)
      (expect fallback-call :to-equal '(nil nil t nil (:demo t)))))

  (it "collects Haskell notebook blocks in document order"
    (with-temp-buffer
      (org-mode)
      (insert "#+begin_src haskell\none = 1\n#+end_src\n\n"
              "#+begin_src python\nprint('skip')\n#+end_src\n\n"
              "#+begin_src haskell :tangle no\none + 1\n#+end_src\n\n"
              "#+begin_src haskell\ntwo = one + 1\n#+end_src\n")
      (let ((blocks (neo--org-haskell-collect-document-blocks)))
        (expect (mapcar (lambda (block) (plist-get block :body)) blocks)
                :to-equal
                '("one = 1"
                  "two = one + 1")))))

  (it "resets the Org element cache before collecting notebook blocks"
    (with-temp-buffer
      (org-mode)
      (insert "#+begin_src haskell\nanswer = 42\n#+end_src\n")
      (let ((cache-reset-p nil)
            (cache-reset-count 0)
            (original-cache-reset (symbol-function 'org-element-cache-reset))
            (original-parse-buffer (symbol-function 'org-element-parse-buffer)))
        (cl-letf (((symbol-function 'org-element-cache-reset)
                   (lambda (&rest args)
                     (setq cache-reset-p t)
                     (cl-incf cache-reset-count)
                     (apply original-cache-reset args)))
                  ((symbol-function 'org-element-parse-buffer)
                   (lambda (&rest args)
                     (expect cache-reset-p :to-be-truthy)
                     (apply original-parse-buffer args))))
          (neo--org-haskell-collect-document-blocks))
        (expect cache-reset-count :to-equal 1))))

  (it "drops indentation common to every non-blank line in a block body"
    (expect (neo--org-haskell-normalize-block-body
             "  foo = do\n    pure 1\n\n  bar = foo\n")
            :to-equal
            "foo = do\n  pure 1\n\nbar = foo\n"))

  (it "executes a manual Babel block with notebook context and one main"
    (with-temp-buffer
      (setq buffer-file-name "/tmp/notebooks/manual.orghs")
      (neo/org-haskell-notebook-mode)
      (insert "#+begin_src haskell\n"
              "module Main where\n"
              "#+end_src\n\n"
              "#+begin_src haskell\n"
              "answer = 42\n"
              "#+end_src\n\n"
              "#+begin_src haskell\n"
              "main = print \"wrong\"\n"
              "#+end_src\n\n"
              "#+begin_src haskell :results output :tangle no\n"
              "main :: IO ()\n"
              "main = print answer\n"
              "#+end_src\n")
      (search-backward "main = print answer")
      (let ((params '((:tangle . "no")
                      (:compile . "no")
                      (:result-params . ("output" "replace"))))
            executed-body
            executed-params)
        (neo--org-haskell-around-babel-execute
         (lambda (body received-params)
           (setq executed-body body
                 executed-params received-params)
           "ok")
         "main :: IO ()\nmain = print answer"
         params)
        (expect executed-body :to-match "module Main where")
        (expect executed-body :to-match "answer = 42")
        (expect executed-body :not :to-match "main = print \\\"wrong\\\"")
        (expect executed-body :to-match "main = print answer")
        (expect (alist-get :compile executed-params) :to-equal "yes")
        (expect (alist-get :result-params executed-params)
                :to-equal
                '("output" "replace"))
        (expect (alist-get :compile params) :to-equal "no"))))

  (it "preserves standard Haskell Babel execution outside notebook mode"
    (with-temp-buffer
      (org-mode)
      (let* ((params '((:tangle . "no") (:compile . "no")))
             (result
              (neo--org-haskell-around-babel-execute
               (lambda (body received-params)
                 (list body received-params))
               "main = pure ()"
               params)))
        (expect result :to-equal (list "main = pure ()" params)))))

  (it "starts the notebook repl from the containing Git worktree root"
    (let ((repl-buffer (generate-new-buffer " *neo-org-ghci*"))
          (repl-directory nil))
      (unwind-protect
          (with-temp-buffer
            (setq buffer-file-name
                  "/tmp/omega-worktree/mlody/haskell/experimental/demo.orghs"
                  default-directory "/home/test/")
            (cl-letf (((symbol-function 'locate-dominating-file)
                       (lambda (start marker)
                         (expect start
                                 :to-equal
                                 "/tmp/omega-worktree/mlody/haskell/experimental/")
                         (expect marker :to-equal ".git")
                         "/tmp/omega-worktree/"))
                      ((symbol-function 'neo--haskell-ensure-standalone-repl)
                       (lambda ()
                         (setq repl-directory default-directory)
                         repl-buffer)))
              (expect (neo--org-haskell-ensure-repl) :to-equal repl-buffer)
              (expect repl-directory :to-equal "/tmp/omega-worktree/")))
        (kill-buffer repl-buffer))))

  (it "sends the current Haskell block through multiline GHCi input"
    (let ((calls nil)
          (repl-buffer (generate-new-buffer " *neo-org-ghci*")))
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (insert "#+begin_src haskell\n  foo x =\n    x + 1\n#+end_src\n")
            (search-backward "foo")
            (let ((source-buffer (current-buffer)))
              (cl-letf (((symbol-function 'neo--haskell-ensure-standalone-repl)
                         (lambda ()
                           repl-buffer))
                        ((symbol-function 'get-buffer-process)
                         (lambda (buffer)
                           (expect buffer :to-equal repl-buffer)
                           'ghci-process))
                        ((symbol-function 'comint-send-string)
                         (lambda (process string)
                           (push (list process string) calls)))
                        ((symbol-function 'message)
                         (lambda (&rest _args) nil)))
                (neo/org-haskell-send-block)
                (with-current-buffer repl-buffer
                  (expect neo--haskell-standalone-repl-source-buffer
                          :to-equal source-buffer))
                (expect calls
                        :to-equal
                        '((ghci-process ":{\nfoo x =\n  x + 1\n:}\n"))))))
        (kill-buffer repl-buffer))))

  (it "loads notebook context before sending a manual Haskell block"
    (let ((calls nil)
          (repl-buffer (generate-new-buffer " *neo-org-ghci*"))
          (temp-dir (make-temp-file "neo-org-haskell-temp" t)))
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (let ((neo/org-haskell-temporary-directory temp-dir))
              (setq buffer-file-name "/tmp/notebooks/manual.org")
              (insert "#+begin_src haskell\nanswer = 42\n#+end_src\n\n"
                      "#+begin_src haskell :tangle no\nanswer + 1\n#+end_src\n")
              (search-backward "answer + 1")
              (cl-letf (((symbol-function 'neo--haskell-ensure-standalone-repl)
                         (lambda () repl-buffer))
                        ((symbol-function 'get-buffer-process)
                         (lambda (buffer)
                           (expect buffer :to-equal repl-buffer)
                           'ghci-process))
                        ((symbol-function 'comint-send-string)
                         (lambda (process string)
                           (push (list process string) calls)))
                        ((symbol-function 'message)
                         (lambda (&rest _args) nil)))
                (neo/org-haskell-send-block)
                (let* ((sent-calls (nreverse calls))
                       (load-command (cadar sent-calls))
                       (path (read (substring load-command 6 -1)))
                       (generated
                        (with-temp-buffer
                          (insert-file-contents path)
                          (buffer-string))))
                  (expect generated :to-match "answer = 42")
                  (expect generated :not :to-match (regexp-quote "answer + 1"))
                  (expect sent-calls
                          :to-equal
                          (list
                           (list 'ghci-process load-command)
                           (list 'ghci-process ":{\nanswer + 1\n:}\n")))))))
        (delete-directory temp-dir t)
        (kill-buffer repl-buffer))))

  (it "tangles and loads all Haskell blocks into the notebook repl"
    (let ((calls nil)
          (repl-buffer (generate-new-buffer " *neo-org-ghci*"))
          (temp-dir (make-temp-file "neo-org-haskell-temp" t)))
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (let ((neo/org-haskell-temporary-directory temp-dir))
              (setq buffer-file-name "/tmp/notebooks/demo.org")
              (insert "* Notes\n"
                      "#+begin_src haskell\n  import Data.List\n#+end_src\n\n"
                      "#+begin_src haskell\n  main = print (sort [3, 1, 2])\n#+end_src\n")
              (goto-char (point-min))
              (cl-letf (((symbol-function 'neo--haskell-ensure-standalone-repl)
                         (lambda ()
                           repl-buffer))
                        ((symbol-function 'get-buffer-process)
                         (lambda (buffer)
                           (expect buffer :to-equal repl-buffer)
                           'ghci-process))
                        ((symbol-function 'comint-send-string)
                         (lambda (process string)
                           (push (list process string) calls)))
                        ((symbol-function 'message)
                         (lambda (&rest _args) nil)))
                (let* ((repl (neo/org-haskell-load-document))
                       (sent-calls (nreverse calls))
                       (command (cadar sent-calls))
                       (path (read (substring command 6 -1)))
                       (generated
                        (with-temp-buffer
                          (insert-file-contents path)
                          (buffer-string))))
                  (expect repl :to-equal repl-buffer)
                  (expect path
                          :to-match
                          (concat "\\`"
                                  (regexp-quote
                                   (file-name-as-directory
                                    (expand-file-name temp-dir)))))
                  (expect generated
                          :to-match
                          "-- Generated from /tmp/notebooks/demo\\.org by neo/org-haskell-notebook-mode\\.")
                  (expect generated
                          :to-match
                          "{-# LINE 3 \"/tmp/notebooks/demo\\.org\" #-}\nimport Data\\.List")
                  (expect generated
                          :to-match
                          (concat "{-# LINE 7 \"/tmp/notebooks/demo\\.org\" #-}\n"
                                  (regexp-quote "main = print (sort [3, 1, 2])")))
                  (expect sent-calls
                          :to-equal
                          (list (list 'ghci-process command)
                                (list 'ghci-process ":main\n")))))))
        (delete-directory temp-dir t)
        (kill-buffer repl-buffer))))

  (it "does not send :main when the notebook has no top-level main"
    (let ((calls nil)
          (repl-buffer (generate-new-buffer " *neo-org-ghci*"))
          (temp-dir (make-temp-file "neo-org-haskell-temp" t)))
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (let ((neo/org-haskell-temporary-directory temp-dir))
              (setq buffer-file-name "/tmp/notebooks/library.org")
              (insert "#+begin_src haskell\n  square x = x * x\n#+end_src\n")
              (cl-letf (((symbol-function 'neo--haskell-ensure-standalone-repl)
                         (lambda ()
                           repl-buffer))
                        ((symbol-function 'get-buffer-process)
                         (lambda (buffer)
                           (expect buffer :to-equal repl-buffer)
                           'ghci-process))
                        ((symbol-function 'comint-send-string)
                         (lambda (process string)
                           (push (list process string) calls)))
                        ((symbol-function 'message)
                         (lambda (&rest _args) nil)))
                (neo/org-haskell-load-document)
                (expect (length (nreverse calls))
                        :to-equal 1))))
        (delete-directory temp-dir t)
        (kill-buffer repl-buffer))))

  (it "shows the repl after an interactive notebook load"
    (let ((calls nil)
          (shown nil)
          (repl-buffer (generate-new-buffer " *neo-org-ghci*"))
          (temp-dir (make-temp-file "neo-org-haskell-temp" t)))
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (let ((neo/org-haskell-temporary-directory temp-dir))
              (setq buffer-file-name "/tmp/notebooks/demo.org")
              (insert "#+begin_src haskell\n  answer = 42\n#+end_src\n")
              (cl-letf (((symbol-function 'neo--haskell-ensure-standalone-repl)
                         (lambda ()
                           repl-buffer))
                        ((symbol-function 'get-buffer-process)
                         (lambda (_buffer) 'ghci-process))
                        ((symbol-function 'comint-send-string)
                         (lambda (process string)
                           (push (list process string) calls)))
                        ((symbol-function 'message)
                         (lambda (&rest _args) nil))
                        ((symbol-function 'called-interactively-p)
                         (lambda (&optional _kind) t))
                        ((symbol-function 'pop-to-buffer)
                         (lambda (buffer &rest _args)
                           (setq shown buffer)
                           buffer)))
                (neo/org-haskell-load-document)
                (expect shown :to-equal repl-buffer)
                (expect (length (nreverse calls)) :to-equal 1))))
        (delete-directory temp-dir t)
        (kill-buffer repl-buffer))))

  (it "switches to the notebook repl without loading the document"
    (let ((calls nil)
          (repl-buffer (generate-new-buffer " *neo-org-ghci*")))
      (unwind-protect
          (cl-letf (((symbol-function 'neo--org-haskell-ensure-repl)
                     (lambda ()
                       (push 'ensure-repl calls)
                       repl-buffer))
                    ((symbol-function 'neo/org-haskell-load-document)
                     (lambda ()
                       (push 'load-document calls)
                       repl-buffer))
                    ((symbol-function 'neo--org-haskell-show-repl)
                     (lambda (buffer)
                       (push (list 'show-repl buffer) calls)
                       buffer)))
            (expect (neo/org-haskell-switch-to-repl) :to-equal repl-buffer)
            (expect (nreverse calls)
                    :to-equal
                    (list 'ensure-repl (list 'show-repl repl-buffer))))
        (kill-buffer repl-buffer))))

  (it "deletes the frame after the requested number of capture exits"
    (let ((neo/org-capture-delete-frame-depth 2)
          (deleted 0))
      (cl-letf (((symbol-function 'delete-frame)
                 (lambda (&rest _) (setq deleted (1+ deleted)))))
        (neo--org-capture-delete-frame-if-needed)
        (expect neo/org-capture-delete-frame-depth :to-equal 1)
        (expect deleted :to-equal 0)
        (neo--org-capture-delete-frame-if-needed)
        (expect neo/org-capture-delete-frame-depth :to-equal 0)
        (expect deleted :to-equal 1)))))

(provide 'test-neo-org)
;;; test-neo-org.el ends here
